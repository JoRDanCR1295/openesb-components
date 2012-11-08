/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)InboundMessageProcessorListenerEndpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.snmpbc.extensions.SNMPOperation;
import com.sun.jbi.snmpengine.SNMPCallback;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;

import java.io.StringWriter;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * SNMP to NMR Inbound message processor
 * 
 * @author echou
 */
public class InboundMessageProcessorListenerEndpoint implements SNMPCallback {

    private static final Messages mMessages =
        Messages.getMessages(InboundMessageProcessorListenerEndpoint.class);
    private static final Logger mLog =
        Logger.getLogger(InboundMessageProcessorListenerEndpoint.class.getName());
    
    private MOF mof;
    private ComponentContext context;
    private SNMPServiceUnitManager suManager;
    
    private MessageExchangeFactory messageXchangeFactory = null;
    private DeliveryChannel dc;
    
    // mapping of exchangeId to queryId, so we know which queryId is the
    // replying msg for, without having to denormalize msg
    // <exchangeId, queryId>
    private ConcurrentMap<String, String> exchangeToQueryMap;
    
    // Maps an exchangeID to a batchID so that the done signal can be propagated to
    // the SNMP enginek
    private ConcurrentMap<String, String> exchangeToBatchId;
    
    // mapping of exchangeId to OutboundReplyContext
    // <exchangeId, OutboundReplyContext
    private ConcurrentMap<String, OutboundReplyContext> outboundReplyContextMap;
    
    // thread local variable for Normalizer
    private static ThreadLocal<Normalizer> normalizer = new ThreadLocal<Normalizer> () {
        protected Normalizer initialValue() {
            try {
                return new Normalizer();
            } catch (Exception e) {
                mLog.log(Level.SEVERE, "SNMPBC_E00707.ERROR_INITIALIZING_NORMALIZER", e);
                return null;
            }
        }
    };
    
    /** Creates a new instance of InboundMessageProcessorEndpoint 
     * @param mof 
     * @param suManager 
     * @throws java.lang.Exception 
     */
    public InboundMessageProcessorListenerEndpoint(MOF mof,
            SNMPServiceUnitManager suManager) throws Exception{
        this.mof = mof;
        this.suManager = suManager;
        
        context = suManager.getComponentContext();
        dc = context.getDeliveryChannel();
        
        exchangeToQueryMap = new ConcurrentHashMap<String, String> ();
        exchangeToBatchId = new ConcurrentHashMap<String, String> ();
        outboundReplyContextMap = new ConcurrentHashMap<String, OutboundReplyContext> ();
    }
    
    public void getMetaData(String queryId, Source query) {
        try {
            // save queryId 
            send(mof, query, exchangeToQueryMap, queryId);
            
        } catch (Exception e) {
            // log & possibly throw runtime exception to ra as final resort
            mLog.log(Level.SEVERE, 
                    "SNMPBC_E00706.ON_MESSAGE_FAILED", 
                    e);
        }
    }

    public void deliverTraps(String batchId, String trapProcessorID, Source traps) {                            
        try {
            SNMPAdaptation snmpAdaptation = 
                    suManager.getAdaptations().get(trapProcessorID);
            if (snmpAdaptation == null) {
                mLog.log(Level.SEVERE, "SNMPBC_E00708.ERROR_PROCESSING_TRAP_UNKNOWN_ADAPTION",
                         trapProcessorID);
                return;
            }
            
            send(snmpAdaptation, traps, exchangeToBatchId, batchId);
            
        } catch (Exception e) {
            // log & possibly throw runtime exception to ra as final resort
            mLog.log(Level.SEVERE, 
                    "SNMPBC_E00706.ON_MESSAGE_FAILED", 
                    e);
            return;
        }
    }

    public void replyPM(String requestId, Source response) {
        try {
            OutboundReplyContext replyContext = outboundReplyContextMap.remove(requestId);
            if (replyContext == null) {
                mLog.log(Level.SEVERE, "SNMPBC_E00709.ERROR_PROCESSING_INCOMING_REPLYPM",
                         requestId);
                return;
            }
            
            // create outmessage and send it
            if (replyContext.msgExchange instanceof InOut) {
                InOut msgExchange = (InOut) replyContext.msgExchange;
                NormalizedMessage outMsg = msgExchange.createMessage();
                normalizer.get().normalize(response,
                    outMsg,
                    replyContext.op,
                    false);
                msgExchange.setOutMessage(outMsg);

                context.getDeliveryChannel().send(msgExchange);
            } else {
                mLog.log(Level.SEVERE,
                         "SNMPBC_E00710.ERROR_PROCESSING_INCOMING_REPLYPM_ONLY_INOUT_SUPPORTED");
                return;
            }
        } catch (Exception e) {
            mLog.log(Level.SEVERE,
                    "SNMPBC_E00706.ON_MESSAGE_FAILED",
                    e);
            return;
        }
    }
    
    private void send(GenericOperation genericOp,
            Source xmlSource, Map<String, String> exchangeToSomethingId,
            String somethingId) throws Exception {
        String msgXchangeID = null;
        try {            
            // Create MessageExchange first base on MEP
            String mep = genericOp.getSNMPOperation().getMEP();

            // Send the MessageExchange to the NMR
            ServiceEndpoint se = context.getEndpoint(genericOp.getServiceName(),
                    genericOp.getEndpointName());
            if (se == null) {
                // Failed to locate provider endpoint
                String errMsg = mMessages.getString("SNMPBC_E00703.ENDPOINT_NOTFOUND",
                                                    new Object[]{genericOp.getServiceName(),
                                                                 genericOp.getEndpointName()});
                throw new MessagingException(errMsg);
            }

            MessageExchange msgXchange = createMessageExchange(mep);
            msgXchangeID = msgXchange.getExchangeId();

            NormalizedMessage normalizedMsg = msgXchange.createMessage();

            boolean isOperationInput = true;
            normalizer.get().normalize(xmlSource,
                    normalizedMsg,
                    genericOp,
                    isOperationInput);

            if (msgXchange instanceof InOnly) {
                ((InOnly)msgXchange).setInMessage(normalizedMsg);
            } else if (msgXchange instanceof InOut) {
                ((InOut)msgXchange).setInMessage(normalizedMsg);
            }

            if (mLog.isLoggable(Level.FINE)) {
                Source inContentSrc = null;
                if (msgXchange instanceof InOnly) {
                    inContentSrc = ((InOnly)msgXchange).getInMessage().getContent();
                } else if (msgXchange instanceof InOut) {
                    inContentSrc = ((InOut)msgXchange).getInMessage().getContent();
                }
                logNormalizedMessage(msgXchangeID, inContentSrc);
            }
            
            
            suManager.getInboundMsgExchanges().put(msgXchangeID, this);
            
            if (somethingId != null) {
                exchangeToSomethingId.put(msgXchangeID, somethingId);
            }

            sendMessageToNMR(se, msgXchange, genericOp.getSNMPOperation());

        } catch (Exception e) {
            
            suManager.getInboundMsgExchanges().remove(msgXchangeID);
            
            if (somethingId != null) {
                exchangeToSomethingId.remove(msgXchangeID);
            }

            throw e;
        }
        
    }

    
    private MessageExchange createMessageExchange(String mepType) 
    throws Exception {
        MessageExchange msgEx = null;
        
        if (messageXchangeFactory == null) {
            messageXchangeFactory = context.getDeliveryChannel()
                                           .createExchangeFactory();
        }

        try {
            if (mepType.equals(SNMPOperation.IN_ONLY)) {
                msgEx = messageXchangeFactory.createInOnlyExchange();
            } else if (mepType.equals(SNMPOperation.IN_OUT)) {
                msgEx = messageXchangeFactory.createInOutExchange();
            } else {
//                 mLog.log(Level.SEVERE,
//                         "SNMPBC_E00701.MXCH_CREATE_UNSUPPORTED_MEP",
//                         new Object[]{mepType});

                throw new Exception(mMessages.getString(
                        "SNMPBC_E00701.MXCH_CREATE_UNSUPPORTED_MEP",
                        new Object[]{mepType}));                
            }
        } catch (MessagingException ex) {
//                 mLog.log(Level.SEVERE,
//                         "SNMPBC_E00702.MXCH_CREATE_FAILED",
//                         new Object[]{mepType, ex});

                throw new Exception(mMessages.getString("SNMPBC_E00702.MXCH_CREATE_FAILED", mepType),
                                    ex);
        }
        
        return msgEx;
    }    

    /**
     * Send a MessageExchange to the NMR
     * @param se 
     * @param msgXchange 
     * @param snmpOp 
     * @throws java.lang.Exception 
     */
    private void sendMessageToNMR(
            ServiceEndpoint se,
            MessageExchange msgXchange,
            SNMPOperation snmpOp) throws Exception {
         
        // Set the ServiceEndpoint and Operation "delivery address" on message exchange
        QName snmpBindingOpQName = new QName(se.getServiceName().getNamespaceURI(),
                                            snmpOp.getBindingOperation().getName());
        msgXchange.setEndpoint(se);
        msgXchange.setOperation(snmpBindingOpQName);
        
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, 
                     "SNMPBC_R00704.SENDING_MESSAGE_EXCHANGE",
                    new Object[]{msgXchange.getExchangeId(),
                                 se.getServiceName().toString()+se.getEndpointName(),
                                 snmpBindingOpQName.toString()});
        }
        
        dc.send(msgXchange);
    }
        
    private void logNormalizedMessage(String exchangeId, Source msgSrc) {
        StringWriter out = null;
        if (msgSrc != null) {
            try {
                TransformerFactory tFactory = TransformerFactory.newInstance();
                Transformer trans = tFactory.newTransformer();
                trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
                trans.setOutputProperty(OutputKeys.INDENT, "yes");
                trans.setOutputProperty(OutputKeys.METHOD, "xml");
                trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
                out = new StringWriter();
                StreamResult result = new StreamResult(out);
                trans.transform(msgSrc, result);
                out.flush();
                out.close();
            } catch (Throwable t) {
                ;;
            }
        }

        if (mLog.isLoggable(Level.FINE)) {
            mLog.log (Level.FINE, 
                      "SNMPBC_R00705.NORMALIZED_MESSAGE_CONTENT_DUMP",
                      new Object[] {exchangeId, (out==null?"null":out.toString())});
        }
    }    

    public MOF getMOF() {
        return mof;
    }
    
    public ConcurrentMap<String, String> getExchangeToQueryMap() {
        return exchangeToQueryMap;
    }

    public ConcurrentMap<String, String> getExchangeToBatchId() {
        return exchangeToBatchId;
    }

    public void putOutboundReplyContextMap(String exchangeId,
            MessageExchange msgExchange, GenericOperation op) {
        outboundReplyContextMap.put(exchangeId,
                new OutboundReplyContext(msgExchange, op));
    }
    
    static class OutboundReplyContext {
        
        MessageExchange msgExchange;
        GenericOperation op;
        
        OutboundReplyContext(MessageExchange msgExchange, GenericOperation op) {
            this.msgExchange = msgExchange;
            this.op = op;
        }
    }
}
