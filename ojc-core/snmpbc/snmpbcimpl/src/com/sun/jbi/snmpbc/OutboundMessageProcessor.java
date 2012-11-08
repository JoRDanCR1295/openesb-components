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
 * @(#)OutboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.snmpengine.SNMPRA;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Process replies/requests received from the NMR.
 * 
 * @author echou
 */

public class OutboundMessageProcessor implements Runnable {
    
    private static final Messages mMessages =
        Messages.getMessages(OutboundMessageProcessor.class);
    private static final Logger mLog = Logger.getLogger(OutboundMessageProcessor.class.getName());

    private MessageExchange msgExchange;
    private boolean testMode;
    private SNMPServiceUnitManager suManager;
    private Map<String, InboundMessageProcessorListenerEndpoint> inboundMessageExchanges;
    private ComponentContext mComponentContext;
    
    private static ThreadLocal<Denormalizer> denormalizer = new ThreadLocal<Denormalizer> () {
        protected Denormalizer initialValue() {
            try {
                return new Denormalizer();
            } catch (Exception e) {
                mLog.log(Level.SEVERE, "SNMPBC_E00720.ERROR_INITIALIZING_DENORMALIZER", e);
                return null;
            }
        }
    };
    
    public OutboundMessageProcessor(MessageExchange msgExchange,
            ComponentContext componentContext,
            boolean testMode,
            SNMPServiceUnitManager suManager,
            Map<String, InboundMessageProcessorListenerEndpoint> inboundMessageExchanges) throws Exception {
        this.msgExchange = msgExchange;
        mComponentContext = componentContext;
        this.testMode = testMode;
        this.suManager = suManager;
        this.inboundMessageExchanges = inboundMessageExchanges;
    }
    
    /**
     * Main entry point to execute this in a thread.
     * Calls accept on the JBI NMR and process the MessageExchange it receives.
     * Delegates the real work of processing the MessageExchange to <code>execute</code>
     */
    public void run() {
        if (msgExchange == null) {
            mLog.log(Level.SEVERE, "SNMPBC_E00721.UNEXPECTED_ERROR_NULL_MESSAGE_EXCHANGE_FROM_NMR");
            return;
        }
        
        try {
            String exchangeId = msgExchange.getExchangeId();
            
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, 
                        "SNMPBC_R00717.NMR_ACCEPT_MXCH",
                        new Object[]{exchangeId});
            }

            // Determine if the message is a request or a reply
            InboundMessageProcessorListenerEndpoint inboundListener = 
                    inboundMessageExchanges.remove(exchangeId);
            if (inboundListener != null) {
                // processing inbound reply
                
                if (msgExchange instanceof InOnly) {
                    // InOnly exchange
                    processInOnly(exchangeId, inboundListener, (InOnly) msgExchange);
                    
                } else if (msgExchange instanceof InOut) {
                    // InOut exchange
                    processInOut(exchangeId, inboundListener, (InOut) msgExchange);
                    
                } else {
                    // unsupported exchange
                    mLog.log(Level.SEVERE, "SNMPBC_E00722.INVALID_MEP", exchangeId);
                    return;
                }
                
            } else {
                // process outbound msg
                
                if (msgExchange instanceof InOnly) {
                    // InOnly exchange
                    processInOnlyOutbound((InOnly) msgExchange);
                    
                } else if (msgExchange instanceof InOut) {
                    // InOut exchange
                    processInOutOutbound(exchangeId, (InOut) msgExchange);
                    
                } else {
                    // unsupported exchange
                    mLog.log(Level.SEVERE, "SNMPBC_E00722.INVALID_MEP", exchangeId);
                    return;
                }
                
            }

            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, 
                         "SNMPBC_R00718.NMR_COMPLETED_MXCH",
                          new Object[]{exchangeId});
            }
        } catch (Throwable ex) {
            mLog.log(Level.SEVERE, 
                    "SNMPBC_E00719.UNEXPECTED_ERROR",
                     ex);
            return;
        }
        
    }

    private void processInOnly(String exchangeId, 
            InboundMessageProcessorListenerEndpoint inboundListener, 
            InOnly msgExchange) throws Exception {
        
        SNMPRA snmpRA = inboundListener.getMOF().getRA();
        if (msgExchange.getStatus() == ExchangeStatus.DONE) {
            //mLog.log(Level.INFO, "got done msg from NMR");
            
            // call SNMPRA
            if (testMode) {
                inboundListener.getMOF().getTestMBean().trapReply(true);
            } else {
                String batchId = inboundListener.getExchangeToBatchId().remove(exchangeId);
                if (batchId == null) {
                    throw new Exception(mMessages.getString("SNMPBC_E00723.INVALID_BATCH_DONE_REPLY", batchId));
                }
                snmpRA.replyTrap(batchId, false);
            }
            
        } else if (msgExchange.getStatus() == ExchangeStatus.ERROR) {
            //mLog.log(Level.INFO, "got error msg from NMR");
            
            // call SNMPRA
            if (testMode) {
                inboundListener.getMOF().getTestMBean().trapReply(false);
            } else {
                String batchId = inboundListener.getExchangeToBatchId().remove(exchangeId);
                if (batchId == null) {
                    throw new Exception(mMessages.getString("SNMPBC_E00723.INVALID_BATCH_DONE_REPLY", batchId));
                }
                snmpRA.replyTrap(batchId, true);
            }
            
        } else {
            mLog.log(Level.SEVERE, "SNMPBC_E00724.INVALID_EXCHANGE_STATUS_FOR_INONLY_REPLY",
                    exchangeId);
            return;
        }
    }
    
    private void processInOut(String exchangeId,
            InboundMessageProcessorListenerEndpoint inboundListener, 
            InOut msgExchange) throws Exception {
        
        String queryId = inboundListener.getExchangeToQueryMap().remove(exchangeId);
        if (queryId == null) {
            throw new Exception(mMessages.getString("SNMPBC_E00725.INVALID_QUERY_REPLY"));
        }
        
        MOF mof = inboundListener.getMOF();
        SNMPRA snmpRA = mof.getRA();
        if (msgExchange.getStatus() == ExchangeStatus.ACTIVE) {
            // handle response
            NormalizedMessage msg = msgExchange.getOutMessage();
            
            Source source = denormalizer.get().denormalize(msg, mof, false);
            
            // call SNMPRA
            if (testMode) {
                inboundListener.getMOF().getTestMBean().replyMetadata(queryId, source, true);
            } else {
                snmpRA.replyMetadata(queryId, source);
            }
            
            msgExchange.setStatus(ExchangeStatus.DONE);
            mComponentContext.getDeliveryChannel().send(msgExchange);

        } else if (msgExchange.getStatus() == ExchangeStatus.ERROR) {
            // handle fault
            Fault fault = msgExchange.getFault();
            
            Source source = fault.getContent();
            
            // call SNMPRA
            if (testMode) {
                inboundListener.getMOF().getTestMBean().replyMetadata(queryId, source, false);
            } else {
                snmpRA.replyMetadata(queryId, source);
            }
            
            msgExchange.setStatus(ExchangeStatus.DONE);
            mComponentContext.getDeliveryChannel().send(msgExchange);

        } else {
            mLog.log(Level.SEVERE, "SNMPBC_E00726.INVALID_EXCHANGE_STATUS_FOR_INOUT_REPLY",
                    exchangeId);
            return;
        }
    }
    
    private void processInOnlyOutbound(InOnly msgExchange) throws Exception {
        msgExchange.setError(new Exception(mMessages.getString("SNMPBC_E00727.INONLY_MEP_NOT_SUPPORTED_FOR_OUTBOUND_MSGS")));
        mComponentContext.getDeliveryChannel().send(msgExchange);
    }
    
    private void processInOutOutbound(String exchangeId,
            InOut msgExchange) throws Exception {
        if (msgExchange.getStatus() == ExchangeStatus.DONE) {
            return;
        } else if (msgExchange.getStatus() == ExchangeStatus.ERROR) {
            mLog.log(Level.SEVERE, "SNMPBC_E00728.GOT_OUTBOUND_INOUT_ERROR_STATUS",
                     exchangeId);
            return;
        }
        
        String pmKey = PM.getEndpointKey(
                msgExchange.getEndpoint().getServiceName(),
                msgExchange.getEndpoint().getEndpointName());
        PM pm = suManager.getPMs().get(pmKey);
        if (pm == null) {
            throw new Exception(mMessages.getString("SNMPBC_E00729.UNABLE_TO_FIND_PM_FOR_ENDPOINT",
                                                    new Object[] {msgExchange.getEndpoint().getServiceName(),
                                                                  msgExchange.getEndpoint().getEndpointName()}));
        }
        
        MOF mof = suManager.getMofs().get(pm.getMOFIdRef());
        if (mof == null) {
            throw new Exception(mMessages.getString("SNMPBC_E00730.UNABLE_TO_FIND_MOF_WITH_GIVEN_PMKEY",
                                                    new Object[] {pm.getMOFIdRef(), pmKey}));
        }
        SNMPRA snmpRA = mof.getRA();
        NormalizedMessage msg = msgExchange.getInMessage();
        Source source = denormalizer.get().denormalize(msg, pm, true);
        
        // save msgExchange to hashtable, so it can be lookup during Callback method
        mof.getInboundMessageProcessorListenerEndpoint().putOutboundReplyContextMap(
                exchangeId, msgExchange, pm);
        
        // call SNMPRA
        if (testMode) {
            mof.getTestMBean().requestPM(exchangeId, source);
        } else {
            snmpRA.requestPM(exchangeId, source);
        }    
    }
    
}

