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
 * @(#)InboundMessageProcessor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc;



import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import com.sun.jbi.internationalization.Messages;


/**
 * author : Gary Zheng
 */
public class InboundMessageProcessor implements Runnable,
        MessageExchangeReplyListener {
    private static final String LDAP_PROD_NAME = "LDAP";
        
    private static final Messages mMessages = Messages.getMessages(InboundMessageProcessor.class);
    private static final Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);
    private static final Map mInboundExchanges = Collections.synchronizedMap(new HashMap());
    Map mEndpoint;
    EndpointImpl epb;
    DocumentBuilder mDocBuilder;
    private DeliveryChannel mChannel;
    private MessageExchange mExchange;
    private ComponentContext mContext;
    private MessageExchangeFactory mMsgExchangeFactory;
    private ServiceEndpoint mServiceEndpoint;
    private final QName mOperation;
    private AtomicBoolean mMonitor;
    private long mPollMilliSeconds;
    
    public InboundMessageProcessor(final DeliveryChannel chnl, final EndpointImpl endpoint,
            final ComponentContext context, final QName opname)
            throws ParserConfigurationException {
        mChannel = chnl;
        epb = endpoint;
        mContext = context;
        mOperation = opname;
        mMonitor = new AtomicBoolean(false);
        
        final DocumentBuilderFactory docBuilderFact = DocumentBuilderFactory.newInstance();
        mDocBuilder = docBuilderFact.newDocumentBuilder();
    }
    
    /**
     *
     * @return
     */
    public static Map getInboundExchanges() {
        return InboundMessageProcessor.mInboundExchanges;
    }
    
    /**
     *
     */
    public void run() {
        if (InboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
            InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00401.IMP_EP_status"));
        }
        
        do {
            try {
                execute();
            } catch (final Exception ex) {
                InboundMessageProcessor.mLogger.log(Level.SEVERE, mMessages.getString("Unexpected exception", new Object[] {ex}));
            }
            
            if (InboundMessageProcessor.mLogger.isLoggable(Level.FINE)) {
                InboundMessageProcessor.mLogger.fine(mMessages.getString("Finished processing:"));
            }
            
            try {
                Thread.sleep(mPollMilliSeconds);
            } catch (final Exception e) {
                InboundMessageProcessor.mLogger.log(Level.SEVERE,
                        mMessages.getString("Unexpected exception Occured after sleep", new Object[] {e}));
            }
        } while (mMonitor.get() != Boolean.TRUE);
    }
    
    /**
     *
     * @throws MessagingException
     * @throws Exception
     */
    public void execute() throws MessagingException, Exception {
        String exchangeId = null;
        
        try {
            if (mMsgExchangeFactory == null) {
                mMsgExchangeFactory = mChannel.createExchangeFactory();
            }
            
            mExchange = mMsgExchangeFactory.createInOnlyExchange();
            
            if (mServiceEndpoint == null) {
                mServiceEndpoint = locateServiceEndpoint();
                epb.setValueObj(EndpointImpl.ENDPOINT_REFERENCE,
                        mServiceEndpoint);
            }
            
            if (mServiceEndpoint == null) {
                throw new MessagingException(InboundMessageProcessor.mMessages.getString(
                        "LDAPBC-E00402.IMP_Failed_locate_EP"));
            }
            
            exchangeId = mExchange.getExchangeId();
            
            final QName serviceName = (QName) epb.getValueObj(EndpointImpl.FULL_SERVICE_NAME);
            final String epntName = epb.getValue(EndpointImpl.ENDPOINT_NAME);
            
            if (InboundMessageProcessor.mLogger.isLoggable(Level.FINE)) {
                InboundMessageProcessor.mLogger.log(Level.FINE,mMessages.getString("Getting bean for", new Object[] {serviceName,epntName}));
            }
            
            mExchange.setEndpoint(mServiceEndpoint);
            mExchange.setOperation(mOperation);
            
            final String status = epb.getValue(EndpointImpl.STATUS);
            
            if (status != EndpointImpl.STATUS_RUNNING) {
                final String endName = epb.getValue(EndpointImpl.ENDPOINT_NAME);
                
                if (InboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
                    InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00402.IMP_EP_NOT_RUNNING",
                            new Object[] { endName, mExchange.getExchangeId() }));
                }
            } else {
                final URI pattern = mExchange.getPattern();
                final String pat = pattern.toString().trim();
                
                if (pat.equals(MEP.IN_OUT.toString())) {
                    InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00403.IMP_Received_INOUT",
                           new Object[] {mExchange.getExchangeId()}));
                    processInOut(mExchange, epb);
                } else if (pat.equals(MEP.IN_ONLY.toString())) {
                    InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00404.IMP_Received_INONLY",
                           new Object[] { mExchange.getExchangeId()}));
                    processInOnly(mExchange, epb);
                } else {
                    InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00405.IMP_Invalid_patter",
                           new Object[] {mExchange.getExchangeId()}));
                    
                    return;
                }
            }
        } catch (final MessagingException ex) {
            InboundMessageProcessor.mInboundExchanges.remove(exchangeId);
            throw ex;
        } catch (final Exception e) {
            InboundMessageProcessor.mInboundExchanges.remove(exchangeId);
            throw e;
        }
    }
    
    /**
     *
     * @param exchange
     * @param epb
     */
    public void processInOut(final MessageExchange exchange, final EndpointImpl epb) {
    }
    
    /**
     *
     * @param exchange
     * @param epb
     */
    public void processInOnly(final MessageExchange exchange,
            final EndpointImpl epb) {
        String exchangeId = null;
        String jndiName = null;
        
        try {
            epb.getEndpointStatus().incrementReceivedRequests();
            
            NormalizedMessage inMsg = mExchange.createMessage();
            exchangeId = exchange.getExchangeId();
            
            final Map operationNameToMetaData = (Map) epb.getValueObj(EndpointImpl.OPERATION_NAME_TO_META_DATA);
            final OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(exchange.getOperation()
            .getLocalPart());
            
            if (meta == null) {
                throw new MessagingException(InboundMessageProcessor.mMessages.getString(
                        "LDAPBC-E00401.IMP_Invalid_Operation",
                        new Object[] { exchange.getOperation() }));
            }
        } catch (final Exception ex) {
            InboundMessageProcessor.mLogger.log(Level.WARNING, mMessages.getString("Exception in processing reply. ", new Object[] {ex}));
        } finally {
        }
    }

    /**
     *
     * @return
     */
    private ServiceEndpoint locateServiceEndpoint() {
        ServiceEndpoint activatedEndpoint = null;
        final QName serviceName = (QName) epb.getValueObj(EndpointImpl.FULL_SERVICE_NAME);
        final String endpointName = epb.getValue(EndpointImpl.ENDPOINT_NAME);
        activatedEndpoint = mContext.getEndpoint(serviceName, endpointName);
        
        if (activatedEndpoint != null) {
            InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00406.IMP_locate_EP",
                    new Object[] { serviceName, endpointName }));
        }
        
        return (activatedEndpoint);
    }
    
    
    /**
     * @param exchange
     * @throws Exception
     */
    public synchronized void processReplyMessage(final MessageExchange exchange)
    throws Exception {
        String sql = null;
        String jndiName = null;
        
        if (!(exchange instanceof InOnly) && !(exchange instanceof InOut)) {
            InboundMessageProcessor.mLogger.log(Level.SEVERE , mMessages.getString("LDAPBC-E00402.IMP_Unsupported_exchange_pattern",
                   new Object[] {exchange.getPattern().toString()}));
            throw new Exception(mMessages.getString("LDAPBC-E00402.IMP_Unsupported_exchange_pattern"));
        }
        
        final String messageId = exchange.getExchangeId();
        
        if (InboundMessageProcessor.mInboundExchanges.containsKey(messageId)) {
            // Any status other than 'DONE' is considered an error
            if (exchange.getStatus() != ExchangeStatus.DONE) {
                final String msgError = "Error occured while getting DONE Response ";
                throw new Exception(mMessages.getString(msgError));
            }
            
            if (exchange.getStatus() == ExchangeStatus.DONE) {
                try {
                } catch (final Exception e) {
                    InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00407.IMP_Remove_exchange_msg_id",
                          new Object[] { e.toString()}));
                    
                    final String msg = InboundMessageProcessor.mMessages.getString("LDAPBC-R00408.IMP_Failed_Executing_SQL");
                    throw new Exception(msg, e);
                } finally {
                } // finally
            }
            
            if (InboundMessageProcessor.mLogger.isLoggable(Level.INFO)) {
                InboundMessageProcessor.mLogger.log(Level.INFO,mMessages.getString("LDAPBC-R00407.IMP_Remove_exchange_msg_id", new Object[] {messageId}));
            }
            InboundMessageProcessor.mInboundExchanges.remove(messageId);
        } else {
            InboundMessageProcessor.mLogger.log(Level.SEVERE, mMessages.getString("LDAPBC-R00409.IMP_Invalid_reply_msgId",new Object[] {messageId}));
        }
    }
    
    
    public void stopReceiving() {
        InboundMessageProcessor.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00410.IMP_Inbound_stopped"));
        mMonitor.set(Boolean.TRUE);
    }
}

