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

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;
import com.sun.jbi.httpsoapbc.util.LoggingMonitoringUtil;
import com.sun.jbi.httpsoapbc.util.GUIDUtil;
import com.sun.jbi.httpsoapbc.util.Util;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIConstants;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIValidationException;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.soap.api.SOAPMsgValidator;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.soap.api.SOAPValidatorFactory;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.xml.wss.SubjectAccessor;
import com.sun.xml.wss.XWSSecurityException;

import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

import javax.activation.DataSource;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.security.auth.Subject;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.handler.MessageContext;
import javax.xml.xpath.XPathExpression;

import net.java.hulp.measure.Probe;

/**
 * Processes inbound messages. Beware that replies come back the same route as
 * outbound requests and will be processed by the OutboundMessageProcessor
 */
public class InboundMessageProcessor implements RedeliveryListener {

    private static final Messages mMessages =
        Messages.getMessages(InboundMessageProcessor.class);
    private final static Logger mLog =
        Messages.getLogger(InboundMessageProcessor.class);
    
    private MessagingChannel mChannel;
    private ComponentContext mContext;
    private MessageExchangeFactory mMessageExchangeFactory;
    private Endpoint mEndpointBean;
    private ServiceEndpoint mEndpointReference;
    private ReplyListener mReplyListener;
    private Normalizer mNormalizer;
    private Object mInput;
    private Transaction mTransaction;
    private SOAPMsgValidator mSoapValidator;
    private MessageContext mMessageContext;
    private Subject subject;
    
    // measurements
    private Probe mSoapToNMRMeasurement = null;
    
    // Use collection that avoids locking
    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());
    private static Map mExchangeIDToMeta = Collections.synchronizedMap(new HashMap());


   // message tracking 
    private boolean mMonitorEnabled = false;
    private String mMode = LoggingMonitoringUtil.getMessageTrackingIDModeInbound();  
    
    public InboundMessageProcessor(Normalizer normalizer, ReplyListener replyListener) 
            throws MessagingException {
        mNormalizer = normalizer;
        mReplyListener = replyListener;
        // At this point the binding component lifecycle might not be started yet.
        // Try to initialize anyway in case it is.
        try {
            ensureInitialized(false);
        } catch (Exception ex) {
            if (mLog.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-E00741.Initialization_error", ex.getLocalizedMessage());
                mLog.log(Level.WARNING, text, ex);
                AlertsUtil.getAlerter().critical(text, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                getServiceUnitID(), 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-E00741");
            }
        }
        
        //Create the validator to test for WSI BP 1.0 conformance.
        try {
            mSoapValidator = SOAPValidatorFactory.getInstance().createSOAPMsgValidator(WSIConstants.BPv10,
                                                                                      WSIConstants.ERROR);
        } catch (WSIValidationException e) {
            throw new MessagingException(e);
        }
    }

    /**
     * Set the input to process. The normalizer set must understand this type of input.
     */
    public void setInput(Object input) {
        mInput = input;
    }

    /**
     * Set the transaction to be associated to the request/exchange this
     * processor will execute.
     * 
     * @param transaction Transaction reference, may be null.
     */
    public void setTransaction(Transaction transaction) {
        mTransaction = transaction;
    }
    
    /**
     * Set the target endpoint the message processor should forward the request to.
     */
    public void setTargetEndpoint(Endpoint targetEndpoint) {
        mEndpointBean = targetEndpoint;
        mMonitorEnabled = LoggingMonitoringUtil.isMonitorEnabled(mEndpointBean);
        mEndpointReference = null;
    }
    
    public void setMessageContext(MessageContext context) {
        mMessageContext = context;
    }

    public static Map getInboundExchanges() {
        return mInboundExchanges;
    }
    
    /**
     * Attempt to initialize.
     * @param failIfNotReady whether the initialization should throw exceptions if everything isn't
     * ready to complete the initialization
     * @return true if it was able to initialize
     */
    boolean ensureInitialized(boolean failIfNotReady) throws MessagingException {
        String endpointName = null;
        
        if (mEndpointReference == null) {
            if (mContext == null)  {
                mContext = HttpSoapComponentContext.getInstance().getContext();
            }
            
            if (mContext != null) {
                if (mChannel == null) {
                    mChannel = HttpSoapComponentContext.getInstance().getBindingChannel();
                }

                // Get all activated endpoints for a given service,
                // Explicitly choose which endpoint to route to                    
                if (mChannel != null) {
                    if (mMessageExchangeFactory == null) {
                        mMessageExchangeFactory = mChannel.createExchangeFactory();
                    }                    
                    if (mEndpointBean != null) {
                        QName fullServiceName = (QName) mEndpointBean.getServiceName();
                        endpointName = mEndpointBean.getEndpointName();
                        mEndpointReference = mContext.getEndpoint(fullServiceName, endpointName);
                    } else {
                        if (failIfNotReady) {
                            throw new MessagingException("Cannot initialize, no target endpoint has yet been provided");
                        }
                    }
                } else {
                    if (failIfNotReady) {
                        throw new MessagingException("Cannot initialize, failed to acquire binding channel from component context");
                    }
                }
            } else {
                if (failIfNotReady) {
                    throw new MessagingException("Cannot initialize, failed to acquire componnt context");
                }
            }
        }
        
        if (failIfNotReady && mEndpointReference == null) {
            String msg = mMessages.getString("HTTPBC-E00151.Provisioner_not_found", endpointName);
            AlertsUtil.getAlerter().warning(msg, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            getServiceUnitID(), 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00151");
            throw new MessagingException(msg);
        }
        
        return (mEndpointReference != null);
    }
    
    /**
     * @param clientContext - a user supplied context that will be returned with the message exchange ID
     * @return the JBI message exchange ID. Beware though that a reply might already come back before this method returns, 
     * use the setMessageExchangeId call-back to get notified earlier of this ID, before the request is sent to the NMR.
     */
    public String execute(Object clientContext) throws MessagingException {
        try {
            ensureInitialized(true);
        } catch (MessagingException ex) {
            mLog.log(Level.SEVERE, ex.getMessage(), ex);
            throw ex;
        }
        
        // There is SOAP coupling here, and it would have been preferable to
        // implement a separate InboundMessageProcessor to handle non-SOAP
        // requests. But, given time constraints to rework all the code that
        // instantiate this specific class (e.g, to use instead some sort of
        // message processor factory scheme), this is a workaround until
        // the BC can be refactored.
        //SOAPMessage soapMsg = mNormalizer.toSOAPMessage(mInput);        
        if (SOAPMessage.class.isInstance(mInput)) {
            return executeSOAP(clientContext);
        } else if (DataSource.class.isInstance(mInput) || mInput == null) {
            String verb = String.valueOf(mMessageContext.get(MessageContext.HTTP_REQUEST_METHOD));
            if ("GET".equals(verb)) {
                return executeGet(clientContext);
            } else if ("POST".equals(verb)) {
                return executePost(clientContext);
            } else {
                String error = mMessages.getString("HTTPBC-E00749.Invalid_HTTP_verb", verb);
                AlertsUtil.getAlerter().warning(error, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                getServiceUnitID(), 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-E00749");
                throw new MessagingException(error);
            }
        } else {
            String error = mMessages.getString("HTTPBC-E00750.Unsupported_message_type", mInput.getClass().toString());
            AlertsUtil.getAlerter().warning(error, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            getServiceUnitID(), 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00750");
            throw new MessagingException(error);
        }
    }
    
    void sendInOnly(InOnly inonly, NormalizedMessage inMsg, OperationMetaData operationMetaData) 
        throws MessagingException {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Sending message " + inonly.getExchangeId());
        }

        checkAndGenerateInMemoryCRMP(inonly, inMsg);
        populateAddressUrlProperty(inMsg);
        
        String exchangeId = null;        
        inonly.setEndpoint(mEndpointReference);
        inonly.setOperation(mEndpointBean.createOperationAddress(operationMetaData));
        try {
            inonly.setInMessage(inMsg);
            exchangeId = inonly.getExchangeId();
            mInboundExchanges.put(exchangeId, new Long(System.currentTimeMillis()));      
            mChannel.send(inonly);
            mEndpointBean.getEndpointStatus().incrementSentRequests();
        } catch(MessagingException ex) {
            String error = mMessages.getString("HTTPBC-E00771.Failed_send_inonly");
            mLog.log(Level.SEVERE, error, ex);
            AlertsUtil.getAlerter().warning(error, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             getServiceUnitID(), 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00771");
            if (exchangeId != null) {
                mInboundExchanges.remove(exchangeId);
                MessageExchangeSupport.removeRedeliveryListener(exchangeId);
            }
            throw ex;
        }
        return;
    }
    
    
    void sendInOut(InOut inout, NormalizedMessage inMsg, OperationMetaData operationMetaData) 
        throws MessagingException {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Sending message " + inout.getExchangeId());
        }
        
        checkAndGenerateInMemoryCRMP(inout, inMsg);
        populateAddressUrlProperty(inMsg);
        
        String exchangeId = null;
        inout.setEndpoint(mEndpointReference);
        inout.setOperation(mEndpointBean.createOperationAddress(operationMetaData));
        Probe deliverInOutToNMRMeasurement = null;
        try {
            inout.setInMessage(inMsg);
            exchangeId = inout.getExchangeId();
            mInboundExchanges.put(exchangeId, Long.valueOf(System.currentTimeMillis()));            
            deliverInOutToNMRMeasurement = Probe.fine(getClass(), "deliverInOutToNMR");   
            mChannel.send(inout);
            mEndpointBean.getEndpointStatus().incrementSentRequests();
            if (this.mSoapToNMRMeasurement != null) {
                this.mSoapToNMRMeasurement.end();
            }
        } catch(MessagingException ex) {
            String error = mMessages.getString("HTTPBC-E00772.Failed_send_inout");
            mLog.log(Level.SEVERE, error, ex);
            AlertsUtil.getAlerter().warning(error, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            getServiceUnitID(), 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00772");
            if (exchangeId != null) {
                mInboundExchanges.remove(exchangeId);
                MessageExchangeSupport.removeRedeliveryListener(exchangeId);
            }            
            throw ex;
        } finally {
            if (deliverInOutToNMRMeasurement != null) {
                deliverInOutToNMRMeasurement.end();
            }
        }
        return;
    }

    private String executePost(Object clientContext)
            throws MessagingException {
        String exchangeID = null;
        DataSource input = (DataSource) mInput;
        OperationMetaData operationMetaData = null;
        String mep = null;
 
        Map nameToMeta = mEndpointBean.getOperationNameToMetaData();
        MessageContext msgContext = mMessageContext;
        if (nameToMeta == null || nameToMeta.size() == 0) {
            String error = mMessages.getString("HTTPBC-E00773.Endpoint_as_no_operations", mEndpointBean.getUniqueName());
            AlertsUtil.getAlerter().warning(error, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            getServiceUnitID(), 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00773");
            throw new MessagingException(error);
        } else {
            try {
                operationMetaData = OperationResolver2.resolveOperation(msgContext, mEndpointBean.getUrlContext(), nameToMeta);
            } catch (OperationResolveException e) {
                String error = mMessages.getString("HTTPBC-E00701.Operation_not_defined", mEndpointBean.getUniqueName());
                AlertsUtil.getAlerter().warning(error, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                getServiceUnitID(), 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-E00701");
                throw new MessagingException(error,e);
            }
        }
        mep = operationMetaData.getMessageExchangePattern();
        
        try {
            if (mep.equalsIgnoreCase("inout")) { // NOI18N
                InOut inout = mMessageExchangeFactory.createInOutExchange();
                exchangeID = inout.getExchangeId();
                MessageExchangeSupport.addReplyListener(inout.getExchangeId(), mReplyListener, clientContext);
                if (isRetryEnabled(inout)) {
                    MessageExchangeSupport.addRedeliveryListener(inout.getExchangeId(), this, operationMetaData, clientContext);
                }

                Probe normalizationMeasurement = Probe.info(getClass(),
                                                            mEndpointBean.getUniqueName(), 
                                                            HttpSoapBindingLifeCycle.PERF_CAT_NORMALIZATION);
                NormalizedMessage inMsg = null;
                try {
                    inMsg = mNormalizer.normalize(input, inout, operationMetaData, mMessageContext);
                } catch (MessagingException t) {
                    throw t;
                } finally {
                    if (normalizationMeasurement!=null) {
                        normalizationMeasurement.end();
                    }
                }
                
                if (mLog.isLoggable(Level.FINE)) {
                    DebugLog.debugLog(mLog, Level.FINE, "Sending to component this normalized message", inMsg.getContent());
                }
                sendInOut(inout, inMsg, operationMetaData);
            } else if(mep.equalsIgnoreCase("inonly")) { // NOI18N
                InOnly inonly = mMessageExchangeFactory.createInOnlyExchange();
                exchangeID = inonly.getExchangeId();
                MessageExchangeSupport.addReplyListener(inonly.getExchangeId(), mReplyListener, clientContext);
                if (isRetryEnabled(inonly)) {
                    MessageExchangeSupport.addRedeliveryListener(inonly.getExchangeId(), this, operationMetaData, clientContext);
                }
                
                Probe normalizationMeasurement = Probe.info(getClass(),
                                                            mEndpointBean.getUniqueName(), 
                                                            HttpSoapBindingLifeCycle.PERF_CAT_NORMALIZATION);
                NormalizedMessage inMsg = null;
                try {
                    inMsg = mNormalizer.normalize(input, inonly, operationMetaData, mMessageContext);
                } catch (MessagingException t) {
                    throw t;
                } finally {
                    if (normalizationMeasurement!=null) {
                        normalizationMeasurement.end();
                    }
                }
                
                if (mLog.isLoggable(Level.FINE)) {
                    DebugLog.debugLog(mLog, Level.FINE, "Sending to component this normalized message", inMsg.getContent());
                }
                sendInOnly(inonly, inMsg, operationMetaData);
                // If not configured to wait for completion of request processing, don't wait for a reply from the SE for inonly
                if (!operationMetaData.getOneWayReplyAfterProcessing()) {
                    MessageExchangeSupport.notifyOfReply(inonly);
                }
            } else if(mep.equalsIgnoreCase("robustinonly")) { // NOI18N
                //exchangeID = robustinonly.getExchangeId();
                String error = mMessages.getString("HTTPBC-E00751.MEP_robust_inonly_not_supported", "");
                AlertsUtil.getAlerter().warning(error, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                getServiceUnitID(), 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-E00751");
                mLog.log(Level.SEVERE, error);
                throw new MessagingException(error);
            } else {
                String error = mMessages.getString("HTTPBC-E00774.Invalid_pattern", new Object[] {mep, mEndpointBean.getUniqueName()});
                AlertsUtil.getAlerter().warning(error, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                getServiceUnitID(), 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-E00774");
                mLog.log(Level.SEVERE, error);
                throw new MessagingException(error);
            }
        } catch (MessagingException ex) {
            String error = mMessages.getString("HTTPBC-E00748.Exception_during_exchange_processing", new Object[] {mep, mEndpointBean.getUniqueName()});
            AlertsUtil.getAlerter().warning(error, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            getServiceUnitID(), 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00748");
            mLog.log(Level.SEVERE, error, ex);
            throw ex;
        }
        return exchangeID;
    }

    private String executeGet(Object clientContext)
            throws MessagingException {
        String exchangeID = null;
        DataSource input = (DataSource) mInput;
        OperationMetaData operationMetaData = null;
        String mep = null;
 
        MessageContext msgContext = mMessageContext;
        Map nameToMeta = mEndpointBean.getOperationNameToMetaData();
        if (nameToMeta == null || nameToMeta.size() == 0) {
            String error = mMessages.getString("HTTPBC-E00773.Endpoint_as_no_operations", mEndpointBean.getUniqueName());
            AlertsUtil.getAlerter().warning(error, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            getServiceUnitID(), 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00773");
            throw new MessagingException(error);
        } else {
            MessageContext context = mMessageContext;
            try {
                operationMetaData = OperationResolver2.resolveOperation(msgContext, mEndpointBean.getUrlContext(), nameToMeta);
            } catch (OperationResolveException e) {
                String error = mMessages.getString("HTTPBC-E00701.Operation_not_defined", mEndpointBean.getUniqueName());
                AlertsUtil.getAlerter().warning(error, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                getServiceUnitID(), 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-E00701");
                throw new MessagingException(error,e);
            }
        }
        mep = operationMetaData.getMessageExchangePattern();
        
        try {
            if (mep.equalsIgnoreCase("inout")) { // NOI18N
                InOut inout = mMessageExchangeFactory.createInOutExchange();
                exchangeID = inout.getExchangeId();
                MessageExchangeSupport.addReplyListener(inout.getExchangeId(), mReplyListener, clientContext);
                if (isRetryEnabled(inout)) {
                    MessageExchangeSupport.addRedeliveryListener(inout.getExchangeId(), this, operationMetaData, clientContext);
                }
                
                Probe normalizationMeasurement = Probe.info(getClass(),
                                                            mEndpointBean.getUniqueName(), 
                                                            HttpSoapBindingLifeCycle.PERF_CAT_NORMALIZATION);
                NormalizedMessage inMsg = null;
                try {
                    inMsg = mNormalizer.normalize(msgContext, inout, operationMetaData, mMessageContext);
                } catch (MessagingException t) {
                    throw t;
                } finally {
                    if (normalizationMeasurement!=null) {
                        normalizationMeasurement.end();
                    }
                }
                
                if (mLog.isLoggable(Level.FINE)) {
                    DebugLog.debugLog(mLog, Level.FINE, "Sending to component this normalized message", inMsg.getContent());
                }
                sendInOut(inout, inMsg, operationMetaData);
            } else if(mep.equalsIgnoreCase("inonly")) { // NOI18N
                InOnly inonly = mMessageExchangeFactory.createInOnlyExchange();
                exchangeID = inonly.getExchangeId();
                MessageExchangeSupport.addReplyListener(inonly.getExchangeId(), mReplyListener, clientContext);
                if (isRetryEnabled(inonly)) {
                    MessageExchangeSupport.addRedeliveryListener(inonly.getExchangeId(), this, operationMetaData, clientContext);
                }
                
                Probe normalizationMeasurement = Probe.info(getClass(),
                                                            mEndpointBean.getUniqueName(), 
                                                            HttpSoapBindingLifeCycle.PERF_CAT_NORMALIZATION);
                NormalizedMessage inMsg = null;
                try {
                    inMsg = mNormalizer.normalize(msgContext, inonly, operationMetaData, mMessageContext);
                } catch (MessagingException t) {
                    throw t;
                } finally {
                    if (normalizationMeasurement!=null) {
                        normalizationMeasurement.end();
                    }
                }
                
                if (mLog.isLoggable(Level.FINE)) {
                    DebugLog.debugLog(mLog, Level.FINE, "Sending to component this normalized message", inMsg.getContent());
                }
                sendInOnly(inonly, inMsg, operationMetaData);
                // If not configured to wait for completion of request processing, don't wait for a reply from the SE for inonly
                if (!operationMetaData.getOneWayReplyAfterProcessing()) {
                    MessageExchangeSupport.notifyOfReply(inonly);
                }
            } else if(mep.equalsIgnoreCase("robustinonly")) { // NOI18N
                //exchangeID = robustinonly.getExchangeId();
                String error = mMessages.getString("HTTPBC-E00751.MEP_robust_inonly_not_supported", "");
                AlertsUtil.getAlerter().warning(error, 
                                                 HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 getServiceUnitID(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "HTTPBC-E00751");
                mLog.log(Level.SEVERE, error);
                throw new MessagingException(error);
            } else {
                String error = mMessages.getString("HTTPBC-E00774.Invalid_pattern", new Object[] {mep, mEndpointBean.getUniqueName()});
                AlertsUtil.getAlerter().warning(error, 
                                                 HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 getServiceUnitID(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "HTTPBC-E00774");
                mLog.log(Level.SEVERE, error);
                throw new MessagingException(error);
            }
        } catch (MessagingException ex) {
            String error = mMessages.getString("HTTPBC-E00748.Exception_during_exchange_processing", new Object[] {mep, mEndpointBean.getUniqueName()});
            AlertsUtil.getAlerter().warning(error, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             getServiceUnitID(), 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00748");
            mLog.log(Level.SEVERE, error, ex);
            throw ex;
        }
        return exchangeID;
    }

    private String executeSOAP(Object clientContext) throws MessagingException {
        String exchangeID = null;
        OperationMetaData operationMetaData = null;
        String mep = null;
        SOAPMessage soapMsg = mNormalizer.toSOAPMessage(mInput);      
        
        try {
            // TODO: add mime extension validation
            mSoapValidator.validate(soapMsg);
        } catch (WSIValidationException e) {
            throw new MessagingException(e.getMessage(), e);
        }
            
        String warnMsg = mSoapValidator.getWarningMsg();
        if (warnMsg != null) {
            if (mLog.isLoggable(Level.WARNING)) {
                String error = mMessages.getString("HTTPBC-E00741.Soap_validator_error", warnMsg);
                mLog.log(Level.WARNING, error);                    
                AlertsUtil.getAlerter().warning(error, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                getServiceUnitID(), 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-E00741");

            }
        }
        
        String errorMsg = mSoapValidator.getErrorMsg();
        if (errorMsg != null) {
            throw new MessagingException(errorMsg);
        }
        
        Map nameToMeta = mEndpointBean.getOperationNameToMetaData();
        if (nameToMeta == null || nameToMeta.size() == 0) {
            String error = mMessages.getString("HTTPBC-E00773.Endpoint_as_no_operations", mEndpointBean.getUniqueName());
            AlertsUtil.getAlerter().warning(error, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             getServiceUnitID(), 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00773");            
            throw new MessagingException(error);
        } else {
            if (nameToMeta.size() == 1) {
                // if there is only one operation defined, don't bother trying to resolve it.
                Map.Entry firstEntry = (Map.Entry) nameToMeta.entrySet().iterator().next();
                operationMetaData = (OperationMetaData) firstEntry.getValue();
            } else {
                operationMetaData = OperationResolver.resolveOperation(soapMsg, nameToMeta);
                if (operationMetaData == null) {
                    String error = mMessages.getString("HTTPBC-E00701.Operation_not_defined", mEndpointBean.getUniqueName());
                    AlertsUtil.getAlerter().warning(error, 
                                                     HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                                     getServiceUnitID(),
                                                     AlertsUtil.getServerType(),
                                                     AlertsUtil.COMPONENT_TYPE_BINDING,
                                                     NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                                     NotificationEvent.EVENT_TYPE_ALERT,
                                                     "HTTPBC-E00701");
                    throw new MessagingException(error);
                }
            }
        }
        
        mep = operationMetaData.getMessageExchangePattern();
        
        XPathExpression xpathExp = LoggingMonitoringUtil.getMessageTrackingXPathExpression(LoggingMonitoringUtil.COMPONENT_NAME,
                    	                                                                   LoggingMonitoringUtil.getServiceInstanceID(mEndpointBean),
                    	                                                                   operationMetaData.getOperationName());
        try {
            if (mep.equalsIgnoreCase("inout")) { // NOI18N
                InOut inout = mMessageExchangeFactory.createInOutExchange();
                exchangeID = inout.getExchangeId();
                MessageExchangeSupport.addReplyListener(inout.getExchangeId(), mReplyListener, clientContext);
                // message tracking
                String trackingId = null;
                
                if (isRetryEnabled(inout)) {
                    MessageExchangeSupport.addRedeliveryListener(inout.getExchangeId(), this, operationMetaData, clientContext);
                }
                Probe normalizationMeasurement = Probe.info(getClass(),
                                                            mEndpointBean.getUniqueName(), 
                                                            HttpSoapBindingLifeCycle.PERF_CAT_NORMALIZATION);
                NormalizedMessage inMsg = null;
                try {
                    mNormalizer.setPropagateSoapHeader(mEndpointBean.getPropagateSoapHeader());
                    inMsg = mNormalizer.normalize(soapMsg, inout, operationMetaData, mMessageContext);
                    // Message tracking and checkpointing
                    if (mMonitorEnabled) {
                        // make sure we have the message tracking IDs
                        checkAndGenerateInMemoryCRMP(inout, inMsg);
                        trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                        if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                            // get the message ID from the payload
                            try {
                                trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                // use this tracking ID for the NM properties too
                                inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                            } catch (Exception e) {
                                // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                if (mLog.isLoggable(Level.WARNING)) {
                                    mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                }
                                trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        } 
                        try {
                            LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Processing-inbound-request", Util.messageAsDom(inMsg));
                        } catch (Exception e) {
                            if (mLog.isLoggable(Level.WARNING)) {
                                mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                            }
                        }
                    }
                } catch (MessagingException t) {
                    throw t;
                } finally {
                    if (normalizationMeasurement!=null) {
                        normalizationMeasurement.end();
                    }
                }
                
                // Propagate the security context in the subject via NMR
                if (subject != null) {
                    inMsg.setSecuritySubject(subject);
                    if (mMonitorEnabled) {
                    	if (trackingId == null) {
                            if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                                // get the message ID from the payload
                                try {
                                    trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                    // use this tracking ID for the NM properties too
                                    inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                    inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                                } catch (Exception e) {
                                    // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                    if (mLog.isLoggable(Level.WARNING)) {
                                        mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                    }
                                    trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                                }
                            }  else {
                                trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        }
                        LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Propagating-security-context", mEndpointBean);
                    }
                }
                
                if (mLog.isLoggable(Level.FINE)) {
                    DebugLog.debugLog(mLog, Level.FINE, "Sending to component this normalized message", inMsg.getContent());
                }
                
                if (mTransaction != null) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "Transaction context present in the inbound request, about to set the transaction property in the InOut Message exchange...");
                    }
                    inout.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, mTransaction);
                    if (mMonitorEnabled) {
                    	if (trackingId == null) {
                            if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                                // get the message ID from the payload
                                try {
                                    trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                    // use this tracking ID for the NM properties too
                                    inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                    inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                                } catch (Exception e) {
                                    // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                    if (mLog.isLoggable(Level.WARNING)) {
                                        mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                    }
                                    trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                                }
                            }  else {
                                trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        }
                        LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Propagating-transaction-context", mEndpointBean);
                    }
                }
                sendInOut(inout, inMsg, operationMetaData);
                if (mMonitorEnabled) {
                    if (trackingId == null) {
                        if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                            // get the message ID from the payload
                            try {
                                trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                // use this tracking ID for the NM properties too
                                inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                            } catch (Exception e) {
                                // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                if (mLog.isLoggable(Level.WARNING)) {
                                    mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                }
                                trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        }  else {
                            trackingId = (String) inout.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inout.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                        }
                    }
                    LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Message-routed-to-NMR", mEndpointBean);
                }
            } else if(mep.equalsIgnoreCase("inonly")) { // NOI18N
                InOnly inonly = mMessageExchangeFactory.createInOnlyExchange();
                exchangeID = inonly.getExchangeId();
                MessageExchangeSupport.addReplyListener(inonly.getExchangeId(), mReplyListener, clientContext, mMessageContext);
                // message tracking
                String trackingId = null;
                
                if (isRetryEnabled(inonly)) {
                    MessageExchangeSupport.addRedeliveryListener(inonly.getExchangeId(), this, operationMetaData, clientContext);
                }
                
                Probe normalizationMeasurement = Probe.info(getClass(),
                                                            mEndpointBean.getUniqueName(), 
                                                            HttpSoapBindingLifeCycle.PERF_CAT_NORMALIZATION);
                NormalizedMessage inMsg = null;
                try {
                    mNormalizer.setPropagateSoapHeader(mEndpointBean.getPropagateSoapHeader());
                    inMsg = mNormalizer.normalize(soapMsg, inonly, operationMetaData, mMessageContext);
                    if (mMonitorEnabled) {
                    	checkAndGenerateInMemoryCRMP(inonly, inMsg);
                        trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                        if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                            // get the message ID from the payload
                            try {
                                trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                // use this tracking ID for the NM properties too
                                inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                            } catch (Exception e) {
                                // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                if (mLog.isLoggable(Level.WARNING)) {
                                    mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                }
                                trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        } 
                        try {
                            LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Processing-inbound-request", Util.messageAsDom(inMsg));
                        } catch (Exception e) {
                            if (mLog.isLoggable(Level.WARNING)) {
                                mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                            }
                        }
                    }
                } catch (MessagingException t) {
                    throw t;
                } finally {
                    if (normalizationMeasurement!=null) {
                        normalizationMeasurement.end();
                    }
                }
                
                // Propagate the security context in the subject via NMR
                if (subject != null) {
                    inMsg.setSecuritySubject(subject);
                    
                    if (mMonitorEnabled) {
                    	if (trackingId == null) {
                            if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                                // get the message ID from the payload
                                try {
                                    trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                    // use this tracking ID for the NM properties too
                                    inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                    inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                                } catch (Exception e) {
                                    // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                    if (mLog.isLoggable(Level.WARNING)) {
                                        mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                    }
                                    trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                                }
                            }  else {
                                trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        }
                        LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Propagating-security-context", mEndpointBean);
                    }
                }
                                
                if (mLog.isLoggable(Level.FINE)) {
                    DebugLog.debugLog(mLog, Level.FINE, "Sending to component this normalized message", inMsg.getContent());
                }
                
                if (mTransaction != null) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "Transaction context present in the inbound request, about to set the transaction property in the InOnly Message exchange...");
                    }
                    inonly.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, mTransaction);
                    if (mMonitorEnabled) {
                    	if (trackingId == null) {
                            if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                                // get the message ID from the payload
                                try {
                                    trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                   // use this tracking ID for the NM properties too
                                   inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                   inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                                } catch (Exception e) {
                                    // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                    if (mLog.isLoggable(Level.WARNING)) {
                                        mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                    }
                                    trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                                }
                            }  else {
                                trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        }
                        LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Propagating-transaction-context", mEndpointBean);
                    }
                }
                sendInOnly(inonly, inMsg, operationMetaData);
                if (mMonitorEnabled) {
                    if (trackingId == null) {
                        if (LoggingMonitoringUtil.MESSAGE_TRACKING_MODE_MSG.equals(mMode) && xpathExp != null) {
                            // get the message ID from the payload
                            try {
                                trackingId = xpathExp.evaluate(soapMsg.getSOAPPart());
                                // use this tracking ID for the NM properties too
                                inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, null);
                                inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, trackingId);
                            } catch (Exception e) {
                                // in case of exception when evalutating the xpath expression, fall back to "AUTO" mode
                                if (mLog.isLoggable(Level.WARNING)) {
                                    mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01309.Failed_to_evaluate_xpath"));
                                }
                                trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                            }
                        }  else {
                            trackingId = (String) inonly.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)inonly.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                        }
                    }
                    LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Message-routed-to-NMR", mEndpointBean);
                }
                // If not configured to wait for completion of request processing, don't wait for a reply from the SE for inonly
                if (!operationMetaData.getOneWayReplyAfterProcessing()) {
                    MessageExchangeSupport.notifyOfReply(inonly);
                }
            } else if(mep.equalsIgnoreCase("robustinonly")) { // NOI18N
                //exchangeID = robustinonly.getExchangeId();
                String error = mMessages.getString("HTTPBC-E00751.MEP_robust_inonly_not_supported", "");
                AlertsUtil.getAlerter().warning(error, 
                                                 HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                                 getServiceUnitID(),
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "HTTPBC-E00751");
                mLog.log(Level.SEVERE, error);
                throw new MessagingException(error);
            } else {
                String error = mMessages.getString("HTTPBC-E00774.Invalid_pattern", new Object[] {mep, mEndpointBean.getUniqueName()});
                AlertsUtil.getAlerter().warning(error, 
                                                 HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 getServiceUnitID(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "HTTPBC-E00774");
                mLog.log(Level.SEVERE, error);
                throw new MessagingException(error);
            }
        } catch (MessagingException ex) {
            String error = mMessages.getString("HTTPBC-E00748.Exception_during_exchange_processing", new Object[] {mep, mEndpointBean.getUniqueName()});
            AlertsUtil.getAlerter().warning(error, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                             getServiceUnitID(),
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00748");
            mLog.log(Level.SEVERE, error, ex);
            throw ex;
        } 
        return exchangeID;
    }
    
    public void setMeasurement(Probe m) {
        this.mSoapToNMRMeasurement = m;
    }
   
    public void setMessageExchangeId(String messageExchangeId, OperationMetaData retryMetaData, Object clientContext) {
        mExchangeIDToMeta.put(messageExchangeId, new Object[] {retryMetaData, clientContext});
    }
    
    public void onRedelivery(MessageExchange exchange) throws MessagingException {
        NormalizedMessage inMsg;
        Object[] metadata = (Object[]) mExchangeIDToMeta.remove(exchange.getExchangeId());
        if (metadata.length != 2 && metadata[0] instanceof OperationMetaData) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00803.Cannot_find_metadata_for_retry", exchange.getExchangeId()));
        }
        OperationMetaData operationMetaData = (OperationMetaData) metadata[0];
        Object clientContext = metadata[1];
        if (clientContext == null) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00804.Cannot-find_callback_context", exchange.getExchangeId()));
        }
        
        String groupId = (String)exchange.getProperty(SoapNormalizer.CRMP_GROUP_ID);
        String messageId =  (String)exchange.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
        
        // message tracking
        String trackingId = null;
        
        // remove the listener associated with the exchange ID
        MessageExchangeSupport.removeRedeliveryListener(exchange.getExchangeId());
        
        // we are about to redelivery the same message, so we will remove the 
        // ReplyListener for the failed exchange here
        MessageExchangeSupport.removeReplyListener(exchange.getExchangeId(), true);
        
        switch (ExchangePattern.valueOf(exchange)) {
            case IN_OUT:
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Resending the InOut exchange with group ID '" + groupId + "' and message ID '" + messageId + "'...");
                }
                inMsg = ((InOut)exchange).getInMessage();
                InOut inout = mMessageExchangeFactory.createInOutExchange();
                // make sure that the message id has is the same 
                inout.setProperty(SoapNormalizer.CRMP_GROUP_ID, groupId);
                inout.setProperty(SoapNormalizer.CRMP_MESSAGE_ID, messageId);
                
                // also make sure the same normalized message level properties are set
                inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, groupId);
                inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, messageId);
                
                // make sure the transaction is set if it's available
                if (mTransaction != null) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "Transaction context present in the inbound request, about to set the transaction property in the InOut Message exchange for the resend...");
                    }
                    inout.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, mTransaction);
                }
                // this is a redelivered exchange, make sure we associate the correct
                // ReplyListener
                MessageExchangeSupport.addReplyListener(inout.getExchangeId(), mReplyListener, clientContext);
                // redelivery configuration has to be set correctly if we get here.
                // adding the redelivery listener w/o having to check redelivery configuration again.
                MessageExchangeSupport.addRedeliveryListener(inout.getExchangeId(), this, operationMetaData, clientContext);
                sendInOut(inout, inMsg, operationMetaData);
                
                // message checkpointing
                if (mMonitorEnabled) {
                    if (trackingId == null) {
                        trackingId = (String) exchange.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)exchange.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                    }
                    LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Message-redelivered", mEndpointBean);
                }
                break;
            case IN_ONLY:
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Resending the InOnly exchange with group ID '" + groupId + "' and message ID '" + messageId + "'...");
                }
                inMsg = ((InOnly)exchange).getInMessage();
                InOnly inonly = mMessageExchangeFactory.createInOnlyExchange();
                // make sure that the message id has is the same 
                inonly.setProperty(SoapNormalizer.CRMP_GROUP_ID, groupId);
                inonly.setProperty(SoapNormalizer.CRMP_MESSAGE_ID, messageId);
                
                // also make sure the same normalized message level properties are set
                inMsg.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, groupId);
                inMsg.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, messageId);
                
                // make sure the transaction is set if it's available
                if (mTransaction != null) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "Transaction context present in the inbound request, about to set the transaction property in the InOnly Message exchange for the resend...");
                    }
                    inonly.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, mTransaction);
                }
                // this is a redelivered exchange, make sure we associate the correct
                // ReplyListener
                MessageExchangeSupport.addReplyListener(inonly.getExchangeId(), mReplyListener, clientContext, mMessageContext);
                // redelivery configuration has to be set correctly if we get here.
                // adding the redelivery listener w/o having to check redelivery configuration again.
                MessageExchangeSupport.addRedeliveryListener(inonly.getExchangeId(), this, operationMetaData, clientContext);
                sendInOnly(inonly, inMsg, operationMetaData);
                // message checkpointing
                if (mMonitorEnabled) {
                    if (trackingId == null) {
                        trackingId = (String) exchange.getProperty(SoapNormalizer.CRMP_GROUP_ID) + (String)exchange.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
                    }
                    LoggingMonitoringUtil.setCheckpoint(mEndpointBean, trackingId, "Message-redelivered", mEndpointBean);
                }
                break;
            default:
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Retry handler receives an unsupported exchange pattern: " + 
                                             ExchangePattern.valueOf(exchange) + ". Ignoring the retry attempt...");
                }
                break;
        }
    }
    
    private String getServiceUnitID () {
        return (mEndpointBean != null? mEndpointBean.getServiceUnitID() : null);
    }
    
    private void checkAndGenerateInMemoryCRMP (MessageExchange mex, NormalizedMessage nm) {
        // If the CRMP groupid and messageid properties are NOT set (i.e., populated by normalizer if ws-rm soap message),
        // then generate GUID for groupid and in memory "one-up" for messageid
        // Note: groupid is random in this case, so there's really no grouping if ws-rm is not used
        if (mex.getProperty (SoapNormalizer.CRMP_GROUP_ID) == null ||
            mex.getProperty (SoapNormalizer.CRMP_MESSAGE_ID) == null) {
            String groupId = GUIDUtil.generateGUID();
            String messageId = Long.toString(mEndpointBean.getCRMPMessageId());
            mex.setProperty(SoapNormalizer.CRMP_GROUP_ID, groupId);
            mex.setProperty(SoapNormalizer.CRMP_MESSAGE_ID, messageId);
            // also make sure the group/message id are set as normalized message properties
            nm.setProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY, groupId);
            nm.setProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY, messageId);
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "The request received is not a ws-rm enabled message; generated " +
                                     SoapNormalizer.CRMP_GROUP_ID + " with value [" + 
                                     groupId + "] and " +
                                     SoapNormalizer.CRMP_MESSAGE_ID + " with value [" +
                                     messageId + "] for message exchange with ID [" + mex.getExchangeId() + "]");
            }
        }
    }
    
    private void populateAddressUrlProperty(NormalizedMessage nm) {
        nm.setProperty(NormalizedMessageProperties.INBOUND_ADDRESS_URL, mEndpointBean.getEndpointUrl().toString());
    }
    
    private boolean isRetryEnabled(MessageExchange exchange) {
    	boolean shouldRetry = false;
        EndpointInfo info = new EndpointInfo(false,
                                             mEndpointBean.getEndpointName(),
                                             null,
                                             mEndpointBean.getServiceName(),
                                             null);
        RedeliveryConfig retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
        if (retryConfig == null) {
            return shouldRetry;
        }
        
        Failure onFailureOption = retryConfig.getFailure();
        
        switch (ExchangePattern.valueOf(exchange)) {
            case IN_OUT:
                if (onFailureOption == Failure.redirect || onFailureOption == Failure.delete) {
                    String option = (onFailureOption == Failure.redirect)? "redirect" : "delete";
                    String warningMsg = mMessages.getString("HTTPBC-W00807.Unsupported_onfailure_option", option);
                    if (mLog.isLoggable(Level.WARNING)) {
                        mLog.log(Level.WARNING, warningMsg);
                    }
                    AlertsUtil.getAlerter().warning(warningMsg, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            getServiceUnitID(), 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00805");
                }
                shouldRetry = (retryConfig != null && 
                               (onFailureOption == Failure.suspend || onFailureOption == Failure.error));
                
                break;
            case IN_ONLY:
                shouldRetry = (retryConfig != null);
                break;
            default:
                break;
        }
        
        return shouldRetry;
    }

	public Subject getSubject() {
		return subject;
	}

	public void setSubject(Subject subject) {
		this.subject = subject;
	}
}
