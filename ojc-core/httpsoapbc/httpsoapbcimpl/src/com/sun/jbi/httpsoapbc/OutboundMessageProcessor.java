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
package com.sun.jbi.httpsoapbc;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.eManager.provider.EndpointStatus;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.httpsoapbc.async.AsyncRequestContext;
import com.sun.jbi.httpsoapbc.async.AsyncResponseDispatcher;
import com.sun.jbi.httpsoapbc.async.AsyncResponseHandler;
import com.sun.jbi.httpsoapbc.descriptors.HttpSoapHandler;
import com.sun.jbi.httpsoapbc.management.HTTPManagementMBean;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;
import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.httpsoapbc.util.StringUtil;
import com.sun.jbi.httpsoapbc.util.TransactionsUtil;
import com.sun.jbi.httpsoapbc.util.TransformerPool;
import com.sun.jbi.httpsoapbc.util.LoggingMonitoringUtil;
import com.sun.jbi.httpsoapbc.util.Util;
import com.sun.jbi.httpsoapbc.util.WSDLUtilities;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import com.sun.xml.ws.developer.JAXWSProperties;

import java.io.File;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

import javax.activation.DataSource;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanException;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLSession;
import javax.transaction.Transaction;
import javax.transaction.SystemException;
import javax.xml.namespace.QName;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.ws.Dispatch;
import javax.xml.ws.http.HTTPBinding;
import javax.xml.ws.Service;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.soap.SOAPFaultException;
import javax.wsdl.Message;
import javax.wsdl.Part;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import net.java.hulp.measure.Probe;

/**
 * Process replies/requests received from the SE.
 */
public class OutboundMessageProcessor {

    private static final Messages mMessages =
            Messages.getMessages(OutboundMessageProcessor.class);
    private static final Logger mLog =
            Messages.getLogger(OutboundMessageProcessor.class);
    private static final TransformerPool cTransformerPool =
            new TransformerPool();
    // The World Wide Web Consortium Recommendation states that UTF-8 should be used.
    // Not doing so may introduce incompatibilities
    // See http://java.sun.com/j2se/1.5.0/docs/api/java/net/URLDecoder.html#decode(java.lang.String,%20java.lang.String)
    private static final String URL_DECODE_ENCODING = "UTF-8";
    private static final String COMPONENT_NAME = "sun-http-binding";
    private Map mEndpoints;
    private MessagingChannel mChannel;
    private MessageFactory mSoapMessageFactory;
    private SOAPConnectionFactory mConnFact;
    private SOAPConnection mConn;
    private SoapNormalizer mNormalizer;
    private SoapDenormalizer mDenormalizer;
    private Map mInboundExchanges;
    private HTTPManagementMBean mManagementMBean;
    private HttpNormalizer mHttpNormalizer;
    private DocumentBuilderFactory mBuilderFactory;
    private DocumentBuilder mBuilder;
    // Disable HTTP/SOAP header propagation because of the lack of SE support at the moment, and the performance overhead.
    private boolean mIsHeaderCopyEnabled = true;     // TODO: may want to have a user configuration to enable/disable the copy
    // measurements
    private Probe mDCReceiveToSoapCallMeasurement = null;
    // Message tracking
    private boolean mMonitorEnabled = false;
    private String mMode = LoggingMonitoringUtil.getMessageTrackingIDModeInbound();

    public OutboundMessageProcessor(MessagingChannel chnl, Map endpoints, Map inboundExchanges, HTTPManagementMBean managementMBean) {
        mChannel = chnl;
        mEndpoints = endpoints;
        mManagementMBean = managementMBean;
        try {
            mNormalizer = new SoapNormalizerImpl();
            mDenormalizer = new SoapDenormalizerImpl();
        } catch (Exception e) {
            String text = mMessages.getString("HTTPBC-E00776.Failed_create_soap_normalizer");
            AlertsUtil.getAlerter().critical(text,
                    HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "HTTPBC-E00776");
            throw new IllegalStateException(text, e);
        }

        try {
            mHttpNormalizer = new HttpNormalizer();
        } catch (Exception e) {
            String text = mMessages.getString("HTTPBC-E00777.Failed_create_http_normalizer");
            AlertsUtil.getAlerter().critical(text,
                    HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "HTTPBC-E00777");
            throw new IllegalStateException(text, e);
        }

        mInboundExchanges = inboundExchanges;

        try {
            mConnFact = SOAPConnectionFactory.newInstance();
            mConn = mConnFact.createConnection();
            mSoapMessageFactory = MessageFactory.newInstance();
            mBuilderFactory = DocumentBuilderFactory.newInstance();
            mBuilder = mBuilderFactory.newDocumentBuilder();
        } catch (Exception e) {
            String text = mMessages.getString("HTTPBC-E00778.Failed_create_outbound_processor");
            AlertsUtil.getAlerter().critical(text,
                    HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "HTTPBC-E00778");
            throw new IllegalStateException(text, e);
        }
    }

    /**
     * Process the message exchange
     */
    public void processMessage(MessageExchange exchange) {
        mDCReceiveToSoapCallMeasurement = Probe.fine(getClass(), "processMessageExchange");
        String exchangeId = exchange.getExchangeId();
        boolean inbound = exchange.getRole().equals(MessageExchange.Role.CONSUMER);
        if (inbound) {
            Long invocationTime = (Long) mInboundExchanges.remove(exchangeId);
            if (mLog.isLoggable(Level.FINE) && invocationTime != null) {
                long difference = System.currentTimeMillis() - invocationTime.longValue();
                mLog.log(Level.FINE, "Response for exchange " + exchangeId + " received (" + Long.valueOf(difference) + " ms}");
            }
        }

        URI pattern = exchange.getPattern();
        if (mLog.isLoggable(Level.FINE)) {
            if (inbound) {
                mLog.log(Level.FINE, "Processing message exchange " + exchange.getExchangeId() + " as inbound; pattern is " + pattern.toString());
            } else {
                mLog.log(Level.FINE, "Processing message exchange " + exchange.getExchangeId() + " as outbound; pattern is " + pattern.toString());
            }
        }
        switch (ExchangePattern.valueOf(exchange)) {
            case IN_OUT:
                if (inbound) {
                    processRequestReplyInbound((InOut) exchange);
                } else {
                    processRequestReplyOutbound((InOut) exchange);
                }
                break;
            case IN_ONLY:
                if (inbound) {
                    processOneWayInbound((InOnly) exchange);
                } else {
                    processOneWayOutbound((InOnly) exchange);
                }
                break;
            case ROBUST_IN_ONLY: {
                String msg = mMessages.getString("HTTPBC-E00751.MEP_robust_inonly_not_supported", exchange.getExchangeId());
                mLog.log(Level.SEVERE, msg);
                try {
                    setErrorUnsupportedExchangePattern(exchange, msg, null);
                    Endpoint epb = getInboundEndpoint(exchange);
                    mChannel.send(exchange);
                    if (epb != null) {
                        updateTallySends(epb, false);
                    }
                } catch (MessagingException ex) {
                    String text = mMessages.getString("HTTPBC-E00775.Exception_during_exchange_processing", exchange.getExchangeId());
                    mLog.log(Level.SEVERE, text, ex);
                    Endpoint epb = getInboundEndpoint(exchange);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            epb.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E00775");
                }
                break;
            }
            default:
                String msg = mMessages.getString("HTTPBC-E00752.Invalid_MEP", new Object[]{exchange.getExchangeId(), pattern.toString()});
                mLog.log(Level.SEVERE, msg);
                try {
                    setErrorUnsupportedExchangePattern(exchange, msg, null);
                    Endpoint epb = getInboundEndpoint(exchange);
                    mChannel.send(exchange);
                    if (epb != null) {
                        updateTallySends(epb, false);
                    }
                } catch (MessagingException ex) {
                    String text = mMessages.getString("HTTPBC-E00775.Exception_during_exchange_processing", exchange.getExchangeId());
                    mLog.log(Level.SEVERE, text, ex);
                    Endpoint epb = getInboundEndpoint(exchange);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            epb.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E00775");
                }
                return;
        }
    }

    /**
     * Process the reply for a request/reply request originating from this BC.
     */
    public void processRequestReplyInbound(InOut inout) {
        boolean success = true;
        //boolean isRetry = isRedeliveryConfigured(inout);  // for some reason, QoS no longer returns Redelivery Status when retries are maxed out
        boolean isRetry = isRedeliveryEnabled(inout);
        Endpoint epb = getInboundEndpoint(inout);
        NormalizedMessage inMsg = inout.getInMessage();

        // message tracking
        String trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") +
                ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");

        // If redelivery is configured and when a message exchange exhausts its redelivery attempts, 
        // redelivery library may re-route the exchange to a different endpoint.
        // That means the endpoint associated with the exchange could be changed.
        // We need to call Redelivery.getEndpoint(exchange) to get the original endpoint back.
        // The following if block tries to locate the inbound endpoint using the above redelivery utility
        // when the first attempt to get it based on the endpoint associated with the exchange fails.
        if (isRetry && epb == null) {
            epb = getInboundEndpoint(inout, true);
        }
        boolean ndcEnabled = LoggingMonitoringUtil.isNdcEnabled(epb);
        if (ndcEnabled) {
            // "Push" the NDC context based on the setting in the NDC properties file
            Logger.getLogger("com.sun.EnterContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                        LoggingMonitoringUtil.getSolutionGroup(epb)});
        }

        if (inout.getOutMessage() != null) {
            updateTallyReceivedReplies(epb);
            // remove the redelivery listener handler - no retry needed.
            if (isRetry) {
                MessageExchangeSupport.removeRedeliveryListener(inout.getExchangeId());
            }

            if (mMonitorEnabled) {
                LoggingMonitoringUtil.setCheckpoint(trackingId, "Processing-response-message", epb);
            }
        } else if (ExchangeStatus.ERROR.equals(inout.getStatus())) {
            updateTallyReceives(epb, false);
            if (mMonitorEnabled) {
                LoggingMonitoringUtil.setCheckpoint(trackingId, "Handling-error-status", epb);
            }

            // send alerts
            String errorMsg = (inout.getError() != null) ? inout.getError().getMessage() : null;
            if (errorMsg != null) {
                String msg = mMessages.getString("HTTPBC-E00720.Message_exchange_error",
                        new Object[]{
                            String.valueOf(epb.getServiceName()),
                            epb.getEndpointName(),
                            errorMsg
                        });
                mLog.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg,
                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HTTPBC-E00720");
            } else {
                String msg = mMessages.getString("HTTPBC-E00721.Message_exchange_error_no_detail",
                        new Object[]{
                            String.valueOf(epb.getServiceName()),
                            epb.getEndpointName()
                        });
                mLog.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg,
                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HTTPBC-E00721");
            }

            // let's redeliver if it is configured and max attempt count has not been exhausted
            RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(inout);
            RedeliveryConfig retryConfig = epb.getRedeliveryConfiguration();
            if (retryStatus != null && retryConfig != null) {
                Failure onFailureOption = retryConfig.getFailure();
                if (!retryStatus.hasFailed() && (onFailureOption == Failure.suspend || onFailureOption == Failure.error)) {
                    //handle redelivery
                    if (mMonitorEnabled) {
                        LoggingMonitoringUtil.setCheckpoint(trackingId, "Handling-redelivery", epb);
                    }
                    handleRedelivery(inout, epb);
                    return;
                } else {
                    if (onFailureOption == Failure.suspend) {
                        // suspend the inbound endpoint
                        try {
                            if (mMonitorEnabled) {
                                LoggingMonitoringUtil.setCheckpoint(trackingId, "Suspending-endpoint", epb);
                            }
                            mManagementMBean.suspend(epb.getUniqueName());
                            // emit warning logs and send alerts 
                            String msg = mMessages.getString("HTTPBC-E00806.About_to_suspend_endpoint",
                                    new Object[]{
                                        String.valueOf(epb.getServiceName()),
                                        epb.getEndpointName()
                                    });
                            mLog.log(Level.WARNING, msg);
                            AlertsUtil.getAlerter().warning(msg,
                                    HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                    epb.getServiceUnitID(),
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "HTTPBC-E00806");
                        } catch (MBeanException e) {
                            errorMsg = e.getTargetException().getMessage();
                            if (errorMsg != null) {
                                String msg = mMessages.getString("HTTPBC-E00805.Failed_to_suspend_endpoint",
                                        new Object[]{
                                            String.valueOf(epb.getServiceName()),
                                            epb.getEndpointName(),
                                            errorMsg
                                        });
                                mLog.log(Level.SEVERE, msg);
                                AlertsUtil.getAlerter().warning(msg,
                                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                        epb.getServiceUnitID(),
                                        AlertsUtil.getServerType(),
                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                        NotificationEvent.EVENT_TYPE_ALERT,
                                        "HTTPBC-E00805");
                            }
                        }
                    }
                }
            }
        }

        try {
            MessageExchangeSupport.notifyOfReply(inout);
        } catch (Exception ex) {
            if (mLog.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
                mLog.log(Level.WARNING, text, ex);
                AlertsUtil.getAlerter().warning(text,
                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HTTPBC-E00759");
            }
            success = false;
        }

        try {
            if (inout.getStatus() == ExchangeStatus.ACTIVE) {
                if (mMonitorEnabled) {
                    LoggingMonitoringUtil.setCheckpoint(trackingId, "Updating-exchange-reply-status", epb);
                }
                if (success) {
                    inout.setStatus(ExchangeStatus.DONE);
                } else {
                    inout.setStatus(ExchangeStatus.ERROR);
                }
                mChannel.send(inout);
                updateTallySends(epb, success);
            }
        } catch (MessagingException ex) {
            String text = mMessages.getString("HTTPBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
            mLog.log(Level.SEVERE, text, ex);
            AlertsUtil.getAlerter().warning(text,
                    HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                    epb.getServiceUnitID(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "HTTPBC-E00759");
        }

        if (ndcEnabled) {
            // "Pop" NDC context
            Logger.getLogger("com.sun.ExitContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                        LoggingMonitoringUtil.getSolutionGroup(epb)});
        }
    }

    /**
     * Process the status reponse for a one-way request originating from this BC.
     */
    public void processOneWayInbound(InOnly inonly) {
        boolean replyAfterProcessing = true;
        //boolean isRetry = isRedeliveryConfigured(inonly);  // for some reason, QoS no longer returns Redelivery Status when retries are maxed out
        boolean isRetry = isRedeliveryEnabled(inonly);
        Endpoint epb = getInboundEndpoint(inonly);
        NormalizedMessage inMsg = inonly.getInMessage();
        String trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") +
                ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");

        // If redelivery is configured and when a message exchange exhausts its redelivery attempts, 
        // redelivery library may re-route the exchange to a different endpoint.
        // That means the endpoint associated with the exchange could be changed.
        // We need to call Redelivery.getEndpoint(exchange) to get the original endpoint back.
        // The following if block tries to locate the inbound endpoint using the above redelivery utility
        // when the first attempt to get it based on the endpoint associated with the exchange fails.
        if (isRetry && epb == null) {
            epb = getInboundEndpoint(inonly, true);
        }

        boolean ndcEnabled = LoggingMonitoringUtil.isNdcEnabled(epb);
        if (ndcEnabled) {
            // "Push" the NDC context based on the setting in the NDC properties file
            Logger.getLogger("com.sun.EnterContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                        LoggingMonitoringUtil.getSolutionGroup(epb)});
        }
        if (ExchangeStatus.DONE.equals(inonly.getStatus())) {
            updateTallyReceives(epb, true);
            // remove the redelivery listener handler - no retry needed.
            if (isRetry) {
                MessageExchangeSupport.removeRedeliveryListener(inonly.getExchangeId());
            }
            if (mMonitorEnabled) {
                LoggingMonitoringUtil.setCheckpoint(trackingId, "Processing-response-message", epb);
            }
        } else if (ExchangeStatus.ERROR.equals(inonly.getStatus())) {
            updateTallyReceives(epb, false);
            if (mMonitorEnabled) {
                LoggingMonitoringUtil.setCheckpoint(trackingId, "Handling-error-status", epb);
            }
            // send alerts
            String errorMsg = (inonly.getError() != null) ? inonly.getError().getMessage() : null;
            if (errorMsg != null) {
                String msg = mMessages.getString("HTTPBC-E00720.Message_exchange_error",
                        new Object[]{
                            String.valueOf(epb.getServiceName()),
                            epb.getEndpointName(),
                            errorMsg
                        });
                mLog.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg,
                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HTTPBC-E00759");
            } else {
                String msg = mMessages.getString("HTTPBC-E00721.Message_exchange_error_no_detail",
                        new Object[]{
                            String.valueOf(epb.getServiceName()),
                            epb.getEndpointName()
                        });
                mLog.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg,
                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HTTPBC-E00721");
            }

            // let's redeliver if it is configured and max attempt count has not been exhausted
            RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(inonly);
            RedeliveryConfig retryConfig = epb.getRedeliveryConfiguration();
            if (retryStatus != null && retryConfig != null) {					// indicator that redelivery is configured
                if (!retryStatus.hasFailed()) {
                    if (mMonitorEnabled) {
                        LoggingMonitoringUtil.setCheckpoint(trackingId, "Handling-redelivery", epb);
                    }
                    handleRedelivery(inonly, epb);
                    return;
                } else {
                    Failure onFailureOption = retryConfig.getFailure();    // we know retryConfig cannot be null if we get this far
                    if (onFailureOption == Failure.suspend) {
                        // suspend the inbound endpoint
                        try {
                            if (mMonitorEnabled) {
                                LoggingMonitoringUtil.setCheckpoint(trackingId, "Suspending-endpoint", epb);
                            }
                            mManagementMBean.suspend(epb.getUniqueName());
                            // emit warning logs and send alerts 
                            String msg = mMessages.getString("HTTPBC-E00806.About_to_suspend_endpoint",
                                    new Object[]{
                                        String.valueOf(epb.getServiceName()),
                                        epb.getEndpointName()
                                    });
                            mLog.log(Level.WARNING, msg);
                            AlertsUtil.getAlerter().warning(msg,
                                    HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                    epb.getServiceUnitID(),
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "HTTPBC-E00806");
                        } catch (MBeanException e) {
                            errorMsg = e.getTargetException().getMessage();
                            if (errorMsg != null) {
                                String msg = mMessages.getString("HTTPBC-E00805.Failed_to_suspend_endpoint",
                                        new Object[]{
                                            String.valueOf(epb.getServiceName()),
                                            epb.getEndpointName(),
                                            errorMsg
                                        });
                                mLog.log(Level.SEVERE, msg);
                                AlertsUtil.getAlerter().warning(msg,
                                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                        epb.getServiceUnitID(),
                                        AlertsUtil.getServerType(),
                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                        NotificationEvent.EVENT_TYPE_ALERT,
                                        "HTTPBC-E00805");
                            }
                        }
                    }
                }
            }
        }

        Map nameToMeta = epb.getOperationNameToMetaData();
        OperationMetaData meta = (OperationMetaData) nameToMeta.get(inonly.getOperation());
        if (meta == null) {
            // try again with operation LocalPart
            meta = (OperationMetaData) nameToMeta.get(inonly.getOperation().getLocalPart());
            if (meta == null) {
                if (mLog.isLoggable(Level.WARNING)) {
                    String msg = mMessages.getString("HTTPBC-W00751.Operation_lookup_failed", inonly.getOperation().toString());
                    mLog.log(Level.WARNING, msg);
                    AlertsUtil.getAlerter().warning(msg,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            epb.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-W00751");
                }
            }
        }

        if (meta != null) {
            replyAfterProcessing = meta.getOneWayReplyAfterProcessing();
        }

        // If configured to wait for completion of request processing, only reply once SE completed processing request
        if (replyAfterProcessing) {
            try {
                MessageExchangeSupport.notifyOfReply(inonly);
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
                    mLog.log(Level.WARNING, text, ex);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            epb.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E00759");
                }
            }
        }
        if (ndcEnabled) {
            // "Pop" NDC context
            Logger.getLogger("com.sun.ExitContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                        LoggingMonitoringUtil.getSolutionGroup(epb)});
        }
    }

    /**
     * Process the request for a request/reply sent to this BC and send back a reply.
     * Also handle the processing of the completion status response 
     */
    public void processRequestReplyOutbound(InOut inout) {
        Endpoint epb = null;
        epb = getOutboundEndpoint(inout);
        boolean ndcEnabled = LoggingMonitoringUtil.isNdcEnabled(epb);
        NormalizedMessage inMsg = inout.getInMessage();
        String trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") +
                ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");

        if (ndcEnabled) {
            // "Push" the NDC context based on the setting in the NDC properties file
            Logger.getLogger("com.sun.EnterContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                        LoggingMonitoringUtil.getSolutionGroup(epb)});
        }
        Transaction transaction = (Transaction) inout.getProperty(
                MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        if (inout.getStatus() == ExchangeStatus.DONE) {
            updateTallyReceives(epb, true);
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
            if (mMonitorEnabled) {
                LoggingMonitoringUtil.setCheckpoint(trackingId, "Handling-error-status-from-inout-exchange", epb);
            }
            if (transaction != null) {
                setTransactionRollbackOnly(transaction);
            }
            updateTallyReceives(epb, false);
        } else {
            Probe soapToDCSendMeasurement = Probe.fine(getClass(),
                    epb.getUniqueName(),
                    "soapToDCSend");
            try {
                updateTallyReceivedRequests(epb);
                Map nameToMeta = epb.getOperationNameToMetaData();
                if (inout.getOperation() == null) {
                    throw new MessagingException(mMessages.getString("HTTPBC-E00808.No_operation_defined_in_ME", inout.getExchangeId()));
                }
                OperationMetaData meta = (OperationMetaData) nameToMeta.get(inout.getOperation().getLocalPart());
                if (meta == null) {
                    throw new MessagingException(mMessages.getString("HTTPBC-E00700.Operation_not_defined",
                            new Object[]{inout.getOperation(), inout.getExchangeId(), inout.getPattern().toString()}));
                }

                // Push the context
                Logger.getLogger("com.sun.EnterContext").fine(epb.getServiceUnitID() + "-" + inout.getOperation());

                if (epb instanceof HttpSoapEndpoint) {
                    dispatch((HttpSoapEndpoint) epb, inout, meta);
                }

                if (epb instanceof HttpEndpoint) {
                    dispatch((HttpEndpoint) epb, inout, meta);
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
                    mLog.log(Level.WARNING, text, ex);
                }
                try {
                    setErrorServerException(inout, ex, (String) null);
                } catch (MessagingException e) {
                    if (mLog.isLoggable(Level.WARNING)) {
                        String text = mMessages.getString("HTTPBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
                        mLog.log(Level.WARNING, text, ex);
                        AlertsUtil.getAlerter().warning(text,
                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                epb.getServiceUnitID(),
                                AlertsUtil.getServerType(),
                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                NotificationEvent.EVENT_TYPE_ALERT,
                                "HTTPBC-E00759");
                    }
                }
                if (transaction != null) {
                    setTransactionRollbackOnly(transaction);
                }
            } finally {
                // Pop the context
                Logger.getLogger("com.sun.ExitContext").fine(epb.getServiceUnitID() + "-" + inout.getOperation());


                //mChannel.send(inout);
                if (soapToDCSendMeasurement != null) {
                    soapToDCSendMeasurement.end();
                }
                //updateTallySentReplies(epb);

            }
        }
        if (ndcEnabled) {
            // "Pop" NDC context
            Logger.getLogger("com.sun.ExitContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                        LoggingMonitoringUtil.getSolutionGroup(epb)});
        }
    }

    /**
     * Process the request for a one-way request sent to this BC and send back a completion status.
     */
    public void processOneWayOutbound(InOnly inonly) {
        Endpoint epb = null;
        Transaction transaction = (Transaction) inonly.getProperty(
                MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        epb = getOutboundEndpoint(inonly);
        boolean ndcEnabled = LoggingMonitoringUtil.isNdcEnabled(epb);
        NormalizedMessage inMsg = inonly.getInMessage();
        String trackingId = ((inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") +
                ((inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null) ? (String) inMsg.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");

        try {
            if (ndcEnabled) {
                // "Push" the NDC context based on the setting in the NDC properties file
                Logger.getLogger("com.sun.EnterContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                            LoggingMonitoringUtil.getSolutionGroup(epb)});
            }
            updateTallyReceivedRequests(epb);

            if (epb instanceof HttpSoapEndpoint) {
                if (inonly.getOperation() == null) {
                    throw new MessagingException(mMessages.getString("HTTPBC-E00808.No_operation_defined_in_ME", inonly.getExchangeId()));
                }
                Map nameToMeta = epb.getOperationNameToMetaData();
                OperationMetaData meta = (OperationMetaData) nameToMeta.get(inonly.getOperation());
                if (meta == null) {
                    // try again with operation LocalPart
                    meta = (OperationMetaData) nameToMeta.get(inonly.getOperation().getLocalPart());
                    if (meta == null) {
                        throw new MessagingException(mMessages.getString("HTTPBC-E00700.Operation_not_defined",
                                new Object[]{inonly.getOperation(), inonly.getExchangeId(), inonly.getPattern().toString()}));
                    }
                }
                if (mMonitorEnabled) {
                    LoggingMonitoringUtil.setCheckpoint(trackingId, "Processing-outbound-request", epb);
                }
                outboundCall(inonly.getInMessage(), meta, (HttpSoapEndpoint) epb, inonly);
            }
        } catch (MessagingException e) {
            // Caused by parsing error (WrapperProcessingException, TransformerException, etc.)
            // during denormalization
            if (mLog.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-W00758.Exception_during_request_processing", e.getLocalizedMessage());
                mLog.log(Level.WARNING, text, e);
            }
            try {
                setErrorClientException(inonly, e, null);
            } catch (MessagingException ex) {
                if (mLog.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00758.Exception_during_request_processing", ex.getLocalizedMessage());
                    mLog.log(Level.WARNING, text, ex);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            epb.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E00758");
                }
            }
            if (transaction != null) {
                setTransactionRollbackOnly(transaction);
            }
        } catch (SOAPException e) {
            // Caused if SOAPMessage changes failed to save
            if (mLog.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-W00758.Exception_during_request_processing", e.getLocalizedMessage());
                mLog.log(Level.WARNING, text, e);
            }
            try {
                setErrorClientException(inonly, e, null);
            } catch (MessagingException ex) {
                if (mLog.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00758.Exception_during_request_processing", ex.getLocalizedMessage());
                    mLog.log(Level.WARNING, text, ex);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            epb.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E00758");
                }
            }
            if (transaction != null) {
                setTransactionRollbackOnly(transaction);
            }
        } catch (Exception e) {
            // Fallback catch for all other exceptions (like runtime exceptions)
            // Parser initialization exceptions caught here too.
            if (mLog.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-W00758.Exception_during_request_processing", e.getLocalizedMessage());
                mLog.log(Level.WARNING, text, e);
            }
            try {
                setErrorServerException(inonly, e, (String) null);
            } catch (MessagingException ex) {
                if (mLog.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00758.Exception_during_request_processing", e.getLocalizedMessage());
                    mLog.log(Level.WARNING, text, e);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            epb.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E00758");
                }
            }
            if (transaction != null) {
                setTransactionRollbackOnly(transaction);
            }
        } finally {
            try {
                if (inonly.getStatus() != ExchangeStatus.ERROR) {
                    inonly.setStatus(ExchangeStatus.DONE);
                }
                mChannel.send(inonly);
                updateTallySends(epb, (inonly.getStatus() == ExchangeStatus.DONE));
            } catch (MessagingException ex) {
                String text = mMessages.getString("HTTPBC-E00780.Exception_set_status");
                mLog.log(Level.SEVERE, text, ex);
                AlertsUtil.getAlerter().warning(text,
                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HTTPBC-E00780");
            }

            if (ndcEnabled) {
                // "Pop" NDC context
                Logger.getLogger("com.sun.ExitContext").log(Level.FINE, "{0}={1}", new Object[]{LoggingMonitoringUtil.SOLUTION_GROUP,
                            LoggingMonitoringUtil.getSolutionGroup(epb)});
            }
        }

    }

    /**
     * Get the endpoint bean for the outbound service
     * @param outboundExchange the message exchange to get the info for
     * @return the endpoint bean corresponding to the outbound service for this
     * message exchange
     */
    Endpoint getOutboundEndpoint(MessageExchange outboundExchange) {
        Endpoint epb = null;
        ServiceEndpoint sep = outboundExchange.getEndpoint();

        // check to see if we have here a dynamic endpoint first
        if (sep instanceof HttpSoapDynamicEndpoint) {
            // grab the Endpoint metadata from the dynamic SE implementation
            epb = ((HttpSoapDynamicEndpoint) sep).getEndpointInfo();
        } else {
            String serviceName = outboundExchange.getEndpoint().getServiceName().toString();
            String endpointName = outboundExchange.getEndpoint().getEndpointName();
            if (mLog.isLoggable(Level.FINER)) {
                mLog.log(Level.FINER, "Trying to locate the endpoint metadata for " + serviceName + " " + endpointName);
            }
            epb = (Endpoint) mEndpoints.get(AbstractEndpoint.getUniqueName(outboundExchange.getEndpoint().getServiceName().getNamespaceURI(),
                    outboundExchange.getEndpoint().getServiceName().getLocalPart(),
                    endpointName,
                    false));
        }
        return epb;
    }

    /**
     * Get the endpoint bean for the inbound service
     * @param inboundExchange the message exchange to get the info for
     * @return the endpoint bean corresponding to the inbound service for this
     * message exchange
     */
    Endpoint getInboundEndpoint(MessageExchange inboundExchange) {
        String serviceName = inboundExchange.getEndpoint().getServiceName().toString();
        String endpointName = inboundExchange.getEndpoint().getEndpointName();
        if (mLog.isLoggable(Level.FINER)) {
            mLog.log(Level.FINER, "Getting inbound info for " + serviceName + " " + endpointName);
        }
        Endpoint epb = (Endpoint) mEndpoints.get(HttpSoapEndpoint.getUniqueName(inboundExchange.getEndpoint().getServiceName().getNamespaceURI(),
                inboundExchange.getEndpoint().getServiceName().getLocalPart(),
                endpointName,
                true));

        return epb;
    }

    /**
     * Get the endpoint bean for the inbound service
     * @param inboundExchange the message exchange to get the info for
     * @param isRetry indicates if redelivery QoS is configured for the endpoint
     * @return the endpoint bean corresponding to the inbound service for this
     * message exchange
     */
    Endpoint getInboundEndpoint(MessageExchange inboundExchange, boolean isRetry) {
        if (isRetry) {
            ServiceEndpoint actualEndpoint = Redelivery.getEndpoint(inboundExchange);
            QName serviceName = actualEndpoint.getServiceName();
            String endpointName = actualEndpoint.getEndpointName();
            if (mLog.isLoggable(Level.FINER)) {
                mLog.log(Level.FINER, "Getting inbound info for " + serviceName + " " + endpointName);
            }

            Endpoint epb = (Endpoint) mEndpoints.get(HttpSoapEndpoint.getUniqueName(serviceName.getNamespaceURI(),
                    serviceName.getLocalPart(),
                    endpointName,
                    true));

            return epb;
        }

        return null;
    }

    /**
     * Handle an outbound message exchange to invoke a webservice over HTTP/SOAP
     * @param outMessage the message to send, still in JBI normalized format
     * @param destinationURL the url of the webservice to invoke
     * @param metadata the descriptions of the webservice methods
     * @param epb endpoint meta-data
     * @param outboundExchange the JBI outbound message exchange
     * @return the reply soap message
     */
    void outboundCall(NormalizedMessage normalizedMessage, OperationMetaData meta, HttpSoapEndpoint endpointMeta, MessageExchange outboundExchange)
            throws SOAPException, MessagingException, Exception {
        // message tracking
        String trackingId = ((normalizedMessage.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) != null) ? (String) normalizedMessage.getProperty(NormalizedMessageProperties.NM_GROUP_ID_PROPERTY) : "") +
                ((normalizedMessage.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) != null) ? (String) normalizedMessage.getProperty(NormalizedMessageProperties.NM_MSG_ID_PROPERTY) : "");

        if (mLog.isLoggable(Level.FINE)) {
            DebugLog.debugLog(mLog, Level.FINE, "Normalized message content", normalizedMessage.getContent());
        }

        Probe denormalizationMeasurement = Probe.info(getClass(),
                endpointMeta.getUniqueName(),
                HttpSoapBindingLifeCycle.PERF_CAT_DENORMALIZATION);
        SOAPMessage soapMsg = null;
        try {
            soapMsg = mDenormalizer.denormalize(normalizedMessage, outboundExchange, meta, mSoapMessageFactory, true);
            if (mMonitorEnabled) {
                try {
                    LoggingMonitoringUtil.setCheckpoint(endpointMeta, trackingId, "Processing-outbound-request", soapMsg.getSOAPPart().getEnvelope());
                } catch (Exception e) {
                    if (mLog.isLoggable(Level.WARNING)) {
                        mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                    }
                }
            }
        } catch (MessagingException t) {
            throw t; // do nothing, need to end instrumentation
        } finally {
            if (denormalizationMeasurement != null) {
                denormalizationMeasurement.end();
            }
        }
        soapMsg.saveChanges();

        // Dispatch to external; make the soap call
        if (mLog.isLoggable(Level.FINE)) {
            DebugLog.debugLog(mLog, Level.FINE, "Sending SOAP envelope", soapMsg.getSOAPPart().getEnvelope());
        }

        Probe createDispatchMeasurement = Probe.fine(getClass(),
                endpointMeta.getUniqueName(),
                "createDispatch");

        Transaction transaction = (Transaction) outboundExchange.getProperty(
                MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        boolean txResumed = false;
        if (transaction != null) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "Got transaction context from message exchange " + outboundExchange.getExchangeId() + ". About to resume transaction before the Dispatch call...");
            }
            if (mMonitorEnabled) {
                try {
                    LoggingMonitoringUtil.setCheckpoint(endpointMeta, trackingId, "Resuming-transaction", soapMsg.getSOAPPart().getEnvelope());
                } catch (Exception e) {
                    if (mLog.isLoggable(Level.WARNING)) {
                        mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                    }
                }
            }
            txResumed = TransactionsUtil.resumeTransaction(transaction);
        }

        Dispatch<SOAPMessage> dispatch = null;

        try {
            boolean isSSL = false;
            // let's see if the NM property is set to overwrite the address URL
            // and to prevent any kind of ClassCastException (you know, user errors)
            String addrUrl = null;
            Object urlProperty = normalizedMessage.getProperty(NormalizedMessageProperties.OUTBOUND_ADDRESS_URL);
            if (urlProperty != null) {
                if (urlProperty instanceof String) {
                    addrUrl = (String) urlProperty;
                } else {
                    if (mLog.isLoggable(Level.WARNING)) {
                        String text = mMessages.getString("HTTPBC-E01052.Invalid_url_nmproperty_type");
                        mLog.log(Level.WARNING, text);
                        AlertsUtil.getAlerter().warning(text,
                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                endpointMeta.getServiceUnitID(),
                                AlertsUtil.getServerType(),
                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                NotificationEvent.EVENT_TYPE_ALERT,
                                "HTTPBC-E01052");
                    }
                }
            }
            if (addrUrl != null) {   // a dynamic URL set as a NM property always takes precedence
                // make sure it's a valid URL
                try {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "A dynamic URL with value '" + addrUrl + "' is assigned as a NM property (" + NormalizedMessageProperties.OUTBOUND_ADDRESS_URL + ")");
                    }
                    URL aUrl = new URL(addrUrl);
                    dispatch = endpointMeta.createDispatch(meta.getSoapActionURL(), addrUrl);
                    if (addrUrl.toLowerCase().startsWith("https")) {
                        isSSL = true;
                    }
                } catch (Exception e) {
                    // Dispatch should not be created properly, but just in case...
                    dispatch = null;

                    // log a warning message, and send an alert for the invalid URL
                    // however, should proceed with normal processing
                    if (mLog.isLoggable(Level.WARNING)) {
                        String text = mMessages.getString("HTTPBC-E01050.Invalid_url_nmproperty", addrUrl);
                        mLog.log(Level.WARNING, text, e);
                        AlertsUtil.getAlerter().warning(text,
                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                endpointMeta.getServiceUnitID(),
                                AlertsUtil.getServerType(),
                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                NotificationEvent.EVENT_TYPE_ALERT,
                                "HTTPBC-E01050");
                    }
                }
            }

            if (dispatch == null) {  // either no address url property is defined, or it has a bad URL.
                ServiceEndpoint sep = outboundExchange.getEndpoint();
                if (sep instanceof HttpSoapDynamicEndpoint) {
                    dispatch = endpointMeta.createDispatch(meta.getSoapActionURL(), ((HttpSoapDynamicEndpoint) sep).getDynamicUrl());
                    if (((HttpSoapDynamicEndpoint) sep).getDynamicUrl().toLowerCase().startsWith("https")) {
                        isSSL = true;
                    }
                } else {
                    dispatch = endpointMeta.createDispatch(meta.getSoapActionURL());
                    if (endpointMeta.getEndpointUrl().toString().toLowerCase().startsWith("https")) {
                        isSSL = true;
                    }
                }
                if (dispatch == null) {
                    if (mLog.isLoggable(Level.SEVERE)) {
                        String endpointName = endpointMeta.getServiceName().toString() + "," + endpointMeta.getEndpointName();
                        String deployedSAName = endpointMeta.getServiceUnitID().substring(0,
                                endpointMeta.getServiceUnitID().indexOf("-" + HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME));
                        String text = mMessages.getString("HTTPBC-E01051.Failed_to_create_Dispatch",
                                new Object[]{endpointName, endpointMeta.getOriginalWSDL().getName(), deployedSAName});
                        mLog.log(Level.SEVERE, text);
                        AlertsUtil.getAlerter().warning(text,
                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                                endpointMeta.getServiceUnitID(),
                                AlertsUtil.getServerType(),
                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                NotificationEvent.EVENT_TYPE_ALERT,
                                "HTTPBC-E01051");
                    }
                }
            }

            // add the JAX-WS handlers if any
            enableJAXWSHandlers(endpointMeta, dispatch);

            Map<String, Object> requestContext = dispatch.getRequestContext();
            putBasicAuthCredential(requestContext, endpointMeta, normalizedMessage);
            propagateCustomProperties(requestContext, normalizedMessage);
            if (mIsHeaderCopyEnabled) {
                setupHTTPHeaders(requestContext, normalizedMessage);
            }

            // set HostnameVerifier if necessary
            HttpClientConnectionProperties clientConnProps = endpointMeta.getHttpClientConnectionProperties();
            if (clientConnProps != null) {
                if (isSSL && !clientConnProps.getHostnameVerification()) {
                    requestContext.put(JAXWSProperties.HOSTNAME_VERIFIER, new HttpsClientVerifier());
                }

                // set connect timeout if any
                if (clientConnProps.getConnectTimeout() != null) {
                    int connectTimeoutInt = clientConnProps.getConnectTimeout().intValue();
                    requestContext.put(JAXWSProperties.CONNECT_TIMEOUT, connectTimeoutInt);
                }

                if (clientConnProps.getReadTimeout() != null) {
                    int readTimeoutInt = clientConnProps.getReadTimeout().intValue();
                    requestContext.put("com.sun.xml.ws.request.timeout", readTimeoutInt);
                }
            }

            if (createDispatchMeasurement != null) {
                createDispatchMeasurement.end();
            }

            // measuring time spent from receiving the ME and right before calling invoke()
            if (mDCReceiveToSoapCallMeasurement != null) {
                mDCReceiveToSoapCallMeasurement.end();
            }            

            Probe callDispatchMeasurement = Probe.fine(getClass(),
                    endpointMeta.getUniqueName(),
                    "invokeDispatch");
            if (outboundExchange instanceof InOnly) {
                if (mMonitorEnabled) {
                    try {
                        LoggingMonitoringUtil.setCheckpoint(endpointMeta, trackingId, "Invoking-one-way-remote-service", soapMsg.getSOAPPart().getEnvelope());
                    } catch (Exception e) {
                        if (mLog.isLoggable(Level.WARNING)) {
                            mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                        }
                    }
                }
                dispatch.invokeOneWay(soapMsg);
            } else {
                if (mMonitorEnabled) {
                    try {
                        LoggingMonitoringUtil.setCheckpoint(endpointMeta, trackingId, "Invoking-inout-remote-service", soapMsg.getSOAPPart().getEnvelope());
                    } catch (Exception e) {
                        if (mLog.isLoggable(Level.WARNING)) {
                            mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                        }
                    }
                }
                //replySoapMsg = dispatch.invoke(soapMsg);
                AsyncRequestContext ctx = new AsyncRequestContext((InOut) outboundExchange, mChannel, meta, endpointMeta);
                ctx.setProbe(callDispatchMeasurement);
                AsyncResponseHandler<SOAPMessage> handler = AsyncResponseDispatcher.instance().getHandler(ctx);
                dispatch.invokeAsync(soapMsg, handler);
                /*if (mMonitorEnabled) {
                try {
                LoggingMonitoringUtil.setCheckpoint(endpointMeta, trackingId, "Received-response-from-inout-remote-service", replySoapMsg.getSOAPPart().getEnvelope());
                } catch (Exception e) {
                if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01310.Failed_to_get_soap_payload_for_checkpointing"));
                }
                }
                }*/
            }

            /*if (callDispatchMeasurement != null) {
            callDispatchMeasurement.end();
            }

            // TODO: allow disabling logging message contents and I18N
            if (replySoapMsg != null && mLog.isLoggable(Level.FINE)) {
            DebugLog.debugLog(mLog, Level.FINE, "Reply received", replySoapMsg.getSOAPPart().getEnvelope());
            }

            return replySoapMsg;        */
        } finally {
            if (dispatch != null)
                endpointMeta.releaseDispatch(dispatch);

            if (txResumed) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Transction was resumed before the Dispatch call. Successfully received a SOAP response. " +
                            "About to suspend the transaction before sending the reply in the message exchange...");
                }
                //TransactionsUtil.suspendTransaction();
            }
        }
    }

    // Transmit the payload as a SOAPMessage.
    private void dispatch(HttpSoapEndpoint endpoint, InOut inout, OperationMetaData opmetadata)
            throws Exception {
        //SOAPMessage response = null;
        SOAPFault soapFault = null;
        SOAPFaultException soapFaultException = null;
        try {
            //response =
            outboundCall(inout.getInMessage(), opmetadata, endpoint, inout);
        } catch (MessagingException e) {
            // Caused by parsing error (WrapperProcessingException, TransformerException, etc.)
            setErrorClientException(inout, e, null);
            throw e;
        } catch (SOAPException e) {
            // Caused if SOAPMessage changes failed to save
            setErrorClientException(inout, e, null);
            throw e;
        } catch (SOAPFaultException e) {
            mLog.log(Level.FINE, "A fault occured, possibly a specified service fault.", e);
            soapFaultException = e;
            soapFault = e.getFault();
        }
    }

    // Extracts from a normalized message all the parts that a message
    // definition requires.  The contents of the parts are returned as
    // DOM nodes or string objects, depending on the value of the stringify
    // argument.
    private List extractPartsValueList(InOut inout, OperationMetaData opmetadata, boolean stringify)
            throws Exception {
        List partNameList = opmetadata.getCachedInputPartNameList();
        List partValueList = new ArrayList();
        if (!partNameList.isEmpty()) {
            NormalizedMessage request = inout.getInMessage();
            Document doc = (Document) Util.messageAsDom(request);
            Element rootNode = doc.getDocumentElement();
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "Request: " + Util.toXml(rootNode, "UTF-8", false));
            }
            partValueList.addAll(createPartValueList(doc, opmetadata.getInputMessage(), stringify));
        }
        return partValueList;
    }

    // Generate the query-string for a HTTP request. The query-string is NOT
    // the full GET URL, only the query portion.
    //
    // If http:urlEncoding is specified, we build an ampersand-delimited
    // sequence of name-value pairs using the specified part name and part value
    // lists.
    // Example:
    //   parts lists have a part "foo" with the value "star"
    //   parts lists have a part "bar" with the value "jax"
    //   we instantiate the string "foo=star&bar=jax"
    //
    // If http:urlReplacement is specified, we take the specified urlContext
    // argument as a 'pattern', and we instantiate from this pattern a
    // string where the replacement points in the pattern are replaced by
    // their corresponding part values from the specified list.
    // Example:
    //    urlContext -> "/(foo)/(bar)/baz"
    //    parts lists have a part "foo" with the value "star"
    //    parts lists have a part "bar" with the value "jax"
    //    we instantiate the string "/star/jax/baz".
    //
    // NOTE: for http:urlEncoding case, we percent-encode as well. This is
    // integral to the name-value pair scheme.
    private String composeGetQueryString(
            String urlContext,
            String urlEncoding,
            String httpOperationLocation,
            List partNameList,
            List partValueList)
            throws Exception {
        StringBuffer buffer = new StringBuffer();

        if (OperationMetaData.HTTP_URL_ENCODING_ENCODED.equals(urlEncoding)) {
            for (int i = 0; i < partNameList.size(); i++) {
                String n = (String) partNameList.get(i);
                buffer.append(URLEncoder.encode(n, URL_DECODE_ENCODING) + "=");
                String v = (String) partValueList.get(i);
                buffer.append(URLEncoder.encode(v, URL_DECODE_ENCODING));
                if (i != partNameList.size() - 1) {
                    buffer.append("&");
                }
            }
        } else if (OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT.equals(urlEncoding)) {
            String tempLocation = httpOperationLocation;
            for (int i = 0; i < partNameList.size(); i++) {
                String n = "(" + (String) partNameList.get(i) + ")";
                String v = (String) partValueList.get(i);
                tempLocation = StringUtil.replaceAll(tempLocation, n, v);
            }
            buffer.append(mergePath(new String[]{urlContext, tempLocation}));
        } else {
            throw new Exception(mMessages.getString("HTTPBC-E00747.Invalid_WSDL_HTTP_URL_encoding", urlEncoding));
        }
        return buffer.toString();
    }

    // Transmit the payload as a HTTP GET request with the payload
    // represented as a query-string or path-info, depending on whether
    // http:urlEncoded or http:urlReplacement representation type is specified.
    private void dispatchHttpGet(HttpEndpoint endpoint, String queryString, InOut inout, OperationMetaData opmetadata)
            throws Exception {
        String httpAddressLocation = endpoint.getEndpointUrl().toString();
        String httpBindingVerb = endpoint.getHttpBindingVerb();
        String httpOperationLocation = opmetadata.getHttpOperationLocation();
        String httpUrlEncoding = opmetadata.getHttpUrlEncoding();



        //Service service = Service.create(endpoint.getServiceName());
        QName portQName = new QName(endpoint.getServiceName().getNamespaceURI(), endpoint.getEndpointName());

        String dynamicURL = getDynamicUrl(endpoint, inout);
        // if the URL is specified using NMProperty or dynamic partnerLink, then
        // the same get the precedence over the URL specified in the WSDL
        String url = dynamicURL != null ? dynamicURL : httpAddressLocation;

        if (httpOperationLocation != null && !"".equals(httpOperationLocation)) {
            url = url.endsWith("/") ? url + httpOperationLocation : url + "/" + httpOperationLocation;
        }
        //service.addPort(portQName, HTTPBinding.HTTP_BINDING, url);
        Dispatch<Source> dispatch = endpoint.createDispatch(url, Source.class);
                //service.createDispatch(portQName, Source.class, Service.Mode.MESSAGE);
        Map<String, Object> requestContext = dispatch.getRequestContext();
        requestContext.put(MessageContext.HTTP_REQUEST_METHOD, httpBindingVerb);
        putBasicAuthCredential(requestContext, endpoint, inout.getInMessage());
        if (mIsHeaderCopyEnabled) {
            setupHTTPHeaders(requestContext, inout.getInMessage());
        }

        HttpClientConnectionProperties clientConnectionProps = endpoint.getHttpClientConnectionProperties();
        if (clientConnectionProps != null) {
            if ((url.toLowerCase().startsWith("https")) && !clientConnectionProps.getHostnameVerification()) {
                requestContext.put(JAXWSProperties.HOSTNAME_VERIFIER, new HttpsClientVerifier());
            }

            // set connect timeout if any
            if (clientConnectionProps.getConnectTimeout() != null) {
                int connectTimeoutInt = clientConnectionProps.getConnectTimeout().intValue();
                requestContext.put(JAXWSProperties.CONNECT_TIMEOUT, connectTimeoutInt);
            }

            if (clientConnectionProps.getReadTimeout() != null) {
                int readTimeoutInt = clientConnectionProps.getReadTimeout().intValue();
                requestContext.put("com.sun.xml.ws.request.timeout", readTimeoutInt);
            }
        }
        if (OperationMetaData.HTTP_URL_ENCODING_ENCODED.equals(httpUrlEncoding)) {
            requestContext.put(MessageContext.QUERY_STRING, queryString);
        } else if (OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT.equals(httpUrlEncoding)) {
            requestContext.put(MessageContext.PATH_INFO, queryString);
        } else {
            throw new Exception(mMessages.getString("HTTPBC-E00747.Invalid_WSDL_HTTP_URL_encoding", httpUrlEncoding));
        }

        try {

            AsyncRequestContext ctx = new AsyncRequestContext(inout, mChannel, opmetadata, endpoint);
            AsyncResponseHandler<Source> handler = AsyncResponseDispatcher.instance().getHandler(ctx);

            //Source response = dispatch.invoke(null);
            dispatch.invokeAsync(null, handler);

        } catch (Exception e) {
            String msg = mMessages.getString("HTTPBC-E00753.Message_dispatch_failed",
                    new Object[]{
                        httpBindingVerb,
                        url,
                        (OperationMetaData.HTTP_URL_ENCODING_ENCODED.equals(httpUrlEncoding) ? queryString : ""),
                        (OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT.equals(httpUrlEncoding) ? queryString : ""),
                        portQName.toString(),
                        e.getLocalizedMessage()
                    });
            throw new Exception(msg, e);
        }
    }

    /**
     * get the dynamic URL if the URL is specified using NMProperty or Dynamic PartnerLink
     * 
     * @param endpoint HttpEndpoint 
     * @param inout MessageExchange 
     * @return dynamic URL
     * @throws MalformedURLException
     */
    private String getDynamicUrl(HttpEndpoint endpoint, InOut inout) throws MalformedURLException {
        String addrUrl = null;

        NormalizedMessage normalizedMessage = inout.getInMessage();

        Object urlProperty = normalizedMessage.getProperty(NormalizedMessageProperties.OUTBOUND_ADDRESS_URL);
        if (urlProperty != null) {
            if (urlProperty instanceof String) {
                addrUrl = (String) urlProperty;
            } else {
                if (mLog.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-E01052.Invalid_url_nmproperty_type");
                    mLog.log(Level.WARNING, text);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            endpoint.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E01052");
                }
            }
        }




        if (addrUrl == null) {
            ServiceEndpoint sep = inout.getEndpoint();
            if (sep != null && sep instanceof HttpSoapDynamicEndpoint) {
                addrUrl = ((HttpSoapDynamicEndpoint) sep).getDynamicUrl();
                mLog.log(Level.FINE, "A dynamic URL with value '" + addrUrl + "' is assigned using DynamicPartnerLink (" + NormalizedMessageProperties.OUTBOUND_ADDRESS_URL + ")");
            }
        } else {
            mLog.log(Level.FINE, "A dynamic URL with value '" + addrUrl + "' is assigned as a NM property (" + NormalizedMessageProperties.OUTBOUND_ADDRESS_URL + ")");
        }



        if (addrUrl != null) {
            URL u = new URL(addrUrl);
        }

        return addrUrl;
    }

    // Transmit the payload as a HTTP POST request. The payload is included as
    // the request's entity-body.
    private void dispatchHttpPost(HttpEndpoint endpoint, Object payload, InOut inout, OperationMetaData opmetadata)
            throws Exception {
        String httpAddressLocation = endpoint.getEndpointUrl().toString();
        String httpBindingVerb = endpoint.getHttpBindingVerb();
        String httpOperationLocation = opmetadata.getHttpOperationLocation();
        String httpUrlEncoding = opmetadata.getHttpUrlEncoding();

        //Service service = Service.create(endpoint.getServiceName());
        QName portQName = new QName(endpoint.getServiceName().getNamespaceURI(), endpoint.getEndpointName());


        String dynamicURL = getDynamicUrl(endpoint, inout);
        // if the URL is specified using NMProperty or dynamic partnerLink, then
        // the same get the precedence over the URL specified in the WSDL
        String url = dynamicURL != null ? dynamicURL : httpAddressLocation;

        if (httpOperationLocation != null && !"".equals(httpOperationLocation)) {
            url = url.endsWith("/") ? url + httpOperationLocation : url + "/" + httpOperationLocation;
        }
        //service.addPort(portQName, HTTPBinding.HTTP_BINDING, url);

        try {            
            if (payload == null) {
                throw new Exception(mMessages.getString("HTTPBC-E01053.Invalid_http_post_payload"));
            }
            if (opmetadata.getInputMessage() == null || opmetadata.getInputMessage().getQName() == null) {
                throw new Exception(mMessages.getString("HTTPBC-E01054.Invalid_http_post_input_message"));
            }

            Dispatch dispatch = null;
            Object request = null;
            
            if (payload instanceof Node){
                dispatch = endpoint.createDispatch(url, Source.class);
                request = new DOMSource((Node)payload);
            }
            else {
                dispatch = endpoint.createDispatch(url, DataSource.class);
                request = new StringDataSourceImpl(portQName,(String) payload);
            }
            Map<String, Object> requestContext = dispatch.getRequestContext();
            requestContext.put(MessageContext.HTTP_REQUEST_METHOD, httpBindingVerb);
            putBasicAuthCredential(requestContext, endpoint, inout.getInMessage());
            if (mIsHeaderCopyEnabled) {
                setupHTTPHeaders(requestContext, inout.getInMessage());
            }
            HttpClientConnectionProperties clientConnectionProps = endpoint.getHttpClientConnectionProperties();
            if (clientConnectionProps != null) {
                if ((url.toLowerCase().startsWith("https")) && !clientConnectionProps.getHostnameVerification()) {
                    requestContext.put(JAXWSProperties.HOSTNAME_VERIFIER, new HttpsClientVerifier());
                }

                // set connect timeout if any
                if (clientConnectionProps.getConnectTimeout() != null) {
                    int connectTimeoutInt = clientConnectionProps.getConnectTimeout().intValue();
                    requestContext.put(JAXWSProperties.CONNECT_TIMEOUT, connectTimeoutInt);
                }

                if (clientConnectionProps.getReadTimeout() != null) {
                    int readTimeoutInt = clientConnectionProps.getReadTimeout().intValue();
                    requestContext.put("com.sun.xml.ws.request.timeout", readTimeoutInt);
                }
            }
            // check if payload and input message type are valid
            

            
            //DataSource response = dispatch.invoke(request);

            AsyncRequestContext ctx = new AsyncRequestContext(inout, mChannel, opmetadata, endpoint);
            AsyncResponseHandler handler = AsyncResponseDispatcher.instance().getHandler(ctx);
            dispatch.invokeAsync(request, handler);


        } catch (Exception e) {
            String msg = mMessages.getString("HTTPBC-E00753.Message_dispatch_failed",
                    new Object[]{
                        httpBindingVerb,
                        url,
                        "", // no query string for POST
                        "", // no path_info for POST
                        portQName.toString(),
                        e.getLocalizedMessage()
                    });
            throw new Exception(msg, e);
        }
    }

    // Execute a HTTP request, GET or POST depending on the "verb" defined
    // for the specified endpoint.
    private void dispatch(HttpEndpoint endpoint, InOut inout, OperationMetaData opmetadata)
            throws Exception {
        String httpBindingVerb = endpoint.getHttpBindingVerb();
        String httpUrlEncoding = opmetadata.getHttpUrlEncoding();
        String httpOperationLocation = opmetadata.getHttpOperationLocation();

        if (HttpEndpoint.HTTP_BINDING_VERB_GET.equals(httpBindingVerb)) {
            // Extract parts from message exchange, and create the GET request
            List partValueList = extractPartsValueList(inout, opmetadata, true);
            List partNameList = opmetadata.getCachedInputPartNameList();

            // Guard for the case when a message definition does not utilize
            // all the parts available in the message exchange. We require
            // complete congruence. If there is a part in the message that is
            // not used by the message definition, or as a more
            // serious offense, if there is a part the message definition needs
            // but is not in the normalized message, then we reject the message.
            if (partValueList.size() < partNameList.size()) {
                String errMsg;
                if (mLog.isLoggable(Level.FINE)) {
                    NormalizedMessage request = inout.getInMessage();
                    Document doc = (Document) Util.messageAsDom(request);
                    Element rootNode = doc.getDocumentElement();
                    errMsg = mMessages.getString("HTTPBC-E00781.Bad_content_detail", Util.toXml(rootNode, "UTF-8", false));
                } else {
                    errMsg = mMessages.getString("HTTPBC-E00781.Bad_content");
                }
                Exception ex = new Exception(errMsg);
                setErrorClientException(inout, ex, null);
                throw ex;
            }

            String queryString = composeGetQueryString(endpoint.getUrlContext(),
                    httpUrlEncoding, httpOperationLocation, partNameList, partValueList);

            // Send the query
            dispatchHttpGet(endpoint, queryString, inout, opmetadata);

        } else if (HttpEndpoint.HTTP_BINDING_VERB_POST.equals(httpBindingVerb)) {
            // Extract parts from message exchange, and create the GET request
            List partValueList = extractPartsValueList(inout, opmetadata, false);

            // We do not allow input consisting of more than one part,
            // because POST expects a blob (or document) to populate the
            // HTTP request's entity-body, and in WSDL/XSD terms, that means
            // one element-type part, or one simpleType part.
            if (partValueList.size() > 1 || partValueList.size() == 0) {
                String errMsg;
                NormalizedMessage request = inout.getInMessage();
                Document doc = (Document) Util.messageAsDom(request);
                Element rootNode = doc.getDocumentElement();
                if (partValueList.size() > 1) {
                    errMsg = mMessages.getString("HTTPBC-E00782.Multipart_POST_Unsupported_detail", Util.toXml(rootNode, "UTF-8", false));
                } else {
                    errMsg = mMessages.getString("HTTPBC-E00781.Bad_content_detail", Util.toXml(rootNode, "UTF-8", false));
                }
                Exception ex = new Exception(errMsg);
                setErrorClientException(inout, ex, null);
                throw ex;
            }

            // Obtain a Node representation of the payload
            Object value = partValueList.get(0);
            Node msgNode = (value instanceof String
                    ? Util.textAsDomWithXMLCheck((String) value)
                    : (value instanceof Document
                    ? ((Document) value).getDocumentElement()
                    : null));
            // Getting here means the payload part is of an unknown/unsupported
            // type, or is a Document that yielded nothing as its document element.
            if ((msgNode == null) && !(value instanceof String)) {
                NormalizedMessage request = inout.getInMessage();
                Document doc = (Document) Util.messageAsDom(request);
                Element rootNode = doc.getDocumentElement();
                String errMsg = mMessages.getString("HTTPBC-E00781.Bad_content_detail", Util.toXml(rootNode, "UTF-8", false));
                Exception ex = new Exception(errMsg);
                setErrorClientException(inout, ex, null);
                throw ex;
            }

            // Send the query
            dispatchHttpPost(endpoint, (msgNode != null ? msgNode : value), inout, opmetadata);

        } else {
            throw new Exception(mMessages.getString("HTTPBC-E00749.Invalid_HTTP_verb", httpBindingVerb));
        }
    }

    private List createPartValueList(Document normalizedMsg, Message msgDef, boolean stringify)
            throws WrapperProcessingException,
            TransformerException,
            UnsupportedEncodingException {
        List partValueList = new ArrayList();
        WrapperParser parser = null;
        Transformer transformer = null;
        try {
            parser = HelperFactory.createParser();
            parser.parse(normalizedMsg, msgDef);
            transformer = cTransformerPool.retrieve();

            String[] partNames = parser.getPartNames();
            for (int i = 0; i < partNames.length; ++i) {
                String partName = partNames[i];
                if (isSimpleMessagePart(msgDef, partName)) {
                    StringBuffer contentBuffer = new StringBuffer();
                    NodeList nodeList = parser.getPartNodes(partName);
                    for (int n = 0, N = nodeList.getLength(); n < N; ++n) {
                        Node node = nodeList.item(n);
                        contentBuffer.append(node.getTextContent());
                    }
                    partValueList.add(contentBuffer.toString());
                    contentBuffer.delete(0, contentBuffer.length());
                } else {
                    Document document = mBuilder.newDocument();
                    NodeList nodeList = parser.getPartNodes(partNames[i]);
                    for (int n = 0, N = nodeList.getLength(); n < N; ++n) {
                        Node node = nodeList.item(n);
                        if (node != null) {
                            copyNode(document, node);
                        }
                    }
                    if (stringify) {
                        StringWriter writer = new StringWriter();
                        StreamResult dest = new StreamResult(writer);
                        DOMSource source = new DOMSource(document);
                        transformer.transform(source, dest);
                        writer.flush();
                        partValueList.add(writer.toString());
                    } else {
                        partValueList.add(document);
                    }
                }
            }
        } finally {
            cTransformerPool.relinquish(transformer);
        }
        return partValueList;
    }

    private boolean isSimpleMessagePart(Message msgDef, String partName) {
        Part p = msgDef.getPart(partName);
        boolean isSimple = p.getElementName() == null && WSDLUtilities.isBuiltInType(p.getTypeName().getNamespaceURI());
        return isSimple;
    }

    private void setErrorUnsupportedExchangePattern(MessageExchange exchange, String msg, String detail)
            throws MessagingException {
        // Do not modify an exchange whose status is already set to ERROR.
        // The exchange may already have been previously initialized by an earlier setError... call.
        if (exchange.getStatus() != ExchangeStatus.ERROR) {
            if (detail == null) {
                detail = "";
            }
            IllegalArgumentException ex = new IllegalArgumentException(msg);
            exchange.setError(ex);
            exchange.setStatus(ExchangeStatus.ERROR);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE, "VersionMismatch");
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING, msg);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR, COMPONENT_NAME);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTDETAIL, detail);
        }
    }

    private void setErrorServerException(MessageExchange exchange, Exception ex, String detail)
            throws MessagingException {
        // Do not modify an exchange whose status is already set to ERROR.
        // The exchange may already have been previously initialized by an earlier setError... call.
        if (exchange.getStatus() != ExchangeStatus.ERROR) {
            if (detail == null) {
                detail = "";
            }
            exchange.setError(ex);
            exchange.setStatus(ExchangeStatus.ERROR);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE, "Server");
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING, ex.getLocalizedMessage());
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR, COMPONENT_NAME);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTDETAIL, detail);
        }
    }

    private void setErrorServerException(MessageExchange exchange, Exception ex, SOAPFault fault)
            throws MessagingException {
        // Do not modify an exchange whose status is already set to ERROR.
        // The exchange may already have been previously initialized by an earlier setError... call.
        if (exchange.getStatus() != ExchangeStatus.ERROR) {
            String faultCode = fault.getFaultCode();
            if (faultCode == null) {
                faultCode = "Server";
            }

            String faultString = fault.getFaultString();
            if (faultString == null) {
                faultString = "";
            }

            String faultActor;
            try {
                faultActor = fault.getFaultActor();
                if (faultActor == null) {
                    faultActor = "";
                }
            } catch (UnsupportedOperationException e) {
                faultActor = "";
            }

            StringBuffer faultDetail = new StringBuffer();
            javax.xml.soap.Detail details = fault.getDetail();
            if (details != null) {
                Iterator detailsIteration = details.getDetailEntries();
                while (detailsIteration.hasNext()) {
                    DetailEntry detailEntry = (DetailEntry) detailsIteration.next();
                    try {
                        faultDetail.append(Util.toXml(detailEntry, "UTF-8", true));
                    } catch (Exception e) {
                        if (faultDetail.length() == 0) {
                            faultDetail.append(faultString);
                        }
                        break;
                    }
                }

            } else {
                faultDetail.append(faultString);
            }


            exchange.setError(ex);
            exchange.setStatus(ExchangeStatus.ERROR);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE, faultCode);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING, faultString);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR, faultActor);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTDETAIL, faultDetail.toString());
        }
    }

    private void setErrorClientException(MessageExchange exchange, Exception ex, String detail)
            throws MessagingException {
        // Do not modify an exchange whose status is already set to ERROR.
        // The exchange may already have been previously initialized by an earlier setError... call.
        if (exchange.getStatus() != ExchangeStatus.ERROR) {
            if (detail == null) {
                detail = "";
            }
            exchange.setError(ex);
            exchange.setStatus(ExchangeStatus.ERROR);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE, "Client");
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING, ex.getLocalizedMessage());
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR, COMPONENT_NAME);
            exchange.setProperty(Denormalizer.PROPERTY_ERROR_FAULTDETAIL, detail);
        }
    }

    private void copyNode(Document document, Node node) {
        node = document.importNode(node, true);
        document.appendChild(node);

        NamedNodeMap attrs = node.getAttributes();
        NamedNodeMap destAttrs = document.getAttributes();
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                Node attr = attrs.item(i);
                Node n = document.importNode(attr, true);
                if (destAttrs != null) {
                    destAttrs.setNamedItemNS(n);
                }
            }
        }
    }

    private void updateTallyReceivedReplies(Endpoint e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementReceivedReplies();
            }
        }
    }

    private void updateTallyReceivedRequests(Endpoint e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementReceivedRequests();
            }
        }
    }

    private void updateTallySentReplies(Endpoint e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementSentReplies();
            }
        }
    }

    private void updateTallySentRequests(Endpoint e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementSentRequests();
            }
        }
    }

    private void updateTallyReceives(Endpoint e, boolean successfulReceive) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                if (successfulReceive) {
                    status.incrementReceivedDones();
                } else {
                    status.incrementReceivedErrors();
                }
            }
        }
    }

    private void updateTallySends(Endpoint e, boolean successfulSend) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                if (successfulSend) {
                    status.incrementSentDones();
                } else {
                    status.incrementSentErrors();
                }
            }
        }
    }

    private String mergePath(String[] segments) {
        StringBuffer pathBuffer = new StringBuffer();
        StringBuffer segBuffer = new StringBuffer();

        for (String seg : segments) {
            if (seg != null) {
                segBuffer.append(seg);

                while (segBuffer.length() > 0 && segBuffer.charAt(0) == '/') {
                    segBuffer.deleteCharAt(0);
                }
                while (segBuffer.length() > 0 && segBuffer.charAt(segBuffer.length() - 1) == '/') {
                    segBuffer.deleteCharAt(segBuffer.length() - 1);
                }

                pathBuffer.append(segBuffer).append("/");
                segBuffer.delete(0, segBuffer.length());
            }
        }

        return pathBuffer.substring(0, pathBuffer.length() - 1);
    }

    private void putBasicAuthCredential(Map<String, Object> requestContext, Endpoint endpoint, NormalizedMessage normalizedMessage) {
        String dynamicUsername = (String) normalizedMessage.getProperty(NormalizedMessageProperties.OUTBOUND_BASIC_AUTH_USERNAME);
        String dynamicPassword = (String) normalizedMessage.getProperty(NormalizedMessageProperties.OUTBOUND_BASIC_AUTH_PASSWORD);

        if (dynamicUsername != null && !"".equals(dynamicUsername)) {   // even if basic auth is not explicitly configured in the WSDL
            requestContext.put(BindingProvider.USERNAME_PROPERTY, dynamicUsername);
        } else {
            if (endpoint.getBasicAuthCredential() != null &&
                    endpoint.getBasicAuthCredential().getName() != null &&
                    !"".equals(endpoint.getBasicAuthCredential().getName())) {
                requestContext.put(BindingProvider.USERNAME_PROPERTY, endpoint.getBasicAuthCredential().getName());
            }
        }

        if (dynamicPassword != null && !"".equals(dynamicPassword)) { // even if basic auth is not explicitly configured in the WSDL
            requestContext.put(BindingProvider.PASSWORD_PROPERTY, dynamicPassword);
        } else {
            if (endpoint.getBasicAuthCredential() != null &&
                    endpoint.getBasicAuthCredential().getPassword() != null) {
                requestContext.put(BindingProvider.PASSWORD_PROPERTY, new String(endpoint.getBasicAuthCredential().getPassword()));
            }
        }
    }

    private void propagateCustomProperties(Map<String, Object> requestContext, NormalizedMessage normalizedMessage) {
        Map<String, Object> customProperties = (Map<String, Object>) normalizedMessage.getProperty(NormalizedMessageProperties.OUTBOUND_CUSTOM_PROPERTY);
        if (customProperties != null) {
            for (Map.Entry<String, Object> entry : customProperties.entrySet()) {
                requestContext.put(entry.getKey(), entry.getValue());
            }
        }
    }

    private void setupHTTPHeaders(Map<String, Object> requestContext, NormalizedMessage normalizedMessage) {
        Map<String, String> httpHeaderProperty =
                (Map<String, String>) normalizedMessage.getProperty(NormalizedMessageProperties.OUTBOUND_HTTP_HEADERS_PROPERTY);
        Map<String, List<String>> headers = new HashMap<String, List<String>>();
        if (httpHeaderProperty != null) {
            for (Map.Entry<String, String> entry : httpHeaderProperty.entrySet()) {
                // make sure we do not propagate the "custom" client host/port entries
                if (!NormalizedMessageProperties.HTTP_HEADER_ClIENT_HOST_NAME.equals(entry.getKey()) &&
                        !NormalizedMessageProperties.HTTP_HEADER_CLIENT_PORT_NUMBER.equals(entry.getKey())) {
                    headers.put(entry.getKey(), Collections.singletonList(entry.getValue()));
                }
            }
        }

        requestContext.put(MessageContext.HTTP_REQUEST_HEADERS, headers);
    }

    private boolean isRedeliveryConfigured(MessageExchange exchange) {
        RedeliveryStatus redeliveryStatus = Redelivery.getRedeliveryStatus(exchange);
        return (redeliveryStatus != null);
    }

    private boolean isRedeliveryEnabled(MessageExchange exchange) {
        boolean configured = false;
        EndpointInfo info = new EndpointInfo(false,
                exchange.getEndpoint().getEndpointName(),
                null,
                exchange.getEndpoint().getServiceName(),
                null);
        RedeliveryConfig retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
        if (retryConfig != null) {
            configured = true;
        } else {
            // Well, try Redelivery.getEndpoint() API. It could be that the max redelivery 
            // attempts have been exhausted, and the ME was rerouted to a QoS endpoint
            ServiceEndpoint actualEndpoint = Redelivery.getEndpoint(exchange);
            QName serviceName = actualEndpoint.getServiceName();
            String endpointName = actualEndpoint.getEndpointName();
            Endpoint epb = (Endpoint) mEndpoints.get(HttpSoapEndpoint.getUniqueName(serviceName.getNamespaceURI(),
                    serviceName.getLocalPart(),
                    endpointName,
                    true));

            if (epb != null) {
                info = new EndpointInfo(false,
                        endpointName,
                        null,
                        serviceName,
                        null);
                retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
                if (retryConfig != null) {
                    configured = true;
                }
            }
        }

        return configured;
    }

    private void handleRedelivery(MessageExchange exchange, Endpoint epb) {
        try {
            MessageExchangeSupport.notifyOfRedelivery(exchange);
        } catch (Exception e) {
            String groupId = (String) exchange.getProperty(SoapNormalizer.CRMP_GROUP_ID);
            String messageId = (String) exchange.getProperty(SoapNormalizer.CRMP_MESSAGE_ID);
            if (mLog.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-E01036.Failed_to_process_redelivery", new Object[]{groupId, messageId});
                mLog.log(Level.WARNING, text, e);
                AlertsUtil.getAlerter().warning(text,
                        HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HTTPBC-E01036");
            }
        }
    }

    private void setTransactionRollbackOnly(Transaction transaction) {
        boolean txResumed = false;
        if (transaction != null) {
            txResumed = TransactionsUtil.resumeTransaction(transaction);
        }
        if (txResumed) {
            TransactionsUtil.setRollbackOnlyOnTransaction(transaction);
            try {
                TransactionsUtil.suspendTransaction();
            } catch (SystemException se) {
                // really no recourse for this as tx state is probably messed up
                // Note - TransactionsUtil logs exception
            }
        }
    }

    private void enableJAXWSHandlers(Endpoint endpointMeta, Dispatch dispatch) throws Exception {
        List handlerList = endpointMeta.getHandlers();
        if (handlerList.size() == 0) {
            return;
        }

        // load the handler jars
        List handlerLibs = endpointMeta.getHandlerLibPaths();
        URL[] handlerLibUrls = new URL[handlerLibs.size()];
        int count = 0;
        for (Iterator it = handlerLibs.iterator(); it.hasNext();) {
            File libFile = (File) it.next();
            if (libFile.exists()) {
                handlerLibUrls[count] = libFile.toURL();
                count++;
            }
        }

        if (handlerLibUrls.length > 0) {
            URLClassLoader handlerClassLoader = new URLClassLoader(handlerLibUrls, Thread.currentThread().getContextClassLoader());
            Thread.currentThread().setContextClassLoader(handlerClassLoader);
        }

        // instantiate the handler instances 
        List handlers = new ArrayList();
        for (Iterator it = handlerList.iterator(); it.hasNext();) {
            // get the fully qualified handler class name
            HttpSoapHandler handlerMeta = (HttpSoapHandler) it.next();
            try {
                Class handlerClass = Class.forName(handlerMeta.getHandlerClassName(), true, Thread.currentThread().getContextClassLoader());
                // we require handler implementation to follow the Java Bean convention
                handlers.add(handlerClass.newInstance());
            } catch (Exception e) {
                if (mLog.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-E01055.Failed_to_instantiate_handler");
                    mLog.log(Level.WARNING, text, e);
                    AlertsUtil.getAlerter().warning(text,
                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME,
                            endpointMeta.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "HTTPBC-E01055");
                }
            }
        }
        ((BindingProvider) dispatch).getBinding().setHandlerChain(handlers);
    }

    // overide java default SSL HostnameVerifier to always return true
    // when verification flag is turned off
    private static class HttpsClientVerifier implements HostnameVerifier {

        public boolean verify(String s, SSLSession sslSession) {
            return true;
        }
    }
}
