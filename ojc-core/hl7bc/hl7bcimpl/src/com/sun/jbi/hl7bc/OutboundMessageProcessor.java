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

package com.sun.jbi.hl7bc;

import static com.sun.jbi.hl7bc.extservice.client.HL7ConnectorFactory.createHL7Connector;
import static com.sun.jbi.hl7bc.util.XmlUtil.createDocumentFromXML;
import static com.sun.jbi.hl7bc.util.XmlUtil.transformToDOMResult;
import static com.sun.jbi.hl7bc.util.XmlUtil.transformToString;

import java.io.StringWriter;
import java.net.URI;
import java.net.URL;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.naming.InitialContext;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import net.java.hulp.measure.Probe;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.qos.redelivery.Redirect;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.connection.Connection;
import com.sun.jbi.hl7bc.connection.ConnectionInfo;
import com.sun.jbi.hl7bc.connection.HL7BCConnectionManager;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7CommunicationControl;
import com.sun.jbi.hl7bc.extensions.HL7Input;
import com.sun.jbi.hl7bc.extensions.HL7Message;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.extensions.HL7Output;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extensions.TcpRoleEnum;
import com.sun.jbi.hl7bc.extservice.HL7OutboundMessageValidationProcessor;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.extservice.ValidationInfo;
import com.sun.jbi.hl7bc.extservice.ack.ACKBuilder;
import com.sun.jbi.hl7bc.extservice.ack.MSAInfo;
import com.sun.jbi.hl7bc.extservice.ack.MSHInfo;
import com.sun.jbi.hl7bc.extservice.client.HL7Connector;
import com.sun.jbi.hl7bc.extservice.client.TCPIpHL7Connector;
import com.sun.jbi.hl7bc.extservice.client.HL7ConnectorFactory.TransportProtocolType;
import com.sun.jbi.hl7bc.extservice.communicationcontrols.HL7CommunicationControlsInfo;
import com.sun.jbi.hl7bc.extservice.llp.LLPProviderFactory.Util;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnection;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObjectFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.JournalHL7MessageLogDBO;
import com.sun.jbi.hl7bc.extservice.persist.dbo.SequenceNumDBO;
import com.sun.jbi.hl7bc.extservice.server.OutboundTcpServerHL7Connector;
import com.sun.jbi.hl7bc.extservice.server.OutboundTcpServerHL7ConnectorPool;
import com.sun.jbi.hl7bc.extservice.support.hl7v25.HL7SFTSegmentProcessor;
import com.sun.jbi.hl7bc.util.AlertsUtil;
import com.sun.jbi.hl7bc.util.HL7MMUtil;
import com.sun.jbi.hl7bc.util.LogSupport;
import com.sun.jbi.hl7bc.util.NMPropertiesUtil;
import com.sun.jbi.hl7bc.util.NMPropertiesUtil.NMPropertiesParsingException;
import com.sun.jbi.hl7bc.util.NMPropertiesUtil.OutboundNMProperties;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

/**
 * Process replies/requests received from the SE.
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class OutboundMessageProcessor implements HL7Constants {

    private static final String CLIENT = "Client";

    private static final String SERVER = "Server";

    private static final Logger mLog = Logger.getLogger(OutboundMessageProcessor.class.getName());

    private Collection mServiceUnits;

    private MessagingChannel mChannel;

    private Map<String, InboundReplyContext> mInboundExchanges;

    private Map<URL, Encoder> mAckEncoders;

    private RuntimeConfiguration mRuntimeConfig;

    private HL7Denormalizer mHL7Denormalizer;

    private HL7Normalizer mHL7Normalizer;

    private int mExpSeqno;

    private int mCountNoResponse = 0;

    private int mCountNakReceived = 0;

    private ComponentContext mComponentContext;

    private DBConnectionFactory mDBConnectionFactory;

    private DBObjectFactory mDBObjectFactory;

    private Transformer mTrans;

    private HL7OutboundMessageValidationProcessor mHL7OutboundMsgValidationprocessor;

    private HL7CommunicationControl mHL7CommunicationControl;

    private HL7CommunicationControlsInfo mHL7CommunicationControlsInfo;

    private static final String PROVISIONING_ID = "Provider";

    private static final String CONSUMING_ID = "Consumer";
    private boolean mHL7ConnectorTakenFromPool = false;
    private NormalizedMessage mAckNormalizedMsg;
    
    private DOMResult mACKDOMresult;

    private OutboundTcpServerHL7ConnectorPool mOutboundServerPool;
    
    // <checkpoint>
    private boolean mMonitorEnabled = false;
    private boolean mNdcEnabled = false;
    private String mChkptMsgId = null;

    // </checkpoint>

    public OutboundMessageProcessor(ComponentContext compContext, MessagingChannel chnl, Collection serviceUnits,
            Map<String, InboundReplyContext> inboundMessageExchanges, RuntimeConfiguration runtimeConfig, OutboundTcpServerHL7ConnectorPool theOutboundServerPool)
            throws Exception {
        mComponentContext = compContext;
        mChannel = chnl;
        mServiceUnits = serviceUnits;
        mInboundExchanges = inboundMessageExchanges;
        mAckEncoders = new ConcurrentHashMap<URL, Encoder>();
        mOutboundServerPool = theOutboundServerPool;
        try {
            mHL7Normalizer = new HL7Normalizer();
        } catch (Exception e) {
            String text = I18n.msg("E0326: Failed to create HL7 Normalizer due to error=[{0}]", e.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(text, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0016");
            throw new IllegalStateException(text, e);
        }
        mHL7Denormalizer = new HL7Denormalizer();
        mRuntimeConfig = runtimeConfig;
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(I18n.msg(
                    "E0188: Exception when creating transformer in InboundMessageProcessor, e=[{0}]", ex.getMessage()),
                    ex);
        }
        // get the Initial Context
        InitialContext ic = mComponentContext.getNamingContext();
        // get the properties
        Properties props = mRuntimeConfig.getProperties();
        //mDBConnectionFactory = new DBConnectionFactory(props, ic, mComponentContext.getInstallRoot());
        //mDBObjectFactory = DBObjectFactory.getDBObjectFactory(mDBConnectionFactory.getType());
    }

    /**
     * Process the message exchange
     */
    public void processMessage(MessageExchange msgExchange) throws Exception {
        final String exchangeId = msgExchange.getExchangeId();
        String redeliveryQoSMsgID = (String) msgExchange.getProperty(InboundMessageProcessor.REDELIVERY_QOS_MSG_ID);
        boolean inbound = redeliveryQoSMsgID != null && mInboundExchanges.containsKey(redeliveryQoSMsgID)
                && !isRequest(msgExchange);
        // capture request/reply time here
        long receivedMsgXchangeTime = System.currentTimeMillis();
        InboundReplyContext inbReplyContext = null;
        if (inbound) {
            inbReplyContext = mInboundExchanges.get(redeliveryQoSMsgID);
            long invocationTime = inbReplyContext.getRequestInvocationTime();
            if (mLog.isLoggable(Level.FINE)) {
                long difference = receivedMsgXchangeTime - invocationTime;
                mLog.log(Level.FINE, I18n.msg("Response for message exchange ID {0} is received after {1} ms",
                        exchangeId, new Long(difference)));
            }
        }
        Endpoint endpoint = findEndpoint(msgExchange);
        if (endpoint == null) {
            boolean error = true;
            if (inbound) {
                // Check if it was a redelivery
                RedeliveryStatus redeliveryStatus = Redelivery.getRedeliveryStatus(msgExchange);
                if (redeliveryStatus != null) {
                    // Get original end point
                    ServiceEndpoint actualEndpoint = Redelivery.getEndpoint(msgExchange);
                    if (actualEndpoint != null) {
                        endpoint = findEndpoint(actualEndpoint);
                        if (endpoint != null) {
                            error = false;
                        }
                    }
                }
            }
            if (error) {
                String errMsg = I18n.msg(
                        "E0194: Unable to locate a {0} endpoint with name {1} of service {2}; fail on processing message exchange with ID {3}",
                        msgExchange.getRole(), msgExchange.getEndpoint().toString(), msgExchange.getService(),
                        msgExchange.getExchangeId());

                MessageExchangeProcessingException mpex = new MessageExchangeProcessingException(errMsg);

                AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0194");

                handleMessagePreProcessingErrors(inbound, inbReplyContext, msgExchange, mpex);

                return;
            }
        }
        // determine whether the endpoint is suspend or not
        if (endpoint.getServiceUnit().isSuspended(endpoint)) {

            String errMsg = I18n.msg(
                    "E0195: Fail to process message exchange with ID {0} as the endpoint with name {1} of service {2} is in suspended state",
                    msgExchange.getExchangeId(), msgExchange.getEndpoint().toString(), msgExchange.getService());

            MessageExchangeProcessingException mpex = new MessageExchangeProcessingException(errMsg);

            AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0195");

            setErrorStatusAndSend(msgExchange, mpex);

            return;

        }

        mHL7OutboundMsgValidationprocessor = new HL7OutboundMessageValidationProcessor();
        mHL7CommunicationControlsInfo = new HL7CommunicationControlsInfo(endpoint);

        // <checkpoint>
        try{
            mMonitorEnabled = HL7MMUtil.isMonitorEnabled(endpoint);
            mNdcEnabled = HL7MMUtil.isNdcEnabled(endpoint);
        }catch(Exception ex){
            //ignore
        }
        // </checkpoint>
        
        URI pattern = msgExchange.getPattern();
        if (mLog.isLoggable(Level.FINER)) {
            mLog.log(Level.FINER, I18n.msg("I0137: Pattern for message exchange ID {0} is {1}",
                    msgExchange.getExchangeId(), pattern.toString()));
        }

        switch (ExchangePattern.valueOf(msgExchange)) {
        case IN_ONLY:
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, I18n.msg("Received in-only message {0}.", exchangeId));
            }
            if (inbound) {
                processOneWayInbound((InOnly) msgExchange, endpoint, inbReplyContext);
            } else {
                processOneWayOutbound((InOnly) msgExchange, endpoint);
            }
            break;
        case IN_OUT:
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, I18n.msg("Received in-out message {0}.", exchangeId));
            }
            if (inbound) {
                processRequestReplyInbound((InOut) msgExchange, endpoint, inbReplyContext);
            } else {
                processRequestReplyOutbound((InOut) msgExchange, endpoint);
            }
            break;
        default:
            String errMsg = I18n.msg(
                    "E0196: The message exchange with ID {0} is of an unsupported message exchange pattern {1}",
                    msgExchange.getExchangeId(), msgExchange.getPattern());

            MessageExchangeProcessingException mpex = new MessageExchangeProcessingException(errMsg);

            handleMessagePreProcessingErrors(inbound, inbReplyContext, msgExchange, mpex);
            return;
        }
    }

    /**
     * Process the reply for a request/reply request originating from this BC.
     */
    private void processRequestReplyInbound(InOut inout, Endpoint endpoint, InboundReplyContext inbReplyContext) {
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg("I0138: Processing request-reply inbound message"));
        }
        QName qualOperation = inout.getOperation();
        String msg = null;
        if (inout.getOutMessage() != null) {
            endpoint.getEndpointStatus().incrementReceivedReplies();
            try {
                try {
                    inbReplyContext.getMessageExchangeReplyListener().onOutput(inout);
                } catch (Exception ex) {
                    throw ex;
                }
                inout.setStatus(ExchangeStatus.DONE);
                endpoint.getEndpointStatus().incrementSentDones();
            } catch (Exception t) { // catch all errors
                String faultCode = null;
                String faultDetail = null;
                String faultString = null;
                if (t instanceof WrapperProcessingException) {
                    faultCode = CLIENT;
                    faultDetail = "Unable to denormalize message. The normalize reply message from consumer is not correct.";
                    faultString = I18n.msg(
                            "E0198: Unable to denormalize reply from internal service engine/binding component for message exchange {0}, end point {1} and operation {2}.",
                            inout.getExchangeId(), endpoint.toString(), qualOperation.toString());
                } else {
                    faultCode = SERVER;
                    faultDetail = "Unable to process message exchange.";
                    faultString = I18n.msg(
                            "W0121: Unable to process message exchange {0}, end point {1} and operation {2}.",
                            inout.getExchangeId(), endpoint.toString(), qualOperation.toString());
                }
                t = new Exception(faultString, t);
                endpoint.getEndpointStatus().incrementSentErrors();
                msg = I18n.msg("E0200: Unable to process the reply for message exhange {0}, e=[{1}]",
                        inout.getExchangeId(), LogSupport.getStackTraceAsString(t));

                mLog.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, null);
                setErrorInExchange(inout, faultCode, faultDetail, t);
                // Failed to deliver the reply successfully,
                // so send error back
                try {
                    inout.setStatus(ExchangeStatus.ERROR);
                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                        mLog.log(
                                LogSupport.LEVEL_DEBUG,
                                I18n.msg(
                                        "Set message exchange status to {0} for message exchange ID {1} and exchange pattern {2}",
                                        "ERROR", inout.getExchangeId(), inout.getPattern()));
                    }
                } catch (MessagingException ex2) {
                    msg = I18n.msg(
                            "E0201: Failed to set the status on the message exchange with ID {0} and pattern {1} due to error: {2}",
                            inout.getExchangeId(), inout.getPattern(), ex2.getLocalizedMessage());
                    mLog.log(Level.SEVERE, msg, ex2);
                    AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                            AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT,
                            "HL7BC-E0201");

                }
            } finally {
                try {
                    // Send back status to consumer
                    mChannel.send(inout);
                    // put back one permit , call releaseThrottle method after sending of inout
                    // message on deliver channel
                    endpoint.releaseThrottle();
                } catch (Throwable mex2) {
                    msg = I18n.msg(
                            "E0202: Failed to send the message exchange with ID {0} and pattern {1} on the delivery channel due to error: {2}",
                            inout.getExchangeId(), inout.getPattern(), mex2.getLocalizedMessage());
                    mLog.log(Level.SEVERE, msg, mex2);
                    AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                            AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT,
                            "HL7BC-E0202");
                }
            }
        } else if (inout.getStatus() == ExchangeStatus.ERROR || inout.getError() != null || inout.getFault() != null) {
            if (inout.getError() != null) {
                Exception ex = inout.getError();
                msg = I18n.msg(
                        "E0203: Received the following error on the message exchange with ID {0} and pattern {1}: {2}",
                        inout.getExchangeId(), inout.getPattern(), ex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0203");
            }
            endpoint.getEndpointStatus().incrementReceivedErrors();

            try {
                checkForRedelivery(inout, endpoint);
                MessageExchangeReplyListener listener = inbReplyContext.getMessageExchangeReplyListener();
                listener.onReply(inout, false);
            } catch (Exception ex) {
                msg = I18n.msg(
                        "E0204: Failed in reply listener while processing message exchange with ID {0} and exchange pattern {1} due to error: {2}",
                        inout.getExchangeId(), inout.getPattern(), ex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0204");
            }
        }
        String messageId = (String) inout.getProperty(InboundMessageProcessor.REDELIVERY_QOS_MSG_ID);
        // Remove the message exchange from the inbound message exchange map
        mInboundExchanges.remove(messageId);
    }

    private void processRequestReplyOutbound(InOut inout, Endpoint endpoint) {
        QName operationName = inout.getOperation();
        if (inout.getStatus() == ExchangeStatus.DONE) {
            // DONE status on reply as a result of:
            // nmr->hl7bc, hl7bc->nmr, nnmr->hl7bc(DONE)
            endpoint.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR || inout.getError() != null) {
            if (inout.getError() != null) {
                Exception ex = inout.getError();
                String msg = I18n.msg(
                        "E0203: Received the following error on the message exchange with ID {0} and pattern {1}: {2}",
                        inout.getExchangeId(), inout.getPattern(), ex.getLocalizedMessage());
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, msg, ex);
                }
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0203");
            }
            // ERROR status on reply as a result of:
            // nmr->hl7bc, hl7bc->nmr, nnmr->hl7bc(ERROR)
            endpoint.getEndpointStatus().incrementReceivedErrors();
        } else {
            // Process outbound request/reply as a result of:
            // nmr->hl7bc
            // <checkpoint>
            if ( mNdcEnabled ) {
                // "Push" NDC context
                Logger.getLogger("com.sun.EnterContext")
                      .log(Level.FINE, "{0}={1}", new Object[] {HL7MMUtil.SOLUTION_GROUP,
                      HL7MMUtil.getSolutionGroup(endpoint)});
            }
            endpoint.getEndpointStatus().incrementReceivedRequests();
            String faultCode = null;
            String faultDetail = null;
            try {
                // Get the Normalized Message from MessageExchange
                NormalizedMessage normalizedMsg = inout.getInMessage();
                //checkpoint
                if ( mMonitorEnabled ) {
                    mChkptMsgId = ifNotNullTrim(normalizedMsg.getProperty(HL7MMUtil.MESSAGE_TRACKING_ID_KEY));
                    HL7MMUtil.setCheckpoint(mChkptMsgId, "Processing-Outbound-Request", endpoint);
                }
                // checkpoint
                Source src = normalizedMsg.getContent();
                // convet streamSource to DomeSource.required if we use javaEE SE.
                if (!(src instanceof DOMSource)) {
                    Document doc = getDocument(src);
                    normalizedMsg.setContent(new DOMSource(doc));
                    src = normalizedMsg.getContent();
                }
                HL7ProtocolProperties hl7ProtocolProps = getHL7ProtocolProperties(normalizedMsg, endpoint);
                // In case of acknowledgments, message validation should not be done
                boolean msaFound = hasSegmentMSA(((DOMSource) src).getNode());
                boolean seqNoEnabled = hl7ProtocolProps.getSeqNumEnabled().booleanValue();
                String ackMode = hl7ProtocolProps.getAckMode();
				boolean journallingEnabled = hl7ProtocolProps.getJournallingEnabled().booleanValue();
				// initilize the DB connection factory if the below are enabled
				if(seqNoEnabled || journallingEnabled || hl7ProtocolProps.getLLPType().equals(MLLPv2)){
				  try{
					initDBConnFactoryAndDBObjFactory();
				  } catch(Exception exdb){
					 throw new ApplicationException(I18n.msg("E0334: Unabled to initialize the DB connection factory"));
				  }
				}
                // Received Acknowledgment from SE and user has enabled sequence number protocol
                // handling
                if (msaFound && seqNoEnabled && ackMode.equals(ACK_MODE_ENHANCED)) {
                    DBConnection dbConn = null;
                    try {
                        dbConn = getDBConnection();
                        // retrieve sequence number value from Database
                        mExpSeqno = retrieveSequenceNumber(endpoint.getServiceUnitPath(), dbConn);
                        // get the MSH.13 field value
                        int recvSeqno = getSeqNumFromMsg(src);
                        int seqNo = processSequenceNumbering(recvSeqno, endpoint.getServiceUnitPath(), dbConn);
                        Document doc = getDocument(src);
                        Document newDoc = updateMSANode(doc, seqNo);
                        normalizedMsg.setContent(new DOMSource(newDoc));
                    } finally {
                        if (dbConn != null) {
                            dbConn.close();
                        }
                    }
                }
                // sequence numer processing in case of outbound
                // check wheather sequence number exists or not in the message
                boolean found = hasSequenceNo(src);
                if (!msaFound && seqNoEnabled && !found) {
                    DBConnection dbConn = null;
                    try {
                        dbConn = getDBConnection();
                        // retrieve sequence number value from Database, if not found store -1 in DB
                        int outboundExpSeqNo = retrieveSequenceNumber(endpoint.getServiceUnitPath(), dbConn);
                        Document doc = getDocument(src);
                        // insert sequence no in the outgoing message
                        Document newDoc = insertSequenceNumberIntoMsg(doc, outboundExpSeqNo);
                        normalizedMsg.setContent(new DOMSource(newDoc));
                        // <checkpoint>
                        if ( mMonitorEnabled ) {
                            HL7MMUtil.setCheckpoint(mChkptMsgId, "Outbound-Sequence-Number-Processed", endpoint);
                        }
                    } finally {
                        if (dbConn != null) {
                            dbConn.close();
                        }
                    }
                }
                boolean validateMSH = hl7ProtocolProps.getValidateMSHEnabled().booleanValue();
                if (validateMSH && !msaFound) {
                    ValidationInfo validationInfo = mHL7OutboundMsgValidationprocessor.messageValidationProcess(
                            endpoint, normalizedMsg);
                    if (!validationInfo.getValidationStatus()) {
                        faultCode = CLIENT;
                        faultDetail = " The normalized message from consumer does not pass the validation check";
                        String faultString = I18n.msg(
                                "E0206: Unable to process message exchange {0} as the normalized message has failed the MSH validation check. The validation error code is: {1}, and error message is: {2}",
                                inout.getExchangeId(), validationInfo.getErrorCode(), validationInfo.getErrorMessage());
                        throw new Exception(faultString);
                    }
                    // <checkpoint>
                    if ( mMonitorEnabled ) {
                        HL7MMUtil.setCheckpoint(mChkptMsgId, "Outbound-Message-Validated", endpoint);
                    }
                    // <checkpoint>
                }
                boolean sftEnabled = hl7ProtocolProps.getSFTEnabled().booleanValue();
                if (sftEnabled) {
                    HL7SFTSegmentProcessor.processSFTSegment(hl7ProtocolProps, normalizedMsg);
                    // <checkpoint>
                    if ( mMonitorEnabled ) {
                        HL7MMUtil.setCheckpoint(mChkptMsgId, "SFT-Segment-Validated", endpoint);
                    }
                    // <checkpoint>
                }
                HL7Operation hl7Operation = locateHL7Operation(operationName, endpoint);
                HL7Input hl7Input = hl7Operation.getHL7OperationInput();
                HL7Message hl7Message = hl7Input.getHL7Message();
                HL7Output hl7Output = hl7Operation.getHL7OperationOutput();
                HL7Message hl7OutputMessage = hl7Output.getHL7Message();
                if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg("Start denormalizing the NMR message to HL7 Message"));
                }
                String hl7PayLoad = null;
                Probe deNormalizationMeasurement = null;
                try {
                    // calculate performance Measurment
                    String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) ? createConsumingEndpointIdentifier(
                            endpoint.getServiceName(), endpoint.getEndpointName())
                            : createProvisioningEndpointIdentifier(endpoint.getServiceName(),
                                    endpoint.getEndpointName());
                    deNormalizationMeasurement = Probe.info(getClass(), endPointID,
                            HL7BindingComponent.PERF_CAT_DENORMALIZATION);
                    // Denormalize from NM message to HL7 request/acknowledgment message
                    hl7PayLoad = mHL7Denormalizer.denormalize(normalizedMsg, operationName, endpoint, hl7Message);
                    // <checkpoint>
                    if ( mMonitorEnabled ) {
                        // Converting source node to xml string for checkpointing
                        String hl7XMLMessage = transformToString(((DOMSource) src).getNode().getFirstChild().getFirstChild().getFirstChild(),
                                        "UTF-8", true, "no", "xml");
                        HL7MMUtil.setCheckpoint(endpoint, mChkptMsgId, "Message-DeNormalized", hl7XMLMessage);
                    }
                    // <checkpoint>
                } catch (Exception e) {
                    faultCode = CLIENT;
                    faultDetail = "Unable to denormalize message. The normalize message from consumer is not correct.";
                    String faultString = I18n.msg(
                            "E0207: Unable to denormalize message for message exchange {0}, end point {1} and operation {2}.",
                            inout.getExchangeId(), endpoint.toString(), inout.getOperation().toString());
                    throw new Exception(faultString, e);

                } finally {
                    if (deNormalizationMeasurement != null) {
                        deNormalizationMeasurement.end();
                    }
                }
                if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLog.log(LogSupport.LEVEL_DEBUG,
                            I18n.msg("Successfully denormalized the NMR message to HL7 message"));
                    mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg("Sending HL7 message to HL7 External System"));
                }
                ProtocolInfo protocolInfo = getProtocolInfo(normalizedMsg, endpoint);
                HL7Connector hl7Connector = getConnector(normalizedMsg, endpoint, protocolInfo);
                if (hl7Connector == null) {
                    mLog.log(Level.SEVERE, I18n.msg("E0208: HL7 Connector creatioin failed"));
                    faultCode = SERVER;
                    faultDetail = "Unable to create HL7 connector";
                    String faultString = I18n.msg("W0120: Unable to create HL7 Connector for transport type : {0}",
                            hl7ProtocolProps.getLLPType());
                    throw new Exception(faultString);
                }
                //ProtocolInfo protocolInfo = getProtocolInfo(normalizedMsg, endpoint);
                try {
                    if (msaFound) {
                        if (ackMode.equals(ACK_MODE_ENHANCED)) {
                            // send acknowledgment to External System
                            sendACKToExtSys(hl7PayLoad, hl7Connector, protocolInfo, endpoint);
                        }
                    } else {
                        // start sending a request/receiving an ack from hl7 external system
                        // get the Output operation HL7 message
                        NormalizedMessage ackMessage = sendRequestAndReceiveACK(inout, hl7PayLoad, hl7Connector,
                                protocolInfo, endpoint, hl7OutputMessage);
                        inout.setOutMessage(ackMessage);
                        if (this.mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLog.log(
                                    LogSupport.LEVEL_DEBUG,
                                    I18n.msg(
                                            "Called setOutMessage with a normalized reply message on message exchange with ID {0} and exchange pattern {1}",
                                            inout.getExchangeId(), inout.getPattern()));
                        }
                    }
                } catch (Exception ex) {
                    throw ex;
                }
            } catch (Exception ex) {
                if (ex instanceof NMPropertiesParsingException) {
                    faultCode = CLIENT;
                    faultDetail = "Error parsing normalized message properties. Set correct message properties.";
                    String faultString = ex.getMessage();
                    ex = new Exception(faultString, ex);
                }
                if (faultCode == null) {
                    faultCode = SERVER;
                    faultDetail = "Unable to process message exchange.";
                    String faultString = I18n.msg(
                            "W0121: Unable to process message exchange {0}, end point {1} and operation {2}.",
                            inout.getExchangeId(), endpoint.toString(), operationName.toString());
                    ex = new Exception(faultString, ex);
                }

                endpoint.getEndpointStatus().incrementSentErrors();
                String msg = I18n.msg(
                        "E0209: Failed to process the message exchange with ID {0} and exchange pattern {1} due to error: {2}",
                        inout.getExchangeId(), inout.getPattern(), ex.getLocalizedMessage());

                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0209");
                setErrorInExchange(inout, faultCode, faultDetail, ex);
                // setError should set status to error, but just in case...
                try {
                    inout.setStatus(ExchangeStatus.ERROR);

                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                        mLog.log(
                                LogSupport.LEVEL_DEBUG,
                                I18n.msg(
                                        "Set message exchange status to {0} for message exchange ID {1} and exchange pattern {2}",
                                        "ERROR", inout.getExchangeId(), inout.getPattern()));
                    }

                } catch (MessagingException ex2) {
                    msg = I18n.msg(
                            "E0201: Failed to set the status on the message exchange with ID {0} and pattern {1} due to error: {2}",
                            inout.getExchangeId(), inout.getPattern(), ex.getLocalizedMessage());
                    mLog.log(Level.SEVERE, msg, ex2);
                    AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                            AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT,
                            "HL7BC-E0201");
                }

            } finally {
                try {
                    mChannel.send(inout);
                } catch (Exception mex2) {
                    String msg = I18n.msg(
                            "E0202: Failed to send the message exchange with ID {0} and pattern {1} on the delivery channel due to error: {2}",
                            inout.getExchangeId(), inout.getPattern(), mex2.getLocalizedMessage());
                    mLog.log(Level.SEVERE, msg, mex2);
                    AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                            AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT,
                            "HL7BC-E0202");
                }
                // <checkpoint>
                if ( mNdcEnabled ) {
                    // "Pop" NDC context
                    Logger.getLogger("com.sun.ExitContext")
                          .log(Level.FINE, "{0}={1}", new Object[] {HL7MMUtil.SOLUTION_GROUP,
                          HL7MMUtil.getSolutionGroup(endpoint)});
                }
                // </checkpoint>
            }
        }

    }

    private void setErrorInExchange(MessageExchange msgExch, String faultCode, String faultDetail, Exception e) {
        msgExch.setError(e);
        msgExch.setProperty("com.sun.jbi.crl.faultcode", faultCode);
        msgExch.setProperty("com.sun.jbi.crl.faultstring", e.getMessage());
        msgExch.setProperty("com.sun.jbi.crl.faultactor", AlertsUtil.SUN_HL7_BINDING);
        msgExch.setProperty("com.sun.jbi.crl.faultdetail", faultDetail);
    }

    private void processOneWayOutbound(InOnly inonly, Endpoint destination) {
        String faultCode = null;
        String faultDetail = null;
        try {
            // <checkpoint>
            if ( mNdcEnabled ) {
                // "Push" NDC context
                Logger.getLogger("com.sun.EnterContext")
                      .log(Level.FINE, "{0}={1}", new Object[] {HL7MMUtil.SOLUTION_GROUP,
                      HL7MMUtil.getSolutionGroup(destination)});
            }
            // Increment received requests on endpoint (bc is provider)
            destination.getEndpointStatus().incrementReceivedRequests();
            // Get the Normalized Message from MessageExchange
            NormalizedMessage normalizedMsg = inonly.getInMessage();
            //checkpoint
            if ( mMonitorEnabled ) {
                mChkptMsgId = ifNotNullTrim(normalizedMsg.getProperty(HL7MMUtil.MESSAGE_TRACKING_ID_KEY));
                HL7MMUtil.setCheckpoint(mChkptMsgId, "Processing-Outbound-Request", destination);
            }
            // checkpoint
            QName operationName = inonly.getOperation();
            Source src = normalizedMsg.getContent();
            // convet streamSource to DomeSource.required if we use javaEE SE.
            if (!(src instanceof DOMSource)) {
                Document doc = getDocument(src);
                normalizedMsg.setContent(new DOMSource(doc));
                src = normalizedMsg.getContent();
            }
            HL7ProtocolProperties hl7ProtocolProps = getHL7ProtocolProperties(normalizedMsg, destination);
            // In case of acknowledgments, message validation should not be done
            boolean msaFound = hasSegmentMSA(((DOMSource) src).getNode());
            boolean seqNoEnabled = hl7ProtocolProps.getSeqNumEnabled().booleanValue();
            String ackMode = hl7ProtocolProps.getAckMode();
            boolean journallingEnabled = hl7ProtocolProps.getJournallingEnabled().booleanValue();
			// initilize the DB connection factory if the below are enabled
			if(seqNoEnabled || journallingEnabled || hl7ProtocolProps.getLLPType().equals(MLLPv2)){
			  try{
			    initDBConnFactoryAndDBObjFactory();
			  } catch(Exception exdb){
			     throw new ApplicationException(I18n.msg("E0334: Unabled to initialize the DB connection factory"));
			  }
			}
            // Received Acknowledgment from SE and user has enabled sequence number protocol
            // handling
            if (msaFound && seqNoEnabled && ackMode.equals(ACK_MODE_ENHANCED)) {
                DBConnection dbConn = null;
                try {
                    dbConn = getDBConnection();
                    // retrieve sequence number value from Database
                    mExpSeqno = retrieveSequenceNumber(destination.getServiceUnitPath(), dbConn);
                    // get the MSH.13 field value
                    int recvSeqno = getSeqNumFromMsg(src);
                    int seqNo = processSequenceNumbering(recvSeqno, destination.getServiceUnitPath(), dbConn);
                    Document doc = getDocument(src);
                    Document newDoc = updateMSANode(doc, seqNo);
                    normalizedMsg.setContent(new DOMSource(newDoc));
                } finally {
                    if (dbConn != null) {
                        dbConn.close();
                    }
                }
            }
            // sequence numer processing in case of outbound
            // check wheather sequence number exists or not in the message
            boolean found = hasSequenceNo(src);
            if (!msaFound && seqNoEnabled && !found) {
                DBConnection dbConn = null;
                try {
                    dbConn = getDBConnection();
                    // retrieve sequence number value from Database, if not found store -1 in DB
                    int outboundExpSeqNo = retrieveSequenceNumber(destination.getServiceUnitPath(), dbConn);

                    Document doc = getDocument(src);
                    // insert sequence no in the outgoing message
                    Document newDoc = insertSequenceNumberIntoMsg(doc, outboundExpSeqNo);
                    normalizedMsg.setContent(new DOMSource(newDoc));
                    // <checkpoint>
                    if ( mMonitorEnabled ) {
                        HL7MMUtil.setCheckpoint(mChkptMsgId, "Outbound-Sequence-Number-Processed", destination);
                    }
                } finally {
                    if (dbConn != null) {
                        dbConn.close();
                    }
                }
            }
            boolean validateMSH = hl7ProtocolProps.getValidateMSHEnabled().booleanValue();
            if (validateMSH && !msaFound) {
                ValidationInfo validationInfo = mHL7OutboundMsgValidationprocessor.messageValidationProcess(
                        destination, normalizedMsg);
                if (!validationInfo.getValidationStatus()) {
                    throw new ApplicationException(I18n.msg("E0210: HL7 Message Validation Failed"));
                }
                // <checkpoint>
                if ( mMonitorEnabled ) {
                    HL7MMUtil.setCheckpoint(mChkptMsgId, "Outbound-Message-Validated", destination);
                }
            }
            boolean sftEnabled = hl7ProtocolProps.getSFTEnabled().booleanValue();
            if (sftEnabled) {
                HL7SFTSegmentProcessor.processSFTSegment(hl7ProtocolProps, normalizedMsg);
                // <checkpoint>
                if ( mMonitorEnabled ) {
                    HL7MMUtil.setCheckpoint(mChkptMsgId, "SFT-Segment-Validated", destination);
                }
            }
            HL7Operation hl7Operation = locateHL7Operation(operationName, destination);
            HL7Input hl7Input = hl7Operation.getHL7OperationInput();
            HL7Message hl7Message = hl7Input.getHL7Message();
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg("Start denormalizing the NMR message to HL7 Message"));
            }
            String hl7PayLoad = null;
            try {
                // calculate performance Measurment
                String endPointID = (destination.getEndpointType() == Endpoint.EndpointType.INBOUND) ? createConsumingEndpointIdentifier(
                        destination.getServiceName(), destination.getEndpointName())
                        : createProvisioningEndpointIdentifier(destination.getServiceName(),
                                destination.getEndpointName());
                Probe deNormalizationMeasurement = Probe.info(getClass(), endPointID,
                        HL7BindingComponent.PERF_CAT_DENORMALIZATION);
                // Denormalize from NMS message to HL7 request/acknowledgment message
                hl7PayLoad = mHL7Denormalizer.denormalize(normalizedMsg, operationName, destination, hl7Message);
                deNormalizationMeasurement.end();
                // <checkpoint>
                if ( mMonitorEnabled ) {
                    // Converting source node to xml string for checkpointing
                    String hl7XMLMessage = transformToString(((DOMSource) src).getNode().getFirstChild().getFirstChild().getFirstChild(),
                                    "UTF-8", true, "no", "xml");
                    HL7MMUtil.setCheckpoint(destination, mChkptMsgId, "Message-DeNormalized", hl7XMLMessage);
                }
                // <checkpoint>
            } catch (Exception e) {
                faultCode = CLIENT;
                faultDetail = "Unable to denormalize message. The normalize message from consumer is not correct.";
                String faultString = I18n.msg(
                        "E0198: Unable to denormalize reply from internal service engine/binding component for message exchange {0}, end point {1} and operation {2}.",
                        inonly.getExchangeId(), destination.toString(), operationName.toString());
                throw new Exception(faultString, e);

            }
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg("Successfully denormalized the NMR message to HL7 message"));
                mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg("Sending HL7 message to HL7 External System"));
            }
            ProtocolInfo protocolInfo = getProtocolInfo(normalizedMsg, destination);
            HL7Connector hl7Connector = getConnector(normalizedMsg, destination, protocolInfo);
            if (hl7Connector == null) {
                mLog.log(Level.SEVERE, I18n.msg("E0208: HL7 Connector creatioin failed"));
                throw new ApplicationException(I18n.msg("E0208: HL7 Connector creatioin failed"));
            }
            //ProtocolInfo protocolInfo = getProtocolInfo(normalizedMsg, destination);
            try {
                if (msaFound) {
                    if (ackMode.equals(ACK_MODE_ENHANCED)) {
                        // send acknowledgment to External System
                        sendACKToExtSys(hl7PayLoad, hl7Connector, protocolInfo, destination);
                    }
                } else {
                    // start sending a request/receiving an ack from hl7 external system
                    start2WayChannelWithExtSystem(inonly, hl7PayLoad, hl7Connector, protocolInfo, destination,
                            hl7Message);
                }
            } catch (Exception ex) {
                throw ex;
            }
            inonly.setStatus(ExchangeStatus.DONE);
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg(
                        "Set message exchange status to {0} for message exchange ID {1} and exchange pattern {2}",
                        "DONE", inonly.getExchangeId(), inonly.getPattern()));
            }
            destination.getEndpointStatus().incrementSentDones();
        } catch (Throwable exc) {
            if (exc instanceof NMPropertiesParsingException) {
                faultCode = CLIENT;
                faultDetail = "Error parsing normalized message properties. Set correct message properties.";
                String faultString = exc.getMessage();
                exc = new Exception(faultString, exc);
            }
            if (faultCode == null) {
                faultCode = SERVER;
                faultDetail = "Unable to process message exchange.";
                String faultString = I18n.msg(
                        "W0121: Unable to process message exchange {0}, end point {1} and operation {2}.",
                        inonly.getExchangeId(), destination.toString(), inonly.getOperation().toString());
                exc = new Exception(faultString, exc);
            }
            destination.getEndpointStatus().incrementSentErrors();
            String msg = I18n.msg(
                    "E0209: Failed to process the message exchange with ID {0} and exchange pattern {1} due to error: {2}",
                    inonly.getExchangeId(), inonly.getPattern(), exc.getLocalizedMessage());
            mLog.log(Level.SEVERE, msg, exc);
            AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0209");
            setErrorInExchange(inonly, faultCode, faultDetail, (Exception) exc);

            try {
                inonly.setStatus(ExchangeStatus.ERROR);

                if (mLog.isLoggable(Level.FINER)) {
                    mLog.log(
                            Level.FINER,
                            I18n.msg(
                                    "I0139: Set message exchange status to {0} for message exchange ID {1} and exchange pattern {2}",
                                    "ERROR", inonly.getExchangeId(), inonly.getPattern()));
                }

            } catch (MessagingException ex) {
                msg = I18n.msg(
                        "E0201: Failed to set the status on the message exchange with ID {0} and pattern {1} due to error: {2}",
                        inonly.getExchangeId(), inonly.getPattern(), ex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0201");
            }
        } finally {
            try {
                // Send back exchange status
                mChannel.send(inonly);
            } catch (Exception mex) {
                String msg = I18n.msg(
                        "E0202: Failed to send the message exchange with ID {0} and pattern {1} on the delivery channel due to error: {2}",
                        inonly.getExchangeId(), inonly.getPattern(), mex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, mex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0202");
            }
            // <checkpoint>
            if ( mNdcEnabled ) {
                // "Pop" NDC context
                Logger.getLogger("com.sun.ExitContext")
                      .log(Level.FINE, "{0}={1}", new Object[] {HL7MMUtil.SOLUTION_GROUP,
                      HL7MMUtil.getSolutionGroup(destination)});
            }
            // </checkpoint>
        }
    }

    private void processOneWayInbound(InOnly inonly, Endpoint endpoint, InboundReplyContext inbReplyContext) {
        String msg = null;
        if (inonly.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
            try {
                String ackMode = getHL7ProtocolProperties(inonly.getInMessage(), endpoint).getAckMode();
                if ((ackMode.equals(ACK_MODE_ORIGINAL))
                        || (ackMode.equals(ACK_MODE_ENHANCED) && inbReplyContext.getAcceptAckCondition() != null && (inbReplyContext.getAcceptAckCondition().equals(
                                ALWAYS_CONDITION) || inbReplyContext.getAcceptAckCondition().equals(SUCCESS_CONDITION)))) {
                    MessageExchangeReplyListener listener = inbReplyContext.getMessageExchangeReplyListener();
                    listener.onReply(inonly, true);
                }
            } catch (Exception ex) {
                msg = I18n.msg(
                        "E0204: Failed in reply listener while processing message exchange with ID {0} and exchange pattern {1} due to error: {2}",
                        inonly.getExchangeId(), inonly.getPattern(), ex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, endpoint.getUniqueName(),
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0204");
            }
        } else if (inonly.getStatus() == ExchangeStatus.ERROR || inonly.getError() != null) {
            if (inonly.getError() != null) {
                Exception ex = inonly.getError();
                msg = I18n.msg(
                        "E0203: Received the following error on the message exchange with ID {0} and pattern {1}: {2}",
                        inonly.getExchangeId(), inonly.getPattern(), ex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, endpoint.getUniqueName(),
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0203");
            }

            endpoint.getEndpointStatus().incrementReceivedErrors();
            try {
                checkForRedelivery(inonly, endpoint);
                MessageExchangeReplyListener listener = inbReplyContext.getMessageExchangeReplyListener();
                listener.onReply(inonly, false);
            } catch (Exception ex) {
                msg = I18n.msg(
                        "E0204: Failed in reply listener while processing message exchange with ID {0} and exchange pattern {1} due to error: {2}",
                        inonly.getExchangeId(), inonly.getPattern(), ex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0204");
            }
        }
        String messageId = (String) inonly.getProperty(InboundMessageProcessor.REDELIVERY_QOS_MSG_ID);
        // Remove the message exchange from the inbound message exchange map
        mInboundExchanges.remove(messageId);
    }

    private void checkForRedelivery(MessageExchange ex, Endpoint endpoint) {
        // Here try to redeliver the code if redelivery is enabled
        RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(ex);
        if (retryStatus != null && retryStatus.hasFailed()) {
            String redeliveryQoSMsgID = (String) ex.getProperty(InboundMessageProcessor.REDELIVERY_QOS_MSG_ID);
            EndpointInfo info = new EndpointInfo(false, endpoint.getEndpointName(), null, endpoint.getServiceName(),
                    null);
            RedeliveryConfig retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
            if (retryConfig != null) {
                String msg = null;
                Failure onFailureOption = retryConfig.getFailure();
                if (onFailureOption == Failure.suspend) {
                    try {
                        endpoint.getServiceUnit().suspend(endpoint);
                        // emit warning logs and send alerts
                        msg = I18n.msg("W0119: About to suspend endpoint: serviceName=[{0}], endpointName=[{1}].",
                                String.valueOf(endpoint.getServiceName()), endpoint.getEndpointName());
                        if (mLog.isLoggable(Level.WARNING)) {
                            mLog.log(Level.WARNING, msg);
                        }
                        AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME,
                                endpoint.getUniqueName(), AlertsUtil.getServerType(),
                                AlertsUtil.COMPONENT_TYPE_BINDING, NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0119");
                    } catch (Exception e) {
                        String errorMsg = e.getLocalizedMessage();
                        if (errorMsg != null) {
                            msg = I18n.msg(
                                    "E0190: MBeanException caught when try to suspend endpoint (redelivery): serviceName=[{0}], endpointName=[{1}], error message=[{2}].",
                                    String.valueOf(endpoint.getServiceName()), endpoint.getEndpointName(), errorMsg);
                            mLog.log(Level.SEVERE, msg);
                            AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME,
                                    endpoint.getUniqueName(), AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING, NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                    NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0190");
                        }
                    }
                } else if (onFailureOption == Failure.error) {
                    mLog.log(
                            Level.WARNING,
                            I18n.msg(
                                    "W0123: Could not deliver message after {0} attempts to end point: {1} service name: {2} and message exchange ID: {3}. On failure mode is Error for the message ID {4}.",
                                    retryConfig.getMaxRetries() + "", info.getEndpointName(), info.getServiceName(),
                                    ex.getExchangeId(), redeliveryQoSMsgID));
                } else if (onFailureOption == Failure.redirect) {
                    Redirect redirect = retryConfig.getRedirect();
                    EndpointInfo redirectEPinfo = redirect.getEndpoint();
                    mLog.log(
                            Level.WARNING,
                            I18n.msg(
                                    "W0124: Could not deliver message after {0} attempts to end point: {1} service name: {2} and message exchange ID: {3}. On failure mode is configured to Redirect, message is redirected to end point: {4} & service name: {5} for the message ID {6}.",
                                    retryConfig.getMaxRetries() + "", info.getEndpointName(), info.getServiceName(),
                                    ex.getExchangeId(), redirectEPinfo.getEndpointName(),
                                    redirectEPinfo.getServiceName(), redeliveryQoSMsgID));

                }
            }
        }
    }

    private boolean isRequest(MessageExchange msgX) {
        boolean request = false;
        if (msgX instanceof InOut) {
            InOut inout = (InOut) msgX;
            request = inout.getOutMessage() == null && inout.getFault() == null
                    && inout.getStatus() == ExchangeStatus.ACTIVE;
        } else if (msgX instanceof InOnly) {
            InOnly inonly = (InOnly) msgX;
            request = inonly.getFault() == null && inonly.getStatus() == ExchangeStatus.ACTIVE;
        }
        return request;
    }

    /**
     * Find the exact Endpoint
     * 
     * @param msgExchange the received Message Exchange
     * @return Endpoint a Endpoint
     */
    private Endpoint findEndpoint(MessageExchange msgExchange) {
        Endpoint endpoint = null;
        for (Iterator it = mServiceUnits.iterator(); it.hasNext();) {
            ServiceUnit su = (ServiceUnit) it.next();
            for (Iterator it2 = su.getEndpoints().iterator(); it2.hasNext();) {
                Endpoint aEndPoint = (Endpoint) it2.next();
                QName serviceName = msgExchange.getEndpoint().getServiceName();
                String endpointName = msgExchange.getEndpoint().getEndpointName();

                if (aEndPoint.getServiceName().equals(serviceName) && aEndPoint.getEndpointName().equals(endpointName)) {
                    endpoint = aEndPoint;
                    break;
                }
            }
        }

        return endpoint;
    }

    /**
     * Find the exact Endpoint
     * 
     * @param ServiceEncpdoint
     * @return Endpoint a Endpoint
     */
    private Endpoint findEndpoint(ServiceEndpoint serviceEndpoint) {
        Endpoint endpoint = null;
        for (Iterator it = mServiceUnits.iterator(); it.hasNext();) {
            ServiceUnit su = (ServiceUnit) it.next();
            for (Iterator it2 = su.getEndpoints().iterator(); it2.hasNext();) {
                Endpoint aEndPoint = (Endpoint) it2.next();
                QName serviceName = serviceEndpoint.getServiceName();
                String endpointName = serviceEndpoint.getEndpointName();

                if (aEndPoint.getServiceName().equals(serviceName) && aEndPoint.getEndpointName().equals(endpointName)) {
                    endpoint = aEndPoint;
                    break;
                }
            }
        }

        return endpoint;
    }

    private void handleMessagePreProcessingErrors(boolean isInboundReply,
                                                  InboundReplyContext inbReplyContext,
                                                  MessageExchange msgEx,
                                                  Exception cause) {
        if (isInboundReply && inbReplyContext != null) {
            try {
                MessageExchangeReplyListener listener = inbReplyContext.getMessageExchangeReplyListener();
                listener.onReply(msgEx, false);
            } catch (Exception ex) {
                String msg = I18n.msg(
                        "E0204: Failed in reply listener while processing message exchange with ID {0} and exchange pattern {1} due to error: {2}",
                        msgEx.getExchangeId(), msgEx.getPattern(), ex.getLocalizedMessage());
                mLog.log(Level.SEVERE, msg, ex);
                AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0204");

            }
        }

        if (msgEx.getStatus() == ExchangeStatus.ACTIVE) {
            setErrorStatusAndSend(msgEx, cause);
        }
    }

    private void setErrorStatusAndSend(MessageExchange msgEx, Exception cause) {
        String msg = null;
        if (cause != null) {
            msgEx.setError(cause);
        }
        try {
            msgEx.setStatus(ExchangeStatus.ERROR);

            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG, I18n.msg(
                        "Set message exchange status to {0} for message exchange ID {1} and exchange pattern {2}",
                        "ERROR", msgEx.getExchangeId(), msgEx.getPattern()));
            }

        } catch (MessagingException ex) {
            msg = I18n.msg(
                    "E0201: Failed to set the status on the message exchange with ID {0} and pattern {1} due to error: {2}",
                    msgEx.getExchangeId(), msgEx.getPattern(), ex.getLocalizedMessage());
            mLog.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0201");
        }

        try {
            // Send back exchange status
            mChannel.send(msgEx);
        } catch (MessagingException mex) {
            msg = I18n.msg(
                    "E0202: Failed to send the message exchange with ID {0} and pattern {1} on the delivery channel due to error: {2}",
                    msgEx.getExchangeId(), msgEx.getPattern(), mex.getLocalizedMessage());
            mLog.log(Level.SEVERE, msg, mex);
            AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0202");
        }

    }

    private HL7Operation locateHL7Operation(QName opname, Endpoint endpoint) {
        // return (HL7Operation) endpoint.getHL7Operations().get(opname);
        PortType portType = getPortType(endpoint);
        return (HL7Operation) endpoint.getHL7Operations().get(
                new QName(portType.getQName().getNamespaceURI(), opname.getLocalPart()));
    }

    private PortType getPortType(Endpoint endpoint) {
        String serviceName = endpoint.getServiceName().toString();
        String endpointName = endpoint.getEndpointName();
        Definition def = endpoint.getDefinition();
        Map services = def.getServices();

        // DO NOT use the getService() method.
        // It checks all imported WSDLs.
        Service svc = (Service) services.get(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }

        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port == null) {
            return null;
        }

        Binding binding = port.getBinding();
        if (binding == null) {
            return null;
        }
        return binding.getPortType();
    }

    private void logNormalizedMessage(String exchangeId, Source msgSrc) {
        try {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer trans = tFactory.newTransformer();
            trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            StringWriter out = new StringWriter();
            StreamResult result = new StreamResult(out);
            trans.transform(msgSrc, result);
            out.flush();
            out.close();
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, I18n.msg("Send to NMR exchangeid {0}, message source {1}", exchangeId,
                        out.toString()));
            }
        } catch (Throwable t) {
            ;
            ;
        }
    }

    /**
     * Finds out the existence of segment MSA in message
     * 
     * @param domMessage represents the hl7 message
     * @return boolean value after checking
     */
    private boolean hasSegmentMSA(Node domMessage) {
        boolean msaFound = false;
        NodeList segments = domMessage.getFirstChild().getFirstChild().getFirstChild().getChildNodes();
        for (int i = 0; i < segments.getLength(); i++) {
            Node segment = segments.item(i);
            String name = segment.getLocalName();
            if (name != null && name.contains(MSA)) {
                msaFound = true;
                break;
            }
        }
        return msaFound;
    }

    private Document getDocument(Source src) throws Exception {
        DOMResult result = transformToDOMResult(mTrans, src);
        Node node = result.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }
        return normalizedDoc;
    }

    /**
     * Retrieve the sequence number from the Database
     * 
     * @param queryString Unique ID for the records in EXPSEQUENCENO Table
     * @param dbConnection Connection to the underlying Database
     * @throws SQLException
     * @throws Exception
     */
    private int retrieveSequenceNumber(String queryKey, DBConnection dbConnection) throws SQLException, Exception {
        int seqNO = -1;
        SequenceNumDBO seqNoDBO = mDBObjectFactory.createSequenceNumDBO(queryKey);
        ResultSet rs = dbConnection.getRow(seqNoDBO);
        if (rs.next()) {
            seqNoDBO.populateDBO(rs);
        } else {
            seqNoDBO.setESN(OUTBOUND_INITIAL_ESN);// This is useful for outbound
            seqNoDBO.setESNState(VALID_ESN_STATE);
            dbConnection.insert(seqNoDBO);
            dbConnection.getUnderlyingConnection().commit();
        }
        seqNO = seqNoDBO.getESN();
        return seqNO;
    }

    /**
     * Updates the sequence number in Database
     * 
     * @param queryString Unique ID for the records in EXPSEQUENCENO Table
     * @param esn expected sequence number
     * @param esnState expected sequence number state
     */
    private void updateSequenceNumberInDB(String queryingKey, int esn, String esnState, DBConnection dbConnection)
            throws SQLException, Exception {
        SequenceNumDBO seqNoDBO = mDBObjectFactory.createSequenceNumDBO(queryingKey, esn, esnState);
        dbConnection.update(seqNoDBO);
        dbConnection.getUnderlyingConnection().commit();
    }

    /**
     * Get the sequence number from the HL7 Message
     * 
     * @param src javax.xml.transform.Source
     */
    private int getSeqNumFromMsg(Source src) {
        Node mshNode = ((DOMSource) src).getNode().getFirstChild().getFirstChild().getFirstChild().getFirstChild();
        NodeList mshList = mshNode.getChildNodes();
        int seqNo = -1;
        Node node = null;
        String name = null;
        for (int i = 0; i < mshList.getLength(); i++) {
            node = mshList.item(i);
            name = node.getLocalName();
            if (name.equals(MSH13)) {
                String seqNumber = node.getFirstChild().getNodeValue();
                try {
                    seqNo = Integer.parseInt(seqNumber);
                } catch (Exception e) {
                    // throw new Exception("Invalid Sequence Numer Exists in the Message");
                }
                break;

            }
        }
        return seqNo;
    }

    /**
     * Find the sequence number from the HL7 Message
     * 
     * @param src javax.xml.transform.Source
     */
    private boolean hasSequenceNo(Source src) {
        Node mshNode = ((DOMSource) src).getNode().getFirstChild().getFirstChild().getFirstChild().getFirstChild();
        NodeList mshList = mshNode.getChildNodes();
        boolean found = false;
        Node node = null;
        String name = null;
        for (int i = 0; i < mshList.getLength(); i++) {
            node = mshList.item(i);
            name = node.getLocalName();
            if (name.equals(MSH13)) {
                String seqNumber = node.getFirstChild().getNodeValue();
                if (seqNumber != null && !seqNumber.equals("")) {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }

    /**
     * process the sequence number and update the status into Database
     * 
     * @param recvSeqNo the sequence number in the received message
     * @param queryingKey Unique ID for the records in EXPSEQUENCENO Table
     * @param dbConnection Connection to the underlying Database
     * @throws Exception
     */
    private int processSequenceNumbering(int recvSeqNo, String queryingKey, DBConnection dbConnection) throws Exception {
        if (mExpSeqno == -1) { // ESN State = NONE
            if (recvSeqNo == -1) { // incoming sequence number = -1
                return mExpSeqno;
            } else if (recvSeqNo == 0) { // incoming sequence number == 0
                return mExpSeqno;
            } else {// incoming sequence number >= 1
                mExpSeqno = recvSeqNo;
                int seqNoInDB = recvSeqNo + 1;
                // update the data base.
                updateSequenceNumberInDB(queryingKey, seqNoInDB, VALID_ESN_STATE, dbConnection);
                return mExpSeqno;
            }
        } else { // ESN State >= 1
            if (recvSeqNo == -1) { // incoming sequence number = -1
                mExpSeqno = recvSeqNo;
                int seqNoInDB = recvSeqNo;
                updateSequenceNumberInDB(queryingKey, seqNoInDB, NONE_ESN_STATE, dbConnection);
                return recvSeqNo;
            } else if (recvSeqNo == 0) { // incoming sequence number == 0
                return mExpSeqno;
            } else {// incoming sequence number >= 1
                mExpSeqno = recvSeqNo;
                int seqNoInDB = recvSeqNo + 1;
                // update the data base.
                updateSequenceNumberInDB(queryingKey, seqNoInDB, VALID_ESN_STATE, dbConnection);
                return mExpSeqno;
            }
        }
    }

    /**
     * Check whether node has the specified element
     * 
     * @param Node org.w3c.dom.Node
     * @param val String
     */
    private Node hasElement(Node node, String val) {
        NodeList list = node.getChildNodes();
        Node returnNode = null;
        for (int i = 0; i < list.getLength(); i++) {
            Node child = list.item(i);
            if (child.getLocalName().equals(val)) {
                returnNode = child;
                break;
            }
        }
        return returnNode;

    }

    /**
     * Update the MSA Segment by setting MSA-4-Expected Sequence Number
     * 
     * @param doc org.w3c.dom.Document
     * @param int sequence number value which is set to MSA-4-Expected Sequence Number
     * @return Document return updated document
     */
    private Document updateMSANode(Document doc, int seqNo) {
        Node root = doc.getFirstChild().getFirstChild().getFirstChild();
        String nameSpaceURL = root.getNamespaceURI();
        Node newNode = null;

        NodeList list = doc.getFirstChild().getFirstChild().getFirstChild().getChildNodes();
        for (int i = 0; i < list.getLength(); i++) {
            Node node = list.item(i);
            if (node.getLocalName().equals(MSA)) {
                newNode = hasElement(node, MSA4);
                if (newNode != null) {
                    newNode.setNodeValue("" + seqNo);
                } else {
                    Element MSA4Ele = doc.createElementNS(nameSpaceURL, MSA4);
                    Text textMSA4 = doc.createTextNode("" + seqNo);
                    MSA4Ele.appendChild(textMSA4);
                    newNode = hasElement(node, MSA3);
                    if (newNode != null) {
                        if (newNode.getNextSibling() != null) {
                            node.insertBefore((Node) MSA4Ele, newNode.getNextSibling());
                        } else {
                            node.appendChild((Node) MSA4Ele);
                        }
                        break;
                    } else {
                        newNode = hasElement(node, MSA2);
                        if (newNode != null) {
                            if (newNode.getNextSibling() != null) {
                                node.insertBefore((Node) MSA4Ele, newNode.getNextSibling());
                            } else {
                                node.appendChild((Node) MSA4Ele);
                            }
                        }
                        break;
                    }

                }
                break;
            }
        }
        return doc;
    }

    /**
     * Insert the MSH Segment by setting MSH-13-Expected Sequence Number
     * 
     * @param doc org.w3c.dom.Document
     * @param int sequence number value which is set to MSH-4-Expected Sequence Number
     * @return Document return updated document
     */

    private Document insertSequenceNumberIntoMsg(Document doc, int seqNo) {
        Node root = doc.getFirstChild().getFirstChild().getFirstChild();
        String nameSpaceURL = root.getNamespaceURI();
        Node newNode = null;

        NodeList list = doc.getFirstChild().getFirstChild().getFirstChild().getChildNodes();
        for (int i = 0; i < list.getLength(); i++) {
            Node node = list.item(i);
            if (node.getLocalName().equals(MSH)) {
                newNode = hasElement(node, MSH13);
                if (newNode != null) {
                    newNode.setNodeValue("" + seqNo);
                } else {
                    Element MSH13Ele = doc.createElementNS(nameSpaceURL, MSH13);
                    Text textMSH13 = doc.createTextNode("" + seqNo);
                    MSH13Ele.appendChild(textMSH13);
                    newNode = hasElement(node, MSH12);
                    if (newNode != null) {
                        if (newNode.getNextSibling() != null) {
                            node.insertBefore((Node) MSH13Ele, newNode.getNextSibling());
                        } else {
                            node.appendChild((Node) MSH13Ele);
                        }
                        break;
                    }
                }
                break;

            }
        }
        return doc;

    }

    /**
     * @param normalizedMsg NormalizedMsg holds the normalized message & properties
     * @param endpoint A service end point.
     * @return HL7Connector an HL7 Connector
     * @throws Exception any exception that creeps when creating a connector to the HL7 System
     */
    private HL7Connector getConnector(NormalizedMessage normalizedMsg, Endpoint endpoint, ProtocolInfo protocolInfo)
            throws NMPropertiesParsingException, Exception {
        HL7Connector hl7Connector = null;
        // String transportProtocolName = endpoint.getHL7Address().getTransportProtocolName();
        // right now only TCPIP transport protocol is supported
        /*
         * if (transportProtocolName.equalsIgnoreCase(TCPIP)) { hl7Connector =
         * createHL7Connector(TransportProtocolType.TCPIP); }
         */

        String llpType = getHL7ProtocolProperties(normalizedMsg, endpoint).getLLPType();
        
        if (endpoint.getHL7Address().getTcpRole() == TcpRoleEnum.SERVER) {
        	hl7Connector = new OutboundTcpServerHL7Connector(mOutboundServerPool);
        } else if (mRuntimeConfig.isAlwaysCreatesNewConnEnabled()) {
			if (MLLPv1.equalsIgnoreCase(llpType)) {
				hl7Connector = createHL7Connector(TransportProtocolType.MLLPV1);
			} else if (MLLPv2.equalsIgnoreCase(llpType)) {
				hl7Connector = createHL7Connector(TransportProtocolType.MLLPV2);
			} else if (HLLP.equalsIgnoreCase(llpType)) {
				hl7Connector = createHL7Connector(TransportProtocolType.HLLP);
			}
		} else {
            hl7Connector = getHL7ConnectorFromPool(protocolInfo, endpoint);
            if( null == hl7Connector ){
                if(MLLPv1.equalsIgnoreCase(llpType)){
                    hl7Connector = createHL7Connector(TransportProtocolType.MLLPV1);                    
                } else if(MLLPv2.equalsIgnoreCase(llpType)){
                    hl7Connector = createHL7Connector(TransportProtocolType.MLLPV2);                    
                } else if(HLLP.equalsIgnoreCase(llpType)){
                    hl7Connector = createHL7Connector(TransportProtocolType.HLLP);                    
                }
                mHL7ConnectorTakenFromPool = false;
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, I18n.msg("I0184: Created new HL7 Connector {0}",
                            hl7Connector));
                }     
            } else{
                mHL7ConnectorTakenFromPool = true;
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, I18n.msg("I0183: Received HL7 Connector from the connection Pool {0}",
                            hl7Connector));
                }                   
            }
		}
        return hl7Connector;
    }

    /**
     * Return the Map of protocol properties that are used in creating a channel to the HL7 System
     * 
     * @param endpoint A service end point.
     * @return ProtocolInfo A Map of required protocol properties
     */
    private ProtocolInfo getProtocolInfo(NormalizedMessage normalizedMsg, Endpoint endpoint)
            throws NMPropertiesParsingException {
        ProtocolInfo protocolInfo = null;
        HL7Address hl7Address = getHL7Address(normalizedMsg, endpoint);
        String transportProtocolName = hl7Address.getTransportProtocolName();
        HL7ProtocolProperties hl7ProtocolProperties = getHL7ProtocolProperties(normalizedMsg, endpoint);
        // get the connection pool properties
        Properties prop  = mRuntimeConfig.getProperties();
        // right now only TCPIP transport protocol is supported
        if (transportProtocolName.equalsIgnoreCase(TCPIP)) {
            protocolInfo = new ProtocolInfo();
            // Server port
            protocolInfo.put(HL7Address.ATTR_HL7_SVR_LOCATION, hl7Address.getHL7ServerLocation());
            protocolInfo.put(HL7Address.ATTR_HL7_SVR_PORT, hl7Address.getHL7ServerPort().toString());
            HL7CommunicationControl commCntrl = mHL7CommunicationControlsInfo.getMaxConnectionRetriesControl();
            if (commCntrl != null && commCntrl.getEnabled()) {
                protocolInfo.put(HL7Constants.MAX_CONNECTION_RETRIES, commCntrl.getValueAsString());
            }
			if(!mRuntimeConfig.isAlwaysCreatesNewConnEnabled()) {

				String connectionPoolMinSize = prop.getProperty(RuntimeConfiguration.CONFIG_POOL_MIN_SZ);
				if(connectionPoolMinSize != null){
					protocolInfo.put(HL7Constants.MIN_POOL_SIZE, connectionPoolMinSize);
				}
				String connectionPoolMaxSize = prop.getProperty(RuntimeConfiguration.CONFIG_POOL_MAX_SZ);
				if(connectionPoolMinSize != null){
					protocolInfo.put(HL7Constants.MAX_POOL_SIZE, connectionPoolMaxSize);
				}
				String connectionIdleTimeOut = prop.getProperty(RuntimeConfiguration.CONFIG_CONN_MAX_IDLE_TIMEOUT);
				if(connectionPoolMinSize != null){
					protocolInfo.put(HL7Constants.MAX_IDLE_TIMEOUT, connectionIdleTimeOut);
				}
			}
            ProtocolInfo llpInfo = Util.populateLLPInfo(hl7ProtocolProperties);
            protocolInfo.putAll(llpInfo);
        }
        return protocolInfo;
    }

    /**
     * Sends an acknowledgment to the HL7 System
     * 
     * @param hl7PayLoad the hl7 payload
     * @param hl7Connector the channel to connect to HL7 System
     * @param protocolInfo Map of properties required in establishing connection to the HL7 System
     * @param endpoint A service end point.
     * @throws ApplicationException any exception that occurs during opening a connection or sending
     *             the message to the HL7 System
     */
    private void sendACKToExtSys(String hl7PayLoad,
                                 HL7Connector hl7Connector,
                                 ProtocolInfo protocolInfo,
                                 Endpoint endpoint) throws ApplicationException {
        // TODO: handling recourse actions
        establishConnectionToExtSys(hl7Connector, protocolInfo, endpoint);
        try {
            hl7Connector.sendHL7Message(hl7PayLoad);
        } catch (Exception ex) {
            mLog.log(Level.SEVERE, I18n.msg("E0212: Sending HL7 message to HL7 External System Failed"));
            throw new ApplicationException(I18n.msg("E0212: Sending HL7 message to HL7 External System Failed"));
        } finally {
            try {
                Thread.sleep(3000);
                // Once the message is sent close the handle to the HL7 external system
                hl7Connector.disconnect();
            } catch (Exception ex) {
                // ignore
            }
        }

    }

    /**
     * Sends and receive HL7 Request/Ack to/from the HL7 External System
     * 
     * @param inout the in-out Message Exchange
     * @param hl7PayLoad the hl7 payload
     * @param hl7Connector the channel to connect to HL7 System
     * @param protocolInfo Map of properties required in establishing connection to the HL7 System
     * @param endpoint A service end point.
     * @return hl7Message represents the extension class for the <hl7:message/> WSDL Extensibility
     *         Element
     * @throws ApplicationException any exception that occurs during opening a connection or sending
     *             the message to the HL7 System
     */
    private NormalizedMessage sendRequestAndReceiveACK(InOut inout,
                                                       String hl7PayLoad,
                                                       HL7Connector hl7Connector,
                                                       ProtocolInfo protocolInfo,
                                                       Endpoint endpoint,
                                                       HL7Message hl7message) throws Exception {
        NormalizedMessage ackNormalizedMsg = null;
        try {
            NormalizedMessage normalizedMsg = inout.getInMessage();
            QName qualOperName = inout.getOperation();
            // TODO: handling recourse actions
            HL7ProtocolProperties hl7ProtocolProps = getHL7ProtocolProperties(normalizedMsg, endpoint);
            String ackMode = hl7ProtocolProps.getAckMode();
            Boolean seqnumberEnabled = hl7ProtocolProps.getSeqNumEnabled();
            Boolean journallingEnabled = hl7ProtocolProps.getJournallingEnabled();
            String acceptACKCond = InboundReplyContext.getAcceptAckCondtion(normalizedMsg.getContent());
            establishConnectionToExtSys(hl7Connector, protocolInfo, endpoint);
            // <checkpoint>
            if ( mMonitorEnabled ) {
                HL7MMUtil.setCheckpoint(mChkptMsgId, "Connection-Established-To-ExternalSystem", endpoint );
            }
            // </checkpoint>
            String ackMsg = sendAndReceiveHL7Message(inout, hl7PayLoad, hl7Connector, endpoint, hl7message, hl7ProtocolProps);
            // <checkpoint>
            if ( mMonitorEnabled ) {
                HL7MMUtil.setCheckpoint(mChkptMsgId, "Message-Sent-And-ACK-Received-From-ExternalSystem", endpoint );
            }
            // </checkpoint>
            // String applicationAckCond = InboundReplyContext.getAppAckCondtion(normalizedMsg);
            // When Acknowledgment mode is enhanced and acknowledgment condition is NE (NEVER)
            // do not wait for the acknowledgment from external system
            /*
             * if (ackMode.equals(ACK_MODE_ENHANCED) && acceptACKCond.equals(NEVER_CONDITION))
             * return null;
             */
            // Disconnect the connection
            hl7Connector.disconnect();
            if (ackMsg != null) {
                try {
                    // calculate performance measurment
                    if(mAckNormalizedMsg == null){
                        String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) ? (createConsumingEndpointIdentifier(
                                endpoint.getServiceName(), endpoint.getEndpointName()))
                                : (createProvisioningEndpointIdentifier(endpoint.getServiceName(),
                                        endpoint.getEndpointName()));
                        Probe normalizationMeasurement = Probe.info(getClass(), endPointID,
                                HL7BindingComponent.PERF_CAT_NORMALIZATION);
                        ackNormalizedMsg = mHL7Normalizer.normalize(inout, qualOperName, endpoint, hl7message, ackMsg);
                        if (normalizationMeasurement != null) {
                            normalizationMeasurement.end();
                        }
                    }else{
                        ackNormalizedMsg =  mAckNormalizedMsg;
                    }
                } catch (Exception exe) {
                    throw exe;
                }
                // get the MSH node
                Node node = ((DOMSource) ackNormalizedMsg.getContent()).getNode().getFirstChild().getFirstChild().getFirstChild().getFirstChild();
                MSHInfo mshInfo = new MSHInfo();
                mshInfo.setMSHSegment(node);
                mshInfo.unmarshal();

                // get the MSA node
                while (node != null && node.getLocalName() != null && !node.getLocalName().equalsIgnoreCase(MSA)) {
                	node = node.getNextSibling();
                }

                if (node == null) {
                	throw new Exception(I18n.msg("E0344: Did not detect MSA segment in response message. Response was: {0}",
                            ackMsg));
                }
                
                MSAInfo msaInfo = new MSAInfo();
                msaInfo.setMSASegment(node);
                msaInfo.unmarshal();

                String ackCode = msaInfo.getAcknowledgmentCode();
                String msgControlID = msaInfo.getMsgControlID();
                if (ackCode.equals(ACKBuilder.APP_ERROR) || ackCode.equals(ACKBuilder.APP_REJECT)
                        || ackCode.equals(ACKBuilder.COMMIT_ERROR) || ackCode.equals(ACKBuilder.COMMIT_REJECT)) {
                    return ackNormalizedMsg;
                }
                if (seqnumberEnabled.booleanValue()) {
                    int mshSeqNo = mshInfo.getSequenceNumber();
                    int msaSeqNo = msaInfo.getSequenceNumner();
                    DBConnection dbConn = null;
                    try {
                        dbConn = getDBConnection();
                        // if msaSeqNo and mshSeqNo are -1, go for normal operation , ie start with
                        // seqno 1
                        if (msaSeqNo == -1) {
                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), 1, VALID_ESN_STATE, dbConn);
                            // if mshSeqNo == 0 and msaSeqNo > 0, update the outgoing sequence no
                            // with
                            // msaSeqNo
                        } else if (mshSeqNo == 0 && msaSeqNo > 0) {
                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), msaSeqNo, VALID_ESN_STATE, dbConn);
                            // if ESN and #SQ No are same and #SQNo+1 = ESN, update the data base
                            // with
                            // #SQNO +1;
                        } else if (mshSeqNo == msaSeqNo || (mshSeqNo + 1) == msaSeqNo) {
                            mshSeqNo += 1;
                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), mshSeqNo, VALID_ESN_STATE, dbConn);
                            // if #SQ No > ESN update the data base with ESN;
                        } else if (mshSeqNo > msaSeqNo) {

                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), msaSeqNo, VALID_ESN_STATE, dbConn);
                        }

                    } finally {
                        if (dbConn != null) {
                            dbConn.close();
                        }
                    }
                }
                // journal message in DB
                if(journallingEnabled){
                    try{
                        journalMessageInDB(endpoint.getUniqueName() , msgControlID, "Client", hl7PayLoad, 
                                ackMsg, null, "DONE");
                        if (mLog.isLoggable(Level.FINE)) {
                                mLog.log(Level.FINE, I18n.msg("I0174: Successfully journal the message in DB"));
                        }
                    }catch(Exception e){
                        mLog.log(Level.WARNING, I18n.msg("W0131: Failed to journal the message due to {0}",
                                e.getLocalizedMessage()));
                    }
                }
            }
        } finally {
            try {
                if (hl7Connector != null) {
                    // Once the transaction with the external system is done; close the handle to
                    // the
                    // HL7 external system
                    hl7Connector.disconnect();
                }
            } catch (Exception ex) {
                throw ex;
            }
            // <checkpoint>
            if ( mNdcEnabled ) {
                // "Pop" NDC context
                Logger.getLogger("com.sun.ExitContext")
                      .log(Level.FINE, "{0}={1}", new Object[] {HL7MMUtil.SOLUTION_GROUP,
                      HL7MMUtil.getSolutionGroup(endpoint)});
            }
            // </checkpoint>

        }
        return ackNormalizedMsg;
    }

    /**
     * Sends and receive HL7 Request/Ack to/from the HL7 System
     * 
     * @param inOnly InOnly Message Exchange
     * @param hl7PayLoad the hl7 payload
     * @param hl7Connector the channel to connect to HL7 System
     * @param protocolInfo Map of properties required in establishing connection to the HL7 System
     * @param endpoint A service end point.
     * @throws ApplicationException any exception that occurs during opening a connection or sending
     *             the message to the HL7 System
     */
    private void start2WayChannelWithExtSystem(InOnly inOnly,
                                               String hl7PayLoad,
                                               HL7Connector hl7Connector,
                                               ProtocolInfo protocolInfo,
                                               Endpoint endpoint,
                                               HL7Message hl7message) throws Exception {
        try {
            NormalizedMessage normalizedMsg = inOnly.getInMessage();
            HL7ProtocolProperties hl7ProtocolProps = getHL7ProtocolProperties(normalizedMsg, endpoint);
            String ackMode = hl7ProtocolProps.getAckMode();
            Boolean seqnumberEnabled = hl7ProtocolProps.getSeqNumEnabled();
            Boolean journallingEnabled = hl7ProtocolProps.getJournallingEnabled(); 
            String acceptACKCond = InboundReplyContext.getAcceptAckCondtion(normalizedMsg.getContent());
            establishConnectionToExtSys(hl7Connector, protocolInfo, endpoint);
            // <checkpoint>
            if ( mMonitorEnabled ) {
                HL7MMUtil.setCheckpoint(mChkptMsgId, "Connection-Established-To-ExternalSystem", endpoint );
            }
            // </checkpoint>
            String ackMsg = sendAndReceiveHL7Message(inOnly, hl7PayLoad, hl7Connector, endpoint, hl7message, hl7ProtocolProps);
            // <checkpoint>
            if ( mMonitorEnabled ) {
                HL7MMUtil.setCheckpoint(mChkptMsgId, "Message-Sent-And-ACK-Received-From-ExternalSystem", endpoint );
            }
            // </checkpoint>
            // When Acknowledgment mode is enhanced and acknowledgment condition is NE (NEVER)
            // do not wait for the acknowledgment from external system
            if (ackMode.equals(ACK_MODE_ENHANCED) && acceptACKCond.equals(NEVER_CONDITION))
                return;
            // Disconnect the connection
            hl7Connector.disconnect();
            if (ackMsg != null) {
                // retrieve the acknwoledgment code
                if(mACKDOMresult == null){
                    mACKDOMresult = getDOMResult(normalizedMsg, ackMsg, endpoint, hl7message);
                }
                //DOMResult result = getDOMResult(normalizedMsg, ackMsg, endpoint, hl7message);
                MSHInfo mshInfo = returnMSHInfo(mACKDOMresult);
                MSAInfo msaInfo = returnMSAInfo(mACKDOMresult);
                String ackCode = msaInfo.getAcknowledgmentCode();
                String msgControlID = msaInfo.getMsgControlID();
                if (ackCode.equals(ACKBuilder.APP_ERROR) || ackCode.equals(ACKBuilder.APP_REJECT)
                        || ackCode.equals(ACKBuilder.COMMIT_ERROR) || ackCode.equals(ACKBuilder.COMMIT_REJECT)) {
                    // this acknowledgment code sets the messge exchanges status to error
                    throw new ApplicationException(I18n.msg("E0217: Received a NAK from HL7 External System"));

                }
                if (seqnumberEnabled.booleanValue()) {
                    int mshSeqNo = mshInfo.getSequenceNumber();
                    int msaSeqNo = msaInfo.getSequenceNumner();
                    DBConnection dbConn = null;
                    try {
                        dbConn = getDBConnection();
                        // if msaSeqNo and mshSeqNo are -1, go for normal operation , ie start with
                        // seqno 1
                        if (msaSeqNo == -1) {
                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), 1, VALID_ESN_STATE, dbConn);
                            // if mshSeqNo == 0 and msaSeqNo > 0, update the outgoing sequence no
                            // with
                            // msaSeqNo
                        } else if (mshSeqNo == 0 && msaSeqNo > 0) {
                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), msaSeqNo, VALID_ESN_STATE, dbConn);
                            // if ESN and #SQ No are same and #SQNo+1 = ESN, update the data base
                            // with
                            // #SQNO +1;
                        } else if (mshSeqNo == msaSeqNo || (mshSeqNo + 1) == msaSeqNo) {
                            mshSeqNo += 1;
                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), mshSeqNo, VALID_ESN_STATE, dbConn);
                            // if #SQ No > ESN update the data base with ESN;
                        } else if (mshSeqNo > msaSeqNo) {
                            updateSequenceNumberInDB(endpoint.getServiceUnitPath(), msaSeqNo, VALID_ESN_STATE, dbConn);
                        }
                    } finally {
                        if (dbConn != null) {
                            dbConn.close();
                        }
                    }
                }
                // persist the received acknowledgment to Database
                // storeAcknowledgmentInDB(msgControlID, ackMsg);
                // journal message in DB
                if(journallingEnabled){
                    try{
                        journalMessageInDB(endpoint.getUniqueName() , msgControlID, "Client", hl7PayLoad, 
                                ackMsg, null, "DONE");
                        if (mLog.isLoggable(Level.FINE)) {
                                mLog.log(Level.FINE, I18n.msg("I0174: Successfully journal the message in DB"));
                        }
                    }catch(Exception e){
                        mLog.log(Level.WARNING, I18n.msg("W0131: Failed to journal the message due to {0}",
                                e.getLocalizedMessage()));
                    }                    
                }
            }
        } finally {
            try {
                if (hl7Connector != null) {
                    // Once the transaction with the external system is done; close the handle to
                    // the HL7 external system
                    hl7Connector.disconnect();
                }
            } catch (Exception ex) {
                throw ex;
            }
            // <checkpoint>
            if ( mNdcEnabled ) {
                // "Pop" NDC context
                Logger.getLogger("com.sun.ExitContext")
                      .log(Level.FINE, "{0}={1}", new Object[] {HL7MMUtil.SOLUTION_GROUP,
                      HL7MMUtil.getSolutionGroup(endpoint)});
            }
            // </checkpoint>

        }
    }

    private void establishConnectionToExtSys(HL7Connector hl7Connector, ProtocolInfo protocolInfo, Endpoint endpoint)
            throws ApplicationException {
        try {
			if (hl7Connector instanceof OutboundTcpServerHL7Connector) {
				hl7Connector.connect(protocolInfo, endpoint);
			} else if (!mRuntimeConfig.isAlwaysCreatesNewConnEnabled()){
				boolean isPooledConnector = true;
				if((hl7Connector != null) && (mHL7ConnectorTakenFromPool) && (hl7Connector.getHL7Connection().isConnected())){
					hl7Connector.setIoSession(hl7Connector.getHL7Connection());
				}else{
					hl7Connector.connect(protocolInfo, endpoint);
				}
			} else{
				// if always creates new connection enabled
				hl7Connector.connect(protocolInfo);
			}
        } catch (Exception ex) {
            mHL7CommunicationControl = mHL7CommunicationControlsInfo.getMaxConnectionRetriesControl();
            if (mHL7CommunicationControl != null
                    && ACTION_SUSPEND.equalsIgnoreCase(mHL7CommunicationControl.getRecourseAction())) {
                suspendRecourseAction("Suspend on Max Connection Retries", hl7Connector, endpoint);
            } else if (mHL7CommunicationControl != null
                    && ACTION_ERROR.equalsIgnoreCase(mHL7CommunicationControl.getRecourseAction())) {
                throw new ApplicationException(I18n.msg("E0213: Unable to connect to the HL7 external system "), ex);
            }
            String msg = I18n.msg("E0214: Connect to HL7 External System Failed. Address: {0}", endpoint.getHL7Address().getHL7ServerLocationURL());
			mLog.log(Level.SEVERE, msg, ex);
            throw new ApplicationException(msg, ex);
        }
    }

    private String sendAndReceiveHL7Message(MessageExchange msgExchange,
                                            String hl7PayLoad,
                                            HL7Connector hl7Connector,
                                            Endpoint endpoint,
                                            HL7Message hl7message,
                                            HL7ProtocolProperties hl7ProtoclProperties) throws Exception {
        String ackMsg = null;
        HL7CommunicationControl commCntrl = null;
        NormalizedMessage normalizedMsg = null;
        if (msgExchange instanceof InOnly) {
            normalizedMsg = ((InOnly) msgExchange).getInMessage();
        } else if (msgExchange instanceof InOut) {
            normalizedMsg = ((InOut) msgExchange).getInMessage();
        }
        //HL7ProtocolProperties hl7ProtoclProperties = hl7ProtoclProperties;
        String ackMode = hl7ProtoclProperties.getAckMode();
        Boolean journallingEnabled = hl7ProtoclProperties.getJournallingEnabled();
        QName qualOperName = msgExchange.getOperation();
        try {
            hl7Connector.sendHL7Message(hl7PayLoad);
            endpoint.setLastHL7MsgSentTimeStamp(System.currentTimeMillis());
        } catch (Exception ex) {
            mLog.log(Level.SEVERE, I18n.msg("E0212: Sending HL7 message to HL7 External System Failed"));
            throw new ApplicationException(I18n.msg("E0212: Sending HL7 message to HL7 External System Failed"));
        }
        String acceptACKCond = InboundReplyContext.getAcceptAckCondtion(normalizedMsg.getContent());
        // String applicationAckCond = InboundReplyContext.getAppAckCondtion(normalizedMsg);
        // When Acknowledgment mode is enhanced and acknowledgment condition is NE (NEVER)
        // do not wait for the acknowledgment from external system
        if (ackMode.equals(ACK_MODE_ENHANCED) && acceptACKCond.equals(NEVER_CONDITION))
            return null;
        try {
            long timeToWait = -1;
            
            mHL7CommunicationControl = mHL7CommunicationControlsInfo.getTimeToWaitCommControl();
            boolean defaultMode = false;
            if (mHL7CommunicationControl != null && mHL7CommunicationControl.getEnabled().booleanValue()) {
                timeToWait = mHL7CommunicationControl.getValue();
            } else {
            	defaultMode = true;
                timeToWait = mRuntimeConfig.getDefaultMaxTimeToWaitForAResponse();
            }
            
            if (timeToWait > 0) {
                ackMsg = hl7Connector.recvHL7Message(timeToWait);
                if (ackMsg == null) {
                    commCntrl = mHL7CommunicationControlsInfo.getMaxNoResponseCommControl();
                    // Max No Response communication control
                    if (commCntrl != null && commCntrl.getEnabled().booleanValue()) {
                        handleMaxNoResponse(hl7Connector, endpoint);
                    }
                    if (ACTION_RESEND.equalsIgnoreCase(mHL7CommunicationControl.getRecourseAction())) {
                        return resendRecourseAction("Resend on no response; no response count  " + mCountNoResponse,
                                msgExchange, hl7PayLoad, hl7Connector, endpoint, hl7message, hl7ProtoclProperties);
                    } else if (ACTION_SUSPEND.equalsIgnoreCase(mHL7CommunicationControl.getRecourseAction())) {
                        suspendRecourseAction("Suspend on no response", hl7Connector, endpoint);
                    } else if (defaultMode || ACTION_RESET.equalsIgnoreCase(mHL7CommunicationControl.getRecourseAction())) {
                        resetRecourseAction("Reset on no response", hl7Connector);
                    }
                }
            } else {
                ackMsg = hl7Connector.recvHL7Message();
                endpoint.setLastACKMsgReceivedTimeStamp(System.currentTimeMillis());
            }
            
            if (ackMsg != null) {
                boolean isAck = true;
                String messageControlID = null;
                // when ME is InOut then apply the encoder (which is configured on hl7:message
                // element
                // of hl7:operation's output element)
                // on the received acknowledgement to get the normalized form for the HL7
                // acknowledgement
                if (msgExchange instanceof InOut) {
                    try {
                        // calculate performance measurment
                        String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) ? (createConsumingEndpointIdentifier(
                                endpoint.getServiceName(), endpoint.getEndpointName()))
                                : (createProvisioningEndpointIdentifier(endpoint.getServiceName(),
                                        endpoint.getEndpointName()));
                        Probe normalizationMeasurement = Probe.info(getClass(), endPointID,
                                HL7BindingComponent.PERF_CAT_NORMALIZATION);
                        mAckNormalizedMsg = mHL7Normalizer.normalize((InOut) msgExchange,
                                qualOperName, endpoint, hl7message, ackMsg);
                        if (normalizationMeasurement != null) {
                            normalizationMeasurement.end();
                        }
                        // get the MSH node
                        Node node = ((DOMSource) mAckNormalizedMsg.getContent()).getNode().getFirstChild().getFirstChild().getFirstChild().getFirstChild();
                        // get the MSA node
                        node = node.getNextSibling();
                        if(!node.getLocalName().equalsIgnoreCase(MSA)) {
                            node = node.getNextSibling();
                        }
                        MSAInfo msaInfo = new MSAInfo();
                        msaInfo.setMSASegment(node);
                        msaInfo.unmarshal();
                        String ackCode = msaInfo.getAcknowledgmentCode();
                        messageControlID = msaInfo.getMsgControlID();
                        if (ackCode.equals(ACKBuilder.APP_ERROR) || ackCode.equals(ACKBuilder.APP_REJECT)
                                || ackCode.equals(ACKBuilder.COMMIT_ERROR) || ackCode.equals(ACKBuilder.COMMIT_REJECT)) {
                            isAck = false;
                        }
                    } catch (Exception exe) {
                        mLog.log(Level.WARNING, I18n.msg("W0125: Normalizing HL7 message acknowledgement failed"), exe);
                        isAck = false;
                    }
                } else if (msgExchange instanceof InOnly) {
                    try {
                        mACKDOMresult = getDOMResult(normalizedMsg, ackMsg, endpoint, hl7message);
                        MSAInfo msaInfo = returnMSAInfo(mACKDOMresult);
                        String ackCode = msaInfo.getAcknowledgmentCode();
                        messageControlID = msaInfo.getMsgControlID();
                        if (ackCode.equals(ACKBuilder.APP_ERROR) || ackCode.equals(ACKBuilder.APP_REJECT)
                                || ackCode.equals(ACKBuilder.COMMIT_ERROR) || ackCode.equals(ACKBuilder.COMMIT_REJECT)) {
                            isAck = false;
                        }
                    } catch (Exception ex) {
                        mLog.log(Level.WARNING, I18n.msg("W0126: Unable to decode the message due to error=[{0}]", ex));
                        isAck = false;
                    }
                }
                if (!isAck) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, I18n.msg("Nak Message Received"), ackMsg);
                    }
                    boolean retry = true;
                    boolean archived = false;
                    commCntrl = mHL7CommunicationControlsInfo.getMaxNakReceivedCommControl();
                    // handle max nak recieved first
                    if (commCntrl != null && commCntrl.getEnabled().booleanValue()) {
                        retry = handleMaxNakReceived(hl7Connector, endpoint, hl7PayLoad, ackMsg, journallingEnabled, messageControlID);
                    }
                    HL7CommunicationControl nakRecvCommCntrl = mHL7CommunicationControlsInfo.getNakReceivedCommControl();
                    if (retry && nakRecvCommCntrl != null && nakRecvCommCntrl.getEnabled().booleanValue()) {
                        // take resend recourse action
                        if (ACTION_RESEND.equalsIgnoreCase(nakRecvCommCntrl.getRecourseAction())) {
                            return resendRecourseAction("Resend on NAK received; NAK receive count  "
                                    + mCountNakReceived, msgExchange, hl7PayLoad, hl7Connector, endpoint, hl7message, hl7ProtoclProperties);
                        } else if (ACTION_RESET.equalsIgnoreCase(nakRecvCommCntrl.getRecourseAction())) {
                            resetRecourseAction("Reset on NAK received; NAK receive", hl7Connector);
                        }else if(SKIPMESSAGE.equalsIgnoreCase(nakRecvCommCntrl.getRecourseAction())){
                            // ToDo: SKIP Message handling
                            skipMessageRecourseAction("Skip message on NAK received", hl7Connector, endpoint, hl7PayLoad, ackMsg,
                                    journallingEnabled, messageControlID);
                            archived = true;
                        }
                    }
                    if(journallingEnabled && !archived){
                        // received NAK from the external system, and archive the message
                        try{
                           journalMessageInDB(endpoint.getUniqueName() , messageControlID, CLIENT, hl7PayLoad, 
                                                     null, ackMsg, "ERROR");
                           if (mLog.isLoggable(Level.FINE)) {
                                   mLog.log(Level.FINE, I18n.msg("I0175: Successfully archive the Error message in DB"));
                            }
                         }catch(Exception e){
                              mLog.log(Level.WARNING, I18n.msg("W0132: Failed to archive the Error message due to {0}",
                                   e.getLocalizedMessage()));
                         }
                    }
                }
                // reset nak count
                mCountNakReceived = 0;
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, I18n.msg("Received HL7 Message"), ackMsg);
                }
            }
        } catch (Exception ex) {
            mLog.log(Level.SEVERE,
                    I18n.msg("E0215: Receiving HL7 message acknowledgement from HL7 External System Failed"));
            throw new ApplicationException(
                    I18n.msg("E0215: Receiving HL7 message acknowledgement from HL7 External System Failed"), ex);
        }
        return ackMsg;
    }

    private boolean handleMaxNakReceived(HL7Connector hl7Connector, Endpoint endpoint, String hl7message, String nakmessage,
                                         Boolean journallingEnabled, String messageControlID) throws Exception {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("Entered handleMaxNakReceived() method"));
        }
        boolean retry = true;
        mCountNakReceived += 1;
        // handle max nak recourse action
        HL7CommunicationControl commCntrl = mHL7CommunicationControlsInfo.getMaxNakReceivedCommControl();
        if (mCountNakReceived > commCntrl.getValue()) {
            // max nak received will be handled, so reset count to 0
            mCountNakReceived = 0;
            if (ACTION_SUSPEND.equalsIgnoreCase(commCntrl.getRecourseAction())) {
                suspendRecourseAction("Suspend on max NAK received; max NAK received is " + commCntrl.getValue(),
                        hl7Connector, endpoint);
            } else if (ACTION_RESET.equalsIgnoreCase(commCntrl.getRecourseAction())) {
                resetRecourseAction("Reset on max NAK received; max NAK received is " + commCntrl.getValue(),
                        hl7Connector);
            }else if(SKIPMESSAGE.equalsIgnoreCase(commCntrl.getRecourseAction())){
                // ToDo: SKIP Message handling
                skipMessageRecourseAction("Skip message on max NAK received; max NAK received is " +commCntrl.getValue(),
                        hl7Connector, endpoint, hl7message, nakmessage, journallingEnabled, messageControlID);
                retry = false;
            }// ToDo: SKIP Message handling
        }
        return retry;
    }

    private void handleMaxNoResponse(HL7Connector hl7Connector, Endpoint endpoint) throws Exception {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("Entered handleMaxNoResponse() method"));
        }
        mCountNoResponse += 1;
        // handle max no response recourse action
        HL7CommunicationControl commCntrl = mHL7CommunicationControlsInfo.getMaxNoResponseCommControl();
        if (mCountNoResponse > commCntrl.getValue()) {
            // max no response will be handled, reset count to 0
            mCountNoResponse = 0;
            if (ACTION_SUSPEND.equalsIgnoreCase(commCntrl.getRecourseAction())) {
                suspendRecourseAction("Suspend on max no response; max no response is " + commCntrl.getValue(),
                        hl7Connector, endpoint);
            } else if (ACTION_RESET.equalsIgnoreCase(commCntrl.getRecourseAction())) {
                resetRecourseAction("Reset on max no response; max no response is " + commCntrl.getValue(),
                        hl7Connector);
            }
        }
    }

    private void skipMessageRecourseAction(String resoneCode, HL7Connector hl7Connector, Endpoint endpoint, String hl7message,
                                              String nakmessage, Boolean journallingEnabled, String messageControlID) throws Exception {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("Entered skipMessageRecourseAction() method"));
        }
        mLog.log(Level.WARNING, I18n.msg("W0133: HL7 message skiped due to an error=[{0}], Message [{1}], NAK message [{2}]", resoneCode, hl7message, nakmessage));
        if(journallingEnabled){
            try{
                journalMessageInDB(endpoint.getUniqueName() , messageControlID, CLIENT, hl7message, 
                        null, nakmessage, "ERROR--"+resoneCode);
                if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, I18n.msg("I0176: Archive the skip error message in DB"));
                }
            }catch(Exception e){
                mLog.log(Level.WARNING, I18n.msg("W0134: Unabled to archive HL7 message due to an error condition {0}",
                        e.getLocalizedMessage()));
                try {
                    // close the existing connection
                    if (hl7Connector != null) {
                        hl7Connector.disconnect();
                    }
                 } catch (Exception ex) {
                    throw new Exception(e.getLocalizedMessage());
                 }
            }
        }else{
            String text = I18n.msg("E0332: HL7 message skiped due to an error=[{0}]", resoneCode);
            AlertsUtil.getAlerter().critical(text, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0332");
        }
    }

    private HL7Address getHL7Address(NormalizedMessage msg, Endpoint endpoint) throws NMPropertiesParsingException {
        HL7Address hl7Address = null;

        if (isDynamicEndpoint(msg)) {
            OutboundNMProperties outboundNMProps = NMPropertiesUtil.getOutBoundNMProperties(msg);
            hl7Address = NMPropertiesUtil.fabricateAddress(outboundNMProps, endpoint.getHL7Address());
        } else {
            hl7Address = endpoint.getHL7Address();
        }
        return hl7Address;
    }

    private HL7ProtocolProperties getHL7ProtocolProperties(NormalizedMessage msg, Endpoint endpoint)
            throws NMPropertiesParsingException {
        HL7ProtocolProperties hl7Props = null;

        if (isDynamicEndpoint(msg)) {
            OutboundNMProperties outboundNMProps = NMPropertiesUtil.getOutBoundNMProperties(msg);
            hl7Props = NMPropertiesUtil.fabricateProtocolProperties(outboundNMProps,
                    endpoint.getHL7ProtocolProperties());
        } else {
            hl7Props = endpoint.getHL7ProtocolProperties();
        }
        return hl7Props;
    }

    private boolean isDynamicEndpoint(NormalizedMessage msg) {
        Object p = msg.getProperty(NMPropertiesUtil.NM_PROP_HL7BC_USE_DYN_EP_BINDING);
        boolean b = false;
        if (p != null) {
            String s = p.toString();
            if (s != null && s.equalsIgnoreCase("true") && mRuntimeConfig.getAllowDynamicEndpoint().booleanValue()) {
                b = true;
            }
        }
        return b;
    }

    /**
     * Retruns the MSAInfo
     * 
     * @param result DOMResult a *
     * @return String MSAInfo
     * @throws Exception
     */
    private MSAInfo returnMSAInfo(DOMResult result) throws Exception {
        try {
            Node msaNode = ((Document) result.getNode()).getFirstChild().getFirstChild().getNextSibling();
            if(!msaNode.getLocalName().equalsIgnoreCase(MSA)){
               msaNode = msaNode.getNextSibling();
            }
            MSAInfo msaInfo = new MSAInfo();
            msaInfo.setMSASegment(msaNode);
            msaInfo.unmarshal();
            return msaInfo;
        } catch (Exception ex) {
            throw ex;
        }

    }

    /**
     * Retruns the MSHInfo
     * 
     * @param result DOMResult
     * @return String MSHInfo
     * @throws Exception
     */
    private MSHInfo returnMSHInfo(DOMResult result) throws Exception {
        try {
            Node mshNode = ((Document) result.getNode()).getFirstChild().getFirstChild();
            MSHInfo mshInfo = new MSHInfo();
            mshInfo.setMSHSegment(mshNode);
            mshInfo.unmarshal();
            return mshInfo;
        } catch (Exception ex) {
            throw ex;
        }

    }

    /**
     * Retruns the DOMResult
     * 
     * @param hl7PayLoad received acknowledgment
     * @param endpoint A service end point.
     * @return DOMResult
     * @throws Exception
     */

    private DOMResult getDOMResult(NormalizedMessage normalizedMsg,
                                   String hl7PayLoad,
                                   Endpoint endpoint,
                                   HL7Message hl7message) throws Exception {
        Encoder encoder = null;
        Source source = null;
        try {
            String use = hl7message.getUseType();
            if (use.equals(HL7Message.ATTR_USE_TYPE_ENCODED)) {
                // create the QName for ACK
                final QName ack = new QName(V2xTNS, ACK);
                // get the xsd file location
                // final String xsdFileLoc = getXsdFileLocation(V2xTNS, ack,
                // endpoint.getXsdsList());
                // get the url of the xsd files
                String compInstallRoot = mComponentContext.getInstallRoot().replace("\\", "/");
                // to handle spaces in the file path
                compInstallRoot = compInstallRoot.replace(" ", "%20");
                String hl7VerDir = getVersionDir(getHL7ProtocolProperties(normalizedMsg, endpoint).getVersionID());
                StringBuilder sb = new StringBuilder(URL_SCHEME).append(compInstallRoot).append(SCHEMAS_LOCATION).append(
                        hl7VerDir);
                sb.append("/").append(ACKXSD_FILENAME);
                final URL url = new URL(sb.toString());
                
                // Build up a map of encoders used to decode the ACK messages
                // received from systems we are sending to
                encoder = mAckEncoders.get(url);
                if (encoder == null) {
                    MetaRef metaRef = new MetaRef() {
                        public String getPath() {
                            return null;
                        }

                        public URL getURL() {
                            return url;
                        }

                        public QName getRootElemName() {
                            return ack;
                        }
                    };

                    EncoderFactory encoderFactory = EncoderFactory.newInstance();
                    encoder = encoderFactory.newEncoder(encoderFactory.makeType(HL7_ENCODINGSTYLE), metaRef);
                    mAckEncoders.put(url, encoder);
                }
                // Decode raw data
                source = encoder.decodeFromString(hl7PayLoad);
            } else {
                Document document = createDocumentFromXML(true, new String(hl7PayLoad));
                Element element = document.getDocumentElement();
                source = new DOMSource((Node) element);
            }
            DOMResult result = transformToDOMResult(mTrans, source);
            return result;

        } finally {
            // It is assumed that hl7encoder-1.0 doesn't hold any heavyweight
            // resources and therefore needs no disposal
//            if (encoder != null) {
//                encoder.dispose();
//                encoder = null;
//            }
        }
    }

    /**
     * Store the received acknowledgment in the database against the Message Control ID
     * 
     * @param msgControlId MSA-2-MessageControlId
     * @param ackMsg Acknowledgment message
     * @throws Exception
     */
    /*
     * private void storeAcknowledgmentInDB(String msgControlId, String ackMsg) throws Exception {
     * DBConnection dbCon = null; try { dbCon = getDBConnection(); AcknowledgmentDBO ackDBO =
     * mDBObjectFactory.createAcknowledgmentDBO(msgControlId, ackMsg); dbCon.insert(ackDBO); } catch
     * (Exception ex) { throw ex; } finally { if (dbCon != null) {
     * dbCon.getUnderlyingConnection().close(); } } }
     */

    /**
     * Returns the DBConnection
     * 
     * @return
     * @throws SQLException
     * @throws Exception
     */
    private DBConnection getDBConnection() throws SQLException, Exception {
        return mDBConnectionFactory.createConnection();
    }

    /**
     * get the DB Connection factory and DB object
     * @throws SQLException
     * @throws Exception
     */
    private void initDBConnFactoryAndDBObjFactory() throws SQLException, Exception {
		if(mDBConnectionFactory != null){
		   return;		
		}
        // get the Initial Context
        InitialContext ic = mComponentContext.getNamingContext();
        // get the properties
        Properties props = mRuntimeConfig.getProperties();
        mDBConnectionFactory = new DBConnectionFactory(props, ic, mComponentContext.getInstallRoot());
        mDBObjectFactory = DBObjectFactory.getDBObjectFactory(mDBConnectionFactory.getType());
    }

    /**
     * Returns the directory name that holds hl7 version specific ACK xsds'
     * 
     * @param version
     * @return
     */
    private String getVersionDir(String version) {
        if (version.equals(HL7v21)) {
            return HL7v21_DIR;
        } else if (version.equals(HL7v22)) {
            return HL7v22_DIR;
        } else if (version.equals(HL7v23)) {
            return HL7v23_DIR;
        } else if (version.equals(HL7v231)) {
            return HL7v231_DIR;
        } else if (version.equals(HL7v24)) {
            return HL7v24_DIR;
        } else if (version.equals(HL7v25)) {
            return HL7v25_DIR;
        } else if (version.equals(HL7v251)) {
            return HL7v251_DIR;
        } else if (version.equals(HL7v26)) {
            return HL7v26_DIR;
        }
        return null;
    }

    private String resendRecourseAction(String resendReason,
                                        MessageExchange msgExchange,
                                        String hl7PayLoad,
                                        HL7Connector hl7Connector,
                                        Endpoint endpoint,
                                        HL7Message hl7message,
                                        HL7ProtocolProperties hl7ProtoclProperties) throws Exception {
        try {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, resendReason);
            }
            return sendAndReceiveHL7Message(msgExchange, hl7PayLoad, hl7Connector, endpoint, hl7message, hl7ProtoclProperties);
        } catch (Exception ex) {
            mLog.log(Level.SEVERE, I18n.msg("E0212: Sending HL7 message to HL7 External System Failed"));
            throw new ApplicationException(I18n.msg("E0212: Sending HL7 message to HL7 External System Failed"));
        }
    }

    private void resetRecourseAction(String resetReason, HL7Connector hl7Connector) throws Exception {
        try {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, resetReason);
            }
            // close the existing connection
            if (hl7Connector != null) {
                hl7Connector.discardConnection();
            }
            throw new Exception(resetReason);
        } catch (Exception ex) {
            mLog.log(Level.SEVERE, resetReason);
            throw new Exception(resetReason);
        }
    }

    private void suspendRecourseAction(String suspendReason, HL7Connector hl7Connector, Endpoint endpoint)
            throws ApplicationException {
        try {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, suspendReason);
            }
            // close the existing connection
            if (hl7Connector != null) {
                hl7Connector.discardConnection();
            }
        } catch (Exception ex) {
            mLog.log(Level.SEVERE, I18n.msg("E0216: Disconnecting the connection to HL7 External System Failed"));
            throw new ApplicationException(
                    I18n.msg("E0216: Disconnecting the connection to HL7 External System Failed"));
        }
        try {
            endpoint.getServiceUnit().suspend(endpoint);
            // emit warning logs and send alerts
            String msg = I18n.msg("W0119: About to suspend endpoint: serviceName=[{0}], endpointName=[{1}].",
                    String.valueOf(endpoint.getServiceName()), endpoint.getEndpointName());
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, msg);
            }
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, endpoint.getUniqueName(),
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0119");
        } catch (JBIException jbiException) {
            throw new ApplicationException(suspendReason, jbiException);
        }

        throw new ApplicationException(suspendReason);
    }

    /**
     * Get a unique provisioning endpoint identifer for use with add/remove endpoints and data
     * retrieval
     * 
     * @param serviceName qualified service name the endpoint belongs to
     * @param portName the local name of the port
     * @return unique endpoint identifier string.
     */
    private String createProvisioningEndpointIdentifier(QName serviceName, String portName) {
        return serviceName.getNamespaceURI() + "," + serviceName.getLocalPart() + "," + portName + ","
                + PROVISIONING_ID;
    }

    /**
     * Get a unique consuming endpoint identifer for use with add/remove endpoints and data
     * retrieval
     * 
     * @param serviceName qualified service name the endpoint belongs to
     * @param portName the local name of the port
     * @return unique endpoint identifier string.
     */
    private String createConsumingEndpointIdentifier(QName serviceName, String portName) {
        return serviceName.getNamespaceURI() + "," + serviceName.getLocalPart() + "," + portName + "," + CONSUMING_ID;
    }

    /**
     * Journal message into the database
     * 
     * @param applicationID endpoint Unique Name.
     * @param MSH.10th field value from the message
     * @param mode specify server mode ot client mode
     * @param hl7message Received HL7 message
     * @param hl7ACKMessage generated ACK message
     * @param hl7NAKMessage generated NAK message
     * @statue specify DONE or ERROR.
     */
    private void journalMessageInDB(String applicationID, String messageControlID, String mode, String hl7Message, String hl7ACKMessage,
                                String hl7NAKMessage, String status) throws Exception { 
        
        DBConnection dbCon = null; 
        try { 
                dbCon = getDBConnection(); 
                JournalHL7MessageLogDBO journalHL7LogDBO =  mDBObjectFactory.createJournalHL7MessageLogDBO();

                journalHL7LogDBO.setApplicationId(applicationID);               
                journalHL7LogDBO.setMessageControlId(messageControlID);
                journalHL7LogDBO.setMode(mode);
                journalHL7LogDBO.setRequestHL7Message(hl7Message);
                journalHL7LogDBO.setResponseACKMessage(hl7ACKMessage);
                journalHL7LogDBO.setResponseNAKMessage(hl7NAKMessage);
                journalHL7LogDBO.setStatus(status);
                dbCon.insert(journalHL7LogDBO);
                dbCon.getUnderlyingConnection().commit();
        } catch(Exception ex) {
            dbCon.getUnderlyingConnection().rollback();
            throw ex; 
        } finally {
            if (dbCon != null) {
                    dbCon.close(); 
            } 
        } 
    }
    private HL7Connector getHL7ConnectorFromPool(ProtocolInfo protocolInfo, Endpoint endpoint) throws Exception {
        HL7Connector hl7connector = null;
        String hostName = protocolInfo.get(HL7Address.ATTR_HL7_SVR_LOCATION);
        int port = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
        String endpointName = endpoint.getEndpointName() + endpoint.getUniqueName();
        // constructing key for maintaing different pools for each endpoint.
        ConnectionInfo connInfo = new ConnectionInfo();
        //ConnectionInfo connInfo = endpoint.getConnectionInfo();
        connInfo.setHost(hostName);
        connInfo.setPort(port);
        connInfo.setEndpointName(endpointName);
        Connection hl7Connection = HL7BCConnectionManager.getPoolledConnection(connInfo);
        if(hl7Connection != null){
            hl7connector = (TCPIpHL7Connector)hl7Connection.getIoHandler();
            hl7connector.setHL7Connection(hl7Connection);
            mHL7ConnectorTakenFromPool = true;
         }
        return hl7connector;
    }

    private String ifNotNullTrim(Object str) {
        String value = (String) str;
        if (value != null && !(value = value.trim()).equals("")) {
            return value;
        }

        return null;
    }

}// end of class
