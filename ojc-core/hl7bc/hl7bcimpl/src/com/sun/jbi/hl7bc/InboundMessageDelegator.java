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
 * @(#)InboundMessageDelegator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.hl7bc.extservice.server.HL7Callback;
import com.sun.jbi.hl7bc.extservice.server.HL7Listener;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.util.UniqueMsgIdGenerator;
import com.sun.jbi.hl7bc.I18n;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import static com.sun.jbi.hl7bc.packaging.WSDLConfigurations.*;

/**
 * This class delegates the hl7 messages to one of the avilable message processor in message
 * processor pool
 * 
 * @author S. Nageswara Rao
 */
public class InboundMessageDelegator implements HL7Listener, HL7Constants {

    private static final Logger mLog = Logger.getLogger(InboundMessageDelegator.class.getName());

    /** handler threads wait time before forced shutdown */
    private final static long HANDLERS_SHUTDOWN_WAIT_TIME = 30; // seconds

    private static Map<String, InboundReplyContext> mInboundExchanges = Collections.synchronizedMap(new HashMap<String, InboundReplyContext>());

    // Map for HL7 client sockets Information
    private Map<String, HL7Callback> mSessionMap = Collections.synchronizedMap(new HashMap<String, HL7Callback>());

    // The component context associated with the hl7 binding component
    private ComponentContext mCompContext;

    // The delivery channel through which messages are sent or received
    private MessagingChannel mMessagingChannel;

    // The runtime configuration bean
    private RuntimeConfiguration mRuntimeConfig;

    private UniqueMsgIdGenerator mMsgGenerator;

    private Endpoint mEndpoint;

    private QName mOperName;

    // HL7 Encoder used for ACKs encoding
    private Encoder mEncoder;

    /** handler thread executor service */
    private ExecutorService mHandlerThreadPool;

    private AtomicBoolean isSuspended;

    public InboundMessageDelegator() {
        isSuspended = new AtomicBoolean(false);
    }

    protected void setComponentContext(ComponentContext compContext) {
        mCompContext = compContext;
    }

    protected void setMessagingChannel(MessagingChannel messagingChannel) {
        mMessagingChannel = messagingChannel;
    }

    protected void setRuntimeConfiguration(RuntimeConfiguration runtimeConfig) {
        mRuntimeConfig = runtimeConfig;
    }

    protected void setEndPoint(Endpoint endpoint) {
        mEndpoint = endpoint;
    }

    protected void setOperationName(QName operName) {
        // mOperName = operName;
        mOperName = new QName(getPortType(mEndpoint).getQName().getNamespaceURI(), operName.getLocalPart());
    }

    protected void setUniqueMsgIdGenerator(UniqueMsgIdGenerator uidGenerator) {
        mMsgGenerator = uidGenerator;
    }

    protected void initialize() throws Exception {
        mHandlerThreadPool = Executors.newCachedThreadPool();
        // create the QName for ACK
        final QName ack = new QName(V2xTNS, ACK);
        // get the xsd file location
        // final String xsdFileLoc = getXsdFileLocation(V2xTNS, ack, mEndpoint.getXsdsList());
        String compInstallRoot = mCompContext.getInstallRoot().replace("\\", "/");
        // to handle spaces in the file path
        compInstallRoot = compInstallRoot.replace(" ", "%20");
        String hl7VerDir = getVersionDir(mEndpoint.getHL7ProtocolProperties().getVersionID());
        StringBuilder sb = new StringBuilder(URL_SCHEME).append(compInstallRoot).append(SCHEMAS_LOCATION).append(
                hl7VerDir);
        sb.append("/").append(ACKXSD_FILENAME);
        final URL url = new URL(sb.toString());
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
        // Share the same HL7 encoder across all the inbound message processors
        mEncoder = encoderFactory.newEncoder(encoderFactory.makeType(HL7_ENCODINGSTYLE), metaRef);
    }

    public void onMessage(String hl7Msg, HL7Callback hl7Callback) throws Exception {
        if (!isSuspended.get()) {
            delegateMessageProcessing(hl7Msg, hl7Callback);
        } else {
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, I18n.msg("W0114: Received the message from client {0}, but failed to process as the endpoint with name {1} of service {2} is in suspended state",
                       hl7Callback.getClientInfo(), mEndpoint.getEndpointName(), mEndpoint.getServiceName() ));
            }
        }
    }

    protected static Map<String, InboundReplyContext> getInboundExchanges() {
        return mInboundExchanges;
    }

    /**
     * Delegate the hl7 message to message processors
     * 
     * @param hl7Msg received hl7 message
     * @param hl7Callback channel to the HL7 system
     * @throws Exception
     */
    private void delegateMessageProcessing(String hl7Msg, HL7Callback hl7Callback) throws Exception {
        InboundMessageProcessor inbMsgProcessor = new InboundMessageProcessor();
        inbMsgProcessor.setComponentContext(mCompContext);
        inbMsgProcessor.setMessagingChannel(mMessagingChannel);
        inbMsgProcessor.setRuntimeConfiguration(mRuntimeConfig);
        inbMsgProcessor.setEndpoint(mEndpoint);
        inbMsgProcessor.setOperationName(mOperName);
        inbMsgProcessor.setMessage(hl7Msg);
        inbMsgProcessor.setCallback(hl7Callback);
        inbMsgProcessor.setInboundMsgExchanges(mInboundExchanges);
        inbMsgProcessor.setHL7Encoder(mEncoder);
        inbMsgProcessor.setSessionMap(mSessionMap);
        inbMsgProcessor.setUniqueMsgIdGenerator(mMsgGenerator);
        inbMsgProcessor.initialize();
        mHandlerThreadPool.submit(inbMsgProcessor);
    }

    /**
     * Terminate and shuts down all the message handlers
     * 
     * @throws Exception
     */
    private void shutdownHandlers() throws Exception {
        boolean terminated = false;
        try {
			if(mEncoder != null){
                // call the encoder's dispose method for clean-up purpose
				mEncoder.dispose();
			}
            this.mHandlerThreadPool.shutdown();
            terminated = this.mHandlerThreadPool.awaitTermination(HANDLERS_SHUTDOWN_WAIT_TIME, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
            mLog.log(Level.SEVERE, I18n.msg("E0176: Failed shutting down the Inbound message handlers, Unexpected exception occured : {0}", ex.getMessage()) );
            String errMsg = I18n.msg("E0176: Failed shutting down the Inbound message handlers, Unexpected exception occured : {0}", ex.getMessage() );
            throw new Exception(errMsg);
        } finally {
            if (!terminated) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, I18n.msg("I0113: Handler threads not shutdown. Forcing shutdown"));
                }
                this.mHandlerThreadPool.shutdownNow();
                // call the encoder's dispose method for clean-up purpose
                mEncoder.dispose();
            }
        }
    }

    /**
     * Stops all the pooled message processors
     */
    public void stopMsgReceiving() throws Exception {
        shutdownHandlers();
    }

    public void suspendMsgProcessing() {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("Suspend InboundMessageDelegator from processing the message"));
        }
        isSuspended.set(true);
    }

    public void resumeMsgProcessing() {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("Resume InboundMessageDelegator to process the message"));
        }
        isSuspended.set(false);
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
}// end of class
