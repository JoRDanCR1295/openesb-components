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

package com.sun.jbi.swiftbc;

import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.xml.namespace.QName;

import com.sun.jbi.swiftbc.extservice.server.SwiftCallback;
import com.sun.jbi.swiftbc.extservice.server.SwiftListener;
import com.sun.jbi.internationalization.Messages;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;

/**
 * This class delegates the Swift messages to one of the avilable message processor in message
 * processor pool
 *
 * @author S. Nageswara Rao
 */
public class InboundMessageDelegator extends SAGConstants implements SwiftListener {
    
    private static final Messages mMessages = Messages.getMessages(InboundMessageDelegator.class);
    
    private static final Logger mLog = Messages.getLogger(InboundMessageDelegator.class);
    
    /** handler threads wait time before forced shutdown */
    private final static long HANDLERS_SHUTDOWN_WAIT_TIME = 30; // seconds
    
    private static Map<String, InboundReplyContext> mInboundExchanges = Collections.synchronizedMap(new HashMap<String, InboundReplyContext>());
    
    // Map for Swift client sockets Information
    private Map<String, SwiftCallback> mSessionMap = Collections.synchronizedMap(new HashMap<String, SwiftCallback>());
    
    // The component context associated with the Swift binding component
    private ComponentContext mCompContext;
    
    // The delivery channel through which messages are sent or received
    private DeliveryChannel mDeliveryChannel;
    
    // The runtime configuration bean
    private RuntimeConfiguration mRuntimeConfig;
    
    private Endpoint mEndpoint;
    
    private QName mOperName;
    
    // Swift Encoder used for encoding
    private Encoder mEncoder;
    
    /** handler thread executor service */
    private ExecutorService mHandlerThreadPool;
    
    public InboundMessageDelegator() {
    }
    
    protected void setComponentContext(ComponentContext compContext) {
        mCompContext = compContext;
    }
    
    protected void setDeliveryChannel(DeliveryChannel deliveryChannel) {
        mDeliveryChannel = deliveryChannel;
    }
    
    protected void setRuntimeConfiguration(RuntimeConfiguration runtimeConfig) {
        mRuntimeConfig = runtimeConfig;
    }
    
    protected void setEndPoint(Endpoint endpoint) {
        mEndpoint = endpoint;
    }
    
    protected void setOperationName(QName operName) {
        mOperName = operName;
    }
    
    protected void initialize() throws Exception {
        mHandlerThreadPool = Executors.newCachedThreadPool();
        // create the QName for ACK
                final QName qName = new QName("V2xTNS", "ACK");
        // get the xsd file location
        //final String xsdFileLoc = getXsdFileLocation("V2xTNS", qName, mEndpoint.getXsdsList());
        String compInstallRoot = mCompContext.getInstallRoot().replace("\\", "/");
        String SwiftVerDir = getVersionDir(mEndpoint.getSwiftProtocolProperties().getVersionID());
        StringBuilder sb = new StringBuilder(URL_SCHEME).append(compInstallRoot).append(
                SwiftVerDir);
        sb.append("/").append(XSD_FILENAME);
        final URL url = new URL(sb.toString());
        MetaRef metaRef = new MetaRef() {
            public String getPath() {
                return null;
            }
            
            public URL getURL() {
                return url;
            }
            
            public QName getRootElemName() {
                return qName;
            }
        };
        EncoderFactory encoderFactory = EncoderFactory.newInstance();
        // Share the same Swift encoder across all the inbound message processors
        mEncoder = encoderFactory.newEncoder(encoderFactory.makeType("SWIFT_ENCODINGSTYLE"), metaRef);
    }
    
    public void onMessage(String swiftMsg, SwiftCallback swiftCallback) throws Exception {
        delegateMessageProcessing(swiftMsg, swiftCallback);
    }
    
    protected static Map<String, InboundReplyContext> getInboundExchanges() {
        return mInboundExchanges;
    }
    
    /**
     * Delegate the Swift message to message processors
     *
     * @param swiftMsg received Swift message
     * @param swiftCallback channel to the Swift system
     * @throws Exception
     */
    private void delegateMessageProcessing(String swiftMsg, SwiftCallback swiftCallback) throws Exception {
        InboundMessageProcessor inbMsgProcessor = new InboundMessageProcessor();
        inbMsgProcessor.setComponentContext(mCompContext);
        inbMsgProcessor.setDeliveryChannel(mDeliveryChannel);
        inbMsgProcessor.setRuntimeConfiguration(mRuntimeConfig);
        inbMsgProcessor.setEndpoint(mEndpoint);
        inbMsgProcessor.setOperationName(mOperName);
        inbMsgProcessor.setMessage(swiftMsg);
        inbMsgProcessor.setCallback(swiftCallback);
        inbMsgProcessor.setInboundMsgExchanges(mInboundExchanges);
        inbMsgProcessor.setSwiftEncoder(mEncoder);
        inbMsgProcessor.setSessionMap(mSessionMap);
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
            this.mHandlerThreadPool.shutdown();
            terminated = this.mHandlerThreadPool.awaitTermination(HANDLERS_SHUTDOWN_WAIT_TIME, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
            mLog.log(Level.SEVERE, "InbMsgDelegator_EXCEPTION_SHUTDOWN_HANDLERS", new Object[] { ex.getMessage() });
            String errMsg = mMessages.getString("InbMsgDelegator_EXCEPTION_SHUTDOWN_HANDLERS",
                    new Object[] { ex.getMessage() });
            throw new Exception(errMsg);
        } finally {
            if (!terminated) {
                if (mLog.isLoggable(Level.INFO)) {
                    mLog.log(Level.INFO, "InbMsgDelegator_FORCE_SHUTDOWN_HANDLERS");
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
    
    /**
     * Returns the directory name that holds Swift version specific ACK xsds'
     *
     * @param version
     * @return
     */
    private String getVersionDir(String version) {
        
        return null;
    }
}// end of class
