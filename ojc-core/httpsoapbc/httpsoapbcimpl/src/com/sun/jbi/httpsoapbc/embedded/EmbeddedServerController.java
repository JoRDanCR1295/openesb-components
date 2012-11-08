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
 * @(#)EmbeddedServerController.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import com.sun.enterprise.web.connector.grizzly.SecureSelector;
import com.sun.jbi.internationalization.Messages;

import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.component.ComponentContext;

import org.apache.catalina.Connector;
import org.apache.catalina.net.ServerSocketFactory;
import org.apache.coyote.Adapter;
import org.apache.coyote.tomcat5.CoyoteServerSocketFactory;

import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;

/**
 * Control the embedded http server
 *
 */
public class EmbeddedServerController {

    private static final Messages mMessages =
        Messages.getMessages(EmbeddedServerController.class);
    private static final Logger mLogger =
        Messages.getLogger(EmbeddedServerController.class);
    
    private Map mStartedServers = new HashMap();
    private HttpSoapBindingLifeCycle mLifeCycle;
    
    /** Creates a new instance of EmbeddedServerController */
    public EmbeddedServerController(HttpSoapBindingLifeCycle httpBCLifeCycle) {
        this.mLifeCycle = httpBCLifeCycle;
    }
    
    /**
     * Start an embedded server if one isn't running yet with the given port
     */
    public void startServer(int port, String protocol, boolean clientAuthEnabled) throws Throwable {    
        if (!mStartedServers.containsKey(new Integer(port))) {

            try {
                // Setting 'address' to NULL will cause Tomcat to pass a
                // NULL InetAddress argument to the java.net.ServerSocket
                // constructor, meaning that the server socket will accept
                // connections on any/all local addresses.
                String address = null;
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Attempting to create Grizzly connector at port " + new Integer(port));
                }

                // TODO: use new "facade" usage pattern to instantiate / configure grizzly, i.e. 
                //>         SelectorThread selectorThread = new SelectorThread();
                //>         selectorThread.setPort(port);
                //>         selectorThread.setAdapter(new PlugNowAdapter());
                //>         selectorThread.initEndpoint();
                //>         selectorThread.startEndpoint();                
                
                Connector connector = new GrizzlyEmbeddedWebContainer().createConnector(address, port, protocol, mLifeCycle);
                
                // TODO: Should we make this setting configurable?
                //connector.setURIEncoding("UTF-8");
                
                BCCoyoteConnector peCon = (BCCoyoteConnector) connector;

                org.apache.coyote.Adapter adapter = new JAXWSGrizzlyRequestProcessor(peCon);                
                peCon.setAdapter(adapter);

                // Set protocol security setting to same as connector
                boolean secure = connector.getSecure();
                // we use asynchronous extensions and don't want to block
                boolean blocking = false; 
                        
                com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol protocolHandler = 
                    new com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol(secure, blocking, null);
                if (secure) {
                    // HTTP BC only support JKS keystore format right now.
                    protocolHandler.setAttribute("keystoreType","JKS");
                    protocolHandler.setAttribute("truststoreType","JKS");
                    if (System.getProperty("javax.net.ssl.keyStore") == null) {
                        // fail the start right here
                        throw new Exception(mMessages.getString("HTTPBC-E00673.keystore_config_not_available", port));
                    }
                    protocolHandler.setAttribute("keystore", System.getProperty("javax.net.ssl.keyStore"));
                    if (System.getProperty("javax.net.ssl.trustStore") == null) {
                        // fail the start right here
                        throw new Exception(mMessages.getString("HTTPBC-E00674.trustore_config_not_available", port));
                    }
                    protocolHandler.setAttribute("truststore", System.getProperty("javax.net.ssl.trustStore")); 
                    if (System.getProperty("javax.net.ssl.keyStorePassword") == null) {
                        // fail the start right here
                        throw new Exception(mMessages.getString("HTTPBC-E00675.keypass_config_not_available", port));
                    }
                    protocolHandler.setAttribute("keypass", System.getProperty("javax.net.ssl.keyStorePassword"));
                    
                    // enabling mutual authentication based on the flag
                    protocolHandler.setAttribute("clientauth", (clientAuthEnabled)? "true" : "false");
                }
                peCon.setProtocolHandler(protocolHandler);

                com.sun.enterprise.web.connector.grizzly.SelectorThread selThread = protocolHandler.selectorThread();
                com.sun.enterprise.web.connector.grizzly.async.DefaultAsyncHandler asyncHandler = 
                    new com.sun.enterprise.web.connector.grizzly.async.DefaultAsyncHandler();                
                selThread.setAsyncHandler(asyncHandler);
                selThread.setEnableAsyncExecution(true);
                com.sun.enterprise.web.connector.grizzly.AsyncFilter filter = 
                    new com.sun.jbi.httpsoapbc.embedded.JBIGrizzlyAsyncFilter();
                asyncHandler.addAsyncFilter(filter);

                // use request throttling enabled pipeline
                selThread.setClassLoader(this.getClass().getClassLoader());
                selThread.setPipelineClassName(com.sun.jbi.httpsoapbc.embedded.LinkedListThrottlePipeline.class.getName());                    
                
                // As of 9.1 the max read worker threads setting is optional - in 9.0 it was required when using asynchronous extensions
                //peCon.setMaxReadWorkerThreads(threadSize);
                //peCon.setMinProcessors(threadSize);
                //peCon.setMaxProcessors(threadSize);
                
                // Configure time-outs
                //peCon.setKeepAliveTimeoutInSeconds(30); // Beware: it seems setting this to no time-out Grizzly has an issue with fully closing connections
                //peCon.setConnectionTimeout(-1);
                //peCon.setMaxKeepAliveRequests(-1);
                //peCon.setProcessorWorkerThreadsTimeout(-1); // The first read timeout. Beware: it seems setting this to no time-out Grizzly has an issue with fully closing connections
                peCon.setChunkingDisabled(true);
                peCon.setProperty("reuseAddress",false);
                 
                peCon.start();
                mStartedServers.put(new Integer(port), connector);
                                
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "HTTP connector created at port " + new Integer(port));
                }
                
            } catch (RuntimeException ex) {
                mLogger.log(Level.SEVERE, "HTTPBC-E00656.Exception_during_grizzly_start", ex);
                throw ex;
            } catch (Throwable ex) {
                mLogger.log(Level.SEVERE, "HTTPBC-E00656.Exception_during_grizzly_start", ex);
                throw ex;
            }
        } else {
            // if the same port is referenced in the URL, but a different protocol is used,
            // we should throw an exception to fail the application deployment. Otherwise, 
            // we would either get a runtime exception when servicing the request, or the transport
            // layer processing would hang, as that seems to be the noted behavior
            Connector connector = (Connector) mStartedServers.get(new Integer(port));
            String currentProtocol = connector.getScheme();
            if (!currentProtocol.equalsIgnoreCase(protocol)) {
                throw new Exception(mMessages.getString("HTTPBC-E00672.Same_port_no_matching_protocol", new Object[] {new Integer(port), currentProtocol, protocol}));
            }
        }
    }

    /**
     * Stop all embedded servers
     */
    public void stopAll() {
        Iterator iter = mStartedServers.values().iterator();
        while (iter.hasNext()) {
            BCCoyoteConnector connector = (BCCoyoteConnector) iter.next();
            try {
                connector.stop();
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Stopped Grizzly connector for port " + new Integer(connector.getPort()));
                }
            } catch (Exception ex) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00655.Exception_during_grizzly_stop",
                            new Object[] { new Integer(connector.getPort()), ex.getLocalizedMessage() } );
                    mLogger.log(Level.WARNING, text, ex);   
                }
            }
        }
        mStartedServers.clear();
    }
    
    /**
     * Stop an embedded server if one is running on the given port
     */
    public void stopServer(int port) {
        Integer aPort = Integer.valueOf(port);
        BCCoyoteConnector connector = (BCCoyoteConnector) mStartedServers.remove(aPort);
        if (connector != null) {
            if (mLogger.isLoggable(Level.FINE)) {
               mLogger.log(Level.FINE, "Stopping embedded HTTP server at port " + aPort);
            }
            try {
                connector.stop();
            } catch (Exception ex) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00655.Exception_during_grizzly_stop",
                            new Object[] { aPort, ex.getLocalizedMessage() } );
                    mLogger.log(Level.WARNING, text, ex);
                }
            }
        }
    }    
}
