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

import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.internationalization.Messages;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.grizzly.http.server.NetworkListener;
import org.glassfish.grizzly.ssl.SSLContextConfigurator;
import org.glassfish.grizzly.ssl.SSLEngineConfigurator;

/**
 * Control the embedded HTTP server
 *
 */
public class EmbeddedServerController {

    private static final Messages mMessages =
            Messages.getMessages(EmbeddedServerController.class);
    private static final Logger mLogger =
            Messages.getLogger(EmbeddedServerController.class);
    private final Map<Integer, HttpServer> mStartedServers = new HashMap<Integer, HttpServer>();

    /**
     * Creates a new instance of EmbeddedServerController
     */
    public EmbeddedServerController(final HttpSoapBindingLifeCycle lifecycle) {
    //    this.initialize(lifecycle);
    }
    
    private void initialize(HttpSoapBindingLifeCycle lifecycle) {
        HttpServer server = new HttpServer();
        server.addListener(GrizzlyEmbeddedWebContainer.createNetworkListener(
                null, 8080, "http"));
        server.getServerConfiguration().addHttpHandler(new EndpointsManagerHttpHandler(lifecycle));
        try {
            server.start();
            mStartedServers.put(8080, server);
        } catch (IOException ex) {
            Logger.getLogger(EmbeddedServerController.class.getName()).log(Level.SEVERE, "Fail to start HTTP container", ex);
        }
    }

    /**
     * Start an embedded server if one isn't running yet with the given port
     */
    public void startServer(int port, String protocol, boolean clientAuthEnabled) throws Throwable {
        if (!mStartedServers.containsKey(port)) {

            try {
                // Setting 'address' to NULL will cause Tomcat to pass a
                // NULL InetAddress argument to the java.net.ServerSocket
                // constructor, meaning that the server socket will accept
                // connections on any/all local addresses.
                String address = null;
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Attempting to create Grizzly connector at port {0}", port);
                }

                // TODO: use new "facade" usage pattern to instantiate / configure grizzly, i.e. 
                //>         SelectorThread selectorThread = new SelectorThread();
                //>         selectorThread.setPort(port);
                //>         selectorThread.setAdapter(new PlugNowAdapter());
                //>         selectorThread.initEndpoint();
                //>         selectorThread.startEndpoint();                

                NetworkListener listener = GrizzlyEmbeddedWebContainer.createNetworkListener(address, port, protocol);

                // Set protocol security setting to same as connector
                boolean secure = listener.isSecure();

                if (secure) {
                    // Grizzly ssl configuration
                    SSLContextConfigurator sslContext = new SSLContextConfigurator();

                    // HTTP BC only support JKS keystore format right now.
                    /*
                     protocolHandler.setAttribute("keystoreType", "JKS");
                     protocolHandler.setAttribute("truststoreType", "JKS");
                     */

                    // set up security context
                    if (System.getProperty("javax.net.ssl.keyStore") == null) {
                        // fail the start right here
                        throw new Exception(mMessages.getString("HTTPBC-E00673.keystore_config_not_available", port));
                    }
                    sslContext.setKeyStoreFile(System.getProperty("javax.net.ssl.keyStore"));

                    if (System.getProperty("javax.net.ssl.trustStore") == null) {
                        // fail the start right here
                        throw new Exception(mMessages.getString("HTTPBC-E00674.trustore_config_not_available", port));
                    }
                    sslContext.setTrustStoreFile(System.getProperty("javax.net.ssl.trustStore"));

                    if (System.getProperty("javax.net.ssl.keyStorePassword") == null) {
                        // fail the start right here
                        throw new Exception(mMessages.getString("HTTPBC-E00675.keypass_config_not_available", port));
                    }
                    sslContext.setKeyStorePass(System.getProperty("javax.net.ssl.keyStorePassword"));

                    // enabling mutual authentication based on the flag
                    SSLEngineConfigurator sslConfig = new SSLEngineConfigurator(sslContext, false, clientAuthEnabled, clientAuthEnabled);
                    listener.setSSLEngineConfig(sslConfig);
                }

                listener.setChunkingEnabled(false);

                HttpServer server = new HttpServer();
                server.addListener(listener);
                
                HttpHandler handler = new JAXWSGrizzlyRequestProcessor(listener);
                server.getServerConfiguration().addHttpHandler(handler);

                /*
                com.sun.enterprise.web.connector.grizzly.AsyncFilter filter =
                        new com.sun.jbi.httpsoapbc.embedded.JBIGrizzlyAsyncFilter();
                asyncHandler.addAsyncFilter(filter);
                */
                
                // use request throttling enabled pipeline
                /*
                selThread.setClassLoader(this.getClass().getClassLoader());
                selThread.setPipelineClassName(com.sun.jbi.httpsoapbc.embedded.LinkedListThrottlePipeline.class.getName());
                */
                
                server.start();
                mStartedServers.put(port, server);

                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "HTTP connector created at port {0}", port);
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
            HttpServer server = (HttpServer) mStartedServers.get(port);
            String currentProtocol = server.getListener(GrizzlyEmbeddedWebContainer.LISTENER_NAME).getScheme();

            if (!currentProtocol.equalsIgnoreCase(protocol)) {
                throw new Exception(mMessages.getString("HTTPBC-E00672.Same_port_no_matching_protocol", new Object[]{new Integer(port), currentProtocol, protocol}));
            }
        }
    }

    /**
     * Stop all embedded servers
     */
    public void stopAll() {
        Collection<HttpServer> servers = mStartedServers.values();
        for (HttpServer server : servers) {
            try {
                server.shutdownNow();

                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Stopped Grizzly connector for port {0}", 
                            server.getListener(GrizzlyEmbeddedWebContainer.LISTENER_NAME).getPort());
                }
            } catch (Exception ex) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00655.Exception_during_grizzly_stop",
                            new Object[]{server.getListener(GrizzlyEmbeddedWebContainer.LISTENER_NAME).getPort(), 
                                ex.getLocalizedMessage()});
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
        HttpServer server = (HttpServer) mStartedServers.remove(port);
        if (server != null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Stopping embedded HTTP server at port {0}", port);
            }
            
            try {
                server.shutdownNow();
            } catch (Exception ex) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00655.Exception_during_grizzly_stop",
                            new Object[]{port, ex.getLocalizedMessage()});
                    mLogger.log(Level.WARNING, text, ex);
                }
            }
        }
    }
}