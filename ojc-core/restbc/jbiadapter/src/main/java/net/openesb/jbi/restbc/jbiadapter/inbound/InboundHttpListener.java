package net.openesb.jbi.restbc.jbiadapter.inbound;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import javax.ws.rs.core.Application;

import javax.ws.rs.core.UriBuilder;

import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.grizzly.http.server.NetworkListener;
import org.glassfish.grizzly.http.server.ServerConfiguration;
import org.glassfish.grizzly.ssl.SSLEngineConfigurator;
import org.glassfish.grizzly.threadpool.ThreadPoolConfig;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ContainerFactory;
import org.glassfish.jersey.server.ResourceConfig;

/**
 * InboundHttpListener.java
 *
 * @author Edward Chou
 */
public class InboundHttpListener {

    public static final String DEFAULT_LISTENER = "default-listener"; // NOI18N
    public static final String DEFAULT_LISTENER_SSL = "default-listener-ssl"; // NOI18N

    private final String listenerName;
    private final int port;
    private final HttpServer webServer;

    private Map<String, HttpHandler> contextMap = new HashMap<String, HttpHandler>();

    private static URI getBaseURI(int port) {
        return UriBuilder.fromUri("http://localhost/")
                .host(NetworkListener.DEFAULT_NETWORK_HOST)
                .port(port)
                .build();
    }

    public InboundHttpListener(String listenerName, int port, int numThreads, SSLEngineConfigurator sslEngineConfigurator) {
        this.listenerName = listenerName;
        this.port = port;

        ThreadPoolConfig config = ThreadPoolConfig.defaultConfig().
                setCorePoolSize(numThreads).
                setMaxPoolSize(numThreads);
        webServer = GrizzlyHttpServerFactory.createHttpServer(getBaseURI(port), false);

        // assign the thread pool
        NetworkListener listener = webServer.getListeners().iterator().next();
        listener.getTransport().setWorkerThreadPoolConfig(config);

        // Assign security if needed
        if (sslEngineConfigurator != null) {
            listener.setSecure(true);
            listener.setSSLEngineConfig(sslEngineConfigurator);
        }
    }

    public void start() throws Exception {
        webServer.start();
    }

    public void stop() throws Exception {
        webServer.shutdownNow();
    }

    public synchronized void registerContext(String context, Application application) throws Exception {
        if (contextMap.containsKey(context)) {
            throw new Exception("context already exists: " + context);
        }
        final ServerConfiguration config = webServer.getServerConfiguration();
        HttpHandler handler = ContainerFactory.createContainer(HttpHandler.class, application);
        config.addHttpHandler(handler, context);

        contextMap.put(context, handler);
    }

    public synchronized void unregisterContext(String context) throws Exception {
        HttpHandler removedValue = contextMap.remove(context);
        if (removedValue == null) {
            throw new Exception("context did not exist: " + context);
        }

        webServer.getServerConfiguration().removeHttpHandler(removedValue);
    }
    
    /**
     * @return the listenerName
     */
    public String getListenerName() {
        return listenerName;
    }

    /**
     * @return the port
     */
    public int getPort() {
        return port;
    }

}
