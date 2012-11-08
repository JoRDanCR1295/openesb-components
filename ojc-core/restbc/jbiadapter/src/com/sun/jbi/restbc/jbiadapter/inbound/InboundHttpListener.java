package com.sun.jbi.restbc.jbiadapter.inbound;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.sun.grizzly.SSLConfig;
import com.sun.grizzly.http.embed.GrizzlyWebServer;
import com.sun.grizzly.http.servlet.ServletAdapter;
import com.sun.grizzly.ssl.SSLSelectorThread;

/**
 * InboundHttpListener.java
 *
 * @author Edward Chou
 */
public class InboundHttpListener {

    public static final String DEFAULT_LISTENER = "default-listener"; // NOI18N
    public static final String DEFAULT_LISTENER_SSL = "default-listener-ssl"; // NOI18N
    
    private String listenerName;
    private int port;
    private GrizzlyWebServer webServer;
    
    private Map<String, ServletAdapter> contextMap = new HashMap<String, ServletAdapter> ();
    
    public InboundHttpListener(String listenerName, int port, int numThreads, SSLConfig sslConfig) {
        this.listenerName = listenerName;
        this.port = port;
        this.webServer = new GrizzlyWebServer(port, ".", (sslConfig == null) ? false : true);
        this.webServer.setCoreThreads(numThreads);
        this.webServer.setMaxThreads(numThreads);
        if (sslConfig != null) {
            webServer.setSSLConfig(sslConfig);
            ((SSLSelectorThread) webServer.getSelectorThread()).setNeedClientAuth(true);
        }
    }
    
    public void start() throws Exception {
        webServer.start();
    }
    
    public void stop() throws Exception {
        webServer.stop();
    }
        
    public synchronized void registerContext(String context, ServletAdapter adapter) throws Exception {
        if (contextMap.containsKey(context)) {
            throw new Exception("context already exists: " + context);
        }
        
        webServer.addGrizzlyAdapter(adapter, new String[] { context });
        
        contextMap.put(context, adapter);
    }
    
    public synchronized void unregisterContext(String context) throws Exception {
        ServletAdapter removedValue = contextMap.remove(context);
        if (removedValue == null) {
            throw new Exception("context did not exist: " + context);
        }
        
        webServer.removeGrizzlyAdapter(removedValue);
    }
        
    public Map<String, ServletAdapter> getContextMap() {
        return Collections.unmodifiableMap(contextMap);
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
