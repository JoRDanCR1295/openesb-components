package com.sun.jbi.restbc.jbiadapter.inbound;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;

import com.sun.grizzly.http.servlet.ServletAdapter;
import com.sun.jbi.restbc.jbiadapter.RestComponent;
import com.sun.jbi.restbc.jbiadapter.ServiceUnitConfig;
import com.sun.jersey.spi.container.servlet.ServletContainer;

/**
 * JaxrsPojoServiceUnit.java
 *
 * @author Edward Chou
 */
public class JaxrsPojoServiceUnit implements ServiceUnitConfig {

    public final static String LISTENER_NAME_PROP = "listener-name";
    public final static String DEFAULT_LISTENER_NAME = "default-listener";
    public final static String CONTEXT_ROOT_PROP = "context-root";
    
    private final static Logger logger = Logger.getLogger(JaxrsPojoServiceUnit.class.getName());
    
    private RestComponent component;
    private ComponentContext context;
    private String rootClasspath;
    private File configFile;
    private Properties configProps = new Properties();
    private String listenerName;
    private String contextRoot;
    
    
    public JaxrsPojoServiceUnit(RestComponent component, ComponentContext context, String rootClasspath, File configFile) throws Exception {
        this.component = component;
        this.context = context;
        this.rootClasspath = rootClasspath;
        this.configFile = configFile;
        init();
    }
    
    private void init() throws Exception {
        InputStream is = null;
        try {
            is = new FileInputStream(configFile);
            configProps.load(is);
        } finally {
            try {
                is.close();
            } catch (IOException ioe) {
                // ignore
            }
        }
        
        listenerName = configProps.getProperty(LISTENER_NAME_PROP, DEFAULT_LISTENER_NAME);
        
        contextRoot = configProps.getProperty(CONTEXT_ROOT_PROP);
        if (contextRoot == null || contextRoot.length() == 0) {
            throw new Exception ("context-root cannot be empty");
        }
    }
    
    
    public void start() throws Exception {
        logger.log(Level.INFO, "service unit start()");
        
        InboundHttpListener listener = component.getInboundHttpListener(listenerName);
        if (listener == null) {
            throw new Exception("cannot find listener with name: " + listenerName);
        }

        // register contextRoot
        ServletAdapter servletAdapter = createServletAdapter();
        listener.registerContext(contextRoot, servletAdapter);
    }

    public void stop() throws Exception {
        logger.log(Level.INFO, "service unit stop()");
        
        InboundHttpListener listener = component.getInboundHttpListener(listenerName);
        if (listener == null) {
            throw new Exception("cannot find listener with name: " + listenerName);
        }

        // unregister contextRoot
        listener.unregisterContext(contextRoot);
    }

    public void shutdown() throws Exception {
        
    }
    
    private ServletAdapter createServletAdapter() throws Exception {
        ServletAdapter adapter = new ServletAdapter();
        final Map<String, String> initParams = new HashMap<String, String>();
        
        initParams.put("com.sun.jersey.config.property.resourceConfigClass", "com.sun.jbi.restbc.jbiadapter.inbound.ServiceUnitResourceConfig");
        initParams.put("com.sun.jersey.config.property.classpath", rootClasspath);
        initParams.put(ServiceUnitResourceConfig.SERVICE_UNIT_ROOT, rootClasspath);
        
        for (Map.Entry<String, String> e : initParams.entrySet()) {
            adapter.addInitParameter(e.getKey(), e.getValue());
        }
        
        adapter.setServletInstance(ServletContainer.class.newInstance());
        adapter.setContextPath(contextRoot);
        
        return adapter;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.restbc.jbiadapter.ServiceUnitConfig#getEndpointName()
     */
    public String getEndpointName() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.restbc.jbiadapter.ServiceUnitConfig#getServiceName()
     */
    public QName getServiceName() {
        // TODO Auto-generated method stub
        return null;
    }
    
}
