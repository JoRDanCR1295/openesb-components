package com.sun.jbi.restbc.jbiadapter.inbound;

import java.io.File;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;
import javax.xml.transform.stream.StreamSource;

import com.sun.grizzly.http.servlet.ServletAdapter;
import com.sun.jbi.restbc.jbiadapter.RestComponent;
import com.sun.jbi.restbc.jbiadapter.ServiceUnitConfig;

/**
 * RestServiceUnit.java
 *
 * @author Edward Chou
 */
public class RestServiceUnit implements ServiceUnitConfig {

    public final static String REST_CONFIG_FILE_NAME = "rest-config.xml";
    
    private final static Logger logger = Logger.getLogger(RestServiceUnit.class.getName());
    
    private RestComponent component;
    private ComponentContext context;
    private String serviceUnitName;
    private String serviceUnitRootPath;
    
    public RestServiceUnit(RestComponent component, ComponentContext context, String serviceUnitName, String serviceUnitRootPath) throws Exception {
        this.component = component;
        this.context = context;
        this.serviceUnitName = serviceUnitName;
        this.serviceUnitRootPath = serviceUnitRootPath;
        
        init();
    }
    
    private void init() throws Exception {
        File rootDir = new File(serviceUnitRootPath);
        File configXML = new File(rootDir, REST_CONFIG_FILE_NAME);
        
    }
    
    
    public void start() throws Exception {
        /*
        for (InboundConfig inboundConfig : restConfig.getInboundConfig()) {
            String listenerName = inboundConfig.getListenerName();
            String contextRoot = inboundConfig.getContextRoot();
            
            InboundHttpListener listener = component.getInboundHttpListener(listenerName);
            if (listener == null) {
                throw new Exception("cannot find listener with name: " + listenerName);
            }
            
            // register contextRoot
            ServletAdapter servletAdapter = null;
            listener.registerContext(contextRoot, servletAdapter);
        }
        */
    }

    public void stop() throws Exception {
        /*
        for (InboundConfig inboundConfig : restConfig.getInboundConfig()) {
            String listenerName = inboundConfig.getListenerName();
            String contextRoot = inboundConfig.getContextRoot();
            
            InboundHttpListener listener = component.getInboundHttpListener(listenerName);
            if (listener == null) {
                throw new Exception("cannot find listener with name: " + listenerName);
            }
            
            // unregister contextRoot
            listener.unregisterContext(contextRoot);
        }
        */
    }

    public void shutdown() throws Exception {
        
    }


    public String getServiceUnitName() {
        return serviceUnitName;
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
