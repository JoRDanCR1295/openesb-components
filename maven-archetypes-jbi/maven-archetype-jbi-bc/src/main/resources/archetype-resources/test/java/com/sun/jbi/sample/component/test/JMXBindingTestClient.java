/*
 * JMXBindingTestClient.java
 */

package com.sun.jbi.sample.component.test;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.ReflectionException;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

/**
 * This class extends the JBIComponentTestClient and implements the invokeService
 * method to test the proxy service consumer implementation by a binding component
 * that can communicate with the external service consumers using JMX interface.
 * @author chikkala
 */
public class JMXBindingTestClient extends JBIComponentTestClient {
    
    public static final String JMX_DOMAIN = "com.sun.jbi.sample.component.jmx.binding";
    public static final String JMX_ENDPOINT_ADDRESS_KEY = "jmx-endpoint-address";
    
    public static final String INBOUND_EP_ADDRESS_URL_PROP = "jmx.binding.inbound.endpoint.address.url";
    public static final String INBOUND_EP_ADDRESS_USERNAME_PROP = "jmx.binding.inbound.endpoint.address.username";
    public static final String INBOUND_EP_ADDRESS_PASSWORD_PROP = "jmx.binding.inbound.endpoint.address.password";
    public static final String INBOUND_EP_ADDRESS_VALUE_PROP = "jmx.binding.inbound.endpoint.address.value";
    
    public static final String INBOUND_OPERATION_PROP = "jmx.binding.inbound.operation";
    
    /**
     * Creates a new instance of JMXBindingTestClient
     */
    public JMXBindingTestClient() {
        super();
    }
    /**
     * creates jmx connection to send the input message to the binding component.
     */
    public static MBeanServerConnection getJMXConnection(Properties testProps)
    throws MalformedURLException, IOException {
        
        String jmxUrl = testProps.getProperty(INBOUND_EP_ADDRESS_URL_PROP);
        String username = testProps.getProperty(INBOUND_EP_ADDRESS_USERNAME_PROP);
        String password = testProps.getProperty(INBOUND_EP_ADDRESS_PASSWORD_PROP);
        
        Map env = new HashMap();
        if ( username != null ) {
            String [] credentials = new String [] {username, password};
            env.put("jmx.remote.credentials", credentials);
        }
        JMXServiceURL  serviceURL = new JMXServiceURL(jmxUrl);
        JMXConnector jmxConnector = JMXConnectorFactory.connect(serviceURL, env);
        MBeanServerConnection mbeanServerConnection =  jmxConnector.getMBeanServerConnection();
        return mbeanServerConnection;
    }
    /**
     * constructs the jmx mbean objectname
     */
    public static ObjectName createJMXEndpointMBeanObjectName(String endpointAddressMBean) throws MalformedObjectNameException {
        String objectName = JMX_DOMAIN + ":" + JMX_ENDPOINT_ADDRESS_KEY + "=" + endpointAddressMBean;
        return new ObjectName(objectName);
    }
    /**
     * invokes a jmx mbean to send message.
     */
    public static StringBuffer invokeSendMessage(MBeanServerConnection jmxConn, String endpointAddressValue,
        String operation, StringBuffer inputDoc)
        throws MalformedObjectNameException, InstanceNotFoundException,
        MBeanException, ReflectionException, IOException {
        StringBuffer outDoc = null;
        
        ObjectName jmxEndpointObjectName = createJMXEndpointMBeanObjectName(endpointAddressValue);
        
        Object result = null;
        String mbeanOperation = "sendMessage";
        
        Object[] params = new Object[2];
        params[0] = operation;
        params[1] = inputDoc;
        
        String[] signature = new String[2];
        signature[0] = "java.lang.String";
        signature[1] = "java.lang.StringBuffer";
        
        result = jmxConn.invoke(jmxEndpointObjectName, mbeanOperation, params, signature);
        if ( result != null ) {
            outDoc = (StringBuffer)result;
        }
        
        return outDoc;
    }
    /**
     * invokes the service via jmx interface.
     */
    public StringBuffer invokeService(StringBuffer inputDoc, Properties testProps) throws Exception {
        
        String endpointAddressValue = testProps.getProperty(INBOUND_EP_ADDRESS_VALUE_PROP);
        String operation = testProps.getProperty(INBOUND_OPERATION_PROP);
        
        MBeanServerConnection jmxConn = getJMXConnection(testProps);
        StringBuffer outputDoc = null;
        try {
            outputDoc = invokeSendMessage(jmxConn, endpointAddressValue,  operation, inputDoc);
        } catch (MBeanException ex) {
            Exception targetEx = ex.getTargetException();
            if ( targetEx != null ) {
                throw targetEx;
            } else {
                throw ex;
            }
        }
        return outputDoc;
    }
    /** no expected output comparison implemented */
    public void compareWithExpectedOutput(StringBuffer inputDoc,
        StringBuffer expectedDoc,
        Properties testProps) throws Exception {
        //  throw new UnsupportedOperationException("Not supported yet.");
    }
    
}
