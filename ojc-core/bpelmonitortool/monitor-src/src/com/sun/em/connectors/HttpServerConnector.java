/*
 *
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.connectors;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

/**
 *
 * @author ylee
 */
public class HttpServerConnector extends ServerConnector implements Serializable {

    public static final String PROTOCOL_CLASS = "com.sun.enterprise.admin.jmx.remote.protocol"; //$NON-NLS-1$
    public static final String HTTP_AUTH_PROPERTY_NAME = "com.sun.enterprise.as.http.auth"; //$NON-NLS-1$
    public static final String DEFAULT_HTTP_AUTH_SCHEME = "BASIC"; //$NON-NLS-1$
    public static final String ADMIN_USER_ENV_PROPERTY_NAME = "USER"; //$NON-NLS-1$
    public static final String ADMIN_PASSWORD_ENV_PROPERTY_NAME = "PASSWORD"; //$NON-NLS-1$
    public static final String HTTP_PROTOCOL = "http"; //$NON-NLS-1$    
    
    private static Logger logger = Logger.getLogger(HttpServerConnector.class.getName());

    
    public HttpServerConnector() {
    }

    public HttpServerConnector(String type, String serverName, String hostName, String port, String instancePort, String userName, String userPassword) {
        super(type, serverName, hostName, port, instancePort, userName, userPassword);
    }    
    
    public MBeanServerConnection getConnection() {
        if ( connection != null ) {
            Map<String,Object> environment = new HashMap<String,Object>();
            environment.put(JMXConnectorFactory.PROTOCOL_PROVIDER_PACKAGES, PROTOCOL_CLASS);
            environment.put(HTTP_AUTH_PROPERTY_NAME, DEFAULT_HTTP_AUTH_SCHEME);
            environment.put(ADMIN_USER_ENV_PROPERTY_NAME, this.getUserName());
            environment.put(ADMIN_PASSWORD_ENV_PROPERTY_NAME, this.getUserPassword());
            
            try {
                int portValue = getAdminPortValue();
                // check for valid port
                if ( portValue>0 ) {
                    JMXServiceURL serviceURL = new JMXServiceURL(getProtocol(), this.getHostName(), portValue);
                    JMXConnector connector = JMXConnectorFactory.connect(serviceURL, environment);
                    this.connection = connector.getMBeanServerConnection();
                } else {
                    logger.info("Invalid port: "+portValue+". Connection failed.");
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return connection;
    }
    
    protected String getProtocol() {
        return HTTP_PROTOCOL;
    }
    
    
    
}
