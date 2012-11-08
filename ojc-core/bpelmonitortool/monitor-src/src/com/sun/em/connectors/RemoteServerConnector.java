/*
 *
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.connectors;

import com.sun.em.common.GenericConstants;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

/**
 *
 * @author ylee
 * @author graj
 */
public class RemoteServerConnector extends ServerConnector implements Serializable {

    public RemoteServerConnector() {
    }
    
    public RemoteServerConnector(String type, String serverName, String hostName, String port, String instancePort, String userName, String userPassword) {
        super(type, serverName, hostName, port, instancePort, userName, userPassword);
    }
    

    public MBeanServerConnection getConnection() {
        MBeanServerConnection connection = null;
        boolean result = false;
        ObjectName mbeanName = null;

        // Try to obtain a JRMP connection
        try {
            connection = getMBeanServerConnection(getHostName(),
                    getAdminPortValue(), getUserName(), getUserPassword(), getJrmpConnectionType());
            //mbeanName = getJbiReferenceAdminUiMBeanObjectName();
            //result = connection.isRegistered(mbeanName);
         } catch (Exception e) {
            connection = null;
        }        

        if (connection == null) {
            // Try to obtain a HTTP connection
            try {
                connection = getMBeanServerConnection(getHostName(), getAdminPortValue(),
                        getUserName(), getUserPassword(), getHttpConnectionType());
                //mbeanName = getJbiReferenceAdminUiMBeanObjectName();
                //result = connection.isRegistered(mbeanName);
             } catch (Exception e) {
                connection = null;
            }
        }

        if (connection == null) {
            // Try to obtain a HTTPS (secure) connection
            try {
                connection = getMBeanServerConnection(getHostName(),
                        getAdminPortValue(), getUserName(), getUserPassword(), getHttpsConnectionType());
                //mbeanName = getJbiReferenceAdminUiMBeanObjectName();
                //result = connection.isRegistered(mbeanName);
            } catch (Exception e) {
                connection = null;
            }
        }
        return connection;

    }
    
    protected String getHttpConnectionType() {
        return ConnectionType.HTTP;
    }

    protected String getHttpsConnectionType() {
        return ConnectionType.HTTPS;
    }
    
    protected String getJrmpConnectionType() {
        return ConnectionType.JRMP;
    }
    
    /**
     * returns the ObjectName for the reference installer Mbean.
     * 
     * @return the ObjectName for the installer installer Mbean.
     * @throws MalformedObjectNameException
     *             if the object name is not formatted according to jmx object
     *             name
     */
    protected ObjectName getJbiReferenceAdminUiMBeanObjectName()
            throws MalformedObjectNameException {
        String mbeanRegisteredName = "com.sun.jbi:ServiceName=JbiReferenceAdminUiService,ComponentType=System";
        ObjectName referenceUiMBeanObjectName = new ObjectName(
                mbeanRegisteredName);
        return referenceUiMBeanObjectName;

    }    
    
    /**
     * This method returns the MBeanServerConnection to used to invoke the MBean
     * methods via HTTP connector.
     * 
     * @param urlString -
     *            service:jmx:rmi:///jndi/rmi://<hostName>:<portNumber>/management/rmi-jmx-connector
     * @param userName - the userName name for authenticating with MBeanServer
     * @param password - the password for authenticating with MBeanServer
     * @return MBeanServerConnection
     *
     */
    protected MBeanServerConnection getMBeanServerConnection(String urlString,
            String userName, String password) {
        // Create a JMXMP connector client and
        // connect it to the JMXMP connector server
        // final JMXServiceURL url = new JMXServiceURL(urlString);
        // final JMXServiceURL url = new JMXServiceURL(null, hostName,
        // portNumber);
//        System.out.println("*** urlString: "+urlString+" userName: "+userName);
        MBeanServerConnection mbeanServerConnection = null;
        try {
            JMXServiceURL url = new JMXServiceURL(urlString);
            String[] credentials = new String[] { userName, password };
            Map<String, String[]> environment = new HashMap<String, String[]>();
            environment.put("jmx.remote.credentials", credentials);
            JMXConnector connector = JMXConnectorFactory.connect(url,environment);
            if ( connector!=null ) {
                mbeanServerConnection = connector.getMBeanServerConnection();
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        return mbeanServerConnection;
    }

    /**
     * This method returns the MBeanServerConnection to used to invoke the MBean
     * methods via HTPP connector.
     * 
     * @param hostName -
     *            the hostName part of the URL. If null, defaults to the local
     *            hostName name, as determined by
     *            InetAddress.getLocalHost().getHostName(). If it is a numeric
     *            IPv6 address, it can optionally be enclosed in square brackets
     *            [].
     * @param portNumber - the portNumber part of the URL.
     * @param userName - the userName name for authenticating with MBeanServer
     * @param password - the password for authenticating with MBeanServer
     * @param protocol
     * @return MBeanServerConnection
     */
    protected MBeanServerConnection getMBeanServerConnection(String hostName,
            int portNumber, String userName, String password, String protocol) {
         MBeanServerConnection mbeanServerConnection = null;
        if ( ConnectionType.JRMP.equals(protocol) ) {
            // Create a JMXMP connector client and
            // connect it to the JMXMP connector server
            // final JMXServiceURL url = new JMXServiceURL(null, hostName,
            // portNumber);
            // String urlString =
            // "service:jmx:rmi:///jndi/rmi://"+hostName+":"+portNumber+"/jmxri";
            String urlString = "service:jmx:rmi:///jndi/rmi://" + hostName
                    + ":" + portNumber + "/"+protocol;
            return getMBeanServerConnection(urlString, userName, password);
        } else {
            try { 
                JMXServiceURL url = new JMXServiceURL(protocol,hostName, portNumber);
                JMXConnector connector = JMXConnectorFactory.connect(url,
                        initEnvironment(userName, password));
                 mbeanServerConnection = connector.getMBeanServerConnection();
            } catch(Exception e) {
                e.printStackTrace();
            }
        }
         return mbeanServerConnection;
    }

    /**
     * This method initialize the environment for creating the JMXConnector.
     * @param userName
     * @param password
     * @return Map - HashMap of environment
     */
    protected Map<String, Object> initEnvironment(String userName, String password) {
        Map<String, Object> environment = new HashMap<String, Object>();
        String PKGS = "com.sun.enterprise.admin.jmx.remote.protocol";

        environment.put(JMXConnectorFactory.PROTOCOL_PROVIDER_CLASS_LOADER,
                RemoteServerConnector.class.getClassLoader());
        environment.put(JMXConnectorFactory.PROTOCOL_PROVIDER_PACKAGES, PKGS);
        environment.put("USER", userName);
        environment.put("PASSWORD", password);
        environment.put("com.sun.enterprise.as.http.auth", "BASIC");
        return environment;
    }    
    
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        String hostName = "localhost", userName="admin", password="adminadmin";
        int portNumber = 4848;
        MBeanServerConnection connection = null;
        RemoteServerConnector remoteConnector = new RemoteServerConnector(GenericConstants.SJSAS_SERVER_TYPE,
                 "server1",hostName, portNumber+"", "8080", userName, password);
        connection = remoteConnector.getConnection();
        if(connection != null) {
            System.out.println(connection.toString());
        } else {
            System.out.println("Connection to MBeanServer could not be retrieved.");
        }
    }    
    
    

}
