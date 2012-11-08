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
 * @(#)RemoteServerConnector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.administration.providers.glassfish.connector;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

/**
 * Used to connect to GlassFish 9.1 and retrieve
 * an MBeanServerConnection either through the
 * HTTP, HTTPS, or through JSR-160 JRMP protocols
 * 
 * @author graj
 * 
 */
public class RemoteServerConnector {

    /**
     * Retrieve an MBeanServer Connection
     * @param hostName
     * @param portNumber
     * @param userName
     * @param password
     * @return the MBeanServerConnection
     */
    public static MBeanServerConnection getConnection(String hostName, int portNumber,
            String userName, String password) {

        MBeanServerConnection connection = null;
        boolean result = false;
        ObjectName mbeanName = null;

        // Try to obtain a HTTP connection
        try {
            connection = getMBeanServerConnection(hostName, portNumber,
                    userName, password, ConnectionType.HTTP);
            mbeanName = getJbiReferenceAdminUiMBeanObjectName();
            result = connection.isRegistered(mbeanName);
        } catch (MalformedObjectNameException e) {
            connection = null;
        } catch (IOException e) {
            connection = null;
        } catch (RuntimeException runtimeException) {
            connection = null;
        } catch (Exception e) {
            connection = null;
        }

        if (connection == null) {
            // Try to obtain a HTTPS (secure) connection
            try {
                connection = getMBeanServerConnection(hostName,
                        portNumber, userName, password, ConnectionType.HTTPS);
                mbeanName = getJbiReferenceAdminUiMBeanObjectName();
                result = connection.isRegistered(mbeanName);
            } catch (MalformedObjectNameException e) {
                connection = null;
            } catch (IOException e) {
                connection = null;
            } catch (RuntimeException runtimeException) {
                connection = null;
            } catch (Exception e) {
                connection = null;
            }
        }

        if (connection == null) {
            // Try to obtain a JRMP connection
            try {
                connection = getMBeanServerConnection(hostName,
                        portNumber, userName, password, ConnectionType.JRMP);
                mbeanName = getJbiReferenceAdminUiMBeanObjectName();
                result = connection.isRegistered(mbeanName);
            } catch (MalformedObjectNameException e) {
                connection = null;
            } catch (IOException e) {
                connection = null;
            } catch (RuntimeException runtimeException) {
                connection = null;
            } catch (Exception e) {
                connection = null;
            }
        }
        return connection;
    }

    /**
     * returns the ObjectName for the reference installer Mbean.
     * 
     * @return the ObjectName for the installer installer Mbean.
     * @throws MalformedObjectNameException
     *             if the object name is not formatted according to jmx object
     *             name
     */
    static ObjectName getJbiReferenceAdminUiMBeanObjectName()
            throws MalformedObjectNameException {
        String mbeanRegisteredName = "com.sun.jbi:ServiceName=JbiReferenceAdminUiService,ComponentType=System";
        ObjectName sReferenceUiMBeanObjectName = new ObjectName(
                mbeanRegisteredName);
        return sReferenceUiMBeanObjectName;

    }

    /**
     * This method returns the MBeanServerConnection to used to invoke the MBean
     * methods via HTTP connector.
     * 
     * @param url -
     *            service:jmx:rmi:///jndi/rmi://<hostName>:<portNumber>/management/rmi-jmx-connector
     * @userName - the userName name for authenticating with MBeanServer
     * @password - the password for authenticating with MBeanServer
     * @return MBeanServerConnection
     * @throws JBIRemoteException
     */
    static MBeanServerConnection getMBeanServerConnection(String urlString,
            String userName, String password) throws MalformedURLException,
            IOException {
        // Create a JMXMP connector client and
        // connect it to the JMXMP connector server
        // final JMXServiceURL url = new JMXServiceURL(urlString);
        // final JMXServiceURL url = new JMXServiceURL(null, hostName,
        // portNumber);
        final JMXServiceURL url = new JMXServiceURL(urlString);
        String[] credentials = new String[] { userName, password };
        Map<String, String[]> environment = new HashMap<String, String[]>();
        environment.put("jmx.remote.credentials", credentials);
        final JMXConnector connector = JMXConnectorFactory.connect(url,
                environment);
        return connector.getMBeanServerConnection();
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
     * @portNumber - the portNumber part of the URL.
     * @userName - the userName name for authenticating with MBeanServer
     * @password - the password for authenticating with MBeanServer
     * @return MBeanServerConnection
     * @throws JBIRemoteException
     */
    static MBeanServerConnection getMBeanServerConnection(String hostName,
            int portNumber, String userName, String password,
            ConnectionType type) throws MalformedURLException, IOException {
        if (type == ConnectionType.JRMP) {
            // Create a JMXMP connector client and
            // connect it to the JMXMP connector server
            // final JMXServiceURL url = new JMXServiceURL(null, hostName,
            // portNumber);
            // String urlString =
            // "service:jmx:rmi:///jndi/rmi://"+hostName+":"+portNumber+"/jmxri";
            String urlString = "service:jmx:rmi:///jndi/rmi://" + hostName
                    + ":" + portNumber + "/management/rmi-jmx-connector";
            return getMBeanServerConnection(urlString, userName, password);
        } else {
            final JMXServiceURL url = new JMXServiceURL(type.getProtocol(),
                    hostName, portNumber);
            final JMXConnector connector = JMXConnectorFactory.connect(url,
                    initEnvironment(userName, password));
            return connector.getMBeanServerConnection();
        }
    }

    /**
     * This method initialize the environment for creating the JMXConnector.
     * 
     * @return Map - HashMap of environemtn
     */
    private static Map<String, Object> initEnvironment(String userName, String password) {
        final Map<String, Object> environment = new HashMap<String, Object>();
        final String PKGS = "com.sun.enterprise.admin.jmx.remote.protocol";

        environment.put(JMXConnectorFactory.PROTOCOL_PROVIDER_CLASS_LOADER,
                RemoteServerConnector.class.getClassLoader());
        environment.put(JMXConnectorFactory.PROTOCOL_PROVIDER_PACKAGES, PKGS);
        environment.put("USER", userName);
        environment.put("PASSWORD", password);
        environment.put("com.sun.enterprise.as.http.auth", "BASIC");
        return (environment);
    }

    /**
     * This method creates the JMXServiceURL
     * 
     * @param protocol
     * @param hostName
     * @param portNumber
     * @throws JBIRemoteException
     */
    private static JMXServiceURL getJMXServiceURL(String protocol, String hostName,
            int portNumber) throws MalformedURLException {
        // Create a JMXMP connector client and connect it to the JMXMP
        // connector server

        final JMXServiceURL url = new JMXServiceURL(protocol, hostName,
                portNumber);
        System.out.println("url = " + url.toString());
        return url;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        String hostName = "localhost", userName="admin", password="adminadmin";
        int portNumber = 4848;
        MBeanServerConnection connection = null;
        connection = RemoteServerConnector.getConnection(hostName, portNumber, userName, password);
        if(connection != null) {
            System.out.println(connection.toString());
        } else {
            System.out.println("Connection to MBeanServer could not be retrieved.");
        }

    }

}
