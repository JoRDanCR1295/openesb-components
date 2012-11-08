/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)HL7ManagementClientFactory.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7.mgmt.client;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import com.sun.esb.management.common.ManagementRemoteException;
import com.sun.jbi.ui.client.ConnectionType;
import com.sun.jbi.ui.common.I18NBundle;
import com.sun.jbi.ui.common.JBIJMXObjectNames;
import com.sun.jbi.ui.common.JBIResultXmlBuilder;
import com.sun.jbi.ui.common.Util;


/**
 * This class is a factory class to create a ManagementClient object.
 *
 * @author ylee
 */
public class HL7ManagementClientFactory implements Serializable {

    /** i18n */
    private static I18NBundle sI18NBundle = null;

    /** Creates a new instance of ManagementClientFactory */
    protected HL7ManagementClientFactory() {
    }

    /**
     * gives the I18N bundle
     *
     * @return I18NBundle object
     */
    protected static I18NBundle getI18NBundle() {
        // lazzy initialize the JBI Client
        if (sI18NBundle == null) {
            sI18NBundle = new I18NBundle("com.sun.jbi.ui.client");
        }
        return sI18NBundle;
    }

    /**
     * Creates a management message string and populates the exception
     *
     * @param bundleKey
     * @param args
     *            of Strings
     * @param sourceException -
     *            the source exception to propagate
     * @return Exception object created with a valid XML Management Message
     */
    static Exception createManagementException(String bundleKey, String[] args,
            Exception sourceException) {
        Exception exception = null;
        String xmlManagementMessage = JBIResultXmlBuilder.createJbiResultXml(
                HL7ManagementClientFactory.getI18NBundle(), bundleKey, args,
                sourceException);
        exception = new Exception(xmlManagementMessage);
        return exception;
    }

    /**
     * Creates a new instance of ManagementClient object
     *
     * @param hostName
     * @param portNumber
     * @param userName
     * @param password
     * @param connectionType
     * @return ManagementClient object
     * @throws ManagementRemoteException
     */
    public static HL7ManagementClient getInstance(String hostName, int portNumber,
            String userName, String password, ConnectionType connectionType)
            throws ManagementRemoteException {

        HL7ManagementClientFactory factory = new HL7ManagementClientFactory();
        HL7ManagementClient commands = null;
        // Create a remote connection
        MBeanServerConnection connection = null;

        try {
            connection = factory.getMBeanServerConnection(hostName, portNumber,
                    userName, password, connectionType);
        } catch (ManagementRemoteException rEx) {
            throw rEx;
        } catch (Exception e) {
            String[] args = { hostName, (new Integer(portNumber)).toString(),
                    userName, connectionType.getProtocol() };
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection.host.port.uname.password.protocol",
                    args, e);
            throw new ManagementRemoteException(exception);
        }
        boolean isLocalHost = false;
        try {
            isLocalHost = Util.isLocalHost(hostName);
        } catch (UnknownHostException e) {
            String[] args = { hostName };
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection.unknown.host", args, null);
            throw new ManagementRemoteException(exception);
        }
        if (isLocalHost == true) {
            commands = new HL7ManagementClient(connection, false);
        } else {
            commands = new HL7ManagementClient(connection, true);
        }
        return commands;
    }

    /**
     * Creates a new instance of ManagementClient object
     *
     * @param url
     * @param userName
     * @param password
     * @param isRemoteConnection -
     *            true if remote, false if local
     * @return ManagementClient object
     * @throws ManagementRemoteException
     */
    public static HL7ManagementClient getInstance(String url, String userName,
            String password, boolean isRemoteConnection)
            throws ManagementRemoteException {

        HL7ManagementClientFactory factory = new HL7ManagementClientFactory();
        HL7ManagementClient commands = null;

        MBeanServerConnection connection = null;

        try {
            connection = factory.getMBeanServerConnection(url, userName,
                    password);
        } catch (ManagementRemoteException rEx) {
            throw rEx;
        } catch (Exception e) {
            String[] args = { url, userName };
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection.url.uname.password",
                    args, e);
            throw new ManagementRemoteException(exception);
        }

        commands = new HL7ManagementClient(connection, isRemoteConnection);
        return commands;
    }

    public static HL7ManagementClient getInstance(String hostName, int portNumber,
            String userName, String password) throws ManagementRemoteException {
       return getInstance(hostName,portNumber,userName,password,false);
    }

    /**
     * Creates a new instance of ManagementClient object First tries to
     * establish a HTTP connection. If that fails, tries to establish a HTTPS
     * connection, and if that fails tries to establish a JRMP Connection.
     *
     * @param hostName
     * @param portNumber
     * @param userName
     * @param password
     * @return ManagementClient object
     * @throws ManagementRemoteException
     */
    public static HL7ManagementClient getInstance(String hostName, int portNumber,
            String userName, String password, boolean isRemoteConnection) throws ManagementRemoteException {

        HL7ManagementClientFactory factory = new HL7ManagementClientFactory();
        HL7ManagementClient commands = null;

        MBeanServerConnection connection = null;
        boolean result = false;
        ObjectName mbeanName = null;
        Exception exceptionArgument = null;

        // Try to obtain a HTTP connection
        try {
            connection = factory.getMBeanServerConnection(hostName, portNumber,
                    userName, password, ConnectionType.HTTP);
            mbeanName = JBIJMXObjectNames
                    .getJavaCapsAdministrationServiceMBeanObjectName();
            result = connection.isRegistered(mbeanName);
        } catch (MalformedObjectNameException e) {
            connection = null;
            exceptionArgument = e;
        } catch (IOException e) {
            connection = null;
            exceptionArgument = e;
        } catch (RuntimeException runtimeException) {
            connection = null;
            exceptionArgument = runtimeException;
        } catch (Exception e) {
            connection = null;
            exceptionArgument = e;
        }

        if (connection == null) {
            // Try to obtain a HTTPS (secure) connection
            try {
                connection = factory.getMBeanServerConnection(hostName,
                        portNumber, userName, password, ConnectionType.HTTPS);
                mbeanName = JBIJMXObjectNames
                .getJavaCapsAdministrationServiceMBeanObjectName();
                result = connection.isRegistered(mbeanName);
            } catch (MalformedObjectNameException e) {
                connection = null;
                exceptionArgument = e;
            } catch (IOException e) {
                connection = null;
                exceptionArgument = e;
            } catch (RuntimeException runtimeException) {
                connection = null;
                exceptionArgument = runtimeException;
            } catch (Exception e) {
                connection = null;
                exceptionArgument = e;
            }
        }

        if (connection == null) {
            // Try to obtain a JRMP connection
            try {
                connection = factory.getMBeanServerConnection(hostName,
                        portNumber, userName, password, ConnectionType.JRMP);
                mbeanName = JBIJMXObjectNames
                .getJavaCapsAdministrationServiceMBeanObjectName();
                result = connection.isRegistered(mbeanName);
            } catch (MalformedObjectNameException e) {
                connection = null;
                exceptionArgument = e;
            } catch (IOException e) {
                connection = null;
                exceptionArgument = e;
            } catch (RuntimeException runtimeException) {
                connection = null;
                exceptionArgument = runtimeException;
            } catch (Exception e) {
                connection = null;
                exceptionArgument = e;
            }
        }

        if (connection == null) {
            // Try to obtain a IIOP connection for websphere
            // The CORBA calls below send errors to stderr,
            // leading to very ugly ant output (CR 6586235).
            // It turns out that this is documented as CR 5068014
            // and the suggested workaround there is to redirect
            // System.err, as we do below:
            java.io.PrintStream oldErr = System.err;
            try {
                java.io.PrintStream newErr = new java.io.PrintStream(
                        new java.io.ByteArrayOutputStream());
                System.setErr(newErr);
                connection = factory.getMBeanServerConnection(hostName,
                        portNumber, userName, password, ConnectionType.IIOP);
                mbeanName = JBIJMXObjectNames
                .getJavaCapsAdministrationServiceMBeanObjectName();
                result = connection.isRegistered(mbeanName);
                newErr.close();
            } catch (MalformedObjectNameException e) {
                connection = null;
                exceptionArgument = e;
            } catch (IOException e) {
                connection = null;
                exceptionArgument = e;
            } catch (RuntimeException runtimeException) {
                connection = null;
                exceptionArgument = runtimeException;
            } catch (Exception e) {
                connection = null;
                exceptionArgument = e;
            }
            System.setErr(oldErr);
        }

        boolean isLocalHost = false;
        if ( isRemoteConnection ) {
            //
        } else {
            try {
                isLocalHost = Util.isLocalHost(hostName);
            } catch (UnknownHostException e) {
                String[] args = { hostName };
                Exception exception = createManagementException(
                        "jbi.ui.client.factory.connection.unknown.host", args, null);
                throw new ManagementRemoteException(exception);
            }
        }

        if (connection != null) {
            if (isLocalHost == true) {
                commands = new HL7ManagementClient(connection, false);
            } else {
                commands = new HL7ManagementClient(connection, true);
            }
        } else {
            String[] args = { hostName, Integer.valueOf(portNumber).toString(),
                    userName };
            Exception exception = createManagementException(
                    "jbi.ui.client.connection.failure", args, null);
            throw new ManagementRemoteException(exception);
        }
        return commands;
    }

    /**
     * Creates a new instance of ManagementClient object for a remote connection
     *
     * @param connection
     * @return ManagementClient object
     * @throws ManagementRemoteException
     */
    public static HL7ManagementClient getInstance(MBeanServerConnection connection)
            throws ManagementRemoteException {
        HL7ManagementClient commands = null;
        if (connection != null) {
            commands = new HL7ManagementClient(connection, true);
        } else {
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection", null, null);
            throw new ManagementRemoteException(exception);
        }

        return commands;
    }

    /**
     * Creates a new instance of ManagementClient object
     *
     * @param connection
     * @param isRemoteConnection -
     *            true if remote, false if local
     * @return ManagementClient object
     * @throws ManagementRemoteException
     */
    public static HL7ManagementClient getInstance(
            MBeanServerConnection connection, boolean isRemoteConnection)
            throws ManagementRemoteException {
        HL7ManagementClient commands = null;
        if (connection != null) {
            commands = new HL7ManagementClient(connection, isRemoteConnection);
        } else {
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection", null, null);
            throw new ManagementRemoteException(exception);
        }

        return commands;
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
     * @throws ManagementRemoteException
     */
    protected MBeanServerConnection getMBeanServerConnection(String urlString,
            String userName, String password) throws ManagementRemoteException {
        try {
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
        } catch (Exception e) {
            String[] args = { urlString, userName };
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection.url.uname.password",
                    args, e);
            throw new ManagementRemoteException(exception);
        }
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
     * @throws ManagementRemoteException
     */
    protected MBeanServerConnection getMBeanServerConnection(String hostName,
            int portNumber, String userName, String password,
            ConnectionType type) throws ManagementRemoteException {
        try {
            if (type == ConnectionType.JRMP) {
                // Create a JMXMP connector client and
                // connect it to the JMXMP connector server
                // final JMXServiceURL url = new JMXServiceURL(null, hostName,
                // portNumber);
                // String urlString =
                // "service:jmx:rmi:///jndi/rmi://"+hostName+":"+portNumber+"/jmxri";
                String urlString = "service:jmx:rmi:///jndi/rmi://" + hostName
                        + ":" + portNumber + "/jmxrmi";
                return this.getMBeanServerConnection(urlString, userName,
                        password);
            } else if (type == ConnectionType.IIOP) {
                String urlString = "service:jmx:iiop://" + hostName + ":"
                        + portNumber + "/jndi/JMXConnector";
                return this.getMBeanServerConnection(urlString, userName,
                        password);
            } else {
                final JMXServiceURL url = new JMXServiceURL(type.getProtocol(),
                        hostName, portNumber);
                final JMXConnector connector = JMXConnectorFactory.connect(url,
                        this.initEnvironment(userName, password));
                return connector.getMBeanServerConnection();
            }
        } catch (Exception e) {
            String[] args = { hostName, (new Integer(portNumber)).toString(),
                    userName, type.getProtocol() };
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection.host.port.uname.password.protocol",
                    args, null);

            throw new ManagementRemoteException(exception);
        }
    }

    /**
     * This method initialize the environment for creating the JMXConnector.
     *
     * @return Map - HashMap of environemtn
     */
    private Map<String, Object> initEnvironment(String userName, String password) {
        final Map<String, Object> environment = new HashMap<String, Object>();
        final String PKGS = "com.sun.enterprise.admin.jmx.remote.protocol";

        environment.put(JMXConnectorFactory.PROTOCOL_PROVIDER_CLASS_LOADER,
                getClass().getClassLoader());
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
     * @throws ManagementRemoteException
     */
    private JMXServiceURL getJMXServiceURL(String protocol, String hostName,
            int portNumber) throws ManagementRemoteException {
        try {
            // Create a JMXMP connector client and connect it to the JMXMP
            // connector server

            final JMXServiceURL url = new JMXServiceURL(protocol, hostName,
                    portNumber);
            System.out.println("url = " + url.toString());
            return url;
        } catch (MalformedURLException mue) {
            String[] args = { protocol, hostName,
                    (new Integer(portNumber)).toString(), };
            Exception exception = createManagementException(
                    "jbi.ui.client.factory.connection.protocol.host.port",
                    args, mue);
            throw new ManagementRemoteException(exception);
        }
    }


}

