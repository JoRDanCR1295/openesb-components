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
 * @(#)HTTPServerConnector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.connectors;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.servlet.http.HttpServletRequest;

import com.sun.jbi.cam.model.common.ServerInformation;
import com.sun.jbi.cam.common.resources.Messages;
import java.util.logging.Logger;


/**
 * @author ylee
 * @author Graj
 *
 */
public class HTTPServerConnector extends ServerConnector implements Serializable {

    public static final String PROTOCOL_CLASS = "com.sun.enterprise.admin.jmx.remote.protocol"; //$NON-NLS-1$
    public static final String HTTP_AUTH_PROPERTY_NAME = "com.sun.enterprise.as.http.auth"; //$NON-NLS-1$
    public static final String DEFAULT_HTTP_AUTH_SCHEME = "BASIC"; //$NON-NLS-1$
    public static final String ADMIN_USER_ENV_PROPERTY_NAME = "USER"; //$NON-NLS-1$
    public static final String ADMIN_PASSWORD_ENV_PROPERTY_NAME = "PASSWORD"; //$NON-NLS-1$
    public static final String RTS_HTTP_CONNECTOR = "s1ashttp"; //$NON-NLS-1$
   
    private Logger logger = Logger.getLogger(HTTPServerConnector.class.getName());


    /**
     * @param hostNameParam
     * @param portParam
     * @param userNameParam
     * @param passwordParam
     */
    public HTTPServerConnector(String hostNameParam, String portParam, String userNameParam, String passwordParam) {
        super(hostNameParam, portParam, userNameParam, passwordParam);
        try {
            initialize();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * @param hostNameParam
     * @param portParam
     * @param userNameParam
     * @param passwordParam
     */
    public void setParameters(String hostNameParam, String portParam, String userNameParam, String passwordParam) {
        setHostName(hostNameParam);
        setPort(portParam);
        setUserName(userNameParam);
        setPassword(passwordParam);
        try {
            initialize();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    /**
     *  This method returns the MBeanServerConnection to used to invoke the
     *  MBean methods via HTPP connector.
     *  @param host - the host part of the URL. If null, defaults to the local
     *  host name, as determined by InetAddress.getLocalHost().getHostName().
     *  If it is a numeric IPv6 address, it can optionally be enclosed in square
     *  brackets [].
     */
    private void initialize() throws Exception {
        boolean result = true;
        final Map<String,Object> environment = new HashMap<String,Object>();
        environment.put(JMXConnectorFactory.PROTOCOL_PROVIDER_PACKAGES, HTTPServerConnector.PROTOCOL_CLASS);
        environment.put(HTTPServerConnector.HTTP_AUTH_PROPERTY_NAME, HTTPServerConnector.DEFAULT_HTTP_AUTH_SCHEME);
        environment.put(HTTPServerConnector.ADMIN_USER_ENV_PROPERTY_NAME, this.getUserName());
        environment.put(HTTPServerConnector.ADMIN_PASSWORD_ENV_PROPERTY_NAME, this.getPassword());

        try {
            int portValue = new Integer(this.getPort()).intValue();
            JMXServiceURL serviceURL = new JMXServiceURL(HTTPServerConnector.RTS_HTTP_CONNECTOR, this.getHostName(), portValue);
            JMXConnector connector = JMXConnectorFactory.connect(serviceURL, environment);
            this.connection = connector.getMBeanServerConnection();
        } catch (Exception exception) {
            throw exception;
        }
    }

    public static void main(String[] args) {
        HTTPServerConnector connector = new HTTPServerConnector("GRajGX270.stc.com", "4848", "admin", "adminadmin"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        if(connector.getConnection() != null) {
            System.out.println(Messages.getString("HTTPServerConnector.ConnectionRetrieved")+connector.toString()); //$NON-NLS-1$
            connector.printOut();
        } else {
            System.out.println(Messages.getString("HTTPServerConnector.ConnectionFailed")); //$NON-NLS-1$
        }
    }
}
