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
 * @(#)RMIServerConnector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.connectors;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.servlet.http.HttpServletRequest;

import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.model.common.ServerInformation;


/**
 * @author ylee
 * @author Graj
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class RMIServerConnector extends ServerConnector implements Serializable {

    static private String URL_PREFIX = "service:jmx:rmi:///jndi/rmi://"; //$NON-NLS-1$
    //static private String URL_POSTFIX = "/jmxrmi"; //$NON-NLS-1$
    static private String URL_POSTFIX = "/management/rmi-jmx-connector"; //$NON-NLS-1$


    /**
     *
     * @param hostNameParam
     * @param portParam
     * @param userNameParam
     * @param passwordParam
     */
    public RMIServerConnector(String hostNameParam, String portParam, String userNameParam, String passwordParam) {
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

    private void initialize() {
        JMXServiceURL jmxURL = null;
        JMXConnector jmxConnector = null;
        String[] credentials = new String[]{ getUserName() , getPassword() };
        Map<String,Object> environment = new HashMap<String,Object>();
        environment.put("jmx.remote.credentials", credentials); //$NON-NLS-1$
        String url = RMIServerConnector.URL_PREFIX+
                      getHostName()+":"+ //$NON-NLS-1$
                      getPort()+
                      RMIServerConnector.URL_POSTFIX;
        try {
            jmxURL = new JMXServiceURL(url);
            jmxConnector = JMXConnectorFactory.connect(jmxURL, environment);
            this.connection = jmxConnector.getMBeanServerConnection();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


     public static void main(String[] args) {
        RMIServerConnector connector = new RMIServerConnector("GRajGX270.stc.com", "8686", "admin", "adminadmin"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        if(connector.getConnection() != null) {
            //System.out.println("Connection Retrieved."+connector.toString());
            //connector.printOut();
        } else {
            System.out.println(Messages.getString("RMIServerConnector.ConnectionFailed")); //$NON-NLS-1$
        }
    }
}
