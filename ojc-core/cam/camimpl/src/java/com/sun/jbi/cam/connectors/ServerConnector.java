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
 * @(#)ServerConnector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.connectors;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.Properties;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.model.common.ServerInformation;
import com.sun.jbi.cam.common.resources.Messages;
import java.util.logging.Logger;
import javax.management.MBeanServerConnection;

/**
 * @author ylee
 *
 */
public class ServerConnector implements Serializable {
    
    protected String password;
    protected String type;
    
    protected ServerInformation serverInformation = new ServerInformation();
    
    protected transient MBeanServerConnection connection;
    
    private Logger logger = Logger.getLogger(ServerConnector.class.getName());

    /**
     *
     * @param hostNameParam
     * @param portParam
     * @param userNameParam
     * @param passwordParam
     */
    public ServerConnector(String hostNameParam, String portParam, String userNameParam, String passwordParam) {
        this.password = passwordParam;

        if ( hostNameParam==null && portParam==null && userNameParam==null && passwordParam==null ) {
            // construct serverInfo
            generateServerInformation();
        } else {
            serverInformation.setHostName(hostNameParam);
            serverInformation.setHttpAdminPort(portParam);
            serverInformation.setUserName(userNameParam);
        }
    }

    /**
     *
     * @param ServerInformation
     */
    public ServerConnector(ServerInformation serverInfo) {
        serverInformation = serverInfo;
    }

    public ServerInformation getServerInformation() {
        return serverInformation;
    }
    
    
    protected void generateServerInformation() {
        serverInformation.setDomainName(GenericConstants.DEFAULT_DOMAIN_NAME);
        serverInformation.setHostName(serverInformation.retrieveHostName());
        serverInformation.setHttpAdminPort(GenericConstants.DEFAULT_ADMIN_PORT);
        // more ...
    }
    
    /**
     *
     * @param request
     * @return
     */
    public ServerInformation getConnectionProperties(HttpServletRequest request) {
        ServerInformation serverInfo = null;
        FileInputStream inputStream = null;
        
        HttpSession session = request.getSession();
        serverInfo = (ServerInformation) session.getAttribute(GenericConstants.SERVER_INFORMATION_KEY);
        if(serverInfo != null) {
            return serverInfo;
        }
        String resourcesRoot = session.getServletContext().getRealPath("/resources"); //$NON-NLS-1$
        // Read properties file.
        Properties properties = new Properties();
        try {
            inputStream = new FileInputStream(resourcesRoot+File.separator+GenericConstants.CONNECTION_PROPERTIES_KEY);
            properties.load(inputStream);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        
        String hostName=properties.getProperty(GenericConstants.HOSTNAME);
        String domainName=properties.getProperty(GenericConstants.DOMAINNAME); 
        String httpAdministrationPort=properties.getProperty(GenericConstants.HTTP_ADMINISTRATION_PORT);
        String iiopPort=properties.getProperty(GenericConstants.IIOP_PORT);
        String jrmpPort=properties.getProperty(GenericConstants.JRMP_PORT);
        String httpEndpointPort = properties.getProperty(GenericConstants.HTTP_ENDPOINT_PORT);
        String userName=properties.getProperty(GenericConstants.USER_NAME);
        String password=properties.getProperty(GenericConstants.PASSWORD);
        String updaterRefreshRate=properties.getProperty(GenericConstants.UPDATER_REFRESH_RATE);

        if(hostName == null) {
            hostName = ServerInformation.retrieveHostName();
        }
        if(domainName == null) {
            domainName = GenericConstants.DEFAULT_DOMAIN_NAME;
        }        
        if(httpAdministrationPort == null) {
            httpAdministrationPort = GenericConstants.DEFAULT_ADMIN_PORT;
        }
        if(iiopPort == null) {
            iiopPort = GenericConstants.DEFAULT_IIOP_ADMIN_PORT;
        }
        if(jrmpPort == null) {
            jrmpPort = GenericConstants.DEFAULT_JRMP_ADMIN_PORT;
        }
        if(httpEndpointPort == null) {
            httpEndpointPort = GenericConstants.DEFAULT_HTTP_USER_PORT;
        }
        if(userName == null) {
            userName = GenericConstants.DEFAULT_USER_NAME;
        }
        if(password == null) {
            password = GenericConstants.DEFAULT_CREDENTIALS;
        }
        if(updaterRefreshRate == null) {
            updaterRefreshRate = GenericConstants.DEFAULT_UPDATER_REFRESH_RATE;
        }

        serverInfo = new ServerInformation(hostName,
                domainName,
                httpAdministrationPort,
                iiopPort,
                jrmpPort,
                httpEndpointPort,
                userName,
                password);
                
        if(serverInfo != null) {
            session.setAttribute(GenericConstants.SERVER_INFORMATION_KEY, serverInfo);
        }
        return serverInfo;
    }

    /**
     * @param password The password to set.
     */
    public void setPassword(String password) {
        this.password = password;
    }
    /**
     * @param userName The userName to set.
     */
    public void setUserName(String userName) {
        serverInformation.setUserName(userName);
    }

    /**
     * @return Returns the password.
     */
    public String getPassword() {
        return this.password;
    }
    /**
     * @return Returns the userName.
     */
    public String getUserName() {
        return serverInformation.getUserName();
    }

    /**
     * @return Returns the hostName.
     */
    public String getHostName() {
        return serverInformation.getHostName();
    }
    /**
     * @param hostName The hostName to set.
     */
    public void setHostName(String hostName) {
        serverInformation.setHostName(hostName);
    }
    /**
     * @return Returns the port.
     */
    public String getPort() {
        return serverInformation.getHttpAdminPort();
    }
    /**
     * @param port The port to set.
     */
    public void setPort(String port) {
        serverInformation.setHttpAdminPort(port);
    }
    
    public String getType() {
        return type;
    }
    
    public void setType(String type) {
        this.type = type;
    }
    
    public String getDomainName() {
        return serverInformation.getDomainName();
    }
    
    /**
     * @return Returns the connection.
     */
    public MBeanServerConnection getConnection() {
        return this.connection;
    }
    
    public void printOut() {
        logger.info(Messages.getString("ServerConnector.SLANT_LINE")); //$NON-NLS-1$
        logger.info(Messages.getString("ServerConnector.ServerConnectorHeader")); //$NON-NLS-1$
        logger.info(Messages.getString("ServerConnector.SLANT_LINE")); //$NON-NLS-1$
        logger.info(Messages.getString("ServerConnector.HostNameIs")+ serverInformation.getHostName()); //$NON-NLS-1$
        logger.info(Messages.getString("ServerConnector.PortIs")+ serverInformation.getHttpAdminPort()); //$NON-NLS-1$
        logger.info(Messages.getString("ServerConnector.UserNameIs")+ serverInformation.getUserName()); //$NON-NLS-1$
        logger.info(Messages.getString("ServerConnector.PasswordIs")+ password); //$NON-NLS-1$
        logger.info(Messages.getString("ServerConnector.SLANT_LINE")); //$NON-NLS-1$
    }

    public static void main(String[] args) {
    }
}
