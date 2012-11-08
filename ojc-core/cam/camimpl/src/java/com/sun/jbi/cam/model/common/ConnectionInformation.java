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
 * @(#)ConnectionInformation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import java.io.Serializable;

import com.sun.jbi.cam.common.GenericConstants;
import java.net.InetAddress;
import java.net.UnknownHostException;


/**
 * @author ylee
 *
 */
public class ConnectionInformation implements Serializable {

    ///////////////////////////////////////////////////////////////
    // -- Members --
    ///////////////////////////////////////////////////////////////
    protected String hostName;
    protected String domainName;
    protected String httpAdminPort;
    protected String httpUserPort;
    protected String userName;
    protected String token;

    /**
     */
    public static String retrieveHostName() {
        String host= GenericConstants.DEFAULT_HOST_NAME;
        try {
            host = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
        }
        if(host == null) {
            host = GenericConstants.DEFAULT_HOST_NAME;
        }
        return host;
    }
    
    /**
     *
     */
    public ConnectionInformation() {
        this.hostName= ConnectionInformation.retrieveHostName();
        this.domainName= GenericConstants.DEFAULT_DOMAIN_NAME;
        this.httpAdminPort= GenericConstants.DEFAULT_ADMIN_PORT;
        this.httpUserPort= GenericConstants.DEFAULT_HTTP_USER_PORT;
        this.userName= GenericConstants.DEFAULT_USER_NAME;
        this.token= GenericConstants.DEFAULT_CREDENTIALS;
    }


    /**
     * @param hostName
     * @param domainName
     * @param httpAdminPort
     * @param httpUserPort
     * @param userName
     * @param token
     */
    public ConnectionInformation(String hostName,
                                 String domainName,
                                 String httpAdminPort,
                                 String httpUserPort,
                                 String userName,
                                 String token) {
        super();
        this.hostName = hostName;
        this.domainName = domainName;
        this.httpAdminPort = httpAdminPort;
        this.httpUserPort = httpUserPort;
        this.userName = userName;
        this.token = token;
    }
    
    /**
     * @return Returns the hostName.
     */
    public String getHostName() {
        return this.hostName;
    }
    /**
     * @param hostName The hostName to set.
     */
    public void setHostName(String hostName) {
        this.hostName = hostName;
    }
    /**
     * @return Returns the domainName.
     */
    public String getDomainName() {
        return this.domainName;
    }
    /**
     * @param hostName The domainName to set.
     */
    public void setDomainName(String domainName) {
        this.domainName = domainName;
    }    
    /**
     * @return Returns the httpAdminPort.
     */
    public String getHttpAdminPort() {
        return this.httpAdminPort;
    }
    /**
     * @param httpAdminPort The httpAdminPort to set.
     */
    public void setHttpAdminPort(String httpAdminPort) {
        this.httpAdminPort = httpAdminPort;
    }
    /**
     * @return Returns the httpUserPort.
     */
    public String getHttpUserPort() {
        return this.httpUserPort;
    }
    /**
     * @param httpUserPort The httpUserPort to set.
     */
    public void setHttpUserPort(String httpUserPort) {
        this.httpUserPort = httpUserPort;
    }
    /**
     * @return Returns the token.
     */
    public String getToken() {
        return this.token;
    }
    /**
     * @param token The token to set.
     */
    public void setToken(String token) {
        this.token = token;
    }
    /**
     * @return Returns the userName.
     */
    public String getUserName() {
        return this.userName;
    }
    /**
     * @param userName The userName to set.
     */
    public void setUserName(String userName) {
        this.userName = userName;
    }
    
    
    public void clone(ConnectionInformation info) {
        info.hostName = hostName;
        info.domainName = domainName;
        info.httpAdminPort = httpAdminPort;
        info.httpUserPort = httpUserPort;
        info.userName = userName;
        info.token = token;
    }


}
