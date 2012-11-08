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
 * @(#)ServerInformation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import java.io.Serializable;

import com.sun.jbi.cam.common.GenericConstants;

/**
 * @author ylee
 *
 */
public class ServerInformation extends ConnectionInformation implements Serializable {

    private String iiopAdminPort;
    private String jrmpAdminPort;

    /**
     *
     */
    public ServerInformation() {
        super();
        this.iiopAdminPort = GenericConstants.DEFAULT_IIOP_ADMIN_PORT;
        this.jrmpAdminPort = GenericConstants.DEFAULT_JRMP_ADMIN_PORT;
    }

    /**
     *
     * @param hostName
     * @param domainName
     * @param httpAdminPort
     * @param iiopAdminPort
     * @param jrmpAdminPort
     * @param httpUserPort
     * @param userName
     * @param token
     */
    public ServerInformation(String hostName, String domainName, String httpAdminPort,
            String iiopAdminPort, String jrmpAdminPort, String httpUserPort,
            String userName, String token) {
        super(hostName, domainName, httpAdminPort, httpUserPort, userName, token);
        this.iiopAdminPort = iiopAdminPort;
        this.jrmpAdminPort = jrmpAdminPort;
    }
    
    /**
     * @return Returns the iiopAdminPort.
     */
    public String getIiopAdminPort() {
        return this.iiopAdminPort;
    }

    /**
     * @param iiopAdminPort The iiopAdminPort to set.
     */
    public void setIiopAdminPort(String iiopAdminPort) {
        this.iiopAdminPort = iiopAdminPort;
    }

    /**
     * @return Returns the jrmpAdminPort.
     */
    public String getJrmpAdminPort() {
        return this.jrmpAdminPort;
    }

    /**
     * @param jrmpAdminPort The jrmpAdminPort to set.
     */
    public void setJrmpAdminPort(String jrmpAdminPort) {
        this.jrmpAdminPort = jrmpAdminPort;
    }

    
    public void clone(ServerInformation serverInfo) {
        super.clone(serverInfo);
        serverInfo.iiopAdminPort = iiopAdminPort;
        serverInfo.jrmpAdminPort = jrmpAdminPort;
    }


}
