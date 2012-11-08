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
 * @(#)LocalServerConnector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.connectors;

import java.io.Serializable;

import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.servlet.http.HttpServletRequest;

import com.sun.jbi.cam.model.common.ServerInformation;

/**
 * @author ylee
 * @author Graj
 *
 */
public class LocalServerConnector extends ServerConnector implements Serializable {


    /**
     *
     */
    public LocalServerConnector() {
        super(null, null, null, null);
        this.initialize();
    }

    /**
     *
     * @param hostNameParam
     * @param portParam
     * @param userNameParam
     * @param passwordParam
     */
    public LocalServerConnector(String hostNameParam, String portParam, String userNameParam, String passwordParam) {
        super(null, null, null, null);
        this.initialize();
    }


    private void initialize() {
        connection = (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
    }


}
