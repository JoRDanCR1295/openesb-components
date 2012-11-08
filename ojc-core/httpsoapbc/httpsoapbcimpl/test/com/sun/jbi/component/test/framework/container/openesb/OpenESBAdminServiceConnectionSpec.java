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
 * @(#)OpenESBAdminServiceConnectionSpec.java 
 *
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
 * @(#)OpenESBAdminServiceConnectionSpec.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.test.framework.container.openesb;

import java.util.Properties;

import com.sun.jbi.component.test.framework.container.AdministrationServiceConnectionSpec;

/**
 *
 * OpenESB connection spec info record
 */
public class OpenESBAdminServiceConnectionSpec implements AdministrationServiceConnectionSpec {

    public final static String CONN_PROP_CONNECTION_TYPE = "connectionType";
    public final static String CONN_PROP_HOST = "host";
    public final static String CONN_PROP_PORT = "port";
    public final static String CONN_PROP_USERNAME = "username";
    public final static String CONN_PROP_PASSWORD = "password";

    public final static String TARGET_NAME = "targetName";
    
    public final static String CONNECTION_TYPE_JRMP = "JRMP";
    public final static String CONNECTION_TYPE_HTTP = "HTTP";
    public final static String CONNECTION_TYPE_HTTPS = "HTTPS";
    public final static String CONNECTION_TYPE_IIOP = "IIOP";
    
    // defaults
    private String host = "localhost";
    private int port = 8686;
    private String username = "admin";
    private String password = "adminadmin";
    
    public OpenESBAdminServiceConnectionSpec (Properties props) {
        if (props != null) {
            if (props.contains(CONN_PROP_HOST)) {
                host = props.getProperty(CONN_PROP_HOST);
            }        
            if (props.contains(CONN_PROP_PORT)) {
                try {
                    port = Integer.parseInt(props.getProperty(CONN_PROP_PORT));
                } catch (Throwable ignore) {
                }
            }
            if (props.contains(CONN_PROP_USERNAME)) {
                username = props.getProperty(CONN_PROP_USERNAME);
            }
            if (props.contains(CONN_PROP_PASSWORD)) {
                password = props.getProperty(CONN_PROP_PASSWORD);
            }
        }
    }
    
    public String getHost() {return host;}
    public void setHost(String host) {this.host = host;}
    
    public int getPort() {return port;}
    public void setPort(int port) {this.port = port;}
    
    public String getUsername() {return username;}
    public void setUsername(String username) {this.username = username;}
    
    public String getPassword() {return password;}
    public void setPassword(String password) {this.password = password;}
}
