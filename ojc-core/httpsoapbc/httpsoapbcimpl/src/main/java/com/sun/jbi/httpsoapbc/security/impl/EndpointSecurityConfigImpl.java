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
 * @(#)EndpointSecurityConfigImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.impl;


import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfigConstants;

import java.util.Properties;

public class EndpointSecurityConfigImpl implements EndpointSecurityConfig {
    //private String name;
    private Properties config;
    
    public EndpointSecurityConfigImpl(Properties config, String name) {
        //this.name = name;
        this.config = config;
    }
    
    /*
     *
     * @see com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig#isInsertHTTPBasicAuthCredential()
     */
    public boolean isInsertHTTPBasicAuthCredential() {
        return Boolean.parseBoolean(config.getProperty(EndpointSecurityConfigConstants.HTTP_INSERT_BASIC_AUTH_HEADER, "false"));
    }
    
    /*
     *
     * @see com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig#isValidateHTTPBasicAuthCredential()
     */
    public boolean isValidateHTTPBasicAuthCredential() {
        return Boolean.parseBoolean(config.getProperty(EndpointSecurityConfigConstants.HTTP_VALIDATE_BASIC_AUTH_HEADER, "false"));
    }
    
    /*
     *
     * @see com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig#getConfigProperties()
     */
    public Properties getConfigProperties() {
        return config;
    }

    public String getConfigProperty(String key) {
        return config.getProperty(key);
    }

    public void setConfigProperty(String key, String value) {
        config.setProperty(key, value);
    }    
}
