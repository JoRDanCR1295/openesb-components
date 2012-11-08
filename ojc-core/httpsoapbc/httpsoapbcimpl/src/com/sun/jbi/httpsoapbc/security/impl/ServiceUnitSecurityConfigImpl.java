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
 * @(#)ServiceUnitSecurityConfigImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.impl;

import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfigConstants;
import com.sun.jbi.httpsoapbc.security.api.ServiceUnitSecurityConfig;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.Properties;

import javax.jbi.JBIException;



public class ServiceUnitSecurityConfigImpl implements ServiceUnitSecurityConfig {    
    private static final Messages mMessages =
        Messages.getMessages(ServiceUnitSecurityConfigImpl.class);
    private static final Logger mLog =
        Messages.getLogger(ServiceUnitSecurityConfigImpl.class);

    //private String name;
    private Properties config;
    
    public ServiceUnitSecurityConfigImpl(String serviceUnitRootPath, String name) 
            throws JBIException {
        //this.name = name;
        loadConfig(serviceUnitRootPath);
    }
    
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.httpsoapbc.security.api.ServiceUnitSecurityConfig#getEndpointSecurityConfig(java.lang.String)
     */
    public EndpointSecurityConfig getEndpointSecurityConfig(String name) {
        return new EndpointSecurityConfigImpl(config, name);
    }

    /*
     * @todo Need to load the user defined security configuration from the
     *       SA for now.
     */
    private void loadConfig(String serviceUnitRootPath)
    throws JBIException {
        loadDefaultConfig(serviceUnitRootPath);
        
//        config = new Properties();
//        File persistentConfigName = new File(serviceUnitRootPath, EndpointSecurityConfigConstants.SU_SECURITY_CONFIG_FILE_NAME);
//        try {
//            FileInputStream fis = new FileInputStream(persistentConfigName);
//            config.load(fis);
//            fis.close();
//        } catch (IOException e) {
//            //@todo Uncomment this, after we package the properties file in the SA
//            // if (mLog.isLoggable(Level.FINE)) {
//            //     mLog.log(Level.FINE, "Failed to load the security configuration of service unit " + name + " : " + e.getMessage());
//            //     mLog.log(Level.FINE, "Loading decfault security configuration");
//            // }
//            
//            loadDefaultConfig();
//        }
    }
    
    private void loadDefaultConfig(String serviceUnitRootPath) {
        config = new Properties(); 
        
        //Turn off security for default
        config.setProperty(EndpointSecurityConfigConstants.HTTP_VALIDATE_BASIC_AUTH_HEADER, "false");
        config.setProperty(EndpointSecurityConfigConstants.HTTP_INSERT_BASIC_AUTH_HEADER, "false");
        
        config.setProperty(EndpointSecurityConfigConstants.AM_AUTHCONTEXT_INDEX_NAME_KEY, 
                           EndpointSecurityConfigConstants.AM_AUTHCONTEXT_INDEX_NAME_DEFAULT_VALUE);
        config.setProperty(EndpointSecurityConfigConstants.AM_AUTHCONTEXT_ORG_NAME_KEY, 
                           EndpointSecurityConfigConstants.AM_AUTHCONTEXT_ORG_NAME_DEFAULT_VALUE);       
    }
}
