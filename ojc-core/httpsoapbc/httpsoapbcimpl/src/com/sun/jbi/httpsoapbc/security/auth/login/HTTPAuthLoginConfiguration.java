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
 * @(#)HTTPAuthLoginConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.auth.login;

import java.util.Map;

import javax.security.auth.login.AppConfigurationEntry;
import javax.security.auth.login.Configuration;

import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfigConstants;

public class HTTPAuthLoginConfiguration extends Configuration {

    private Map jbiOptions;
    
    public HTTPAuthLoginConfiguration() {       
    }
    
    public HTTPAuthLoginConfiguration(Map jbiOptions) {
        this.jbiOptions = jbiOptions;
    }
    
    /*
     * (non-Javadoc)
     * @see javax.security.auth.login.Configuration#getAppConfigurationEntry(java.lang.String)
     */
    @Override
    public AppConfigurationEntry[] getAppConfigurationEntry(String name) {
        AppConfigurationEntry[] entries = new AppConfigurationEntry[1];
                
        if (name.equalsIgnoreCase(EndpointSecurityConfigConstants.ENDPOINT_JAAS_CONFIG_NAME_DEFAULT_VALUE)) {
            entries[0] = new AppConfigurationEntry(EndpointSecurityConfigConstants.ENDPOINT_JAAS_LOGIN_MODULE_CLASS_DEFAULT_VALUE,
                                                   AppConfigurationEntry.LoginModuleControlFlag.REQUIRED,
                                                   jbiOptions);
        }
        
        return entries;
    }

    /*
     * @todo Update to refresh configuration from config MBean
     * @see javax.security.auth.login.Configuration#refresh()
     */
    @Override
    public void refresh() {
    }
}
