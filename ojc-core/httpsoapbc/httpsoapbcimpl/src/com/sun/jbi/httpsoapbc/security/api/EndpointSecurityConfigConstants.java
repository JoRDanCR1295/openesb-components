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
 * @(#)EndpointSecurityConfigConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.api;

public class EndpointSecurityConfigConstants {    
    public static final String SU_SECURITY_CONFIG_FILE_NAME = "security.properties";
    
    public static final String HTTP_AUTH_TYPE_BASIC = "basic";
    public static final String HTTP_VALIDATE_BASIC_AUTH_HEADER = "http.validate.basic.auth.header";
    public static final String HTTP_INSERT_BASIC_AUTH_HEADER = "http.insert.basic.auth.header";
    
    //HTTP BASIC authentication static security context keys
    public static final String HTTP_BASIC_AUTH_USERNAME_KEY = "http.basic.auth.username";
    public static final String HTTP_BASIC_AUTH_PASSWORD_KEY = "http.basic.auth.password";
    
    public static final String ENDPOINT_JAAS_CONFIG_NAME_KEY = "http.bc.jaas.config.name";
    public static final String ENDPOINT_JAAS_CONFIG_NAME_DEFAULT_VALUE = "jbi";
    
    public static final String ENDPOINT_JAAS_LOGIN_MODULE_CLASS_NAME_KEY = "http.bc.jaas.login.module.name";
    public static final String ENDPOINT_JAAS_LOGIN_MODULE_CLASS_DEFAULT_VALUE = "com.sun.jbi.httpsoapbc.security.auth.login.HTTPAuthLoginModule";
    
    // Access Manager config properties file for the client sdk
    public static final String AM_CLIENT_SDK_CONFIG_PROPS_DIR_KEY = "com.iplanet.services.configpath.jbi";
    public static final String AM_CLIENT_SDK_CONFIG_PROPS_FILE_NAME = "AMConfig.properties";
        
    // The name of the user's organization used to create the AuthContext
    public static final String AM_AUTHCONTEXT_ORG_NAME_KEY = "com.sun.identity.authentication.AuthContext.orgName";

    public static final String AM_AUTHCONTEXT_ORG_NAME_DEFAULT_VALUE = "/";

    // The index name used to identify the authentication module 
    // used by the AuthContext for authenticating the user
    public static final String AM_AUTHCONTEXT_INDEX_NAME_KEY = "com.sun.identity.authentication.AuthContext.indexName"; 
    
    public static final String AM_AUTHCONTEXT_INDEX_NAME_DEFAULT_VALUE = "DataStore";
    
    public static final String AM_CLIENT_SDK_CONFIG_SERVER_MODE_KEY = "com.iplanet.am.serverMode";

    public static final String USE_SUN_ACCESS_MANAGER = "use.sun.access.manager";
    
    public static final String AM_CLIENT_SDK_PASSWORD_ENCRYPTION_KEY = "am.encryption.pwd";
}
