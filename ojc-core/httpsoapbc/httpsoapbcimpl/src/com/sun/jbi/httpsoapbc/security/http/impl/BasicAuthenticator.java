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
 * @(#)BasicAuthenticator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.http.impl;

import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.HttpSoapEndpoint;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityManager;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;
import com.sun.jbi.httpsoapbc.security.auth.HttpAuthenticator;
import com.sun.jbi.httpsoapbc.security.impl.EndpointSecurityManagerImpl;
import com.sun.jbi.internationalization.Messages;

import java.net.URL;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author pponnala
 * Encapsulates HttpAuthenticator (which needs to be only one per jvm) and contains a hashmap of EndPoint-->HttpBasicAuthCredential
 *
 */
public class BasicAuthenticator {
    
    private static final Logger mLog = Messages.getLogger(BasicAuthenticator.class);
    
    private static Map<Endpoint,  HTTPBasicAuthCredential> epCredentialMap = new HashMap();
    private static EndpointSecurityManager securityManager;
    
    /** Creates a new instance of BasicAuthenticator */
    public BasicAuthenticator() {
        securityManager = new EndpointSecurityManagerImpl();
    }    
    
    public static void registerEndPointCredentials(Endpoint ep, HTTPBasicAuthCredential cred) {
        if(!epCredentialMap.containsKey(ep)) {
            epCredentialMap.put(ep, cred);
        }
    }
    
    public static void unregisterEndPointCredentials(Endpoint ep, HTTPBasicAuthCredential credentials) {
        if(epCredentialMap.containsKey(ep)) {
            epCredentialMap.remove(ep);            
        }
    }
    
    public void register() {
        HttpAuthenticator.registerBasicAuthenticator(this);
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "BasicAuthenticator registered");
        }
    }
    
    public void unregister() {
        HttpAuthenticator.unregisterBasicAuthenticator();
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "BasicAuthenticator unregistered");
        }
    }

    public static void initializeEndPointSecurityManager(EndpointSecurityConfig securityConfig) {
        if(!securityManager.isInitialized()) {
            securityManager.initialize(securityConfig);
        }
    }
    
    public HTTPBasicAuthCredential getCredentialsForEndpointURI(URL url) {
       Set<Endpoint> eps = epCredentialMap.keySet();
       for(Endpoint ep : eps) {
           if(ep.getEndpointUrl().equals(url)) {
               return epCredentialMap.get(ep);
           }
       }
       return null;
    }
    
    public static EndpointSecurityManager getEndpointSecurityManager() {
        return securityManager;
    }
}
