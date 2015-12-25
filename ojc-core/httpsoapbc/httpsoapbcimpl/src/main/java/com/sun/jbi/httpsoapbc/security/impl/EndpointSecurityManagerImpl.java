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
 * @(#)EndpointSecurityManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.impl;

import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityManager;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;
import com.sun.jbi.httpsoapbc.security.api.HttpBcSecurityException;
import com.sun.jbi.httpsoapbc.security.http.impl.HTTPSecurityContextHandler;

import javax.security.auth.Subject;
import javax.xml.soap.SOAPMessage;

public class EndpointSecurityManagerImpl implements EndpointSecurityManager {

    private EndpointSecurityConfig securityConfig;    
    private HTTPSecurityContextHandler httpSecurityContextHandler;
    private boolean isSecurityConfigSet = false;
    private Subject securitySubject = null;
    
    public EndpointSecurityManagerImpl (EndpointSecurityConfig securityConfig) {
        this.securityConfig = securityConfig;
        httpSecurityContextHandler = new HTTPSecurityContextHandler(securityConfig);
        isSecurityConfigSet = true;
    }

    public EndpointSecurityManagerImpl() {        
    }
    
    public void initialize(EndpointSecurityConfig securityConfig) {
        this.securityConfig = securityConfig;
        httpSecurityContextHandler = new HTTPSecurityContextHandler(securityConfig);
        isSecurityConfigSet = true;
    }
    
    /*
     * 
     * @see com.sun.jbi.httpsoapbc.security.api.EndpointSecurityManager#handleSecurity(java.lang.Object, javax.security.auth.Subject)
     */
    public void handleSecurity(String authorizationHeader, Subject subject, boolean inbound, HTTPBasicAuthCredential bacredential) throws HttpBcSecurityException {
        //@todo always expect SOAPMessage for now; need to add type checking later        
        if (inbound && securityConfig.isValidateHTTPBasicAuthCredential()) {
                httpSecurityContextHandler.validateMessage(authorizationHeader, subject, bacredential);
                //validateMessage() call above sets the subject to the value returned by AccessManager if successfully authenticated.
                setSubject(subject);
            }
        }
    
    public boolean isInitialized(){
        return isSecurityConfigSet;
    }
    
    public void setSubject(Subject sub) {
    	this.securitySubject = sub;
    }
    
    public Subject getSubject() {
    	return this.securitySubject;
    }
}
