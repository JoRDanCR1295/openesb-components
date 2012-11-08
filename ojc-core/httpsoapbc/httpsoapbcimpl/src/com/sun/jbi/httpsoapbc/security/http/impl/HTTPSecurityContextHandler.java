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
 * @(#)HTTPSecurityContextHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.http.impl;

import com.sun.jbi.httpsoapbc.security.am.impl.SunAccessManagerCredentialValidator;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfigConstants;
import com.sun.jbi.httpsoapbc.security.api.HttpBcSecurityException;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;
import com.sun.jbi.httpsoapbc.security.api.SecurityContextHandler;
import com.sun.jbi.httpsoapbc.security.impl.UserPrincipal;
import com.sun.jbi.httpsoapbc.security.util.api.Base64;
import com.sun.jbi.httpsoapbc.security.util.impl.Base64Impl;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.Subject;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPMessage;

public class HTTPSecurityContextHandler implements SecurityContextHandler {
    private static final Messages mMessages =
            Messages.getMessages(HTTPSecurityContextHandler.class);
    private static final Logger mLog =
        Messages.getLogger(HTTPSecurityContextHandler.class);
        
    private Base64 base64;
    private EndpointSecurityConfig securityConfig;
    private SunAccessManagerCredentialValidator sunAMValidator;
    
    public HTTPSecurityContextHandler(EndpointSecurityConfig securityConfig) {
        base64 = new Base64Impl();
        this.securityConfig = securityConfig;
        
        try {
            /* Todo: revisit code in code reviews for removal of this class!
            if(securityConfig.getConfigProperties().contains(EndpointSecurityConfigConstants.USE_SUN_ACCESS_MANAGER) &&
               securityConfig.getConfigProperty(EndpointSecurityConfigConstants.USE_SUN_ACCESS_MANAGER).equals("true")) {
                sunAMValidator = new SunAccessManagerCredentialValidator(securityConfig.getConfigProperties());
            }
             */
        } catch (Exception e) {
            //@todo i18n; propagate the exception
            if (mLog.isLoggable(Level.SEVERE)) {
                mLog.log(Level.SEVERE, "HTTPBC-E01021.Failed_init_credential_validator", e);
            }
        }
    }
    
    public void secureMessage(SOAPMessage soapMsg, Subject subject)
    throws HttpBcSecurityException {
        
        String username = securityConfig.getConfigProperty(EndpointSecurityConfigConstants.HTTP_BASIC_AUTH_USERNAME_KEY);
        String password = securityConfig.getConfigProperty(EndpointSecurityConfigConstants.HTTP_BASIC_AUTH_PASSWORD_KEY);
        
        if (username != null && password != null) {
            String basic = new StringBuffer(username).append(':').append(password).toString();
            String authorization = new StringBuffer("Basic ").append(base64.encode(basic)).toString();
            
            soapMsg.getMimeHeaders().setHeader("Authorization", authorization);
        }
        
//@todo refactor in this code when implementing dynamic security context propagation
//        if (subject == null) {
//            //Assume static security contetxt and use the run-as principal from the wsdl
//            String username = securityConfig.getConfigProperty(EndpointSecurityConfigConstants.HTTP_BASIC_AUTH_USERNAME_KEY);
//            String password = securityConfig.getConfigProperty(EndpointSecurityConfigConstants.HTTP_BASIC_AUTH_PASSWORD_KEY);
//            
//            if (username != null && password != null) {
//                String basic = new StringBuffer(username).append(':').append(password).toString();
//                String authorization = new StringBuffer("Basic ").append(base64.encode(basic)).toString();
//                
//                soapMsg.getMimeHeaders().setHeader("authorization", authorization);
//            }
//        } else {
//            Set credentials = subject.getPrivateCredentials(com.sun.jbi.httpsoapbc.security.http.impl.HTTPBasicAuthCredential.class);
//            
//            if (credentials != null && credentials.size() > 0) {
//                //@todo need to first check for HTTPBasicAuthCredential type before casting
//                //There should be only one crdential of this type, so pick the first one.
//                HTTPBasicAuthCredential credential = (HTTPBasicAuthCredential) credentials.toArray()[0];
//                
//                String basic = new StringBuffer(credential.getName())
//                    .append(':')
//                    .append(credential.getPassword()).toString();
//                
//                String authorization = new StringBuffer("Basic ").append(base64.encode(basic)).toString();
//                
//                soapMsg.getMimeHeaders().setHeader("authorization", authorization);
//            }
//        }
    }
    
    public void validateMessage(String authorizationHeader, Subject subject, HTTPBasicAuthCredential bacredential)
    throws HttpBcSecurityException {
        //Get the http authorization header from the message
        String authorization = null;
        String decoded = "";

        //You will get here only if the webservice is secured with HTTP Basic authentication;
        //So, must have the authorization header
        if (authorizationHeader == null) {
            throw new HttpBcSecurityException(mMessages.getString("HTTPBC-E01023.NoAuthorizationHeader"));
        }
        
        //Only Basic auth is handled for now.
        if (!authorizationHeader.startsWith("Basic")) {
            throw new HttpBcSecurityException(mMessages.getString("HTTPBC-E01024.BasicAuthorizationHeaderRequired", authorization));
        }
        
        authorizationHeader = authorizationHeader.trim();
        try {
            decoded = base64.decode(authorizationHeader.substring(5).trim());
        } catch(Exception e) {
            throw new HttpBcSecurityException("Unable to decode the authorization string");
        }
        String username = null;
        String password = null;
        String[] credentials = decoded.split(":");
        if (credentials.length == 2) {
            username = credentials[0];
            password = credentials[1];
        } else {
            //Need to have both username and password
            throw new HttpBcSecurityException(mMessages.getString("HTTPSecurityContextHandler.InvalidAuthorizationHeader", authorizationHeader));
        }
        
        try {
            if(bacredential != null) {
                if(!((bacredential.getName().equals(username)) && (String.valueOf(bacredential.getPassword()).equals(password)))) {
                    throw new HttpBcSecurityException("Invalid username and password");
                }                
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Successfully authenticated user:" + username);
                }
                UserPrincipal userPrincipal = new UserPrincipal (username);
                HTTPBasicAuthCredential httpAuthCredential = new HTTPBasicAuthCredential(username,password.toCharArray());
                subject.getPrincipals().add(userPrincipal);        
                subject.getPrivateCredentials().add(httpAuthCredential);
                return;
            }
            if(sunAMValidator != null) {
                subject = sunAMValidator.validateCredential(username, password.toCharArray());
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Successfully authenticated user:" + username);
                }
                return;
            } 
        } catch (CredentialValidationException e) {
            throw new HttpBcSecurityException(e);
        }                
                
////@todo refactor in this code when packaging JAAS login module to enable pluggability
//        //Authenticate and populate the subject with the credentials
//        CallbackHandler callbackHandler = new HTTPAuthCallbackHandler(username, password.toCharArray());
//        Configuration config = new HTTPAuthLoginConfiguration(securityConfig.getConfigProperties());
//        
//        try {
//            LoginContext loginCtx = new LoginContext("jbi", subject, callbackHandler, config);
//            loginCtx.login();
//            subject = loginCtx.getSubject();
//        } catch (LoginException e) {
//            throw new HttpBcSecurityException(e);
//        }
    }
}
