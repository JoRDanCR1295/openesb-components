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
 * @(#)HTTPAuthLoginModule.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.auth.login;

import com.sun.jbi.httpsoapbc.security.am.impl.SunAccessManagerCredentialValidator;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;
import com.sun.jbi.httpsoapbc.security.impl.UserPrincipal;

import java.io.IOException;
import java.util.Map;

import javax.security.auth.Subject;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.auth.login.LoginException;
import javax.security.auth.spi.LoginModule;

public class HTTPAuthLoginModule implements LoginModule {
//    private static final Messages messages =
//        Messages.getMessages(HTTPAuthLoginModule.class);
    
    private Subject subject;
    private CallbackHandler callbackHandler;
    private boolean loginSuccess;
    private boolean commitSuccess;
    
    private String name;
    private char[] pwd;
    
    private UserPrincipal userPrincipal;
    private HTTPBasicAuthCredential httpAuthCredential;
    private SunAccessManagerCredentialValidator credentialValidator;
    
    public boolean abort() throws LoginException {
        if (commitSuccess) {
            logout();
        } else if (loginSuccess) {
            cleanLoginState();
            loginSuccess = false;
        }
 
        return true;
    }

    public boolean commit() throws LoginException {
        if (!loginSuccess) {
            cleanLoginState();
            return false;
        }
        
        userPrincipal = new UserPrincipal (name);
        httpAuthCredential = new HTTPBasicAuthCredential(name,pwd);
        
        subject.getPrincipals().add(userPrincipal);        
        subject.getPrivateCredentials().add(httpAuthCredential);
        
        commitSuccess = true;
        cleanLoginState();
        return true;
    }

    public void initialize(Subject subject, CallbackHandler handler,
                           Map<String, ?> sharedState, Map<String, ?> options) {
        this.subject = subject;
        this.callbackHandler = handler;
        
        /* Todo: revisit code in code reviews for removal of this class!
        try {
            credentialValidator = new SunAccessManagerCredentialValidator(options);           
        } catch (CredentialValidationException e) {
            //@todo Log the error message
        }
         */
    }

    public boolean login() throws LoginException {
        //proceed only when a validator is available
        //to validate (login) the credentials
        if (credentialValidator == null) {
            throw new LoginException("NoCredentialValidator");
        }
        
        //proceed only when a callback handler is available
        if (callbackHandler == null) {
//            throw new LoginException(messages.getString("NoCallbackHandler"));
            throw new LoginException("NoCallbackHandler");
        }
        
        //populate the callback list
        Callback callbacks[] = new Callback[] {new NameCallback("Enter username: "),
                                               new PasswordCallback("Enter Password: ", false)};
        
        try {
            callbackHandler.handle(callbacks);
        } catch (IOException ioe) {
            throw new LoginException(ioe.getMessage());
        } catch (UnsupportedCallbackException uce) {
            throw new LoginException(uce.getMessage());
        }
        
        String name = ((NameCallback) callbacks[0]).getName();
        char[] pwd = ((PasswordCallback) callbacks[1]).getPassword();
        
        try {
            credentialValidator.validateCredential(name, pwd);
        } catch (CredentialValidationException e) {
            throw new LoginException (e.getMessage());
        }
        
        loginSuccess = true;
        
        this.name = name;
        this.pwd = pwd;
        
        return true;
    }

    public boolean logout() throws LoginException {
        subject.getPrincipals().remove(userPrincipal);
        subject.getPrivateCredentials().remove(httpAuthCredential);
        
        userPrincipal = null;
        httpAuthCredential = null;
        cleanLoginState();
        
        loginSuccess = false;
        commitSuccess = false;
        
        return true; 
    }
    
    private void cleanLoginState() {
        name = null;
        for (int i = 0; i < pwd.length; i++) {
            pwd[i] = ' ';
        }        
    }
}
