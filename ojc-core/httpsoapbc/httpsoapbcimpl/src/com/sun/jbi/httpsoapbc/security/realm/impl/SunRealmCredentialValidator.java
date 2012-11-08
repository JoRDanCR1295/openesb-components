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
 * @(#)SunRealmCredentialsValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.httpsoapbc.security.realm.impl;

import javax.security.auth.Subject;

import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidator;

import com.sun.appserv.security.ProgrammaticLogin;
import com.sun.enterprise.security.SecurityContext;

import com.sun.jbi.internationalization.Messages;

/**
 *
 * Credentials validation based on Sun Application Server Realms
 */
public class SunRealmCredentialValidator implements CredentialValidator{
    
    private static final Messages mMessages =
        Messages.getMessages(SunRealmCredentialValidator.class);
    
    private String realm;
    private ProgrammaticLogin progLogin;
    
    public SunRealmCredentialValidator(String realm) {
        this.realm = realm;
        this.progLogin = new ProgrammaticLogin();
    }

    public String getRealmName () {return realm;}
    
    public Subject validateCredential(String username, char[] password) 
    throws CredentialValidationException {
        try {
            Boolean result = progLogin.login(username, String.valueOf(password), realm, true);
            if (result.booleanValue()) {
                return SecurityContext.getCurrent().getSubject();            
            } else {
                throw new CredentialValidationException (
                        mMessages.getString("HTTPBC-E01025.Failed_realm_authentication",
                                            new Object [] {username, realm}));
            }
        } catch (Throwable t) {
            throw new CredentialValidationException (t);
        }
    }

}
