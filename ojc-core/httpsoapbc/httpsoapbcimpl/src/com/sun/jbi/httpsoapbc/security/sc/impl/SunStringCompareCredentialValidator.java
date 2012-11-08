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
 * @(#)SunAccessManagerCredentialValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.sc.impl;

import javax.security.auth.Subject;

import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidator;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;
import com.sun.jbi.httpsoapbc.security.impl.UserPrincipal;

import com.sun.jbi.internationalization.Messages;

public class SunStringCompareCredentialValidator implements CredentialValidator {

    private static final Messages mMessages =
    Messages.getMessages(SunStringCompareCredentialValidator.class);

    private String mUniqueEndpointName;
    private String mUsername;
    private char [] mPassword;
    
    public SunStringCompareCredentialValidator(String uniqueEndpointName,
                                               String username, 
                                               char [] password) {
        mUniqueEndpointName = uniqueEndpointName;
        mUsername = username;
        mPassword = password;
    }

    public Subject validateCredential(String username, char[] password) throws CredentialValidationException {
        if (mUsername != null && password != null) {
            if (mUsername.equals(username) && String.valueOf(mPassword).equals(String.valueOf(password))) {
                Subject subj = new Subject();
                subj.getPrincipals().add(new UserPrincipal(mUsername));
                subj.getPrivateCredentials().add(new HTTPBasicAuthCredential(mUsername, mPassword));
                return subj;
            } else {
                throw new CredentialValidationException (
                        mMessages.getString("HTTPBC-E01026.Failed_string_compare_authentication",
                                            new Object [] {username}));                
            }
        } else {
            throw new CredentialValidationException (
                    mMessages.getString("HTTPBC-E01027.Failed_string_compare_invalid_config",
                                        new Object [] {username}));
        }
    }
    
    public String getUsername() { return mUsername;}
    
    public char [] getPassword() {return mPassword;}
    
    public String getEndpointName() {return mUniqueEndpointName;}
}
