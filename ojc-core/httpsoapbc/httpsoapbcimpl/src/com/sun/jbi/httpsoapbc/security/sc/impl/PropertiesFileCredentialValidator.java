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
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class PropertiesFileCredentialValidator implements CredentialValidator {

    private static final Messages mMessages =
            Messages.getMessages(PropertiesFileCredentialValidator.class);
    private String mUniqueEndpointName;
    private String propertiesFileLocation = "";
    Properties propertyFile = null;

    public PropertiesFileCredentialValidator(String uniqueEndpointName, String propertiesFileLocation) throws CredentialValidationException {
        mUniqueEndpointName = uniqueEndpointName;
        this.propertiesFileLocation = propertiesFileLocation;
        getPasswordFromFile(propertiesFileLocation);
    }

    public void getPasswordFromFile(String filePath) throws CredentialValidationException {
        this.propertyFile = new Properties();
        if (filePath != null && filePath.length() > 0) {
            InputStream input = null;
            try {
                input = new FileInputStream(filePath);
                try {
                    propertyFile.load(input);
                } catch (IOException ex) {
                    Logger.getLogger(PropertiesFileCredentialValidator.class.getName()).log(Level.WARNING, ex.getMessage(), ex);
                } finally {
                    if (input != null) {
                        try {
                            input.close();
                        } catch (IOException e) {
                            Logger.getLogger(PropertiesFileCredentialValidator.class.getName()).log(Level.WARNING, e.getMessage(), e);
                        }
                    }
                }
            } catch (FileNotFoundException ex) {
                Logger.getLogger(PropertiesFileCredentialValidator.class.getName()).log(Level.WARNING, ex.getMessage(), ex);
                throw new CredentialValidationException(
                        mMessages.getString("HTTPBC-E01026.Failed_string_compare_authentication", new Object[]{filePath}));
            }
        }



    }

    public Subject validateCredential(String username, char[] password) throws CredentialValidationException {

        String passwordFromFile = null;
        if (propertiesFileLocation == null || propertiesFileLocation.length() < 1) {
            throw new CredentialValidationException(
                    mMessages.getString("HTTPBC-E01026.Failed_string_compare_authentication",
                    new Object[]{username}));
        }
        passwordFromFile = propertyFile.getProperty(username);


        String passwordFromrequest = String.valueOf(password);

        if (username != null && passwordFromFile != null && passwordFromrequest.equals(passwordFromFile)) {
            Subject subj = new Subject();
            subj.getPrincipals().add(new UserPrincipal(username));
            subj.getPrivateCredentials().add(new HTTPBasicAuthCredential(username, passwordFromFile.toCharArray()));
            return subj;

        } else {
            throw new CredentialValidationException(
                    mMessages.getString("HTTPBC-E01026.Failed_string_compare_authentication",
                    new Object[]{username}));
        }
    }

    public String getEndpointName() {
        return mUniqueEndpointName;
    }

    public String getPropertiesFileLocation() {
        return propertiesFileLocation;
    }
}
