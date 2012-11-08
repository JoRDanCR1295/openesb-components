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
 * @(#)HttpAddressValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.httpvalidators;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;
import com.sun.jbi.httpsoapbc.validator.AbstractValidator;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.http.HTTPAddress;

public class HttpAddressValidator extends AbstractValidator implements Validator {
    private static final String HTTP_DEFAULT_PORT_TOKEN = "${HttpDefaultPort}";
    private static final String HTTPS_DEFAULT_PORT_TOKEN = "${HttpsDefaultPort}";
    
    private static final Messages mMessages =
        Messages.getMessages(HttpAddressValidator.class);
    
    public HttpAddressValidator() {
        super();
    }
    
    public HttpAddressValidator(RuntimeConfigurationMBean runtimeConfig, boolean resolveTokens) {
        super(runtimeConfig, resolveTokens);
    }

    public void validate(ExtensibilityElement element)
        throws ValidationException {

        HTTPAddress address = (HTTPAddress)element;

        String location = address.getLocationURI();
        if (location == null || "".equals(location)) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00273.Address_required"));
        }
        
        ////////////////////////////////////////////////////////
        // GSR changed for Java EE Service Engine
        // As instructed by Jerry Waldorf.
        ////////////////////////////////////////////////////////
        if (location.equals("REPLACE_WITH_ACTUAL_URL")) {
            return;
        }
        
         ///////////////////////////////////////////////////////
        // Check for valid tokens for default HTTP and HTTPS port
        // Introduced to support clustering
        ////////////////////////////////////////////////////////
        
        if (location.indexOf(HTTP_DEFAULT_PORT_TOKEN, 6) > 0) {
            int colonIndex = -1;
            int contextStartIndex = -1;
            
            if (location.startsWith("https://")) {
                throw new ValidationException(mMessages.getString("HTTPBC-E00276.Http_var_protocol_mismatch",
                        new Object[] { HTTP_DEFAULT_PORT_TOKEN, "https" }));
            }
            
            if (location.startsWith("http://")) {
                // look for ${HttpDefaultPort} token 
                colonIndex = location.indexOf(":", 6);
                contextStartIndex = location.indexOf("/", 7);
                
                if (!HTTP_DEFAULT_PORT_TOKEN.equals(location.substring(colonIndex + 1, contextStartIndex))) {
                    throw new ValidationException(mMessages.getString("HTTPBC-E00277.Http_var_invalid",
                            new Object[] { HTTP_DEFAULT_PORT_TOKEN, location }));
                }
            }
        }
        
        if (location.indexOf(HTTPS_DEFAULT_PORT_TOKEN, 7) > 0) {
            int colonIndex = -1;
            int contextStartIndex = -1;
            
            if (location.startsWith("http://")) {
                throw new ValidationException(mMessages.getString("HTTPBC-E00276.Http_var_protocol_mismatch",
                        new Object[] { HTTPS_DEFAULT_PORT_TOKEN, "http" }));
            }
            if (location.startsWith("https://")) {
                // look for ${HttpDefaultPort} token 
                colonIndex = location.indexOf(":", 7);
                contextStartIndex = location.indexOf("/", 8);
                
                if (!HTTPS_DEFAULT_PORT_TOKEN.equals(location.substring(colonIndex + 1, contextStartIndex))) {
                    throw new ValidationException(mMessages.getString("HTTPBC-E00277.Http_var_invalid",
                            new Object[] { HTTPS_DEFAULT_PORT_TOKEN, location } ));
                }
            }
        }

        try {
            if (!mResolveTokens) {
                updateAppVariableMap(location, "STRING");
            } else {
                String newLocation = resolveEmbeddedTokensInURL(location);   // resolve any tokens in the URL location
                newLocation = resolveHostNameInUrl(newLocation);	     // resolve "localhost" in the URL location
                validateLocationURI(newLocation);
                address.setLocationURI(newLocation);
            }
        } catch (Exception ex) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00275.Address_unsupported", new Object[] { location, ex.getLocalizedMessage() }));
        }
    }
}
