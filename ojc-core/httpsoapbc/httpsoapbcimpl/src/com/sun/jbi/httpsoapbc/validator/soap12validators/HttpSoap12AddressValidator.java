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
 * @(#)HttpSoap12AddressValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.soap12validators;


import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.validator.AbstractValidator;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap12.SOAP12Address;

/**
 * 
 * @author Sujit Biswas
 *
 */
public class HttpSoap12AddressValidator extends AbstractValidator implements Validator {
    private static final Messages mMessages =
        Messages.getMessages(HttpSoap12AddressValidator.class);
    
    public HttpSoap12AddressValidator() {
        super();
    }
    
    public HttpSoap12AddressValidator(RuntimeConfigurationMBean runtimeConfig, boolean resolveTokens) {
        super(runtimeConfig, resolveTokens);
    }
    
    public void validate(ExtensibilityElement element)
        throws ValidationException {

        SOAP12Address address = (SOAP12Address)element;
        String location = address.getLocationURI();
        if (location == null || "".equals(location)) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00290.Location_missing", "<soap:address>"));
        }
        
        
        ////////////////////////////////////////////////////////
        // GSR changed for Java EE Service Engine
        // As instructed by Jerry Waldorf.
        ////////////////////////////////////////////////////////
        if("REPLACE_WITH_ACTUAL_URL".equals(location)) {
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
                throw new ValidationException(mMessages.getString("HTTPBC-E00293.Address_protocol_mismatch",
                        new Object[] { HTTP_DEFAULT_PORT_TOKEN, location, "https" } ));
            }
            if (location.startsWith("http://")) {
                // look for ${HttpDefaultPort} token 
                colonIndex = location.indexOf(":", 6);
                contextStartIndex = location.indexOf("/", 7);
                
                if (!HTTP_DEFAULT_PORT_TOKEN.equals(location.substring(colonIndex + 1, contextStartIndex))) {
                    throw new ValidationException(mMessages.getString("HTTPBC-E00294.Token_wrong_position",
                            new Object[] { HTTP_DEFAULT_PORT_TOKEN, location } ));
                }
            }
        }
        
        if (location.indexOf(HTTPS_DEFAULT_PORT_TOKEN, 7) > 0) {
            int colonIndex = -1;
            int contextStartIndex = -1;
            
            if (location.startsWith("http://")) {
                throw new ValidationException(mMessages.getString("HTTPBC-E00293.Address_protocol_mismatch",
                        new Object[] { HTTPS_DEFAULT_PORT_TOKEN, location, "http" } ));
            }
            if (location.startsWith("https://")) {
                // look for ${HttpDefaultPort} token 
                colonIndex = location.indexOf(":", 7);
                contextStartIndex = location.indexOf("/", 8);
                
                if (!HTTPS_DEFAULT_PORT_TOKEN.equals(location.substring(colonIndex + 1, contextStartIndex))) {
                    throw new ValidationException(mMessages.getString("HTTPBC-E00294.Token_wrong_position",
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
            throw new ValidationException(mMessages.getString("HTTPBC-E00291.Location_unsupported",
                    new Object[] { "<soap:address>", location, ex.getLocalizedMessage() }));
        }
            
    }
    
     
}
