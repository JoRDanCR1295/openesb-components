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
 * @(#)HttpSoapBindingValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.soapvalidators;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPBinding;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;

public class HttpSoapBindingValidator implements Validator {
    private static final Messages mMessages =
        Messages.getMessages(HttpSoapBindingValidator.class);

    public void validate(ExtensibilityElement element)
        throws ValidationException {

        SOAPBinding binding = (SOAPBinding)element;
        String transportURI = binding.getTransportURI();
        if (transportURI == null) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00281.Transport_URI_required"));
        } else if (!transportURI.equals("http://schemas.xmlsoap.org/soap/http")) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00282.Unsupported_transport", transportURI));
        }
            
        String style = binding.getStyle();
        if (style != null) {
            if (!style.equals("rpc") && !style.equals("document")) {
                throw new ValidationException(mMessages.getString("HTTPBC-E00283.Unsupported_style_attribute",
                        new Object[] { "<soap:binding>", style } ));
            }
        }
    }
}
