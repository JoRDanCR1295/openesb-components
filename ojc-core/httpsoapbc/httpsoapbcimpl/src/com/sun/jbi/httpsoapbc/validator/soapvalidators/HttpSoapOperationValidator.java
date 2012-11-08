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
 * @(#)HttpSoapOperationValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.soapvalidators;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPOperation;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;

public class HttpSoapOperationValidator implements Validator {

    private static final Messages mMessages =
        Messages.getMessages(HttpSoapOperationValidator.class);
    
    public void validate(ExtensibilityElement element)
        throws ValidationException {

        SOAPOperation operation = (SOAPOperation)element;
//         String soapActionURI = operation.getSoapActionURI();
//         if (soapActionURI != null) {
//             // This is fine.  The URI can be anything.  In reality,
//             // we should verify that this is a valid URI, but I don't want
//             // to be too restrictive.
//         }

        String style = operation.getStyle();
        if (style != null) {
            if (!style.equals("rpc") && !style.equals("document")) {
                throw new ValidationException(mMessages.getString("HTTPBC-E00283.Unsupported_style_attribute",
                        new Object[] { "<soap:operation>", style }));
            }
        }
    }
}
