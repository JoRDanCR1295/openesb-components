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
 * @(#)HttpSoapFaultValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.soapvalidators;

import java.util.List;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPFault;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;

public class HttpSoapFaultValidator implements Validator {

    private static final Messages mMessages =
        Messages.getMessages(HttpSoapFaultValidator.class);
    
    public void validate(ExtensibilityElement element)
        throws ValidationException {

        SOAPFault fault = (SOAPFault)element;

        String name = fault.getName();
        if (name == null) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00286.Fault_name_missing"));
        }

//         List encodingStyles = fault.getEncodingStyles();
//         if (encodingStyles != null) {
//             // This is optional.  Don't verify contents at this point.
//         }

//         String namespace = fault.getNamespaceURI();
//         if (namespace != null) {
//             // This is optional.  We should verify that it is a valid URI, but
//             // I don't want to be too restrictive at this point.
//         }

        String use = fault.getUse();
        if (use != null) {
            if (!use.equals("literal") && !use.equals("encoded")) {
                throw new ValidationException(mMessages.getString("HTTPBC-E00284.Unsupported_use_attribute",
                        new Object[] { "<soap:fault>", use } ));
            }
        }
    }
}
