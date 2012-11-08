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
 * @(#)HttpBindingValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.httpvalidators;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.http.HTTPBinding;

import com.sun.jbi.httpsoapbc.HttpEndpoint;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;

public class HttpBindingValidator implements Validator {
    private static final Messages mMessages =
        Messages.getMessages(HttpBindingValidator.class);

    public void validate(ExtensibilityElement element)
        throws ValidationException {

        HTTPBinding binding = (HTTPBinding)element;
        String httpVerb = binding.getVerb();
        if (httpVerb == null) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00271.Verb_required"));
        } else if (!httpVerb.equals(HttpEndpoint.HTTP_BINDING_VERB_GET) && !httpVerb.equals(HttpEndpoint.HTTP_BINDING_VERB_POST)) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00272.Verb_unsupported", httpVerb));
        }
    }
}
