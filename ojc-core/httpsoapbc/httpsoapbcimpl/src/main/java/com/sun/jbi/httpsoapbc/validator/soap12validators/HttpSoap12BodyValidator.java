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
 * @(#)HttpSoapBodyValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.soap12validators;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap12.SOAP12Body;

import com.sun.jbi.httpsoapbc.validator.Visitable;
import com.sun.jbi.httpsoapbc.validator.Visitor;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.ValidationException;
import com.sun.jbi.wsdlvalidator.Validator;

public class HttpSoap12BodyValidator implements
            Validator,
            ValidatorObserver,
            Visitable,
            Visitor {

    private static final Messages mMessages =
        Messages.getMessages(HttpSoap12BodyValidator.class);
    
    private final List<ValidatorObserver> mObservers =
            new LinkedList<ValidatorObserver>();
    
    private final List<MessagePart> mParts =
            Collections.synchronizedList(new LinkedList<MessagePart>());
    
    public void validate(ExtensibilityElement element)
        throws ValidationException {

        SOAP12Body body = (SOAP12Body)element;



        String use = body.getUse();
        if (use != null) {
            if (!use.equals("literal") && !use.equals("encoded")) {
                throw new ValidationException(mMessages.getString("HTTPBC-E00284.Unsupported_use_attribute",
                        new Object[] { "<soap:body>", use } ));
            }
                
        }

        List parts = body.getParts();
        if (parts != null && parts.size() != 0) {
            mParts.clear();
            for (Iterator iter = parts.iterator(); iter.hasNext(); ) {
                Object part = iter.next();
                String value = (part == null ? null : part.toString());
                if (value == null || "".equals(value)) {
                    // parts are optional for SOAPBody; do nothing if it's missing
                    // http://wiki.open-esb.java.net/jbiwiki/Wiki.jsp?page=HTTPSOAPExtensibilityElements
                    ;
                } else {
                    MessagePart aPart = new MessagePart(null, value);
                    mParts.add(aPart);
                }
            }
            notifyObservers();
        }

    }

    public void attachObserver(ValidatorObserver observer) {
        if (observer != null) {
            synchronized (mObservers) {
                mObservers.remove(observer);
                mObservers.add(observer);
            }
        }
    }
    
    public void notify(Object subject) throws ValidationException {
        if (subject instanceof Visitable) {
            Visitable visitable = (Visitable) subject;
            visitable.accept(this);
        }
    }
    
    public void accept(Visitor visitor) throws ValidationException {
        visitor.visit(this);
    }

    public void visit(Validator subject) throws ValidationException {
        if (subject instanceof HttpSoap12HeaderValidator) {
            HttpSoap12HeaderValidator validator = (HttpSoap12HeaderValidator) subject;
            String subjectName = "<soap:header>";
            //checkPart(validator.getPart(), subjectName);   //disable this validation for now. The unqiue part validation will be moved to SOAPNormalizer.
            
        } else if (subject instanceof HttpSoap12HeaderFaultValidator) {
            HttpSoap12HeaderFaultValidator validator = (HttpSoap12HeaderFaultValidator) subject;
            String subjectName = "<soap:headerfault>";
            checkPart(validator.getPart(), subjectName);
        }
    }
    
    List<MessagePart> getParts() {
        List<MessagePart> list;
        synchronized (mParts) {
            list = new ArrayList<MessagePart>(mParts);
        }
        return list;
    }
    
    private void notifyObservers() throws ValidationException {
        synchronized (mObservers) {
            for (ValidatorObserver o : mObservers) {
                o.notify(this);
            }
        }
    }

    private void checkPart(MessagePart part, String subject) throws ValidationException {
        if (part != null) {
            synchronized (mParts) {
                // A SOAP Body with an empty parts list is taken to mean
                // that ALL parts are in use.  In such a case, then,
                //  there is a conflict automatically.
                if (mParts.isEmpty()) {
                    throw new ValidationException(
                            mMessages.getString(
                                "HTTPBC-E00285.Part_in_use",
                                new Object[] { part.getPartName(), "<soap:body>", subject } ));
                }
                for (MessagePart value : mParts) {
                    if (part.getPartName().equals(value.getPartName())) {
                        throw new ValidationException(
                                mMessages.getString(
                                    "HTTPBC-E00285.Part_in_use",
                                    new Object[] { part.getPartName(), "<soap:body>", subject } ));
                    }
                }
            }
        }
    }
}
