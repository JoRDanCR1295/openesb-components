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
 * @(#)HttpSoapHeaderFaultValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.soap12validators;

import com.sun.jbi.httpsoapbc.validator.Visitable;
import com.sun.jbi.httpsoapbc.validator.Visitor;
import java.util.List;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap12.SOAP12HeaderFault;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidationException;
import java.util.LinkedList;

public class HttpSoap12HeaderFaultValidator implements
            Validator,
            ValidatorObserver,
            Visitable,
            Visitor {

    private static final Messages mMessages =
        Messages.getMessages(HttpSoap12HeaderFaultValidator.class);
    
    private final List<ValidatorObserver> mObservers =
            new LinkedList<ValidatorObserver>();
    
    private MessagePart mPart;
    
    public void validate(ExtensibilityElement element)
        throws ValidationException {

        mPart = null;
        SOAP12HeaderFault headerFault = (SOAP12HeaderFault)element;

        QName message = headerFault.getMessage();
        if (message == null) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00287.Message_missing", "<soap:headerfault>"));
        }

        String part = headerFault.getPart();
        if (part == null || "".equals(part)) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00288.Part_missing", "<soap:headerfault>"));
        } else {
            synchronized (this) {
                mPart = new MessagePart(headerFault.getMessage(), part);
            }
            notifyObservers();
        }

        String use = headerFault.getUse();
        if (use == null) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00289.Use_missing", "<soap:headerfault>"));
        } else if (!use.equals("literal") && !use.equals("encoded")) {
            throw new ValidationException(mMessages.getString("HTTPBC-E00284.Unsupported_use_attribute",
                    new Object[] { "<soap:headerfault>", use } ));
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
            ((Visitable) subject).accept(this);
        }
    }
    
    MessagePart getPart() {
        synchronized (this) {
            return mPart;
        }
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
            MessagePart aPart;
            synchronized (this) {
                aPart = mPart;
            }
            if (part.equals(aPart)) {
                throw new ValidationException(
                        mMessages.getString(
                            "HTTPBC-E00285.Part_in_use",
                            new Object[] { part.getPartName(), "<soap:headerfault>", subject } ));
            }
        }
    }
    
    private void checkParts(List<MessagePart> parts, String subject) throws ValidationException {
        MessagePart aPart;
        synchronized (this) {
            aPart = mPart;
        }
        if (aPart == null) {
            return;
        }
        // A SOAP Body with an empty parts list is taken to mean
        // that ALL parts are in use.  In such a case, then,
        // there is a conflict automatically.
        if (parts.isEmpty()) {
            throw new ValidationException(
                    mMessages.getString(
                        "HTTPBC-E00285.Part_in_use",
                        new Object[] { aPart.getPartName(), "<soap:headerfault>", subject } ));
        }
        for (MessagePart part : parts) {
            if (part != null && part.getPartName().equals(aPart.getPartName())) {
                throw new ValidationException(
                        mMessages.getString(
                            "HTTPBC-E00285.Part_in_use",
                            new Object[] { part.getPartName(), "<soap:headerfault>", subject } ));
            }
        }
    }

    public void accept(Visitor visitor) throws ValidationException {
        visitor.visit(this);
    }

    public void visit(Validator subject) throws ValidationException {
        if (subject instanceof HttpSoap12HeaderValidator) {
            HttpSoap12HeaderValidator validator = (HttpSoap12HeaderValidator) subject;
            String subjectName = "<soap:header>";
            checkPart(validator.getPart(), subjectName);
            
        } else if (subject instanceof HttpSoap12BodyValidator) {
            HttpSoap12BodyValidator validator = (HttpSoap12BodyValidator) subject;
            String subjectName = "<soap:body>";
            checkParts(validator.getParts(), subjectName);
        }
    }
}
