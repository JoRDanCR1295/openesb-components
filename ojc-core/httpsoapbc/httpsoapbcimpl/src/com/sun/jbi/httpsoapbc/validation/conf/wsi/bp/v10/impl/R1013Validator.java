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
 * @(#)R1013Validator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.v10.impl;

import java.util.Iterator;

import javax.xml.soap.Name;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPHeaderElement;
import javax.xml.soap.SOAPMessage;

import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.Statement;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.ValidationResult;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIConstants;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIValidationException;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.soap.api.SOAPStatementValidator;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.impl.ValidationResultImpl;

/**
 * Validator for R1013
 *
 */
public class R1013Validator implements SOAPStatementValidator {
    Statement R1013 = new R1013Statement();
    String level;
    
    public R1013Validator(String level) {
        this.level = level;
    }
    
    public String getValidationLevel() {
         return level;
    }

    public void setValidationLevel(String level) {
        this.level = level;
    }

    public ValidationResult validate(SOAPMessage msg) throws WSIValidationException {
        if (level.equalsIgnoreCase(WSIConstants.IGNORE)) {
            return new ValidationResultImpl();
        }
        
        try {
            SOAPHeader header = msg.getSOAPHeader();
            SOAPHeaderElement e = null;
            Name name = null;
            String value = null;
            if (header != null) {
                for (Iterator it1 = header.examineAllHeaderElements(); it1.hasNext();) {
                    e = (SOAPHeaderElement) it1.next();
                    for (Iterator it2 = e.getAllAttributes(); it2.hasNext();) {
                        name = (Name) it2.next();
                        if (name.getLocalName().equalsIgnoreCase("mustUnderstand")) {
                            value = e.getAttributeValue(name);
                            if (value != null && (value.equals("1") || value.equals("0"))) {
                                //valid value; break out to check the next header element
                                break;
                            } else if (value != null && (value.equals("true") || value.equals("false"))) {
                                //relax validation - we know that, at the least, the wsit AT client uses 
                                //true/false instead of 0/1 - see wsit AT TxClientPipe.java
                                break;
                            } else {
                                //validation failed
                                return new ValidationResultImpl(false, 
                                                                R1013, 
                                                                new StringBuffer("Header Element: ")
                                                                    .append(e.getElementName().getQualifiedName())
                                                                    .append(" - ")
                                                                    .append(R1013).toString());
                            }
                        }
                    }
                }
            }
        } catch (SOAPException e) {
                throw new WSIValidationException(e);
        }

        //validation success
        return new ValidationResultImpl();
    }
}
