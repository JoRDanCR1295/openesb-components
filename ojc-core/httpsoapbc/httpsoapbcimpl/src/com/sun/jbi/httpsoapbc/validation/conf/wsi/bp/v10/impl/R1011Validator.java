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
 * @(#)R1011Validator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.v10.impl;

import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;

import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.Statement;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.ValidationResult;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIConstants;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIValidationException;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.soap.api.SOAPStatementValidator;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.impl.ValidationResultImpl;

/**
 * Validator for R1011
 *
 */
public class R1011Validator implements SOAPStatementValidator {
    Statement R1011 = new R1011Statement();
    String level;
    
    public R1011Validator(String level) {
        this.level = level;
    }
    
    public String getValidationLevel() {
        return level;
    }

    public void setValidationLevel(String level) {
        this.level = level;
    }

    public ValidationResult validate(SOAPMessage msg) throws WSIValidationException {
        if (level.equals(WSIConstants.IGNORE)) {
            return new ValidationResultImpl();
        }
        
        try {           
            SOAPBody body = msg.getSOAPBody();
            org.w3c.dom.Node domNode = body.getNextSibling();
            while (domNode != null) {
                if (domNode.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
                    //validation failed
                    return new ValidationResultImpl(false, 
                                                    R1011,
                                                    R1011.toString());                    
                }
                
                domNode = domNode.getNextSibling();
            }
            
            //validation success
    		return new ValidationResultImpl();
        } catch (SOAPException e) {
            throw new WSIValidationException (e);
        }
    }
}
