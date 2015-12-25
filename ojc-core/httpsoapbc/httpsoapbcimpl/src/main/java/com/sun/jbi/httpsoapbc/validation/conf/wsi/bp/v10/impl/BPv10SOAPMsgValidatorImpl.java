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
 * @(#)BPv10SOAPMsgValidatorImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.v10.impl;

import java.util.ArrayList;
import java.util.List;

import javax.xml.soap.SOAPMessage;

import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.ValidationResult;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIConstants;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.WSIValidationException;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.soap.api.SOAPStatementValidator;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.soap.api.SOAPMsgValidator;
import com.sun.jbi.internationalization.Messages;

/**
 * The SOAP message validator for WSI Basic Profile version 1.0.
 *
 */
public class BPv10SOAPMsgValidatorImpl implements SOAPMsgValidator {
    private static final Messages messages =
        Messages.getMessages(BPv10SOAPMsgValidatorImpl.class);
    
    private List wsiBP10Validators;

    private List errors = new ArrayList();

    private List warnings = new ArrayList();

    private String errorMsg;

    private String warnMsg;

    private String level;

    public BPv10SOAPMsgValidatorImpl(String level) {
        // Create the list of validators to test for WSI BP 1.0 conformance.
        wsiBP10Validators = new ArrayList(2);
        wsiBP10Validators.add(new R1011Validator(level));
        wsiBP10Validators.add(new R1013Validator(level));
        this.level = level;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    public List getErrors() {
        return errors;
    }

    public String getWarningMsg() {
        return warnMsg;
    }

    public List getWarnings() {
        return warnings;
    }

    public void setValidationLevel(String level) {
        this.level = level;
        for (int i = 0; i < wsiBP10Validators.size(); i++) {
            ((SOAPStatementValidator) wsiBP10Validators.get(i)).setValidationLevel(level);
        }
    }

    public String getValidationLevel() {
        return level;
    }

    public List getValidators() {
        return wsiBP10Validators;
    }

    public void validate(SOAPMessage msg) throws WSIValidationException {
        errors.clear();
        warnings.clear();

        errorMsg = null;
        warnMsg = null;

        SOAPStatementValidator validator = null;
        ValidationResult result = null;
        String valLevel = null;
        for (int i = 0; i < wsiBP10Validators.size(); i++) {
            validator = (SOAPStatementValidator) wsiBP10Validators.get(i);

            valLevel = validator.getValidationLevel();
            if (!valLevel.equalsIgnoreCase(WSIConstants.IGNORE)) {
                result = validator.validate(msg);

                if (!result.getStatus()) {
                    if (valLevel.equalsIgnoreCase(WSIConstants.ERROR)) {
                        errors.add(result);
                    } else if (valLevel.equalsIgnoreCase(WSIConstants.WARN)) {
                        warnings.add(result);
                    }
                }
            }
        }

        if (errors.size() > 0) {
            StringBuffer errorBuf = new StringBuffer(messages.getString("WSI_BP_10_Error"));

            for (int i = 0; i < errors.size(); i++) {
                errorBuf.append("\n")
                        .append(((ValidationResult) errors.get(i)).getErrorMsg());
            }

            errorMsg = errorBuf.toString();
        }

        if (warnings.size() > 0) {
            StringBuffer warnBuf = new StringBuffer(messages.getString("WSI_BP_10_Warning"));

            for (int i = 0; i < warnings.size(); i++) {
                warnBuf.append("\n")
                       .append(((ValidationResult) warnings.get(i)).getErrorMsg());
            }

            warnMsg = warnBuf.toString();
        }
    }
}
