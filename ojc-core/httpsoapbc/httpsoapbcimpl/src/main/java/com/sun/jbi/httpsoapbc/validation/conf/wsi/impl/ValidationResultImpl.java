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
 * @(#)ValidationResultImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validation.conf.wsi.impl;

import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.Statement;
import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.ValidationResult;

/**
 * An implementation for WSI normative statement conformance validation result. 
 *
 */
public class ValidationResultImpl implements ValidationResult {

    private boolean status;

    private Statement statement;

    private String errorMsg;

    public ValidationResultImpl() {
        this.status = true;
    }

    public ValidationResultImpl(boolean status, Statement statement,
            String errorMsg) {
        this.status = status;
        this.statement = statement;
        this.errorMsg = errorMsg;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    public Statement getStatement() {
        return statement;
    }

    public boolean getStatus() {
        return status;
    }
}
