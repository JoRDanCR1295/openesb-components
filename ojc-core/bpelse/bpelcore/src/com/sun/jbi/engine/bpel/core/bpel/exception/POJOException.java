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
 * @(#)POJOException.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.exception;

import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 *
 * @author Sun Microsystems
 *
 */
public class POJOException extends BPELException {

    private String className;
    private String operationName;

    public POJOException(String className, String operationName, Throwable cause) {
        super(I18n.loc("BPCOR-6194: encountered exception: {0} while invoking: {1} on class: {2}",
                cause.getClass().getName(),
                operationName,
                className),
                cause);
        this.className = className;
        this.operationName = operationName;
    }

    public String getClassName() {
        return className;
    }

    public String getOperationName() {
        return operationName;
    }
}
