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
 * @(#)KeywordVariables.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.servlet;

import org.apache.commons.jxpath.Variables;

/**
 * Implementation of the Variables interface that provides access
 * to a single object using a reserved name (keyword).
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class KeywordVariables implements Variables {

    private String keyword;
    private Object object;

    public KeywordVariables(String keyword, Object object) {
        this.keyword = keyword;
        this.object = object;
    }

    public boolean isDeclaredVariable(String variable) {
        return variable.equals(keyword);
    }

    public Object getVariable(String variable) {
        if (variable.equals(keyword)) {
            return object;
        }
        return null;
    }

    public void declareVariable(String variable, Object value) {
        throw new UnsupportedOperationException(
            "Cannot declare new keyword variables.");
    }

    public void undeclareVariable(String variable) {
        throw new UnsupportedOperationException(
            "Cannot declare new keyword variables.");
    }
}
