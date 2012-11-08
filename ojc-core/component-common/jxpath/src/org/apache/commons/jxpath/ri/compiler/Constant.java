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
 * @(#)Constant.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.EvalContext;

/**
 * A compile tree element containing a constant number or string.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class Constant extends Expression {

    private Object value;

    public Constant(Number number) {
        this.value = number;
    }

    public Constant(String string) {
        this.value = string;
    }

    public Object compute(EvalContext context) {
        return value;
    }

    /**
     * Returns the value of the constant.
     */
    public Object computeValue(EvalContext context) {
        return value;
    }

    /**
     * Returns false
     */
    public boolean isContextDependent() {
        return false;
    }

    /**
     * Returns false
     */
    public boolean computeContextDependent() {
        return false;
    }

    public String toString() {
        if (value instanceof Number) {
            double doubleValue = ((Number) value).doubleValue();
            long longValue = ((Number) value).longValue();
            if (doubleValue == longValue) {
                return String.valueOf(longValue);
            }
            else {
                return String.valueOf(doubleValue);
            }
        }
        else {
            return "'" + value + "'";
        }
    }
}
