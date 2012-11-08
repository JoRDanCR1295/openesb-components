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
 * @(#)VariableReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.EvalContext;

/**
 * An element of the compile tree holding a variable reference.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class VariableReference extends Expression {

    private QName varName;

    public VariableReference(QName varName) {
        this.varName = varName;
    }

    public QName getVariableName() {
        return varName;
    }

    public String toString() {
        return "$" + varName;
    }

    public boolean isContextDependent() {
        return false;
    }

    public boolean computeContextDependent() {
        return false;
    }

    public Object compute(EvalContext context) {
        return computeValue(context);
    }

    /**
     * Returns the value of the variable.
     */
    public Object computeValue(EvalContext context) {
        return context.getRootContext().getVariableContext(varName);
    }
}
