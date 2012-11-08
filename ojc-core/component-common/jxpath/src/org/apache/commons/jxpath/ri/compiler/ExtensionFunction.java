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
 * @(#)ExtensionFunction.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import java.util.Arrays;

import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.QName;

/**
 * Represents  an element of the parse tree representing an extension function
 * call.
 *
 * @author Dmitri Plotnikov
 * @version
 */
public class ExtensionFunction extends Operation {

    private QName functionName;

    public ExtensionFunction(QName functionName, Expression args[]) {
        super(args);
        this.functionName = functionName;
    }

    public QName getFunctionName() {
        return functionName;
    }

    /**
     * An extension function gets the current context, therefore it MAY be
     * context dependent.
     */
    public boolean computeContextDependent() {
        return true;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append(functionName);
        buffer.append('(');
        Expression args[] = getArguments();
        if (args != null) {
            for (int i = 0; i < args.length; i++) {
                if (i > 0) {
                    buffer.append(", ");
                }
                buffer.append(args[i]);
            }
        }
        buffer.append(')');
        return buffer.toString();
    }

    public Object compute(EvalContext context) {
        return computeValue(context);
    }

    public Object computeValue(EvalContext context) {
        Object[] parameters = null;
        if (args != null) {
            parameters = new Object[args.length];
            for (int i = 0; i < args.length; i++) {
                // don't evalute parameters yet, let function do it
                //parameters[i] = convert(args[i].compute(context));
                parameters[i] = args[i].compute(context);
            }
        }

        Function function =
            context.getRootContext().getFunction(functionName, parameters);
        if (function == null) {
            throw new JXPathException(
                "No such function: "
                    + functionName
                    + Arrays.asList(parameters));
        }

        return function.invoke(context, parameters);
    }

    private Object convert(Object object) {
        if (object instanceof EvalContext) {
            return ((EvalContext) object).getValue();
        }
        return object;
    }
}
