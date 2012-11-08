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
 * @(#)LocationPath.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.axes.InitialContext;

/**
 * @author Dmitri Plotnikov
 * @version  
 */
public class LocationPath extends Path {

    private boolean absolute;

    public LocationPath(boolean absolute, Step[] steps) {
        super(steps);
        this.absolute = absolute;
    }

    public boolean isAbsolute() {
        return absolute;
    }

    public boolean computeContextDependent() {
        if (!absolute) {
            return true;
        }

        return super.computeContextDependent();
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        Step steps[] = getSteps();
        if (steps != null) {
            for (int i = 0; i < steps.length; i++) {
                if (i > 0 || absolute) {
                    buffer.append('/');
                }
                buffer.append(steps[i]);
            }
        }
        return buffer.toString();
    }

    public Object compute(EvalContext context) {
        // Create a chain of contexts
        EvalContext rootContext;
        if (isAbsolute()) {
            rootContext = context.getRootContext().getAbsoluteRootContext();
        }
        else {
            rootContext = new InitialContext(context);
        }
        return evalSteps(rootContext);
    }


    public Object computeValue(EvalContext context) {
        // Create a chain of contexts
        EvalContext rootContext;
        if (isAbsolute()) {
            rootContext = context.getRootContext().getAbsoluteRootContext();
        }
        else {
            rootContext = new InitialContext(context);
        }
        return getSingleNodePointerForSteps(rootContext);
    }
}
