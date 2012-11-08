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
 * @(#)MethodFunction.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.functions;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.util.TypeUtils;
import org.apache.commons.jxpath.util.ValueUtils;

/**
 * An XPath extension function implemented as an individual Java method.
 * 
 * @author Dmitri Plotnikov
 * @version  
 */
public class MethodFunction implements Function {

    private Method method;
    private static final Object EMPTY_ARRAY[] = new Object[0];

    public MethodFunction(Method method) {
        this.method = ValueUtils.getAccessibleMethod(method);
    }

    public Object invoke(ExpressionContext context, Object[] parameters) {
        try {
            Object target;
            Object[] args;
            if (Modifier.isStatic(method.getModifiers())) {
                target = null;
                if (parameters == null) {
                    parameters = EMPTY_ARRAY;
                }
                int pi = 0;
                Class types[] = method.getParameterTypes();
                if (types.length >= 1
                    && ExpressionContext.class.isAssignableFrom(types[0])) {
                    pi = 1;
                }
                args = new Object[parameters.length + pi];
                if (pi == 1) {
                    args[0] = context;
                }
                for (int i = 0; i < parameters.length; i++) {
                    args[i + pi] =
                        TypeUtils.convert(parameters[i], types[i + pi]);
                }
            }
            else {
                int pi = 0;
                Class types[] = method.getParameterTypes();
                if (types.length >= 1
                    && ExpressionContext.class.isAssignableFrom(types[0])) {
                    pi = 1;
                }
                target =
                    TypeUtils.convert(
                        parameters[0],
                        method.getDeclaringClass());
                args = new Object[parameters.length - 1 + pi];
                if (pi == 1) {
                    args[0] = context;
                }
                for (int i = 1; i < parameters.length; i++) {
                    args[pi + i - 1] =
                        TypeUtils.convert(parameters[i], types[i + pi - 1]);
                }
            }

            return method.invoke(target, args);
        }
        catch (Throwable ex) {
            if (ex instanceof InvocationTargetException) {
                ex = ((InvocationTargetException) ex).getTargetException();
            }
            throw new JXPathException("Cannot invoke " + method, ex);
        }
    }
    
    public String toString() {
        return method.toString();
    }
}
