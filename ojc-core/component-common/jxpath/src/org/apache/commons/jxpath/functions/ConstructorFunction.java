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
 * @(#)ConstructorFunction.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.functions;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.util.TypeUtils;

/**
 * An extension function that creates an instance using a constructor.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class ConstructorFunction implements Function {

    private Constructor constructor;
    private static final Object EMPTY_ARRAY[] = new Object[0];

    public ConstructorFunction(Constructor constructor) {
        this.constructor = constructor;
    }

    /**
     * Converts parameters to suitable types and invokes the constructor.
     */
    public Object invoke(ExpressionContext context, Object[] parameters) {
        try {
            Object[] args;
            if (parameters == null) {
                parameters = EMPTY_ARRAY;
            }
            int pi = 0;
            Class types[] = constructor.getParameterTypes();
            if (types.length > 0
                && ExpressionContext.class.isAssignableFrom(types[0])) {
                pi = 1;
            }
            args = new Object[parameters.length + pi];
            if (pi == 1) {
                args[0] = context;
            }
            for (int i = 0; i < parameters.length; i++) {
                args[i + pi] = TypeUtils.convert(parameters[i], types[i + pi]);
            }
            return constructor.newInstance(args);
        }
        catch (Throwable ex) {
            if (ex instanceof InvocationTargetException) {
                ex = ((InvocationTargetException) ex).getTargetException();
            }
            throw new JXPathException(
                "Cannot invoke constructor " + constructor,
                ex);
        }
    }
}
