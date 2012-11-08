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
 * @(#)MethodLookupUtils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.JXPathException;

/**
 * Method lookup utilities, which find static and non-static methods as well
 * as constructors based on a name and list of parameters.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class MethodLookupUtils {

    private static final int NO_MATCH = 0;
    private static final int APPROXIMATE_MATCH = 1;
    private static final int EXACT_MATCH = 2;
    private static final Object[] EMPTY_ARRAY = new Object[0];

     public static Constructor lookupConstructor(
        Class targetClass,
        Object[] parameters) 
     {
        boolean tryExact = true;
        int count = parameters == null ? 0 : parameters.length;
        Class types[] = new Class[count];
        for (int i = 0; i < count; i++) {
            Object param = parameters[i];
            if (param != null) {
                types[i] = param.getClass();
            }
            else {
                types[i] = null;
                tryExact = false;
            }
        }

        Constructor constructor = null;

        if (tryExact) {
            // First - without type conversion
            try {
                constructor = targetClass.getConstructor(types);
                if (constructor != null) {
                    return constructor;
                }
            }
            catch (NoSuchMethodException ex) {
                // Ignore
            }
        }

        int currentMatch = 0;
        boolean ambiguous = false;

        // Then - with type conversion
        Constructor[] constructors = targetClass.getConstructors();
        for (int i = 0; i < constructors.length; i++) {
            int match =
                matchParameterTypes(
                    constructors[i].getParameterTypes(),
                    parameters);
            if (match != NO_MATCH) {
                if (match > currentMatch) {
                    constructor = constructors[i];
                    currentMatch = match;
                    ambiguous = false;
                }
                else if (match == currentMatch) {
                    ambiguous = true;
                }
            }
        }
        if (ambiguous) {
            throw new JXPathException(
                "Ambigous constructor " + Arrays.asList(parameters));
        }
        return constructor;
    }

    public static Method lookupStaticMethod(
        Class targetClass,
        String name,
        Object[] parameters) 
    {
        boolean tryExact = true;
        int count = parameters == null ? 0 : parameters.length;
        Class types[] = new Class[count];
        for (int i = 0; i < count; i++) {
            Object param = parameters[i];
            if (param != null) {
                types[i] = param.getClass();
            }
            else {
                types[i] = null;
                tryExact = false;
            }
        }

        Method method = null;

        if (tryExact) {
            // First - without type conversion
            try {
                method = targetClass.getMethod(name, types);
                if (method != null
                    && Modifier.isStatic(method.getModifiers())) {
                    return method;
                }
            }
            catch (NoSuchMethodException ex) {
                // Ignore
            }
        }

        int currentMatch = 0;
        boolean ambiguous = false;

        // Then - with type conversion
        Method[] methods = targetClass.getMethods();
        for (int i = 0; i < methods.length; i++) {
            if (Modifier.isStatic(methods[i].getModifiers())
                && methods[i].getName().equals(name)) {
                int match =
                    matchParameterTypes(
                        methods[i].getParameterTypes(),
                        parameters);
                if (match != NO_MATCH) {
                    if (match > currentMatch) {
                        method = methods[i];
                        currentMatch = match;
                        ambiguous = false;
                    }
                    else if (match == currentMatch) {
                        ambiguous = true;
                    }
                }
            }
        }
        if (ambiguous) {
            throw new JXPathException("Ambigous method call: " + name);
        }
        return method;
    }

    public static Method lookupMethod(
        Class targetClass,
        String name,
        Object[] parameters) 
    {
        if (parameters == null
            || parameters.length < 1
            || parameters[0] == null) {
            return null;
        }

        if (matchType(targetClass, parameters[0]) == NO_MATCH) {
            return null;
        }

        targetClass = TypeUtils.convert(parameters[0], targetClass).getClass();

        boolean tryExact = true;
        int count = parameters.length - 1;
        Class types[] = new Class[count];
        Object arguments[] = new Object[count];
        for (int i = 0; i < count; i++) {
            Object param = parameters[i + 1];
            arguments[i] = param;
            if (param != null) {
                types[i] = param.getClass();
            }
            else {
                types[i] = null;
                tryExact = false;
            }
        }

        Method method = null;

        if (tryExact) {
            // First - without type conversion
            try {
                method = targetClass.getMethod(name, types);
                if (method != null
                    && !Modifier.isStatic(method.getModifiers())) {
                    return method;
                }
            }
            catch (NoSuchMethodException ex) {
                // Ignore
            }
        }

        int currentMatch = 0;
        boolean ambiguous = false;

        // Then - with type conversion
        Method[] methods = targetClass.getMethods();
        for (int i = 0; i < methods.length; i++) {
            if (!Modifier.isStatic(methods[i].getModifiers())
                && methods[i].getName().equals(name)) {
                int match =
                    matchParameterTypes(
                        methods[i].getParameterTypes(),
                        arguments);
                if (match != NO_MATCH) {
                    if (match > currentMatch) {
                        method = methods[i];
                        currentMatch = match;
                        ambiguous = false;
                    }
                    else if (match == currentMatch) {
                        ambiguous = true;
                    }
                }
            }
        }
        if (ambiguous) {
            throw new JXPathException("Ambigous method call: " + name);
        }
        return method;
    }

    private static int matchParameterTypes(
        Class types[],
        Object parameters[]) 
    {
        int pi = 0;
        if (types.length >= 1
            && ExpressionContext.class.isAssignableFrom(types[0])) {
            pi++;
        }
        int length = parameters == null ? 0 : parameters.length;
        if (types.length != length + pi) {
            return NO_MATCH;
        }
        int totalMatch = EXACT_MATCH;
        for (int i = 0; i < length; i++) {
            int match = matchType(types[i + pi], parameters[i]);
            if (match == NO_MATCH) {
                return NO_MATCH;
            }
            if (match < totalMatch) {
                totalMatch = match;
            }
        }
        return totalMatch;
    }

    private static int matchType(Class expected, Object object) {
        if (object == null) {
            return APPROXIMATE_MATCH;
        }

        Class actual = object.getClass();

        if (expected.equals(actual)) {
            return EXACT_MATCH;
        }
        if (expected.isAssignableFrom(actual)) {
            return EXACT_MATCH;
        }

        if (TypeUtils.canConvert(object, expected)) {
            return APPROXIMATE_MATCH;
        }

        return NO_MATCH;
    }
}
