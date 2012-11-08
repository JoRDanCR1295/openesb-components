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
 * @(#)ClassFunctions.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Set;

import org.apache.commons.jxpath.functions.ConstructorFunction;
import org.apache.commons.jxpath.functions.MethodFunction;
import org.apache.commons.jxpath.util.MethodLookupUtils;

/**
 * Extension functions provided by a Java class.
 *
 * Let's say, we declared a ClassFunction like this:
 * <blockquote><pre>
 *     new ClassFunctions(Integer.class, "int")
 * </pre></blockquote>
 *
 * We can now use XPaths like:
 * <dl>
 *  <dt><code>"int:new(3)"</code></dt>
 *  <dd>Equivalent to <code>new Integer(3)</code></dd>
 *  <dt><code>"int:getInteger('foo')"</code></dt>
 *  <dd>Equivalent to <code>Integer.getInteger("foo")</code></dd>
 *  <dt><code>"int:floatValue(int:new(4))"</code></dt>
 *  <dd>Equivalent to <code>new Integer(4).floatValue()</code></dd>
 * </dl>
 *
 * <p>
 * If the first argument of a method is ExpressionContext, the
 * expression context in which the function is evaluated is passed to
 * the method.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class ClassFunctions implements Functions {
    private Class functionClass;
    private String namespace;
    private static final Object[] EMPTY_ARRAY = new Object[0];

    public ClassFunctions(Class functionClass, String namespace) {
        this.functionClass = functionClass;
        this.namespace = namespace;
    }

    /**
     * Returns a set of one namespace - the one specified in the constructor.
     *
     * @returns a singleton
     */
    public Set getUsedNamespaces() {
        return Collections.singleton(namespace);
    }

    /**
     * Returns a Function, if any, for the specified namespace,
     * name and parameter types.
     *
     * @param namespace if it is not the namespace specified in the constructor,
     *     the method returns null
     * @param name is a function name or "new" for a constructor.
     *
     * @return a MethodFunction, a ConstructorFunction or null if there is no
     *      such function.
     */
    public Function getFunction(
        String namespace,
        String name,
        Object[] parameters) 
    {
        if (!namespace.equals(this.namespace)) {
            return null;
        }

        if (parameters == null) {
            parameters = EMPTY_ARRAY;
        }

        if (name.equals("new")) {
            Constructor constructor =
                MethodLookupUtils.lookupConstructor(functionClass, parameters);
            if (constructor != null) {
                return new ConstructorFunction(constructor);
            }
        }
        else {
            Method method = MethodLookupUtils.
                lookupStaticMethod(functionClass, name, parameters);
            if (method != null) {
                return new MethodFunction(method);
            }

            method = MethodLookupUtils.
                lookupMethod(functionClass, name, parameters);
            if (method != null) {
                return new MethodFunction(method);
            }
        }

        return null;
    }
}
