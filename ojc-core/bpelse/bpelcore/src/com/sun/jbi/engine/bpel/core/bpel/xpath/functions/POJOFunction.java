/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;
import org.w3c.dom.Node;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.engine.bpel.core.bpel.exception.POJOException;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELXPathContext;

/**
 * It is JXPath extension function enabling to call static methods on Java
 * classes (including JDK). It expects to find the classes in the jar files
 * packaged in service unit.
 */
public class POJOFunction implements Function {

    private String className;
    private String localName;
    private String suName;
    private String operationName;

    public POJOFunction(String className, String localName, String suName) {
        this.className = className;
        this.localName = localName;
        this.suName = suName;
    }

    /*
     * @see org.apache.commons.jxpath.Function#invoke(org.apache.commons.jxpath.ExpressionContext,
     *      java.lang.Object[])
     */
    public Object invoke(ExpressionContext context, Object[] parameters) {
        if (!(context.getJXPathContext() instanceof BPELXPathContext)) {
            return null;
        }
        BPELXPathContext ctxXPath = (BPELXPathContext) context.getJXPathContext();

        //set thread context class loader to SU
        ctxXPath.getClassLoaderContext().switchClassLoader(suName,
                SwitchType.service_classloader);

        Object retValue = null;
        try {
            Class<?> targetClass = Class.forName(className, true, Thread.currentThread().getContextClassLoader());
            Object[] convParam = null;
            if (parameters != null && parameters.length > 0) {
            	convParam = new Object[parameters.length];
            	int index = 0;
            	for (Object obj : parameters) {
            		convParam[index] = BPWSFunctions.convertParam(obj);
            		index++;
            	}
            } else {
            	// if the function has no arguments then pass a empty array. issue #1299
            	convParam = new Object[0];
            }
            retValue = invoke(targetClass, localName, convParam);
        } catch (Exception e) {
            Throwable e1 = e;
            if(e instanceof InvocationTargetException){
                e1 = e.getCause();
            }
            throw new POJOException(className, operationName, e1);
        } finally {
            ctxXPath.getClassLoaderContext().switchClassLoader(suName,
                    SwitchType.context_classloader);
        }
        return retValue;
    }

    private Object invoke(Class<?> targetClass, String targetMethodName,
            Object[] parameters) throws ClassNotFoundException,
            SecurityException, NoSuchMethodException, IllegalArgumentException,
            IllegalAccessException, InvocationTargetException, InstantiationException {
    	
    	Method method;
    	try {
    		method = targetClass.getDeclaredMethod(targetMethodName, getTypes(parameters));
    	} catch (NoSuchMethodException nsme) {
    		try {
    			method = targetClass.getDeclaredMethod(targetMethodName, getTypesDeprecated(parameters));
    		} catch (Exception ignore) {
    			throw nsme;
    		}
    	}

        operationName = method.toGenericString();
        
        if(Modifier.isStatic(method.getModifiers())){
            return method.invoke(null, parameters);
        } else {
            return method.invoke(targetClass.newInstance(), parameters);
        }
        
    }

    private Class<?>[] getTypes(Object[] params) {
        Class<?> types[] = new Class[params.length];
        int index = 0;
        for (Object obj : params) {
            types[index] = getType(obj);
            index++;
        }
        return types;
    }

    private Class<?> getType(Object object) {
        if (object instanceof Node) {
            return Node.class;
        } else if (object instanceof Boolean) {
            return Boolean.class;
        } else if (object instanceof Number) {
            return Double.class;
        } else if (object instanceof String) {
            return String.class;
        } else {
            return object.getClass();
        }
    }
    
    // Private method with deprecated functionality. See <code>getTypeDeprecated</code> method.
    private Class<?>[] getTypesDeprecated(Object[] params) {
        Class<?> types[] = new Class[params.length];
        int index = 0;
        for (Object obj : params) {
            types[index] = getTypeDeprecated(obj);
            index++;
        }
        return types;
    }

    // Private method with deprecated functionality. As per the design we should have returned java.lang.Double.class
    // and java.lang.Boolean.class instead of double.class and boolean.class when parameter types were Double or 
    // Boolean.
    private Class<?> getTypeDeprecated(Object object) {
        if (object instanceof Node) {
            return Node.class;
        } else if (object instanceof Boolean) {
            return boolean.class;
        } else if (object instanceof Number) {
            return double.class;
        } else {
            return String.class;
        }
    }
}
