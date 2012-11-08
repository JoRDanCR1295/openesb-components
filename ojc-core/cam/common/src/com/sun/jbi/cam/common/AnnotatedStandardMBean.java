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
 * @(#)AnnotatedStandardMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.common;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;

/**
 * The idea is to create a subclass of javax.management.StandardMBean that will
 * add the information from the annotations to the information that is already
 * deduced by the normal rules for Standard MBeans
 * 
 * @author graj
 */
public class AnnotatedStandardMBean extends StandardMBean {

    private static final Map<String, Class<?>> primitiveClasses = new HashMap<String, Class<?>>();

    static {
        Class<?>[] primitives = { byte.class, short.class, int.class,
                long.class, float.class, double.class, char.class,
                boolean.class, };
        for (Class<?> classInstance : primitives)
            primitiveClasses.put(classInstance.getName(), classInstance);
    }

    /**
     * Instance where the MBean interface is implemented by another object.
     * 
     * @param <T>
     * @param impl
     * @param mbeanInterface
     * @throws NotCompliantMBeanException
     */
    public <T> AnnotatedStandardMBean(T impl, Class<T> mbeanInterface)
            throws NotCompliantMBeanException {
        super(impl, mbeanInterface);
    }

    /**
     * Instance where the MBean interface is implemented by this object.
     * 
     * @param mbeanInterface
     * @throws NotCompliantMBeanException
     */
    protected AnnotatedStandardMBean(Class<?> mbeanInterface)
            throws NotCompliantMBeanException {
        super(mbeanInterface);
    }

    /**
     * Override that will return the value of the
     * 
     * @Description annotation if there is one
     */
    @Override
    protected String getDescription(MBeanOperationInfo operation) {
        String descriptorString = operation.getDescription();
        Method method = methodFor(getMBeanInterface(), operation);
        if (method != null) {
            Description description = method.getAnnotation(Description.class);
            if (description != null)
                descriptorString = description.value();
        }
        return descriptorString;
    }

    /**
     * Unfortunately, there is no standard method to convert back from
     * Class.getName() to the original Class. Class.forName comes close, but it
     * doesn't do the right thing for primitive types like int.class (also known
     * as Integer.TYPE). int.class.getName() returns "int", but if you give that
     * to Class.forName it will look for a class called int, which it is
     * unlikely to find. So we need this helper method
     * 
     * @param name
     * @param loader
     * @return original Class
     * @throws ClassNotFoundException
     */
    static Class<?> classForName(String name, ClassLoader loader)
            throws ClassNotFoundException {
        Class<?> classInstance = primitiveClasses.get(name);
        if (classInstance == null)
            classInstance = Class.forName(name, false, loader);
        return classInstance;
    }

    /**
     * Method that finds the Method object for an MBeanOperationInfo that comes
     * from a given MBean interface
     * 
     * @param mbeanInterface
     * @param operation
     * @return
     */
    private static Method methodFor(Class<?> mbeanInterface,
            MBeanOperationInfo operation) {
        final MBeanParameterInfo[] params = operation.getSignature();
        final String[] paramTypes = new String[params.length];
        for (int index = 0; index < params.length; index++)
            paramTypes[index] = params[index].getType();

        return findMethod(mbeanInterface, operation.getName(), paramTypes);
    }

    /**
     * Method that finds the Method object for an MBeanOperationInfo that comes
     * from a given MBean interface
     * 
     * @param mbeanInterface
     * @param name
     * @param paramTypes
     * @return
     */
    private static Method findMethod(Class<?> mbeanInterface, String name,
            String... paramTypes) {
        try {
            final ClassLoader loader = mbeanInterface.getClassLoader();
            final Class<?>[] paramClasses = new Class<?>[paramTypes.length];
            for (int index = 0; index < paramTypes.length; index++)
                paramClasses[index] = classForName(paramTypes[index], loader);
            return mbeanInterface.getMethod(name, paramClasses);
        } catch (RuntimeException e) {
            // avoid accidentally catching unexpected runtime exceptions
            throw e;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * There is no method Method.getParameterAnnotation that would allow you to
     * get the value of a particular annotation for a particular method
     * parameter, the way you can with Method.getAnnotation.
     * 
     * A method that works for any annotation rather than just
     * 
     * @ParameterName
     * 
     * @param <A>
     * @param method
     * @param paramNo
     * @param annotationObject
     * @return
     */
    static <A extends Annotation> A getParameterAnnotation(Method method,
            int paramNo, Class<A> annotationObject) {
        for (Annotation annotation : method.getParameterAnnotations()[paramNo]) {
            if (annotationObject.isInstance(annotation))
                return annotationObject.cast(annotation);
        }
        return null;
    }

    /**
     * Override that extracts a parameter name from the
     * 
     * @ParameterName annotation
     */
    @Override
    protected String getParameterName(MBeanOperationInfo operation,
            MBeanParameterInfo parameter, int paramNo) {
        String name = parameter.getName();
        Method method = methodFor(getMBeanInterface(), operation);
        if (method != null) {
            ParameterName parameterName = getParameterAnnotation(method,
                    paramNo, ParameterName.class);
            if (parameterName != null)
                name = parameterName.value();
        }
        return name;
    }

}
