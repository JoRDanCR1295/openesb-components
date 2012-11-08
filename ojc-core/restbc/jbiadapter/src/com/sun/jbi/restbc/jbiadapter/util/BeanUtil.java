package com.sun.jbi.restbc.jbiadapter.util;

import java.lang.reflect.Method;

/**
 * BeanUtil.java
 *
 * @author Edward Chou
 */
public class BeanUtil {
    
    public static Object getProperty(Object o, String propertyName) throws Exception {
        if (propertyName == null || propertyName.length() < 1) {
            throw new Exception("propertyName cannot be empty");
        }
        String methodName = "get" + propertyName.substring(0,1).toUpperCase() + propertyName.substring(1);
        
        Class<?> clazz = o.getClass();
        Method method = clazz.getMethod(methodName, null);
        Object property = method.invoke(o, null);

        return property;
    }

    public static void setProperty(Object o, String propertyName, String propertyValue) throws Exception {
        if (propertyName == null || propertyName.length() < 1) {
            throw new Exception("propertyName cannot be empty");
        }
        String methodName = "set" + propertyName.substring(0,1).toUpperCase() + propertyName.substring(1);
        Class<?> clazz = o.getClass();
        Method m = clazz.getMethod(methodName, new Class[] { String.class } );
        m.invoke(o, new Object[] { propertyValue } );
    }
    
}
