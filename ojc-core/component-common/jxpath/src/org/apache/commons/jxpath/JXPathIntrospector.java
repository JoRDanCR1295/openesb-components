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
 * @(#)JXPathIntrospector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.Date;
import java.util.Map;
import java.util.HashMap;

/**
 * JXPathIntrospector  maintains a registry of {@link JXPathBeanInfo
 * JXPathBeanInfo} objects for Java classes.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JXPathIntrospector {

    private static HashMap byClass = new HashMap();
    private static HashMap byInterface = new HashMap();

    static {
        registerAtomicClass(Class.class);
        registerAtomicClass(Boolean.TYPE);
        registerAtomicClass(Boolean.class);
        registerAtomicClass(Byte.TYPE);
        registerAtomicClass(Byte.class);
        registerAtomicClass(Character.TYPE);
        registerAtomicClass(Character.class);
        registerAtomicClass(Short.TYPE);
        registerAtomicClass(Short.class);
        registerAtomicClass(Integer.TYPE);
        registerAtomicClass(Integer.class);
        registerAtomicClass(Long.TYPE);
        registerAtomicClass(Long.class);
        registerAtomicClass(Float.TYPE);
        registerAtomicClass(Float.class);
        registerAtomicClass(Double.TYPE);
        registerAtomicClass(Double.class);
        registerAtomicClass(String.class);
        registerAtomicClass(Date.class);
        registerAtomicClass(java.sql.Date.class);
        registerAtomicClass(java.sql.Time.class);
        registerAtomicClass(java.sql.Timestamp.class);

        registerDynamicClass(Map.class, MapDynamicPropertyHandler.class);
    }

    /**
     * Automatically creates and registers a JXPathBeanInfo object
     * for the specified class. That object returns true to isAtomic().
     */
    public static void registerAtomicClass(Class beanClass) {
        byClass.put(beanClass, new JXPathBasicBeanInfo(beanClass, true));
    }

    /**
     * Automatically creates and registers a JXPathBeanInfo object
     * for the specified class. That object returns true to isDynamic().
     */
    public static void registerDynamicClass(
        Class beanClass,
        Class dynamicPropertyHandlerClass) 
    {
        JXPathBasicBeanInfo bi =
            new JXPathBasicBeanInfo(beanClass, dynamicPropertyHandlerClass);
        if (beanClass.isInterface()) {
            byInterface.put(beanClass, bi);
        }
        else {
            byClass.put(beanClass, bi);
        }
    }

    /**
     * Creates  and registers a JXPathBeanInfo object for the supplied class. If
     * the class has already been registered, returns the registered
     * JXPathBeanInfo object.
     * <p>
     * The process of creation of JXPathBeanInfo is as follows:
     * <ul>
     * <li>If class named <code>&lt;beanClass&gt;XBeanInfo</code> exists,
     *     an instance of that class is allocated.
     * <li>Otherwise, an instance of {@link JXPathBasicBeanInfo
     *     JXPathBasicBeanInfo}  is allocated.
     * </ul>
     */
    public static JXPathBeanInfo getBeanInfo(Class beanClass) {
        JXPathBeanInfo beanInfo = (JXPathBeanInfo) byClass.get(beanClass);
        if (beanInfo == null) {
            beanInfo = findDynamicBeanInfo(beanClass);
            if (beanInfo == null) {
                beanInfo = findInformant(beanClass);
                if (beanInfo == null) {
                    beanInfo = new JXPathBasicBeanInfo(beanClass);
                }
            }
            byClass.put(beanClass, beanInfo);
        }
        return beanInfo;
    }

    /**
     * Find a dynamic bean info if available for any superclasses or
     * interfaces.
     */
    private static JXPathBeanInfo findDynamicBeanInfo(Class beanClass) {
        JXPathBeanInfo beanInfo = null;
        if (beanClass.isInterface()) {
            beanInfo = (JXPathBeanInfo) byInterface.get(beanClass);
            if (beanInfo != null && beanInfo.isDynamic()) {
                return beanInfo;
            }
        }

        Class interfaces[] = beanClass.getInterfaces();
        if (interfaces != null) {
            for (int i = 0; i < interfaces.length; i++) {
                beanInfo = findDynamicBeanInfo(interfaces[i]);
                if (beanInfo != null && beanInfo.isDynamic()) {
                    return beanInfo;
                }
            }
        }

        Class sup = beanClass.getSuperclass();
        if (sup != null) {
            beanInfo = (JXPathBeanInfo) byClass.get(sup);
            if (beanInfo != null && beanInfo.isDynamic()) {
                return beanInfo;
            }
            return findDynamicBeanInfo(sup);                
        }
        return null;
    }

    private static synchronized JXPathBeanInfo findInformant(Class beanClass) {
        String name = beanClass.getName() + "XBeanInfo";
        try {
            return (JXPathBeanInfo) instantiate(beanClass, name);
        }
        catch (Exception ex) {
            // Just drop through
        }

        // Now try checking if the bean is its own JXPathBeanInfo.
        try {
            if (JXPathBeanInfo.class.isAssignableFrom(beanClass)) {
                return (JXPathBeanInfo) beanClass.newInstance();
            }
        }
        catch (Exception ex) {
            // Just drop through
        }

        return null;
    }

    /**
     * Try to create an instance of a named class.
     * First try the classloader of "sibling", then try the system
     * classloader.
     */
    private static Object instantiate(Class sibling, String className)
        throws Exception 
    {

        // First check with sibling's classloader (if any).
        ClassLoader cl = sibling.getClassLoader();
        if (cl != null) {
            try {
                Class cls = cl.loadClass(className);
                return cls.newInstance();
            }
            catch (Exception ex) {
                // Just drop through and try the system classloader.
            }
        }

        // Now try the bootstrap classloader.
        Class cls = Class.forName(className);
        return cls.newInstance();
    }
}
