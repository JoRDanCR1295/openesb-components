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
 * @(#)JXPathBasicBeanInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.Comparator;

/**
 * An implementation of JXPathBeanInfo based on JavaBeans' BeanInfo. Properties
 * advertised by JXPathBasicBeanInfo are the same as those advertised by
 * BeanInfo for the corresponding class.
 *
 * See java.beans.BeanInfo, java.beans.Introspector
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JXPathBasicBeanInfo implements JXPathBeanInfo {
    private boolean atomic = false;
    private Class clazz;
    private PropertyDescriptor propertyDescriptors[];
    private String[] propertyNames;
    private Class dynamicPropertyHandlerClass;

    public JXPathBasicBeanInfo(Class clazz) {
        this.clazz = clazz;
    }

    public JXPathBasicBeanInfo(Class clazz, boolean atomic) {
        this.clazz = clazz;
        this.atomic = atomic;
    }

    public JXPathBasicBeanInfo(Class clazz, Class dynamicPropertyHandlerClass) {
        this.clazz = clazz;
        this.atomic = false;
        this.dynamicPropertyHandlerClass = dynamicPropertyHandlerClass;
    }

    /**
     * Returns true if objects of this class are treated as atomic
     * objects which have no properties of their own.
     */
    public boolean isAtomic() {
        return atomic;
    }

    /**
     * Return true if the corresponding objects have dynamic properties.
     */
    public boolean isDynamic() {
        return dynamicPropertyHandlerClass != null;
    }

    public PropertyDescriptor[] getPropertyDescriptors() {
        if (propertyDescriptors == null) {
            try {
                BeanInfo bi = null;
                if (clazz.isInterface()) {
                    bi = Introspector.getBeanInfo(clazz);
                }
                else {
                    bi = Introspector.getBeanInfo(clazz, Object.class);
                }
                PropertyDescriptor[] pds = bi.getPropertyDescriptors();
                propertyDescriptors = new PropertyDescriptor[pds.length];
                System.arraycopy(pds, 0, propertyDescriptors, 0, pds.length);
                Arrays.sort(propertyDescriptors, new Comparator() {
                    public int compare(Object left, Object right) {
                        return ((PropertyDescriptor) left).getName().compareTo(
                            ((PropertyDescriptor) right).getName());
                    }
                });
            }
            catch (IntrospectionException ex) {
                ex.printStackTrace();
            }
        }
        return propertyDescriptors;
    }

    public PropertyDescriptor getPropertyDescriptor(String propertyName) {
        if (propertyNames == null) {
            PropertyDescriptor[] pds = getPropertyDescriptors();
            propertyNames = new String[pds.length];
            for (int i = 0; i < pds.length; i++) {
                propertyNames[i] = pds[i].getName();
            }
        }

        for (int i = 0; i < propertyNames.length; i++) {
            if (propertyNames[i] == propertyName) {
                return propertyDescriptors[i];
            }
        }

        for (int i = 0; i < propertyNames.length; i++) {
            if (propertyNames[i].equals(propertyName)) {
                return propertyDescriptors[i];
            }
        }
        return null;
    }

    /**
     * For  a dynamic class, returns the corresponding DynamicPropertyHandler
     * class.
     */
    public Class getDynamicPropertyHandlerClass() {
        return dynamicPropertyHandlerClass;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("BeanInfo [class = ");
        buffer.append(clazz.getName());
        if (isDynamic()) {
            buffer.append(", dynamic");
        }
        if (isAtomic()) {
            buffer.append(", atomic");
        }
        buffer.append(", properties = ");
        PropertyDescriptor[] jpds = getPropertyDescriptors();
        for (int i = 0; i < jpds.length; i++) {
            buffer.append("\n    ");
            buffer.append(jpds[i].getPropertyType());
            buffer.append(": ");
            buffer.append(jpds[i].getName());
        }
        buffer.append("]");
        return buffer.toString();
    }
}
