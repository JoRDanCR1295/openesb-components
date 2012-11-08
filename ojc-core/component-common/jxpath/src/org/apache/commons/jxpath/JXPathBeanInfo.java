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
 * @(#)JXPathBeanInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.beans.PropertyDescriptor;

/**
 * JXPathBeanInfo  is similar to java.beans.BeanInfo in that it describes
 * properties of a JavaBean class.  By default, JXPathBeanInfo classes are
 * automatically generated by {@link JXPathIntrospector JXPathIntrospector}
 * based on the java.beans.BeanInfo. As with JavaBeans, the user can supply an
 * alternative implementation of JXPathBeanInfo for a custom class.  The
 * alternative implementation is located by class name, which is the same as the
 * name of the class it represents with the suffix "XBeanInfo".  So, for
 * example, if you need to provide an alternative JXPathBeanInfo class for class
 * "com.foo.Bar", write a class "com.foo.BarXBeanInfo" and make it implement the
 * JXPathBeanInfo interface.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface JXPathBeanInfo  {

    /**
     * Returns true if objects of this class are treated as atomic
     * objects which have no properties of their own.
     * For example, java.lang.String and java.lang.Number are atomic.
     */
    boolean isAtomic();

    /**
     * Returns true if the objects of this class have dynamic properties
     * (e.g. java.util.Map). If this method returns true, getPropertyDescriptors
     * should return null and getDynamicPropertyHandlerClass should return
     * a valid class name.  An object cannot have both static and dynamic
     * properties at the same time.
     */
    boolean isDynamic();

    /**
     * Returns a list of property descriptors for the beans described by this
     * bean info object.  Returns null for atomic beans.
     */
    PropertyDescriptor[] getPropertyDescriptors();

    /**
     * Returns a PropertyDescriptor for the specified name or null if there
     * is no such property.
     */
    PropertyDescriptor getPropertyDescriptor(String propertyName);

    /**
     * For dynamic objects, returns the class implementing
     * the DynamicPropertyHandler interface. That class can
     * be used to access dynamic properties.
     */
    Class getDynamicPropertyHandlerClass();
}
