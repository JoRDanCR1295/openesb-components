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
 * @(#)DynamicPropertyHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

/**
 * A generic mechanism for accessing collections of name/value pairs.
 * Examples of such collections are HashMap, Properties,
 * ServletContext.  In order to add support for a new such collection
 * type to JXPath, perform the following two steps:
 * <ol>
 * <li>Build an implementation of the DynamicPropertyHandler interface
 * for the desired collection type.</li>
 * <li>Invoke the static method {@link JXPathIntrospector#registerDynamicClass
 * JXPathIntrospector.registerDynamicClass(class, handlerClass)}</li>
 * </ol>
 * JXPath allows access to dynamic properties using these three formats:
 * <ul>
 * <li><code>"myMap/myKey"</code></li>
 * <li><code>"myMap[@name = 'myKey']"</code></li>
 * <li><code>"myMap[name(.) = 'myKey']"</code></li>
 * </ul>
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface DynamicPropertyHandler {

    /**
     * Returns a list of dynamic property names for the supplied object.
     */
    String[] getPropertyNames(Object object);

    /**
     * Returns the value of the specified dynamic property.
     */
    Object getProperty(Object object, String propertyName);

    /**
     * Modifies the value of the specified dynamic property.
     */
    void setProperty(Object object, String propertyName, Object value);
}
