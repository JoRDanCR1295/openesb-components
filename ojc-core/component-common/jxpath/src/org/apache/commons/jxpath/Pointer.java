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
 * @(#)Pointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.io.Serializable;

/**
 * Pointers represent locations of objects and their properties
 * in Java object graphs. JXPathContext has methods
 * ({@link JXPathContext#getPointer(java.lang.String) getPointer()}
 * and  ({@link JXPathContext#iteratePointers(java.lang.String)
 * iteratePointers()}, which, given an XPath, produce Pointers for the objects
 * or properties described the the path. For example, <code>ctx.getPointer
 * ("foo/bar")</code> will produce a Pointer that can get and set the property
 * "bar" of the object which is the value of the property "foo" of the root
 * object. The value of <code>ctx.getPointer("aMap/aKey[3]")</code> will be a
 * pointer to the 3'rd element of the array, which is the value for the key
 * "aKey" of the map, which is the value of the property "aMap" of the root
 * object.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface Pointer extends Cloneable, Comparable, Serializable {

    /**
     * Returns the value of the object, property or collection element
     * this pointer represents. May convert the value to one of the 
     * canonical InfoSet types: String, Number, Boolean, Set.
     * 
     * For example, in the case of an XML element, getValue() will
     * return the text contained by the element rather than 
     * the element itself.
     */
    Object getValue();

    /**
     * Returns the raw value of the object, property or collection element
     * this pointer represents.  Never converts the object to a
     * canonical type: returns it as is. 
     * 
     * For example, for an XML element, getNode() will
     * return the element itself rather than the text it contains.
     */
    Object getNode();

    /**
     * Modifies the value of the object, property or collection element
     * this pointer represents.
     */
    void setValue(Object value);

    /**
     * Returns the node this pointer is based on. 
     */
    Object getRootNode();
    
    /**
     * Returns a string that is a proper "canonical" XPath that corresponds to
     * this pointer.  Consider this example:
     * <p><code>Pointer  ptr = ctx.getPointer("//employees[firstName = 'John']")
     * </code>
     * <p>The  value of <code>ptr.asPath()</code> will look something like
     * <code>"/departments[2]/employees[3]"</code>, so, basically, it represents
     * the concrete location(s) of the result of a search performed by JXPath.
     * If an object in the pointer's path is a Dynamic Property object (like a
     * Map), the asPath method generates an XPath that looks like this: <code>"
     * /departments[@name = 'HR']/employees[3]"</code>.
     */
    String asPath();
    
    /**
     * Pointers are cloneable
     */
    Object clone();
}
