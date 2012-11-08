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
 * @(#)AbstractFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

/**
 * The  {@link JXPathContext#createPath JXPathContext.createPath()} method of
 * JXPathContext can create missing objects as it traverses an XPath; it
 * utilizes an AbstractFactory for that purpose. Install a factory on
 * JXPathContext by calling {@link JXPathContext#setFactory JXPathContext.
 * setFactory()}.
 * <p>
 * All  methods of this class return false.  Override any of them to return true
 * to indicate that the factory has successfully created the described object.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public abstract class AbstractFactory {

    /**
     * The  parameters may describe a collection element or an individual
     * object. It is up to the factory to infer which one it is. If it is a
     * collection, the factory should check if the collection exists.  If not,
     * it should create the collection. Then it should create the index'th
     * element of the collection and return true.
     * <p>
     * 
     * @param context can be used to evaluate other XPaths, get to variables
     * etc.
     * @param pointer describes the location of the node to be created
     * @param parent is the object that will server as a parent of the new
     * object
     * @param name is the name of the child of the parent that needs to be
     * created. In the case of DOM may be qualified.
     * @param index is used if the pointer represents a collection element. You
     * may need to expand or even create the collection to accomodate the new
     * element.
     * 
     * @return true if the object was successfully created
     */
    public boolean createObject(JXPathContext context, Pointer pointer, 
                                Object parent, String name, int index) 
    {
        return false;
    }

    /**
     * Declare the specified variable
     * 
     * @param context hosts variable pools. See 
     * {@link JXPathContext#getVariables() JXPathContext.getVariables()}
     * @param name is the name of the variable without the "$" sign
     * 
     * @return true if the variable was successfully defined
     */
    public boolean declareVariable(JXPathContext context, String name) {
        return false;
    }
}
