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
 * @(#)Variables.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

/**
 * Describes the &lt;containers&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Variables extends BPELElement {
    
    /** Tag for this element. */
    public static final String TAG = "variables";
    
    /** Indexed getter for property container.
     * @param index Index of the property.
     * @return Value of the property at <CODE>index</CODE>.
     *
     */
    Variable getVariable(int index);
    
    /** get Variable given its name.
     * @param name name of the Variable.
     * @return Variable with the given name.
     *
     */
    Variable getVariable(String name);
    
    
    /** Number of container elements present.
     * @return  Number of container's.
     */
    int getVariableSize();
    
    /** Add a new container property to the list.
     * @param   c   New container.
     */
    void addVariable(Variable c); 
    
       
    
    /** Removes a container element from the list.
     * @param   c   container element to remove.
     * @return  <tt>true</tt> if container element successfully removed.
     */
    boolean removeVariable(Variable c);
    
        
    /** Gets collection of container elements.
     * @return Unmodifiable collection of container elements.
     */
    Collection getVariables();
}
