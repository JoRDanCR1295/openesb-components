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
 * @(#)FaultHandlers.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;faultHandlers&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface FaultHandlers extends BPELElement {
    
    /** Tag for this element. */
    public static final String TAG = "faultHandlers";
    
    /** Indexed getter for property catch.
     * @param index Index of the property.
     * @return Value of the property at <CODE>index</CODE>.
     *
     */
    Catch getCatch(int index);
    
    /** Indexed setter for property catch.
     * @param index Index of the property.
     * @param c New value of the property at <CODE>index</CODE>.
     *
     */
    void setCatch(int index, Catch c);
    
    /** Number of catch elements present.
     * @return  Number of catch's.
     */
    int getCatchSize();
    
    /** Add a new catch property to the list.
     * @param   c   New catch.
     */
    void addCatch(Catch c);
    
    /** Inserts catch element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert catch at.
     * @param   c   catch element to insert.
     */
    void addCatch(int i, Catch c);
    
    /** Removes all the catch elements within the list.
     */
    void clearCatches();
    
    /** Remove a catch property from the list.
     * @param   c   <tt>Catch</tt> object to remove.
     * @return  <tt>true</tt> if the catch property was removed successfully.
     */
    boolean removeCatch(Catch c);
    
    /** Index of catch element within list.
     * @param   c   catch element to index.
     * @return  Index (0-based) of catch element.
     */
    int indexOfCatch(XMLNode c);
    
    /** Gets collection of catch elements.
     * @return Unmodifiable collection of catch elements.
     */
    Collection getCatches();
    
    /** Remove a catch property at the indicated position in the list.
     * @param   i   Index of the catch property to remove.
     */
    void removeCatch(int i);
    
    /** Getter for property catchAll.
     * @return Value of property catchAll.
     *
     */
    CatchAll getCatchAll();
    
    /** Setter for property catchAll.
     * @param catchAll New value of property catchAll.
     *
     */
    void setCatchAll(CatchAll catchAll);
}
