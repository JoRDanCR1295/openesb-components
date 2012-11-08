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
 * @(#)CorrelationSets.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes a &lt;correlationSets&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface CorrelationSets extends BPELElement {
    
    /** Tag for this element. */
    public static final String TAG = "correlationSets";
    
    /** Indexed getter for property correlationSet.
     * @param index Index of the property.
     * @return Value of the property at <CODE>index</CODE>.
     *
     */
    CorrelationSet getCorrelationSet(int index);
    
    /** get CorrelationSet given a name
     * @param name name of the correlaton set
     * @return CorrelationSet
     *
     */
    CorrelationSet getCorrelationSet(String name);
    
    /** Indexed setter for property correlationSet.
     * @param index Index of the property.
     * @param correlationSet New value of the property at <CODE>index</CODE>.
     *
     */
    void setCorrelationSet(int index, CorrelationSet correlationSet);
    
    /** Number of correlationSet elements present.
     * @return  Number of correlationSet's.
     */
    int getCorrelationSetSize();
    
    /** Add a new correlationSet property to the list.
     * @param   c   New correlationSet.
     */
    void addCorrelationSet(CorrelationSet c);
    
    /** Inserts correlationSet element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert correlationSet at.
     * @param   c   correlationSet element to insert.
     */
    void addCorrelationSet(int i, CorrelationSet c);
    
    /** Removes all the correlationSet elements within the list.
     */
    void clearCorrelationSets();
    
    /** Removes a correlationSet element from the list.
     * @param   i   Index to the correlationSet element.
     */
    void removeCorrelationSet(int i);
    
    /** Removes a correlationSet element from the list.
     * @param   c   correlationSet element to remove.
     * @return  <tt>true</tt> if correlationSet element successfully removed.
     */
    boolean removeCorrelationSet(CorrelationSet c);
    
    /** Index of a correlationSet element within list.
     * @param   cs  correlationSet element to index
     * @return  Index (0-based) of element.
     */
    int indexOfCorrelationSet(XMLNode cs);
    
    /** Gets collection of correlationSet elements.
     * @return Unmodifiable collection of correlationSet elements.
     */
    Collection getCorrelationSets();
    
    
}
