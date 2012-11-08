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
 * @(#)Correlations.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describe the &lt;correlations&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Correlations extends BPELElement {
    
    /** Tag for this element */
    public static final String TAG = "correlations";
    
    /** Indexed getter for element correlation.
     * @param index Index of the element.
     * @return Value of the element at <CODE>index</CODE>.
     *
     */
    Correlation getCorrelation(int index);
    
    /** Indexed setter for element correlation.
     * @param index Index of the element.
     * @param correlation New value of the element at <CODE>index</CODE>.
     *
     */
    void setCorrelation(int index, Correlation correlation);
    
    /** Number of correlation elements present.
     * @return  Number of correlation's.
     */
    int getCorrelationSize();
    
    /** Add a new correlation element to the list.
     * @param   c   New correlation.
     */
    void addCorrelation(Correlation c);
    
    /** Inserts correlation element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert correlation at.
     * @param   c   correlation element to insert.
     */
    void addCorrelation(int i, Correlation c);
    
    /** Removes all the correlation elements within the list.
     */
    void clearCorrelations();
    
    /** Removes correlation element from the list.
     * @param   i   Index to correlation element.
     */
    void removeCorrelation(int i);
    
    /** Removes correlation element from the list.
     * @param   c   correlation element to remove.
     * @return  <tt>true</tt> if removal successful.
     */
    boolean removeCorrelation(Correlation c);
    
    /** Index of correlation element within list.
     * @param   correlation     correlation element to index.
     * @return  Index (0-based) of element.
     */
    int indexOfCorrelation(XMLNode correlation);
    
    /** Gets collection of correlation elements.
     * @return Unmodifiable collection of correlation elements.
     */
    Collection getCorrelations();
}
