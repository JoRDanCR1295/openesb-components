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
 * @(#)Switch.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;switch&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Switch extends Activity {
    /** Tag for this element. */    
    public static final String TAG = Tags.SWITCH;
    
    /** Indexed getter for case elements.
     * @param   i   Index to element.
     * @return  case element.
     */
    Case getCase(int i);
    
    /**
     * get case with a given line label attribute
     * @param lineLabel line label attribute of the case
     * @return Case
     */
    Case getCase(String lineLabel);
    
    /** Indexed setter for case elements.
     * @param   i   Index to element.
     * @param   c   case element.
     */
    void setCase(int i, Case c);
    
    /** Adds a case element to the list.
     * @param   c   case element.
     */
    void addCase(Case c);
    
    /** Inserts case element at given index within list; pushes all elements after to the right.
     * @param   i   Index to case link at.
     * @param   c   case element to insert.
     */
    void addCase(int i, Case c);
    
    /** Removes all the case elements within the list.
     */
    void clearCases();
    
    /** Removes a case element from the list.
     * @param   i   Index to element.
     */
    void removeCase(int i);
    
    /** Removes a case element from the list.
     * @param   c   case element.
     * @return  <tt>true</tt> if successfully removed.
     */
    boolean removeCase(Case c);
    
    /** Gives the size of the case list.
     * @return  List size.
     */
    int getCaseSize();
    
    /** Index of case element within list.
     * @param   c   case element to index.
     * @return  Index (0-based) of element.
     */
    int indexOfCase(XMLNode c);
    
    /** Returns the collection of case elements.
     * @return  Unmodifiable collection of case elements.
     */
    Collection getCases();
    
    /** Getter for otherwise element.
     * @return  otherwise element.
     */
    Otherwise getOtherwise();
    
    /** Setter for otherwise element.
     * @param   o   otherwise element.
     */
    void setOtherwise(Otherwise o);
}
