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
 * @(#)Choose.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.extensions;


import java.util.Collection;

import com.sun.bpel.model.Activity;
import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;switch&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Choose extends Activity {
    /** Tag for this element. */    
    public static final String TAG = "choose";
    
    /** Indexed getter for When elements.
     * @param   i   Index to element.
     * @return  When element.
     */
    When getWhen(int i);
    
    /** Indexed setter for When elements.
     * @param   i   Index to element.
     * @param   w   When element.
     */
    void setWhen(int i, When w);
    
    /** Adds a When element to the list.
     * @param   c   When element.
     */
    void addWhen(When c);
    
    /** Inserts When element at given abstract index within list; 
     *  pushes all elements after to the right. Note that, the index passed in is 
     *  not the index within the When list but the index within the children list
     * @param   i   Abstract Index of the element in the parent XMLElement
     * @param   c   When element to insert.
     */
    void addWhen(int i, When c);
    
    /** Removes all the When elements within the list.
     */
    void clearWhens();
    
    /** Removes a When element from the list.
     * @param   i   Index to element.
     */
    void removeWhen(int i);
    
    /** Removes a When element from the list.
     * @param   c   When element.
     * @return  <tt>true</tt> if successfully removed.
     */
    boolean removeWhen(When c);
    
    /** Gives the size of the When list.
     * @return  List size.
     */
    int getWhenSize();
    
    /** Index of When element within list.
     * @param   c   When element to index.
     * @return  Index (0-based) of element.
     */
    int indexOfWhen(XMLNode c);
    
    /** Returns the collection of When elements.
     * @return  Unmodifiable collection of When elements.
     */
    Collection getWhens();
    
    /** Getter for otherwise element.
     * @return  otherwise element.
     */
    Default getDefault();
    
    /** Setter for otherwise element.
     * @param   o   otherwise element.
     */
    void setDefault(Default o);
}
