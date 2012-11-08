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
 * @(#)Pick.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;pick&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Pick extends Activity {
    
    /** Tag for this element. */
    public static final String TAG = Tags.PICK;
    
    /** Describes the attributes of this element.
     */
    public interface ATTR extends Activity.ATTR {
        
        /** "createInstance" attribute token */
        public static final String CREATE_INSTANCE = "createInstance";
    }
    /** Ordinal position of createInstance attribute */
    public static final int CREATE_INSTANCE = NUM_STANDARD_ATTRS;

    /** Total number of attributes */
    public static final int NUM_ATTRS = CREATE_INSTANCE + 1;
    
    /** Getter for createInstance attribute.
     * @return  Value of createInstance attribute.
     */
    String getCreateInstance();
     
    /** Setter for createInstance attribute.
     * @param   c   Value of createInstance attribute.
     */
    void setCreateInstance(String c);
    
    /** Indexed getter for the onMessage sub-element.
     * @param   i   Index to onMessage sub-element.
     * @return  onMessage sub-element.
     */
    OnMessage getOnMessage(int i);
    
    /** Setter for the onMessage sub-element.
     * @param   i   Index to onMessage sub-element.
     * @param   o   onMessage sub-element.
     */
    void setOnMessage(int i, OnMessage o);
    
    /** Adds a onMessage sub-element.
     * @param   o   onMessage sub-element.
     */
    void addOnMessage(OnMessage o);
    
    /** Inserts onMessage element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert onMessage at.
     * @param   o   onMessage element to insert.
     */
    void addOnMessage(int i, OnMessage o);
    
    /** Removes all the onMessage elements within the list.
     */
    void clearOnMessages();
    
    /** Removes a onMessage sub-element.
     * @param   i   Index to onMessage sub-element.
     */
    void removeOnMessage(int i);
    
    /** Removes a onMessage sub-element.
     * @param   o   onMessage sub-element to remove.
     * @return  <code>true</code> if successfully removed.
     */
    boolean removeOnMessage(OnMessage o);
    
    /** Getter for the number of onMessage sub-elements there are.
     * @return  Size of list.
     */
    int getOnMessageSize();
    
    /** Index of onMessage element within list.
     * @param   onMsg   onMessage element to index
     * @return  Index (0-based) of element.
     */
    int indexOfOnMessage(XMLNode onMsg);
    
    /** Gets collection of onMessage elements.
     * @return Unmodifiable collection of onMessage elements.
     */
    Collection getOnMessages();
    
    /** Indexed getter for the onAlarm sub-element.
     * @param   i   Index to onAlarm sub-element.
     * @return  onAlarm sub-element.
     */
    OnAlarm getOnAlarm(int i);
    
    /** Setter for the onAlarm sub-element.
     * @param   i   Index to onAlarm sub-element.
     * @param   o   onAlarm sub-element.
     */
    void setOnAlarm(int i, OnAlarm o);
    
    /** Adds a onAlarm sub-element.
     * @param   o   onAlarm sub-element.
     */
    void addOnAlarm(OnAlarm o);
    
    /** Inserts onAlarm element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert onAlarm at.
     * @param   o   onAlarm element to insert.
     */
    void addOnAlarm(int i, OnAlarm o);
    
    /** Removes all the onAlarm elements within the list.
     */
    void clearOnAlarms();
    
    /** Removes a onAlarm sub-element.
     * @param   i   Index to onAlarm sub-element.
     */
    void removeOnAlarm(int i);
    
    /** Removes a onAlarm sub-element.
     * @param   o   onAlarm sub-element to remove.
     * @return  <code>true</code> if successfully removed.
     */
    boolean removeOnAlarm(OnAlarm o);
    
    /** Getter for the number of onAlarm sub-elements there are.
     * @return  Size of list.
     */
    int getOnAlarmSize();
   
    /** Index of onAlarm element within list.
     * @param   onAlarm     onAlarm element to index
     * @return  Index (0-based) of element.
     */
    int indexOfOnAlarm(XMLNode onAlarm);
    
    /** Gets collection of onAlarm elements.
     * @return Unmodifiable collection of onAlarm elements.
     */
    Collection getOnAlarms();
}
