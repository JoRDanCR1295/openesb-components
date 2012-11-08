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
 * @(#)MultipleActivityHolder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes a BPEL element that holds multiple activities.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface MultipleActivityHolder {
    
    /** Indexed getter for activity elements.
     * @param   i   Index to activity element.
     * @return  activity element.
     */
    Activity getActivity(int i);
    
    /** Indexed setter for activity elements.
     * @param   i   Index to activity element.
     * @param   a   activity element to set.
     */
    void setActivity(int i, Activity a);
    
    /** Adds a activity element to the list.
     * @param   a   activity element to add.
     */
    void addActivity(Activity a);
    
    /** Inserts activity element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert activity at.
     * @param   a   activity element to insert.
     */
    void addActivity(int i, Activity a);
    
    /** Removes all the activity elements within the list.
     */
    void clearActivities();
    
    /** Removes a activity element from the list.
     * @param   i   Index to element.
     */
    void removeActivity(int i);
    
    /** Removes a activity element from the list.
     * @param   a   activity element to remove.
     * @return  <tt>true</tt> if successfully removed.
     */
    boolean removeActivity(Activity a);
    
    /** Size of the activity element list.
     * @return  List size.
     */
    int getActivitySize();
    
    /** Gets index of a activity element in the list.
     * @param   activity    activity element to index.
     * @return  Index of (0-based) activity element in the list.
     */
    int indexOfActivity(XMLNode activity);
    
    /** Gets collection of activity elements.
     * @return Unmodifiable collection of activity elements.
     */
    Collection getActivities();
    
}
