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
 * @(#)Links.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;links&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Links extends BPELElement {
    
    /** Tag for this element. */
    public static final String TAG = "links";
    
    /** Indexed getter to link element.
     * @param   i   Index to element.
     * @return  link element.
     */
    Link getLink(int i);
    
    /** Indexed setter to link element.
     * @param   i   Index to element.
     * @param   l   link element
     */
    void setLink(int i, Link l);
    
    /** Adds a link element to the list.
     * @param   l   link element to add.
     */
    void addLink(Link l);
    
    /** Inserts link element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert link at.
     * @param   l   link element to insert.
     */
    void addLink(int i, Link l);
    
    /** Removes all the link elements within the list.
     */
    void clearLinks();
    
    /** Removes a link element from the list.
     * @param   i   Index to element.
     */
    void removeLink(int i);
    
    /** Removes a link element from the list.
     * @param   l   link element to remove.
     * @return  <tt>true</tt> if successfully removed.
     */
    boolean removeLink(Link l);
    
    /** Size of the link element list.
     * @return  list size.
     */
    int getLinkSize();
    
    /** Index of link element within list.
     * @param   link    link element to index.
     * @return  Index (0-based) of element.
     */
    int indexOfLink(XMLNode link);
    
    /** Gets collection of link elements.
     * @return Unmodifiable collection of link elements.
     */
    Collection getLinks();
    
    /**
     * Get a link given link name.
     * @param linkName nae of the link
     * @return Link or null if not found
     */
    Link getLink(String linkName);
}
