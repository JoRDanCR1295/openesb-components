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
 * @(#)PartnerLinks.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;partners&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface PartnerLinks extends BPELElement {
    
    /** Tag for this element */
    public static final String TAG = "partnerLinks";
    
    /** name based getter for property partner.
     * @param name name of the partner.
     * @return Value of the property with given <CODE>name</CODE>.
     *
     */
    PartnerLink getPartnerLink(String name);
    
    
    /** Indexed getter for property partner.
     * @param index Index of the property.
     * @return Value of the property at <CODE>index</CODE>.
     *
     */
    PartnerLink getPartnerLink(int index);
    
    /** Adds a new partnerLink property to the list.
     * @param   partnerLink     New partnerLink.
     */
    void addPartnerLink(PartnerLink partnerLink);
    
    /** Inserts partnerLink element at given index within list; pushes all elements after to the right.
     * @param   i       Index to insert partnerLink at.
     * @param   partnerLink partnerLink element to insert.
     */
    void addPartner(int i, PartnerLink partnerLink);
    
    /** Removes all the partnerLink elements within the list.
     */
    void clearPartners();
    
    /** Removes a partnerLink element from the list.
     * @param   i   Index of the partnerLink to be removed.
     */
    void removePartnerLink(int i);
    
    /** Removes the specified partnerLink element from the list.
     * @param   p   partnerLink element to remove.
     * @return  <tt>true</tt> if partner was successfully removed.
     */
    boolean removePartnerLink(PartnerLink p);
    
    /** Size of property partners
     * @return The number of partnerLink defined.
     *
     */
    int getPartnerLinksSize();
    
    
    /** Gets collection of partnerLink elements.
     * @return Unmodifiable collection of partnerLink elements.
     */
    Collection getPartnerLinks();
}
