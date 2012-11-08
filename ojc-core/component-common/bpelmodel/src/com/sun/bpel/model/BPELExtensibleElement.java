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
 * @(#)BPELExtensibleElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the BPEL extensible element interface.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface BPELExtensibleElement extends BPELElement {
        
    /** Getter for the BPEL extensibility element.
     * @param   i   Index to the extensibility element.
     * @return  The BPEL extensibility element used.
     */
    ExtensibilityElement getExtensibilityElement(int i);
    
    /** Setter for the BPEL extensibility element.
     * @param   i   Index to the extensibility element.
     * @param   elem    The extensibility element.
     */
    void setExtensibilityElement(int i, ExtensibilityElement elem);
    
    /** Adds the BPEL extensibility element to the list.
     * @param   elem    The BPEL extensibility element to use.
     */
    void addExtensibilityElement(ExtensibilityElement elem);
    
    /** Removes the BPEL extensibility element from the list.
     * @param   i   Index to the extensibility element.
     */
    void removeExtensibilityElement(int i);
    
    /** Removes the BPEL extensibility element from the list.
     * @param   elem    The BPEL extensibility element to remove.
     * @return  <code>true</code> if successfully removed; <code>false</code> otherwise.
     */
    boolean removeExtensibilityElement(ExtensibilityElement elem);
    
    /** Counts the number of existing extensibility elements.
     * @return  The number of existing extensibility elements.
     */
    int getExtensibilityElementsSize();
    
    /** Returns a unmodifiable collection of BPEL Extensibility elements.
     * @return  Unmodifiable collection of BPEL Extensibility elements.
     */
    Collection getExtensibilityElements();
    
    /** Gets the index of the BPEL extensibility element within the list.
     * @param   elem    BPEL extensibility element to locate.
     * @return  Index of the BPEL extensibility element in the list or <code>-1</code> if not found.
     * @since   5.1.0
     */
    int indexOfExtensibilityElement(XMLNode elem);
    
}
