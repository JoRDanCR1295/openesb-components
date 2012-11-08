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
 * @(#)WSDLExtensibleElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import com.sun.wsdl.model.common.model.XMLCharacterData;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

import java.util.Collection;

/**
 * Describes the WSDL extensible element interface.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface WSDLExtensibleElement extends WSDLElement, XMLCharacterData {
        
    /** Getter for the WSDL extensibility element.
     * @param   i   Index to the extensibility element.
     * @return  The WSDL extensibility element used.
     */
    ExtensibilityElement getExtensibilityElement(int i);
    
    /** Adds the WSDL extensibility element to the list.
     * @param   elem    The WSDL extensibility element to use.
     */
    void addExtensibilityElement(ExtensibilityElement elem);
    
    /**
     * add extensibility element at tail
     * @param elem
     */
    void addExtensibilityElementAtTail(ExtensibilityElement elem);
    
    
    /** Removes the WSDL extensibility element from the list.
     * @param   elem    The WSDL extensibility element to remove.
     * @return  <code>true</code> if successfully removed; <code>false</code> otherwise.
     */
    boolean removeExtensibilityElement(ExtensibilityElement elem);
    
    /** Counts the number of existing extensibility elements.
     * @return  The number of existing extensibility elements.
     */
    int getExtensibilityElementsSize();
    
    /** Returns a unmodifiable collection of WSDL Extensibility elements.
     * @return  Unmodifiable collection of WSDL Extensibility elements.
     */
    Collection getExtensibilityElements();
    
}
