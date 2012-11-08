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
 * @(#)ExtensibilityElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions;

import com.sun.wsdl.model.WSDLExtensibleElement;
import javax.xml.namespace.QName;

/**
 * Describes an extensiblitiy element in WSDL.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface ExtensibilityElement extends WSDLExtensibleElement {
    
    /** Tag for this element. */
    public static final String TAG = "#extension";
    
    /**
     * Set the type of this extensibility element.
     *
     * @param elementType the type
     */
    void setElementType(QName elementType);
    
    /**
     * Get the type of this extensibility element.
     *
     * @return the extensibility element's type
     */
    QName getElementType();
    
    /**
     * Set whether or not the semantics of this extension
     * are required. Relates to the wsdl:required attribute.
     *
     * @param   required    <code>true</code> if semantics is required.
     */
    void setRequired(Boolean required);
    
    /**
     * Get whether or not the semantics of this extension
     * are required. Relates to the wsdl:required attribute.
     *
     * @return  <code>true</code> if semantics is required.
     */
    Boolean getRequired();
    
}
