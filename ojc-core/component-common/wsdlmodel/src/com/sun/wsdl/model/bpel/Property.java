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
 * @(#)Property.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.bpel;

import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

/**
 * Describes the &lt;property&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Property extends ExtensibilityElement {
    
    /** Tag for this element. */
    public static final String TAG = "property";
    
    /** Describes attributes of this element.
     */
    public interface ATTR {
        
        /** "name" attribute token */
        public static final String NAME = "name";
        
        /** "type" attribute token */
        public static final String TYPE = "type";
    }
    /** Ordinal position of attribute: name. */
    public static final int NAME = 0;
    
    /** Ordinal position of attribute: type. */
    public static final int TYPE = NAME + 1;
    
    /** Getter for name attribute.
     * @return  Value of name attribute.
     */
    String getName();
    
    /** Setter for name attribute.
     * @param   n   Value of name attribute.
     */
    void setName(String n);
    
    /** Getter for type attribute.
     * @return  Value of type attribute.
     */
    String getType();
    
    /** Setter for type attribute.
     * @param   t   Value of type attribute.
     */
    void setType(String t);
    
}
