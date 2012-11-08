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
 * @(#)PartnerLinkRole.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

import java.util.Collection;

/**
 * Describes the SLT &lt;role&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface PartnerLinkRole
    	extends ExtensibilityElement {
        
    /** Tag for this element */
    public static final String TAG = "role";
    
    /** Describes the attributes for this element */
    public interface ATTR {
        
        /** "name" attribute token */
        public static final String NAME = "name";
        
        /** "portType" attribute token */
        public static final String PORTTYPE = "portType";
    }
    
    /** Ordinal position of name attribute. */
    public static final int NAME = 0;
    
    public static final int PORTTYPE = 1;
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    String getName();
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    void setName(String name);
    
    /**
     * set PortType
     * @param portType
     */
    void setPortType(QName portType);
    
    /**
     * Get PortType 
     * @return
     */
    QName getPortType();
    
    
    /**
     * Get the PortType this SeriviceLinkPortType refers to
     * @return PortType
     */
    PortType getWSDLPortType();
    
}
