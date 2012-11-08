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
 * @(#)OperationInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import javax.xml.namespace.QName;

/**
 * Describes the portType operation &lt;input&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface OperationInput
    extends WSDLElement {
        
    /** Tag for this element */
    public static final String TAG = "input";
    
    /** Describes the attributes for this element */
    public interface ATTR extends WellKnownAttr {
        
        /** "name" attribute token */
        public static final String NAME = "name";
        
        /** "message" attribute token */
        public static final String MESSAGE = "message";
    }
    
    /** Ordinal position of name attribute. */
    public static final int NAME = 0;
    
    /** Ordinal position of message attribute. */
    public static final int MESSAGE = 1;
    
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
     * Getter for property message.
     * @return Value of property message.
     */
    QName getMessage();
    
    /**
     * Setter for property message.
     * @param message   New value of property message.
     */
    void setMessage(QName message);
    
    
    WSDLMessage getWSDLMessage();
    
}
