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
 * @(#)NamedWSDLElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import javax.xml.namespace.QName;

/**
 * @author Sun Microsystems
 *
 * QName for all the Named Element which can be referenced by there name
 */
public interface NamedWSDLElement  extends WSDLElement {
	
	 /** Ordinal position of name attribute. */
    public static final int NAME = 0;
    
	/** Describes the attributes for this element */
    public interface ATTR {
        
        /** "name" attribute token */
        public static final String NAME = "name";
    }
    
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
    
	/***
     * get the QName of this element. Qname will have namespace where this element is defined and name of this element.
     * @return QName
     */
    public QName getQName();
    
    /***
     * Utility method.
     * get the QName of this element. This method calls getQName() then 
     * if there is a namespace available in QName then gets the first prefix for the QName and use that in place of namespace.
     * @return QName
     */
    public QName getPrefixedQName();

}
