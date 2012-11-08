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
 * @(#)Part.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.XMLType;

import javax.xml.namespace.QName;

/**
 * Describes the &lt;part&gt; element.
 *
 * TODO: change element and type to QName
 * @author Sun Microsystems
 * @version 
 */
public interface Part extends WSDLElement, NamedWSDLElement {
    /** Tag for this element */
    public static final String TAG = "part";
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends WellKnownAttr, NamedWSDLElement.ATTR {
        
        /** "type" attribute token */
        public static final String TYPE = "type";
        
        /** "element" attribute token */
        public static final String ELEMENT = "element";
    }
    
    /** Ordinal position for type attribute */
    public static final int TYPE = NAME + 1;
    
    /** Ordinal position for element attribute */
    public static final int ELEMENT = TYPE + 1;
    
    /** Getter for property type.
     * @return Value of property type.
     *
     */
    QName getType();
    
    /** Setter for property type.
     * @param type New value of property type.
     *
     */
    void setType(QName type);
    
    
    /** Getter for property element.
     * @return Value of property element.
     *
     */
    QName getElement();
    
    /** Setter for property element.
     * @param element New value of property element.
     *
     */
    void setElement(QName element);
    
   /**
    * get XMLType object represented by type attribute.
    * @return XMLType
    */
    XMLType getXSDType();
    
    /**
     * set XMLType object which will result in type attribute.
     * If targetNamespace of the schema where this element is defined is
      * not available then IllegalArgumentException will be throws
     * @param type XMLType
     */
    void setXSDType(XMLType type);
    
    
    /**
     * get ElementDecl object represented by type attribute.
     * @return ElementDecl
     */
     ElementDecl getXSDElement();
     
     /**
      * set ElementDecl object which will result in element attribute.
      * If targetNamespace of the schema where this element is defined is
      * not available then IllegalArgumentException will be throws
      * @param element ElementDecl
      */
     void setXSDElement(ElementDecl element);
     
   
    
}
