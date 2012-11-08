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
 * @(#)SOAPFault.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.soap;

import com.sun.wsdl.model.extensions.ExtensibilityElement;

/**
 * Describes the SOAP fault extension.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface SOAPFault extends ExtensibilityElement, SOAPConstants {
    
    /** Tag for this element */
    public static final String TAG = "fault";
    
    /** Qualified Tag for this element */
    public static final String QTAG = PREFIX + ":" + TAG;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR {
        
        /** "name" attribute token */
        public static final String NAME = "name";
        
        /** "use" attribute token */
        public static final String USE = "use";
        
        /** "encodingStyle" attribute token */
        public static final String ENCODING_STYLE = "encodingStyle";
        
        /** "namespace" attribute token */
        public static final String NAMESPACE = "namespace";
    }
    
    /** Ordinal position for name attribute */
    public static final int NAME = 0;
    
    /** Ordinal position for use attribute */
    public static final int USE = NAME + 1;
    
    /** Ordinal position for encodingStyle attribute */
    public static final int ENCODING_STYLE = USE + 1;
    
    /** Ordinal position for namespace attribute */
    public static final int NAMESPACE = ENCODING_STYLE + 1;
    
    /** Gets the name attribute for this SOAP header.
     * @return  The name attribute.
     */
    String getName();
    
    /** Sets the name attribute for this SOAP header.
     * @param   name     Value of the name attribute.
     */
    void setName(String name);
    
    /** Sets the name attribute for this SOAP header.
     * @param   qName    Qualified name of attribute.
     * @param   name     Value of the name attribute.
     */
    void setName(String qName, String name);
    
    /** Gets the use attribute of this SOAP header.
     * @return  The use attribute.
     */
    String getUse();
    
    /** Sets the use attribute of this SOAP header.
     * @param   use     Value of the use attribute.
     */
    void setUse(String use);
    
    /** Sets the use attribute of this SOAP header.
     * @param   qName   Qualified name of attribute.
     * @param   use     Value of the use attribute.
     */
    void setUse(String qName, String use);
    
    /** Gets the encoding style of this SOAP header.
     * @return  SOAP body encoding style.
     */
    String getEncodingStyle();
    
    /** Sets the encoding style of this SOAP header.
     * @param   style   SOAP body encoding style.
     */
    void setEncodingStyle(String style);
    
    /** Sets the encoding style of this SOAP header.
     * @param   qName   Qualified name of this attribute.
     * @param   style   SOAP body encoding style.
     */
    void setEncodingStyle(String qName, String style);
    
    /** Gets the namespace attribute of this SOAP header.
     * @return  The namespace attribute.
     */
    String getNamespaceURI();
    
    /** Sets the namespace attribute of this SOAP header.
     * @param   namespace   Value of the namespace attribute
     */
    void setNamespaceURI(String namespace);
    
    /** Sets the namespace attribute of this SOAP header.
     * @param   qName   Qualified name of this attribute.
     * @param   namespace   Value of the namespace attribute
     */
    void setNamespaceURI(String qName, String namespace);
}
