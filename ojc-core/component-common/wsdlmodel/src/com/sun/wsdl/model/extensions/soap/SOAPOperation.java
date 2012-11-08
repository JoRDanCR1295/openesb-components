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
 * @(#)SOAPOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.soap;

import com.sun.wsdl.model.extensions.ExtensibilityElement;

/**
 * Describes the SOAP operation extension.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface SOAPOperation extends ExtensibilityElement, SOAPConstants {
    
    /** Tag for this element */
    public static final String TAG = "operation";
    
    /** Qualified Tag for this element */
    public static final String QTAG = PREFIX + ":" + TAG;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR {
        
        /** "soapAction" attribute token */
        public static final String SOAP_ACTION = "soapAction";
        
        /** "style" attribute token */
        public static final String STYLE = "style";
    }
    
    /** Ordinal position for soapAction attribute */
    public static final int SOAP_ACTION = 0;
    
    /** Ordinal position for style attribute */
    public static final int STYLE = SOAP_ACTION + 1;
    
    /** Gets the URI for the SOAPAction header.
     * @return  URI for SOAPAction header.
     */
    String getSoapAction();
    
    /** Sets the URI for the SOAPAction header.
     * @param   uri     URI for the SOAPAction header.
     */
    void setSoapAction(String uri);
    
    /** Sets the URI for the SOAPAction header.
     * @param   qName   Qualified name of this attribute.
     * @param   uri     URI for the SOAPAction header.
     */
    void setSoapAction(String qName, String uri);
    
    /** Gets the style of this SOAP operation.
     * @return  SOAP operation style.
     */
    String getStyle();
    
    /** Sets the style of this SOAP operation.
     * @param   style   SOAP operation style.
     */
    void setStyle(String style);
    
    /** Sets the style of this SOAP operation.
     * @param   qName   Qualified name of this attribute.
     * @param   style   SOAP operation style.
     */
    void setStyle(String qName, String style);
}
