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
 * @(#)SOAPBinding.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.soap;

import com.sun.wsdl.model.extensions.ExtensibilityElement;

/**
 * Describes the SOAP binding extension.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface SOAPBinding extends ExtensibilityElement, SOAPConstants {
    
    /** Tag for this element */
    public static final String TAG = "binding";
    
    /** Qualified Tag for this element */
    public static final String QTAG = PREFIX + ":" + TAG;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR {
        
        /** "style" attribute token */
        public static final String STYLE = "style";
        
        /** "transport" attribute token */
        public static final String TRANSPORT = "transport";
    }
    
    /** Ordinal position for style attribute */
    public static final int STYLE = 0;
    
    /** Ordinal position for transport attribute */
    public static final int TRANSPORT = STYLE + 1;
    
    /** Gets the style of this SOAP binding.
     * @return  SOAP binding style.
     */
    String getStyle();
    
    /** Sets the style of this SOAP binding.
     * @param   style   SOAP binding style.
     */
    void setStyle(String style);
    
    /** Sets the style of this SOAP binding.
     * @param   qName   Qualified name of this attribute.
     * @param   style   SOAP binding style.
     */
    void setStyle(String qName, String style);
    
    /** Gets the URI of the SOAP transport.
     * @return  SOAP transport URI.
     */
    String getTransport();
    
    /** Sets the URI of the SOAP transport.
     * @param   transport   SOAP transport URI.
     */
    void setTransport(String transport);
    
    /** Sets the URI of the SOAP transport.
     * @param   qName       Qualified name of this attribute.
     * @param   transport   SOAP transport URI.
     */
    void setTransport(String qName, String transport);
}
