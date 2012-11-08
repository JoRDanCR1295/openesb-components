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
 * @(#)SOAPHeader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.soap;

import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

/**
 * Describes the SOAP header extension.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface SOAPHeader extends ExtensibilityElement, SOAPConstants {
    
    /** Tag for this element */
    public static final String TAG = "header";
    
    /** Qualified Tag for this element */
    public static final String QTAG = PREFIX + ":" + TAG;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR {
        
        /** "message" attribute token */
        public static final String MESSAGE = "message";
        
        /** "part" attribute token */
        public static final String PART = "part";
        
        /** "use" attribute token */
        public static final String USE = "use";
        
        /** "encodingStyle" attribute token */
        public static final String ENCODING_STYLE = "encodingStyle";
        
        /** "namespace" attribute token */
        public static final String NAMESPACE = "namespace";
    }
    
    /** Ordinal position for message attribute */
    public static final int MESSAGE = 0;
    
    /** Ordinal position for part attribute */
    public static final int PART = MESSAGE + 1;
    
    /** Ordinal position for use attribute */
    public static final int USE = PART + 1;
    
    /** Ordinal position for encodingStyle attribute */
    public static final int ENCODING_STYLE = USE + 1;
    
    /** Ordinal position for namespace attribute */
    public static final int NAMESPACE = ENCODING_STYLE + 1;
    
    /** Gets the message attribute for this SOAP header.
     * @return  The message attribute.
     */
    String getMessage();
    
    /** Sets the message attribute for this SOAP header.
     * @param   message     Value of the message attribute.
     */
    void setMessage(String message);
    
    /** Sets the message attribute for this SOAP header.
     * @param   qName       Qualified name of attribute.
     * @param   message     Value of the message attribute.
     */
    void setMessage(String qName, String message);
    
    /** Gets the part attribute for this SOAP header.
     * @return  The part attribute.
     */
    String getPart();
    
    /** Sets the part attribute for this SOAP header.
     * @param   part    Value of the part attribute.
     */
    void setPart(String part);
    
    /** Sets the part attribute for this SOAP header.
     * @param   qName   Qualified name of attribute.
     * @param   part   Value of the part attribute.
     */
    void setPart(String qName, String part);
    
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
    
    /** Getter for the SOAP header fault element.
     * @param index Index of the fault element.
     * @return SOAP header fault instance at <CODE>index</CODE>.
     */
    SOAPHeaderFault getSOAPHeaderFault(int index);
    
    /** Indexed setter for SOAP header fault element.
     * @param index Index of the fault element.
     * @param fault SOAP header fault at <CODE>index</CODE>.
     */
    void setSOAPHeaderFault(int index, SOAPHeaderFault fault);
    
    /** Number of SOAP header fault elements present.
     * @return  Number of SOAP header fault's.
     */
    int getSOAPHeaderFaultSize();
    
    /** Add a new SOAP header fault to the list.
     * @param   fault   New SOAP header fault.
     */
    void addSOAPHeaderFault(SOAPHeaderFault fault);
    
    /** Removes a SOAP header fault element from the list.
     * @param   i   Index to the SOAP header fault element.
     */
    void removeSOAPHeaderFault(int i);
    
    /** Removes a SOAP header fault element from the list.
     * @param   fault   SOAP header fault element to remove.
     * @return  <tt>true</tt> if SOAP header fault element successfully removed.
     */
    boolean removeSOAPHeaderFault(SOAPHeaderFault fault);
    
    /** Gets the index of a SOAP header fault element within the list.
     * @param   fault   SOAP head fault to locate.
     * @return  Index of the SOAP header fault element or <code>-1</code> if none found.
     * @since   5.1.0
     */
    int indexOfSOAPHeaderFault(XMLNode fault);
    
}
