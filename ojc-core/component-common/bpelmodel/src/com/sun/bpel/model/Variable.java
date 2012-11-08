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
 * @(#)Variable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;

import com.sun.wsdl4j.ext.DeferredActionAccepter;

/**
 * Describes the &lt;container&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Variable extends BPELElement, NamedElement, DeferredActionAccepter {
    
    /** Tag for this element */
    public static final String TAG = "variable";
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends NamedElement.ATTR {
        /** "messageType" attribute token */
        public static final String MESSAGE_TYPE = "messageType";
        
        /** "type" attribute **/
        public static final String TYPE = "type";
        
        /** "element" attribute **/
        public static final String ELEMENT = "element";
        
        /** "generateEvent" attribute token */
    	public static final String GENERATE_EVENTS = "generateEvents";
        
    }
    
    /** Ordinal position for messageType attribute */
    public static final int MESSAGE_TYPE = NAME + 1;
    
    /** Ordinal position for type attribute */
    public static final int TYPE = MESSAGE_TYPE + 1;
    
    /** Ordinal position for element attribute */
    public static final int ELEMENT = TYPE + 1;
    
    /** Ordinal position for generateEvent attribute. */
    public static final int GENERATE_EVENTS = ELEMENT + 1;
    
    
    /** Setter for property name.
     * @param qName New qName of property name.
     * @param name  New value of property name.
     *
     */
    void setName(String qName, String name);
    
    /** Getter for property messageType.
     * @return Value of property messageType.
     *
     */
    QName getMessageType();
    
    /** Setter for property messageType.
     * @param messageType New value of property messageType.
     *
     */
    void setMessageType(QName messageType);
    
    /**
     * Get xsd type property for this variable.
     * @return type
     */
    QName getType();
    
    /**
     * Set xsd type property for this variable. 
     * @param type xsd type.
     */
    void setType(QName type);
    
    /**
     * Get xsd element property for this variable.
     * @return xsd element.
     */
    QName getElement();
    
    /**
     * set xsd element property for this variable.
     * @param element xsd element.
     */
    void setElement(QName element);
    
    /** Getter for property generateEvents.
     * @return Value of property generateEvents.
     *
     */
    String getGenerateEvents();
    
    /** Setter for property generateEvents.
     * @param generateEvents New value of property generateEvents.
     *
     */
    void setGenerateEvents(String generateEvents);
    
    /**
     * Get WSDLMessage.
     * This will return WSDLMessage for which is represented
     * by messageType attribute.
     * @return WSDLMessage
     */
    Message getWSDLMessageType();
    
    /**
     * Get XMLType object correponding to type
     * @return
     */
    SchemaType getXSDType();
    
    /**
     * Get ElementDecl object correponding to element
     * @return ElementDecl
     */
    SchemaGlobalElement getXSDElement();
    
    /**
     * Initializes member variables
     */    
    void initMembers();
}
