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
 * @(#)Catch.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;

import com.sun.wsdl4j.ext.DeferredActionAccepter;

/**
 * Describes the &lt;catch&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Catch extends BPELElement, SingleActivityHolder, VariableScope,
        DeferredActionAccepter {
    
    /** Tag for this element. */
    public static final String TAG = "catch";
    
    /** Describes the attributes of this element.
     */
    public interface ATTR {
        
        /** "faultName" attribute token */
        public static final String FAULT_NAME = "faultName";
        
        /** "faultContainer" attribute token */
        public static final String FAULT_CONTAINER = "faultVariable";
        
        /** "faultMessageType" attribute token */
        public static final String FAULT_MESSAGE_TYPE = "faultMessageType";
        
        /** "faultElement" attribute token */
        public static final String FAULT_ELEMENT = "faultElement";
        
        
    }
    
    /** Ordinal position of faultName attribute */
    public static final int FAULT_NAME = 0;
    
    /** Ordinal position of faultContainer attribute */
    public static final int FAULT_CONTAINER = 1;
    
    /** Ordinal position of faultMessageType attribute */
    public static final int FAULT_MESSAGE_TYPE = 2;
    
    /** Ordinal position of faultElement attribute */
    public static final int FAULT_ELEMENT = 3;
    
    /** Getter for property faultName.
     * @return Value of property faultName.
     *
     */
    QName getFaultName();
    
    /** Setter for property faultName.
     * @param faultName New value of property faultName.
     *
     */
    void setFaultName(QName faultQName);
    
    /** Setter for property faultVariable.
     * @param faultVariable New value of property faultContainer.
     *
     */
    void setFaultVariable(String faultVariable);
    
    
    /** Getter for property faultVariable.
     * @return Value of property faultVariable.
     *
     */
    String getFaultVariable();
    
    
    /**
     * Set fault message type
     * @param faultMessageType
     */
    void setFaultMessageType(QName faultMessageType);
    
    /**
     * Get fault message type.
     * @return fault message type.
     */
    QName getFaultMessageType();
    
    /**
     * Set fault element.
     * @param faultElement
     */
    void setFaultElement(QName faultElement);
    
    /**
     * Get Fault element
     * @return fault element
     */
    QName getFaultElement();
    
    
    /**
     * get fault variable
     * @return Variable
     */
    Variable getBPELFaultVariable();
    
    /**
     * set fault variable
     * @param variable Variable
     */
    void setBPELFaultVariable(Variable variable);
    
    /**
     * Get WSDLMessage object correponding to faultMessageType
     * @return WSDLMessage
     */
    Message getWSDLFaultMessageType();
    
    /**
     * Get ElementDecl object correponding to faultElement
     * @return ElementDecl
     */
    SchemaGlobalElement getXSDFaultElement();
    
    /**
     * Initializes member variables
     */    
    void initMembers();
    
    /**
     * Initializes member variables locally without resolving to XSD artifacts.
     */    
    void initMembersLocally();
}
