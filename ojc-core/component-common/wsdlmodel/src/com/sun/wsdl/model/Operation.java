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
 * @(#)Operation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import com.sun.wsdl.model.common.model.XMLNode;

import java.util.Collection;

/**
 * Describes the portType &lt;operation&gt; element.
 * 
 * An operation element can be represented in DTD as:
 * &lt;!ELEMENT operation
 * ((input, (output, fault*)?) | (output, (input, fault*)?))
 * &gt;
 * 
 * Since order is important even though the same elements are involved,
 * we have to remember whether operation is of the first type,
 * a request-response-one-way-operation, or the second type, a
 * solicit-response-notification-operation.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Operation
    extends WSDLExtensibleElement {
        
    /** unknown operation type. */
    public static final int UNKNOWN_OPERATION = -1;
    
    /** one-way-operation type */
    public static final int ONE_WAY_OPERATION = 1;
    
    /** request-response-operation type */
    public static final int REQUEST_RESPONSE_OPERATION = 2;
    
    /** solicit-response-operation type */
    public static final int SOLICIT_RESPONSE_OPERATION = 4;
    
    /** notification-operation type */
    public static final int NOTIFICATION_OPERATION = 8;
        
    /** request-response-one-way-operation type */
    public static final int REQUEST_RESPONSE_ONE_WAY_OPERATION = 3;
    
    /** solicit-response-notification-operation type */
    public static final int SOLICIT_RESPONSE_NOTIFICATION_OPERATION = 12;
        
    /** Tag for this element */
    public static final String TAG = "operation";
    
    /** Describes the attributes for this element */
    public interface ATTR extends WellKnownAttr {
        
        /** "name" attribute token */
        public static final String NAME = "name";
        
        /** "parameterOrder" attribute token */
        public static final String PARAMETER_ORDER = "parameterOrder";
    }
    
    /** Ordinal position of name attribute. */
    public static final int NAME = 0;
    
    /** Ordinal position of parameterOrder attribute. */
    public static final int PARAMETER_ORDER = 1;
    
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
     * Getter for property parameterOrder.
     * @return Value of property parameterOrder.
     */
    String getParameterOrder();
    
    /**
     * Setter for property parameterOrder.
     * @param parameterOrder   New value of property parameterOrder.
     */
    void setParameterOrder(String parameterOrder);
    
    /**
     * Gets the operation type.
     * @return the operation type
     */
    int getOperationType();
    
    /**
     * Sets the operation type.
     * @param operationType the operation type; it must be one of
     * REQUEST_RESPONSE_ONE_WAY_OPERATION or
     * SOLICIT_RESPONSE_NOTIFICATION_OPERATION.
     */
    void setOperationType(int operationType);
    
    /**
     * Gets the &lt;input&gt; element.
     * @return the input element
     */
    OperationInput getInput();
    
    /**
     * Sets the &lt;input&gt; element.
     * @param input the input element
     */
    void setInput(OperationInput input);
    
    
    /**
     * Sets the &lt;input&gt; element and add it at the end of collection.
     * @param input the input element
     */
    void setInputAtTail(OperationInput input);
    
    /**
     * Gets the &lt;output&gt; element.
     * @return the output element
     */
    OperationOutput getOutput();
    
    /**
     * Sets the &lt;output&gt; element.
     * @param output the output element
     */
    void setOutput(OperationOutput output);
   
    
    /**
     * Sets the &lt;output&gt; element and add it at the end of collection.
     * @param output the output element
     */
    void setOutputAtTail(OperationOutput output);
    
    /**
     * Getter for property fault.
     * @param index index of the property to get
     * @return Value of property fault.
     *
     */
    OperationFault getFault(int index);
    
    
    /**
     * Adds a new fault.
     * @param fault the fault to add
     */
    void addFault(OperationFault fault);
    
    
    /**
     * Adds a new fault at the end of collection.
     * @param fault the fault to add
     */
    void addFaultAtTail(OperationFault fault);
    
    /**
     * Gets the number of faults.
     * @return the number of faults
     */
    int countFaults();
    

    /**
     * Gets the list of all faults.
     * @return a read-only collection of OperationFaults.
     */
    Collection getFaults();
    
}
