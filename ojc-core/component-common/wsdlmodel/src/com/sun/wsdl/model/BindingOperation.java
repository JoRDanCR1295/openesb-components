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
 * @(#)BindingOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.util.Collection;

import com.sun.wsdl.model.common.model.XMLNode;

/**
 * Describes the binding &lt;operation&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface BindingOperation extends WSDLExtensibleElement {
    
    /** Tag for this element */
    public static final String TAG = "operation";
        
    /** Describes the attributes for this element.
     */
    public interface ATTR {
        
        /** "name" attribute token */
        public static final String NAME = "name";
    }
    
    /** Ordinal position for name attribute */
    public static final int NAME = 0;
    
    /** Getter for the name of the binding operation element.
     * @return  Name of binding operation element.
     */
    String getName();
    
    /** Setter for the name of the binding operation element.
     * @param   name    Value of the name for binding operation element.
     */
    void setName(String name);
    
    /** Setter for the name of the binding operation element.
     * @param   qName   Qualified name of the binding operation element.
     * @param   name    Value of the name for binding operation element.
     */
    void setName(String qName, String name);
    
    /** Getter for the binding operation input element.
     * @return  The binding operation input element.
     */
    BindingInput getBindingInput();
    
    /** Setter for the binding operation input element.
     * @param   input   The binding operation input element
     */
    void setBindingInput(BindingInput input);
    
    /** Setter for the binding operation input element to add it at the end of list.
     * @param   input   The binding operation input element
     */
    void setBindingInputAtTail(BindingInput input);
    
    /** Getter for the binding operation output element.
     * @return  The binding operation output element.
     */
    BindingOutput getBindingOutput();
    
    /** Setter for the binding operation output element.
     * @param   output  The binding operation output element.
     */
    void setBindingOutput(BindingOutput output);
    
    /** Setter for the binding operation output element to add it at the end of list.
     * @param   output  The binding operation output element.
     */
    void setBindingOutputAtTail(BindingOutput output);
    
    /**
     * get all binding faults
     * @return collection of binding faults
     */
    Collection getBindingFaults();
    
    /** Getter for the binding operation fault element.
     * @param index Index of the fault element.
     * @return Binding operation fault instance at <CODE>index</CODE>.
     *
     */
    BindingFault getBindingFault(int index);
    
    /** Number of binding operation fault elements present.
     * @return  Number of binding operation fault's.
     */
    int getBindingFaultSize();
    
    /** Add a new binding operation fault to the list.
     * @param   fault   New binding operation fault.
     */
    void addBindingFault(BindingFault fault);
    
    
    /** Add a new binding operation fault at the end of list.
     * @param   fault   New binding operation fault.
     */
    void addBindingFaultAtTail(BindingFault fault);
    
    /** Removes a binding operation fault element from the list.
     * @param   fault   Binding operation fault element to remove.
     * @return  <tt>true</tt> if binding operation fault element successfully removed.
     */
    boolean removeBindingFault(BindingFault fault);
  
    
}
