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
 * @(#)Binding.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.util.Collection;

import javax.xml.namespace.QName;

/**
 * Describes the &lt;binding&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Binding extends NamedWSDLExtensibleElement {
    
    /** Tag for this element */
    public static final String TAG = "binding";
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends NamedWSDLExtensibleElement.ATTR {
        
        /** "type" attribute token */
        public static final String TYPE = "type";
    }

    /** Ordinal position for type attribute */
    public static final int TYPE = NAME + 1;
    
    /** Getter for the name of the binding element.
     * @return  Name of binding element.
     */
    String getName();
    
    /** Setter for the name of the binding element.
     * @param   name    Value of the name for binding element.
     */
    void setName(String name);
    
    
    /** Getter for property type.
     * @return Value of Port type.
     *
     */
    QName getType();
    
    /** Setter for Port type.
     * @param type New value of Port type.
     *
     */
    void setType(QName type);
    
    /**
     * Get the PortType for this binding.
     * @return PortType.
     */
    PortType getWSDLPortType();
    
    
    /** Setter for Port type.
     * @param qName New qName of Port type.
     * @param type New value of Port type.
     *
     */
    void setType(String qName, String type);
    
    /**
     * get all biniding operations
     * @return collection of biniding operation.
     */
    Collection getBindingOperations();
    
    /** Getter for the binding operation.
     * @param index Index of the operation.
     * @return operation instance at <CODE>index</CODE>.
     *
     */
    BindingOperation getBindingOperation(int index);
    
    /** Number of binding operation elements present.
     * @return  Number of binding operation's.
     */
    int getBindingOperationSize();
    
    /** Add a new binding operation to the list.
     * @param   op  New binding operation.
     */
    void addBindingOperation(BindingOperation op);
    
    
    /** Add a new binding operation to the end of list.
     * @param   op  New binding operation.
     */
    void addBindingOperationAtTail(BindingOperation op);
        
    
    /** Removes a binding operation element from the list.
     * @param   op  Binding operation element to remove.
     * @return  <tt>true</tt> if binding operation element successfully removed.
     */
    boolean removeBindingOperation(BindingOperation op);
    
}
