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
 * @(#)PortType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.util.Collection;

/**
 * Describes the WSDL &lt;portType&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface PortType
    extends WSDLElement, NamedWSDLElement {
        
    /** Tag for this element */
    public static final String TAG = "portType";
    
    
    /**
     * Gets the &lt;operation&gt; element at the specified position.
     * @param index the position
     * @return the operation element
     */
    Operation getOperation(int index);
    
    /**
     * Get collection of Operation matching operation name
     * @return all operation which have matching operation name
     */
    Collection getOperations(String operationName);
    
    /**
     * Adds a new operation.
     * @param operation the operation to add
     */
    void addOperation(Operation operation);
    
    /**
     * add operation at tail
     * @param operation
     */
    void addOperationAtTail(Operation operation);
    
    
    /**
     * Removes the given operation.
     * @param operation to be removed
     */
    void removeOperation(Operation operation);
    
    /**
     * Gets the number of operations.
     * @return the number of operations
     */
    int countOperations();

    
    /**
     * Gets the list of all operations.
     * @return a read-only collection of Operations.
     */
    Collection getOperations();
}
