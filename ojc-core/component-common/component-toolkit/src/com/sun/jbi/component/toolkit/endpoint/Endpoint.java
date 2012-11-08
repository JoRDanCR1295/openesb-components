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
 * @(#)Endpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.endpoint;

import java.util.Collection;
import javax.jbi.JBIException;
import javax.wsdl.Operation;
import com.sun.jbi.common.descriptor.EndpointInfo;

/**
 * Describes a JBI endpoint.
 * 
 * @author Kevan Simpson
 */
public interface Endpoint<T> {
	/**
	 * Returns <code>true</code> if endpoint is started.
	 * @return <code>true</code> if endpoint is started, else <code>false</code>.
	 */
    public boolean isStarted();
    
    /**
     * Starts the endpoint servicing message exchanges.
     * @throws JBIException if an error occurs starting endpoint.
     */
    public void start() throws JBIException;
    
    /**
     * Stops the endpoint from servicing message exchanges.
     * @throws JBIException if an error occurs stoppoing endpoint.
     */
    public void stop() throws JBIException;
    
    /**
     * Fetches the endpoint definition.
     * @return the endpoint definition.
     */
    public EndpointInfo getInfo();

    /**
     * Gets the service definition for the specified operation (by name).
     * 
     * @param opName The name of the operation.
     */
	public T getServiceDef(String opName);
    
	/**
     * Sets the service definition associated with the specified operation(s).
     * 
	 * @param def The process defintion implementing the specified operation(s).
	 * @param ops The name(s) of implementing operation(s).
	 */
    public void setServiceDef(T def, Operation... ops);
    
    /**
     * Returns the implemented operation by its name.
     * @return the implemented operation by its name. 
     */
    public Operation getOperation(String name);
    
    /**
     * Returns all operations implemented by this endpoint.
     * @return all operations implemented by this endpoint.
     */
    public Collection<Operation> getOperations();
}
