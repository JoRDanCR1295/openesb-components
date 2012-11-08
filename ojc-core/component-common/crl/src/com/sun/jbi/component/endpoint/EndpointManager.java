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
 * @(#)EndpointManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.endpoint;

import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.ServiceUnit;

/**
 * Defines a manager of JBI endpoints.
 * 
 * @author Kevan Simpson
 */
public interface EndpointManager {
	/**
	 * Fetches the {@link EndpointFactory} for this utility.
	 * @return the <code>EndpointFactory</code> for this utility.
	 */
    public EndpointFactory getEndpointFactory();

    /**
     * Initializes the specified service unit.
     * @param srvcUnit The service unit to initialize.
     * @throws DeploymentException if an error occurs initializing service unit.
     */
    public void addServiceUnit(ServiceUnit srvcUnit) throws DeploymentException;

    /**
     * Removes the specified {@link ServiceUnit} from this registry.
     * @param unit The service unit to remove.
     */
    public void removeServiceUnit(ServiceUnit unit);

    /**
     * Fetches an {@link Endpoint} by service and endpoint names.
     * 
     * @param serviceName The qualified service name of the endpoint.
     * @param endpointName The name of the endpoint.
     * @param provisioning <code>true</code> if the endpoint provisions a service.
     * @return an <code>Endpoint</code> or <code>null</code>.
     */
    public Endpoint lookupEndpoint(QName serviceName,
                                   String endpointName,
                                   boolean provisioning);
    
    /**
     * Stores an {@link Endpoint} by service and endpoint names.
     * 
     * @param serviceName The qualified service name of the endpoint.
     * @param endpointName The name of the endpoint.
     * @param ept The endpoint to store.
     */
    public void registerEndpoint(QName serviceName, 
                                 String endpointName, 
                                 Endpoint ept);

    /**
     * Removes an {@link Endpoint} from this utility.
     * 
     * @param serviceName The qualified service name of the endpoint.
     * @param endpointName The name of the endpoint.
     * @param provisioning <code>true</code> if the endpoint provisions a service.
     * @return The removed endpoint or <code>null</code>.
     */
    public Endpoint removeEndpoint(QName serviceName, 
                                   String endpointName,
                                   boolean provisioning);

    /**
     * Fetches the {@link ServiceUnit} responsible for deploying the {@link Endpoint}
     * which corresponds to the specified {@link ServiceEndpoint}.
     * 
     * @param endpt A <code>ServiceEndpoint</code>, likely acquired from a message exchange.
     * @return a <code>ServiceUnit</code> or <code>null</code>.
     */
    public ServiceUnit lookupServiceUnit(ServiceEndpoint endpt);
}
