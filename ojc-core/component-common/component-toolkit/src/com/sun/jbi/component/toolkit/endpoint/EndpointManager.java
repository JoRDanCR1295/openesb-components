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

package com.sun.jbi.component.toolkit.endpoint;

import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.toolkit.lifecycle.ContextAware;

/**
 * Defines a manager of JBI endpoints.
 * 
 * @author Kevan Simpson
 */
public interface EndpointManager extends ContextAware {
    /**
     * Creates an endpoint for the specified service unit.
     * 
     * @param info The endpoint configuration info.
     * @param srvcUnit The deploying service unit.
     * @return an endpoint.
     * @throws DeploymentException if an error occurs creating endpoint.
     */
    public Endpoint createEndpoint(EndpointInfo info, ServiceUnit srvcUnit) throws DeploymentException;

    /**
     * Fetches the {@link EndpointLifeCycle} utility to support conventions
     * of endpoint availability.
     * 
     * @see EndpointLifeCycle
     * @return an <code>EndpointLifeCycle</code> utility.
     */
    public EndpointLifeCycle getLifeCycle();
    
    /**
     * Fetches an {@link Endpoint} by service and endpoint names.
     * 
     * @param info The endpoint definition.
     * @return an <code>Endpoint</code> or <code>null</code>.
     */
    public Endpoint lookupEndpoint(EndpointInfo info);
    
    /**
     * Stores an {@link Endpoint} by service and endpoint names.
     * 
     * @param ept The endpoint to store.
     */
    public void registerEndpoint(Endpoint ept);

    /**
     * Removes an {@link Endpoint} from this utility.
     * 
     * @param info The endpoint definition.
     * @return The removed endpoint or <code>null</code>.
     */
    public Endpoint removeEndpoint(EndpointInfo info);
    
    /**
     * Initializes the specified service unit.
     * @param srvcUnit The service unit to initialize.
     * @throws DeploymentException if an error occurs initializing service unit.
     */
    public void addServiceUnit(ServiceUnit srvcUnit) throws DeploymentException;

    /**
     * Fetches the {@link ServiceUnit} responsible for deploying the {@link Endpoint}
     * which corresponds to the specified {@link ServiceEndpoint}.
     * 
     * @param endpt A <code>ServiceEndpoint</code>, likely acquired from a message exchange.
     * @return a <code>ServiceUnit</code> or <code>null</code>.
     */
    public ServiceUnit lookupServiceUnit(ServiceEndpoint endpt);

    /**
     * Removes the specified {@link ServiceUnit} from this registry.
     * @param unit The service unit to remove.
     */
    public void removeServiceUnit(ServiceUnit unit);
}
