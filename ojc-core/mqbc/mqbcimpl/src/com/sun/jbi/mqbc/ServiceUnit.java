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
 * @(#)ServiceUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.mqbc;

import java.util.Collection;
import javax.jbi.JBIException;


/**
 * ServiceUnit represents a Service Unit managed by a ServiceUnitManager.  
 * This interface provides lifecycle management of a ServiceUnit and Endpoint
 * management of Endpoints associated with this ServiceUnit.
 * ServiceUnit has a set of Endpoints which can be in various states
 * (initialized, started, stopped, etc).  This interface provides read-only
 * access to the set of Endpoints contained by this ServiceUnit.  Clients to this
 * interface should not have the ability to remove or add Endpoints.  The only
 * interface that allows this ability is through the ServiceUnitInitializer.
 *
 *
 */
public interface ServiceUnit {

    /**
     * Retrieves the Id of this ServiceUnit.
     *
     * @return       the name of the Service as a QName
     */
    public String getServiceUnitId();
    
     /**
     * Deploys the ServiceUnit
     *
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    void deploy() throws JBIException;

    /**
     * Initializes the ServiceUnit
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    void init() throws JBIException;

    /**
     * Starts this ServiceUnit.  This involves activating
     * all Endpoints that are part of this ServiceUnit.
     * <p>
     * TODO: What should happen if not all the Endpoints
     * can be activated?  Should I deactivate them or just leave
     * them?  For now, I'm going to assume that this method is
     * transactional.  Either all the Endpoints activate or none.
     * If any one fails to activate, the other activated Endpoints
     * will be deactivated.
     *
     * @exception    JBIException if a any Endpoint fails
     * to activate
     */
    void start() throws JBIException;

    /**
     * Stops this ServiceUnit.  This involves deactivating
     * all Endpoints that are part of this ServiceUnit;
     * <p>
     * TODO: What should happen if not all Endpoints deactivate?
     * Unlike the activate() method, I'm NOT going to assume
     * this is transactional.  It seems silly to deactivate a number of
     * Endpoint, and if one fails, re-activate them.  I'll just throw
     * an error, and have the user decide how to deal with it.
     *
     * @exception    JBIException if any Endpoint fails
     * to deactivate
     */
    void stop() throws JBIException;

    /**
     * Shuts down this ServiceUnit
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    void shutdown() throws JBIException;

    /**
     * Retrieves the Collection of Endpoints handled by this ServiceUnit.  This is
     * an unmodifiable Collection
     *
     * @return       the set of Endpoints
     */
    Collection<Endpoint> getEndpoints();

    /**
     * Determines if the specified endpoint is in a suspended state. A suspended
     * endpoint is a started endpoint, not stopped, not shut down, but not
     * accepting calls for its service.
     *
     * @param endpoint An endpoint managed by the service unit
     *
     * @return true if the endpoint is suspended, false otherwise.
     * @throws JBIException if the specified endpoint is null, or is not one
     * managed by the service, or its state cannot be determined for whatever
     * reason.
     */
    boolean isSuspended(Endpoint endpoint) throws JBIException;

    /**
     * Suspends the specified endpoint. The endpoint must be one that the
     * service unit is managing.  An endpoint that is suspended is started, not
     * stopped nor shut down, but not accepting calls for its service.
     *
     * @param endpoint An endpoint managed by the service unit
     *
     * @throws JBIException if the specified endpoint is null, is not one
     * managed by the service unit, or its suspension cannot be effected for
     * whatever reason.
     */
    void suspend(Endpoint endpoint) throws JBIException;

    /**
     * Resumes (unsuspends) the specified endpoint. The endpoint must be one
     * that the service unit is managing. An endpoint that is resumed is started
     * and accepting calls for its service.
     *
     * @param endpoint An endpoint managed by the service unit
     *
     * @throws JBIException if the specified endpoint is null, is not one
     * managed by the service unit, or its resumption cannot be effected for
     * whatever reason.
     */
    void resume(Endpoint endpoint) throws JBIException;
}
