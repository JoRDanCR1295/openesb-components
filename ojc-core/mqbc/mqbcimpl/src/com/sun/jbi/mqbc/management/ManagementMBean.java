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
 * @(#)$Id: ManagementMBean.java,v 1.1 2008/09/23 23:20:45 noel_ang Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.management;

import javax.management.MBeanException;


/**
 * Interface for Management MBean-driven Endpoint administration.
 *
 * @author Noel.Ang@sun.com
 */
public interface ManagementMBean {

    /**
     * Suspend a endpoint.
     *
     * @param endpointName Endpoint identification. The identifier is not
     * necessarily a canonical name; it must be in the set of identifiers
     * returned by a preceding call to {@link #listActiveEndpoints()} or {@link
     * #listInactiveEndpoints()}.
     *
     * @throws MBeanException if the endpoint cannot be suspended for any
     * reason.
     */
    void suspend(String endpointName) throws MBeanException;

    /**
     * Resume a consuming endpoint.
     *
     * @param endpointName Endpoint identification. The identifier is not
     * necessarily a canonical name; it must be in the set of identifiers
     * returned by a preceding call to {@link #listActiveEndpoints()} or {@link
     * #listInactiveEndpoints()}.
     *
     * @throws MBeanException if the endpoint cannot be resumed for any reason.
     */
    void resume(String endpointName) throws MBeanException;

    /**
     * Indicates if the named endpoint is active (i.e., not suspended).
     *
     * @param endpointName Endpoint identification. The identifier is not
     * necessarily a canonical name; it must be in the set of identifiers
     * returned by a preceding call to {@link #listActiveEndpoints()} or {@link
     * #listInactiveEndpoints()}.
     *
     * @return true if the specified endpoint is active.
     * @throws MBeanException if the status of the specified endpoint cannot be
     * obtained for any reason.
     */
    boolean isEndpointActive(String endpointName) throws MBeanException;

    /**
     * Returns identifiers for all active managed endpoints.
     *
     * @return An array with zero or more endpoint identifiers.
     */
    String[] listActiveEndpoints();

    /**
     * Returns identifiers for all suspended managed endpoints.
     *
     * @return An array with zero or more endpoint identifiers.
     */
    String[] listInactiveEndpoints();
}