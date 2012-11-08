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
 * @(#)StatusReporting.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.eManager.provider;

/**
 * Methods for collecting status that should not appear on the MBean
 * interface
 * @author Sun Microsystems
 */
public interface StatusReporting {
    /**
     * Add consuming endpoints to the status reporting
     */
    void addConsumingEndpoints(String[] consumingEndpoints);
    /**
     * Add consuming endpoint to the status reporting
     */
    void addConsumingEndpoint(String consumingEndpoint);
    /**
     * Remove consuming endpoints from the status reporting
     */
    void removeConsumingEndpoints(String[] consumingEndpoints);
    /**
     * Add provisioning endpoints to the status reporting
     */
    void addProvisioningEndpoints(String[] provisioningEndpoints);
    /**
     * Add provisioning endpoint to the status reporting
     */
    void addProvisioningEndpoint(String provisioningEndpoint);
    /**
     * Remove consuming endpoints from the status reporting
     */
    void removeProvisioningEndpoints(String[] provisioningEndpoints);

    /**
     * Get the status object for a given object to query or update the status.
     * The endpoint needs to be registered via addConsumingEndpoint or addProvisioningEndpoint
     * for an EndpointStatus to be available.
     * @return EndpointStatus for endpoints registered with StatusReporting, null for removed/not registered endpoints.
     */
     EndpointStatus getEndpointStatus(String endpoint);
}
