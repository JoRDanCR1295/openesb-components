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
 * @(#)EndpointLifeCycle.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.endpoint;

import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.ServiceUnit;

/**
 * Defines contract to support a convention for endpoint availability,
 * an example of which is described here: <a 
 * href="http://wiki.open-esb.java.net/Wiki.jsp?page=LifecycleHandlingOfComponentsAndServiceUnits">
 * Endpoint LifeCycle</a>
 * 
 * @author Kevan Simpson
 */
public interface EndpointLifeCycle {
    /**
     * Service engines (SEs) handle invokes, &quot;continue&quot; business logic that 
     * was paused before an invoke, whereas binding components (BCs) start inbound 
     * external connections (or at least servicing them) and process requests.
     * 
     * @param srvcUnit The service unit for which to start consuming.
     * @throws DeploymentException if an error occurs starting consuming.
     */
    public void startConsuming(ServiceUnit srvcUnit) throws DeploymentException;
    
    /**
     * Register endpoints with NMR, if receive/accept loop from NMR is enabled 
     * (component is started) process messages from the NMR and binding components
     * (BCs) service outbound external connections, whereas service engines (SEs)
     * execute business logic, but &quot;pause&quot; when it reaches an invoke.
     * 
     * @param srvcUnit The service unit for which to start provisioning.
     * @throws DeploymentException if an error occurs starting provisioning.
     */
    public void startProvisioning(ServiceUnit srvcUnit) throws DeploymentException;
    
    /**
     * Service engines &quot;pause&quot; executing business logic when it reaches 
     * an invoke, whereas binding components (BCs) stop inbound external connections 
     * (or don’t service them).
     * 
     * @param srvcUnit The service unit for which to stop consuming.
     * @throws DeploymentException if an error occurs stopping consuming.
     */
    public void stopConsuming(ServiceUnit srvcUnit) throws DeploymentException;
    
    /**
     * Unregister endpoints with NMR, service engines (SEs) stop processing business logic, 
     * whereas binding components (BCs) stop outbound external connections (or don't service them).
     * 
     * @param srvcUnit The service unit for which to stop provisioning.
     * @throws DeploymentException if an error occurs stopping provisioning.
     */
    public void stopProvisioning(ServiceUnit srvcUnit) throws DeploymentException;
}
