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
 * @(#)EndpointConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc.packaging;

import java.util.List;

/**
 * EndpointConfiguration is an interface representing all BC endpoint
 * configurations.
 * 
 */
public interface EndpointConfiguration {
    
    /**
     *  Adds an endpoint configuration to the collection of endpoint configurations
     *  @param ep The Endpoint instance to add.
     */
    public void addEndpoint(EndpointData ep);
    
    /**
     *  Adds an endoint configurationto the collection of endpoint configurations
     *  @param interfaceName The interface name
     *  @param service The service name
     *  @param endPoint The endpoint name
     *  @param direction The endpoint direction
     */
    public void addEndpoint(String interfaceName,
                            String service,
                            String endPoint,
                            int direction);

    /**
     *  Returns a collection of all endpoint configurations
     *  @return The List of all endpoint configurations
     */
    public List endpoints();
}
