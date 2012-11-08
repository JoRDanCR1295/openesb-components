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
 * @(#)EndpointData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.packaging;

import com.sun.jbi.common.descriptor.EndpointInfo;

/**
 * Interface representing "raw" data endpoint information independent of the
 * format of the endpoint configuration file
 *
 */
public interface EndpointData {
    
    /**
     * Gets the interface name
     * @return The interface name.
     */
    String getInterface();
    
    /**
     * Gets the service name
     * @return The service name.
     */
    String getService();
    
    /**
     * Gets the service name
     * @return The endpoint name.
     */
    String getEndpoint();
    
    /**
     * Gets the direction
     * @return The endpoint direction.
     */
    int getDirection();

    /**
     * The name of the application configuration object associated with this
     * endpoint, if any.
     * 
     * @return The name of the associated application configuration object, if
     *         any, or else a blank string.
     */
    String getApplicationConfigurationObjectName();

    /**
     * Returns the EndpointInfo representation.
     */
    EndpointInfo toEndpointInfo();
}
