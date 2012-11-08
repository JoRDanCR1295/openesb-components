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

package com.sun.jbi.imsbc.packaging;


/**
 * Interface representing "raw" data endpoint information independent of the
 * format of the endpoint configuration file
 *
 * @author Sun Microsystems
 * 
 */
public interface EndpointData {
    
    /**
     * Gets the interface name
     * @return The interface name.
     */
    public String getInterface();
    
    /**
     * Gets the service name
     * @return The service name.
     */    
    public String getService();

    /**
     * Gets the service name
     * @return The endpoint name.
     */    
    public String getEndpoint();

    /**
     * Gets the direction
     * @return The endpoint direction.
     */    
    public int getDirection();

	 /**
     * The name of the application configuration object associated with this
     * endpoint, if any.
     * 
     * @return The name of the associated application configuration object, if
     *         any, or else a blank string.
     */
    public String getApplicationConfigurationObjectName();

}
