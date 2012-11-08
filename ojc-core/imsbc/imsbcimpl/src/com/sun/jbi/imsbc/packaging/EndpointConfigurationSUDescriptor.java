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

import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.management.descriptor.Consumes;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.descriptor.ConfigurationException;

import com.sun.jbi.imsbc.Endpoint.EndpointType;


/**
 * EndpointConfigurationSUDescriptor is a class to hold the consumes and
 * provides information from the jbi.xml SU descriptor.
 * 
 * @author Sun Microsystems
 */
public class EndpointConfigurationSUDescriptor implements EndpointConfiguration {

    // SUDescriptor helper
    private SUDescriptorSupport suDescriptor = null;

    // use arraylist for now
    private List services;

    public EndpointConfigurationSUDescriptor(String rootDir) throws ConfigurationException {

        services = new ArrayList();

        suDescriptor = new SUDescriptorSupport(rootDir);

        addEndpoints(suDescriptor.getProvides());

        addEndpoints(suDescriptor.getConsumes());

    }

    private void addEndpoints(EndpointIdentifier[] endpoints) {
        if (endpoints != null) {
            for (int i = 0; i < endpoints.length; i++) {
                EndpointIdentifier endpoint = endpoints[i];
                int direction = endpoint.isProvider() ? EndpointType.OUTBOUND : EndpointType.INBOUND;
				
				String appconfName = endpoint.getApplicationConfigurationName();
                
				addEndpoint(endpoint.getInterfaceName().toString(), endpoint.getServiceName().toString(),
                        endpoint.getEndpointName(), appconfName, direction);
            }
        }
    }

    public void addEndpoint(EndpointData p) {
        services.add(p);
    }

    public void addEndpoint(String interfaceName, String service, String endPoint, String appconfName, int direction) {
        services.add(new EndpointDataImpl(interfaceName, service, endPoint, appconfName, direction));
    }

    public List endpoints() {
        return services;
    }

    /**
     * Determines whether JBI routing is enabled (switched on)
     **/
    public static boolean isJBIRoutingEnabled() {
        return SUDescriptorSupport.TEMP_SWITCH_ENABLE_JBI_ROUTING;
    }
}