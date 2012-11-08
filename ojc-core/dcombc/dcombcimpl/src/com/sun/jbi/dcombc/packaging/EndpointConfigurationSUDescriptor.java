/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.packaging;

import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.management.descriptor.Consumes;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.descriptor.ConfigurationException;

import com.sun.jbi.dcombc.Endpoint.EndpointType;

/**
 * EndpointConfigurationSUDescriptor is a class to hold the consumes and
 * provides information from the jbi.xml SU descriptor.
 * 
 * @author Chandrakanth Belde
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

                addEndpoint(endpoint.getInterfaceName().toString(), endpoint.getServiceName().toString(),
                        endpoint.getEndpointName(), direction);
            }
        }
    }

    public void addEndpoint(EndpointData p) {
        services.add(p);
    }

    public void addEndpoint(String interfaceName, String service, String endPoint, int direction) {
        services.add(new EndpointDataImpl(interfaceName, service, endPoint, direction));
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
