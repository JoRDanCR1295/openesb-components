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
 * @(#)EndpointConfigurationSUDescriptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.packaging;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.descriptor.ConfigurationException;
import javax.xml.namespace.QName;

/**
 * EndpointConfigurationSUDescriptor is a class to hold the consumes and
 * provides information from the jbi.xml su descriptor.
 * 
 */
public class EndpointConfigurationSUDescriptor implements EndpointConfiguration {
    public EndpointConfigurationSUDescriptor (String rootDir) throws ConfigurationException {
        services = new ArrayList();
        suDescriptor = new SUDescriptorSupport(rootDir);
        
        // No end-point discrimination necessary because I am guaranteed
        // by the JBI implementation that the data in the referenced service
        // unit deployment artifact pertains to this Binding Component.
        addEndpoints (suDescriptor.getProvides());
        addEndpoints (suDescriptor.getConsumes());
    }

    private void addEndpoints (EndpointIdentifier [] endpoints) {
        for (int i=0; i < endpoints.length; i++) {
            EndpointIdentifier endpoint = endpoints[i];
            EndpointType direction = endpoint.isProvider()? 
                            EndpointType.OUTBOUND : EndpointType.INBOUND;
            
            addEndpoint (endpoint.getInterfaceName(),
                         endpoint.getServiceName(),
                         endpoint.getEndpointName(),
                         direction);
        }
    }
        
    public void addEndpoint(EndpointData p) {
        synchronized (services) {
            services.add(p);
        }
    }
    
    public void addEndpoint(QName interfaceName,
                            QName service,
                            String endPoint,
                            EndpointType direction) {
        synchronized (services) {
            services.add(new EndpointDataImpl(
                    interfaceName,
                    service,
                    endPoint,
                    direction));
        }
    }

    public List endpoints() {
        return Collections.unmodifiableList(services);
    }
    
    private final SUDescriptorSupport suDescriptor;
    private final ArrayList services;
}
