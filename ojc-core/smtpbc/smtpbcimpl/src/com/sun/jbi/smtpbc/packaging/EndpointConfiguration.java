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

package com.sun.jbi.smtpbc.packaging;

import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.management.descriptor.ConfigurationException;
import com.sun.jbi.management.descriptor.Consumes;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;

import com.sun.jbi.smtpbc.Endpoint.EndpointType;

/**
 * EndpointConfiguration is a class to parse the endpoints.xml file.  This file
 * is part of the packaging of a Service Assembly
 * 
 * @author Alexander Fung
 */
public final class EndpointConfiguration {

    // use arraylist for now
    private ArrayList services;
    
    public static EndpointConfiguration newConfiguration(final String rootDir)
        throws ConfigurationException {

        final SUDescriptorSupport support = new SUDescriptorSupport(rootDir);
        final EndpointConfiguration conf = new EndpointConfiguration();
        final Provides[] providesList = support.getProvides();
        for (final Provides provides : providesList) {
            conf.addPortMap(provides.getServiceName().toString(),
                            provides.getEndpointName(),
                            EndpointType.OUTBOUND);
        }
        final Consumes[] consumesList = support.getConsumes();
        for (final Consumes consumes : consumesList) {
            conf.addPortMap(consumes.getServiceName().toString(),
                            consumes.getEndpointName(),
                            EndpointType.INBOUND);
        }
        return conf;
    }

    /**
     * Creates a new instance of EndpointConfiguration 
     */
    private EndpointConfiguration() {
        services = new ArrayList();
    }
    
    public void addPortMap(final PortMap p) {
        services.add(p);
    }
    
    public void addPortMap(final String service,
                           final String endPoint,
                           final EndpointType direction) {
        services.add(new PortMap(service,
                                 endPoint,
                                 direction));
    }

    public List portMaps() {
        return services;
    }

    public class PortMap {
        private String mService;
        private String mEndPoint;
        private EndpointType mDirection;
        
        protected PortMap(final String service,
                          final String endPoint,
                          final EndpointType direction) {
            mService = service;
            mEndPoint = endPoint;
            mDirection = direction;
        }
        
        public String getService() {
            return mService;
        }
        
        public String getEndpoint() {
            return mEndPoint;
        }
        
        public EndpointType getDirection() {
            return mDirection;
        }
    }    
           
}
