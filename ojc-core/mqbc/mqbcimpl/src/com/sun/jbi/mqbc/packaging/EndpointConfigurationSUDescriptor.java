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

package com.sun.jbi.mqbc.packaging;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.descriptor.QosServices;
import com.sun.jbi.common.qos.descriptor.QosServicesDescriptor;

/**
 * EndpointConfigurationSUDescriptor is a class to hold the consumes and
 * provides information from the jbi.xml su descriptor.
 *
 */
public class EndpointConfigurationSUDescriptor implements EndpointConfiguration {
    private final ArrayList<EndpointData> services
            = new ArrayList<EndpointData>();

    public EndpointConfigurationSUDescriptor(String rootDir)
            throws DeploymentException {
        QosServices qos = QosServicesDescriptor.parse(rootDir); 
        addEndpoints(qos);
    }
    
    private void addEndpoints(QosServices qos) {
        synchronized (services) {
            for (EndpointInfo endpoint : qos.getConsumes()) {
                services.add(new EndpointDataImpl(endpoint, qos));
            }
            for (EndpointInfo endpoint : qos.getProvides()) {
                services.add(new EndpointDataImpl(endpoint, qos));
            }
        }
    }
    
    public List<EndpointData> endpoints() {
        synchronized (services) {
            return Collections.unmodifiableList(services);
        }
    }
}
