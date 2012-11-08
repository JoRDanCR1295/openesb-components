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
 * @(#)BindingServicesBuilder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.services;

import java.io.File;
import java.util.Map;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.model.Services;

/**
 * 
 * @author Kevan Simpson
 */
public class BindingServicesBuilder extends AbstractServicesBuilder {

    /** @see com.sun.jbi.component.toolkit.project.services.AbstractServicesBuilder#generateServices(java.lang.String, java.lang.String) */
    @Override
    public Services generateServices(String unitName, String artifactRoot) {
        File root = new File(artifactRoot);
        loadWsdlCache(root);
        for (Definition def : getWsdls().values()) {
            Map srvcMap = def.getServices();
            for (Object o : srvcMap.values()) {
                Service srvc = (Service) o;
                Map portMap = srvc.getPorts();
                for (Object p : portMap.values()) {
                    Port port = (Port) p;
                    Binding binding = port.getBinding();
                    PortType portType = binding.getPortType();
                    // add endpoint
                    addService(createInfo(srvc, port, portType));
                }
            }
        }

        return createServices();
    }

    protected EndpointInfo createInfo(Service srvc, Port port, PortType portType) {
        return new EndpointInfo(false,  // default - assume consuming
                                port.getName(), 
                                portType.getQName(),
                                srvc.getQName(),
                                null);
    }
}
