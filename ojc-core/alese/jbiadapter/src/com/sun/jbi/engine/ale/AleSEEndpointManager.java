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
 * @(#)AleSEEndpointManager.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.ale;

import java.io.File;
import java.io.FileReader;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.xml.sax.InputSource;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.common.tale.core.domain.service.TaleService;
import com.sun.jbi.common.tale.core.domain.service.LoggingService;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * The {@link EndpointManager} for ALE-SE.
 * @author Kevan Simpson
 */
public class AleSEEndpointManager extends AbstractEndpointManager {
    private static final QName LOGGING_PORTTYPE = 
            new QName(TaleService.ALE_WSDL_NS, "ALEServiceLoggingPortType");
    private static final QName ALERT_PORTTYPE = 
            new QName(TaleService.ALE_WSDL_NS, "ALEServiceAlertPortType");
    private static final QName ERROR_PORTTYPE = 
            new QName(TaleService.ALE_WSDL_NS, "ALEServiceErrorPortType");
    
    /**
     * Constructs an <code>AleSEEndpointManager</code>.
     */
    public AleSEEndpointManager(ComponentContext ctx) {
        super(ctx);
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#createEndpoint(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.descriptor.ServiceUnit) */
    public Endpoint<TaleService> createEndpoint(EndpointInfo info, ServiceUnit srvcUnit) 
            throws DeploymentException {
        // TODO create ALE-SE endpoint
        if (false) {    // TODO search for lookup service configuration file... TBD
            // we have no config file, hence the literal 'false' in this IF block
        }
        else {
            try {
                TaleDomain domain = (TaleDomain) getContext()
                        .getCorrelationMap().get(TaleDomain.class.getName());
                Definition aleWsdl = readWsdl(
                        new File(srvcUnit.getRootPath(), "TaleService.wsdl"));
                AleSEEndpoint endpt = new AleSEEndpoint(info);
                TaleService srvc = null;
                PortType portType = null;
                if (info.getServiceName().equals(TaleService.LOGGING_SERVICE) && 
                    info.getEndpointName().equals(TaleService.LOGGING_ENDPOINT)) {
                    // logging
                    srvc = new LoggingService(domain);
                    portType = aleWsdl.getPortType(LOGGING_PORTTYPE);
                }
                else if (info.getServiceName().equals(TaleService.ALERT_SERVICE) && 
                        info.getEndpointName().equals(TaleService.ALERT_ENDPOINT)) {
                    // TODO alert
                    srvc = new LoggingService(domain);
                    portType = aleWsdl.getPortType(ALERT_PORTTYPE);
                }
                else if (info.getServiceName().equals(TaleService.ERROR_SERVICE) && 
                        info.getEndpointName().equals(TaleService.ERROR_ENDPOINT)) {
                    // TODO error
                    srvc = new LoggingService(domain);
                    portType = aleWsdl.getPortType(ERROR_PORTTYPE);
                }

                // add service for each operation
                for (Object obj : portType.getOperations()) {
                    endpt.setServiceDef(srvc, (Operation) obj);
                }
                
                return endpt;
            }
            catch (Exception e) {
                // TODO wrap + rethrow
            }
        }
        
        return null;
    }

    protected Definition readWsdl(File file) throws Exception {
        WSDLReader reader = WSDL4JExt.newWSDLReader(null);
        return reader.readWSDL(file.toURI().toString(), 
                               new InputSource(new FileReader(file)));
    }
}
