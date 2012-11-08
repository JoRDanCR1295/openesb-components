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
 * @(#)$Id: WLMSEEndPointLifeCycle.java,v 1.1 2010/02/15 19:25:08 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.impl.DefaultEndpointLifeCycle;
import com.sun.jbi.component.toolkit.util.I18n;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;
import com.sun.jbi.workflow.model.Task;

public class WLMSEEndPointLifeCycle extends DefaultEndpointLifeCycle {
   
    
    public WLMSEEndPointLifeCycle(ComponentContext ctx) {
        super(ctx);
        // TODO Auto-generated constructor stub
    }
    
    
    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointLifeCycle#startConsuming(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void startConsuming(ServiceUnit srvcUnit) throws DeploymentException {
        try {
            EndpointInfo[] endpts = srvcUnit.getServices().getConsumes();
            for (EndpointInfo info : endpts) {
                Endpoint ept = getEndpointManager().lookupEndpoint(info);
                WLMSEEndPoint wlmemp = (WLMSEEndPoint) ept;
                if (wlmemp != null) {
                    if (!wlmemp.getServiceUnitNames().contains(srvcUnit.getName())) {
                        wlmemp.getServiceUnitNames().add(srvcUnit.getName());
                    }                 
                ept.start();
                }
            }
        }
        catch (JBIException ex) {
            String msg = I18n.loc("COMPTK-6004: Service unit \"{0}\" failed to start consumer endpoints: {1}", 
                                  srvcUnit.getName(), ex.getMessage());
            log().log(Level.WARNING, msg, ex);
            throw new DeploymentException(msg, ex);
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointLifeCycle#startProvisioning(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void startProvisioning(ServiceUnit srvcUnit) throws DeploymentException {
        Map<EndpointInfo, ServiceEndpoint> activated = 
            new HashMap<EndpointInfo, ServiceEndpoint>();
        Map<EndpointInfo, Endpoint> created = 
            new HashMap<EndpointInfo, Endpoint>();
        EndpointInfo[] endpts = srvcUnit.getServices().getEndpoints();
        
        try {
            for (EndpointInfo info : endpts) {
                // has other SU already created this endpoint
//                Endpoint ept = getEndpointManager().lookupEndpoint(info);
//                if (ept != null) {
//                    throw new DeploymentException(I18n.loc(
//                            "COMPTK-6005: Duplicate endpoint ({0}) in component: {1}", 
//                            String.valueOf(info), getContext().getComponentName()));
//                }
                
                // create endpoint
                Endpoint ept = getEndpointManager().createEndpoint(info, srvcUnit);
                
                if (ept == null) {
                    log().info(I18n.loc(
                            "COMPTK-5005: {0} created NULL endpoint (info={1}) for service unit: {2}", 
                            getEndpointManager().getClass().getSimpleName(),
                            String.valueOf(info), srvcUnit.getName()));
                    continue;
                }
                else if (log().isLoggable(Level.FINER)) {
                    log().finer("COMPTK-2001: Endpoint created: "+ String.valueOf(ept) 
                                +" for SU \""+ srvcUnit.getName() +"\"");
                }
                
                // start provisioning
                if (info.isProvides()) {
                    // activate with component context
                    ServiceEndpoint srvcEPT = getContext()
                            .activateEndpoint(info.getServiceName(), 
                                              info.getEndpointName());
                    activated.put(info, srvcEPT);
                    ept.start();
                    if (log().isLoggable(Level.FINER)) {
                        log().finer("COMPTK-2002: Endpoint activated: "+ 
                                    String.valueOf(srvcEPT));
                    }
                }
                
                // check for duplicate endpoint in this SU
                if (created.get(info) != null) {
                    throw new DeploymentException(I18n.loc(
                            "COMPTK-6006: Duplicate endpoint ({0}) in service unit: {1}", 
                            String.valueOf(info), srvcUnit.getName()));
                }
                created.put(info, ept);
            }
        }
        catch (Exception e) {
            String msg = I18n.loc("COMPTK-6007: Service unit \"{0}\" failed to start provider endpoints: {1}", 
                                  srvcUnit.getName(), e.getMessage());
            log().log(Level.WARNING, msg, e);
    
            // rollback started endpoints - all or nothing
            for (EndpointInfo info : activated.keySet()) {
                try {
                    if (log().isLoggable(Level.FINER)) {
                        log().finer("COMPTK-2003: Deactivating endpoint during rollback: " 
                                + String.valueOf(info));
                    }
                    ServiceEndpoint srvcEPT = activated.get(info);
                    getContext().deactivateEndpoint(srvcEPT);
                }
                catch (Exception e2) {
                    log().warning(I18n.loc(
                            "COMPTK-6008: Failed to deactivate endpoint: {0}",
                            e2.getMessage()));
                }
            }

            throw new DeploymentException(msg, e);
        }
        
        // register created Endpoints with EndpointManager
        for (Endpoint endpt : created.values()) {
            getEndpointManager().registerEndpoint(endpt);
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointLifeCycle#stopConsuming(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void stopConsuming(ServiceUnit srvcUnit) throws DeploymentException {
        try {
            EndpointInfo[] endpts = srvcUnit.getServices().getConsumes();
            for (EndpointInfo info : endpts) {
                Endpoint ept = getEndpointManager().lookupEndpoint(info);
                WLMSEEndPoint wlmemp = (WLMSEEndPoint) ept;
                if (wlmemp != null) {
                    wlmemp.getServiceUnitNames().remove(srvcUnit.getName());
                    if (wlmemp.getServiceUnitNames().size() == 0) {                
                        ept.stop();
                    }
                }
            }
        }
        catch (JBIException ex) {
            String msg = I18n.loc("COMPTK-6011: Service unit \"{0}\" failed to stop consumer endpoints: {1}", 
                    srvcUnit.getName(), ex.getMessage());
            log().log(Level.WARNING, msg, ex);
            throw new DeploymentException(msg, ex);
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointLifeCycle#stopProvisioning(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void stopProvisioning(ServiceUnit srvcUnit) throws DeploymentException {
        EndpointInfo[] endpts = srvcUnit.getServices().getEndpoints();
        DeploymentException error = null;
        
        for (EndpointInfo info : endpts) {
            try {
                Endpoint ept = ((WLMSEEndPointManager) getEndpointManager()).removeEndpoint(info, srvcUnit);
                if (ept != null) {
                    if (info.isProvides()) {
                        ept.stop();    
                        ServiceEndpoint srvcEPT = getContext()
                            .getEndpoint(info.getServiceName(), info.getEndpointName());
                        getContext().deactivateEndpoint(srvcEPT);
                    
                    if (log().isLoggable(Level.FINER)) {
                        log().finer("COMPTK-2004: Endpoint deactivated: "+ String.valueOf(srvcEPT));
                    }
                    }
                }
            }
            catch (Exception e) {
                String msg = I18n.loc("COMPTK-6009: Endpoint \"{0}\" deactivation failed: {1}", 
                                    (info == null) ? "NULL" : info.getServiceName(), 
                                    e.getMessage());
                error = new DeploymentException(msg, e);
                log().log(Level.WARNING, msg, e);
            }
        }
        
        if (error != null) {
            Throwable t = error.getCause();
            log().log(Level.WARNING,
                            I18n.loc("COMPTK-6010: Service unit \"{0}\" failed to stop provisioning endpoints: {1}", 
                                     srvcUnit.getName(), (t == null) ? "???" : t.getMessage()), 
                            error);
            throw error;
        }
    }

 
}
