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
 * @(#)TransformEndpointManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Operation;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import com.sun.transform.I18n;
import com.sun.transform.descriptor.ProcessDescriptor;
import com.sun.transform.descriptor.TransformEndpoint;
import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.ProcessDef;
import com.sun.transform.engine.model.ProcessFactory;
import com.sun.transform.engine.model.Transform;

/**
 * Manages XSLT SE service endpoints and acts as its own {@link Endpoint} factory.
 * 
 * @author Kevan Simpson
 */
public class TransformEndpointManager extends AbstractEndpointManager {
    private EntryRegistry<String, ProcessDescriptor> mDescriptors = 
            new EntryRegistry<String, ProcessDescriptor>();
    private Map<String, String> mUnitNamespaces = new HashMap<String, String>();
    private ProcessFactory mProcessFactory = null;
    private String mCompName;
    
    /** Constructs an XSLT SE endpoint manager. */
    public TransformEndpointManager(ComponentContext ctx, EngineConfig engine) {
        super(ctx);	// this EndpointManager acts as its own EndpointFactory
        mProcessFactory = engine.getProcessFactory();
        mCompName = ctx.getComponentName(); // cache for logging
    }

	/** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#createEndpoint(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.descriptor.ServiceUnit) */
    public Endpoint<ProcessDef> createEndpoint(EndpointInfo info, ServiceUnit srvcUnit)
            throws DeploymentException {
        getContext().getCustomClassLoaderUtil()
                .switchClassLoader(srvcUnit.getName(), SwitchType.service_classloader);
    	try {
    		// lookup descriptor by targetNamespace
    		ProcessDescriptor descriptor = mDescriptors.lookup(info.getServiceName().getNamespaceURI());
    		if (descriptor == null) {
    			throw error(null,
    					    "TRANSL-6052: {0} endpoint creation failed, no descriptor found for service unit: {1}", 
    					    mCompName, srvcUnit.getName()); 
    		}

    		TransformEndpoint endpt = descriptor.lookupEndpointDef(info);
    		if (endpt == null) {
    			throw error(null,
    			            "TRANSL-6053: {0} {1} is missing from descriptor in service unit: {2}", 
    			            mCompName, String.valueOf(info), srvcUnit.getName()); 
    		}
    		/* 
    		 * Create stylesheets for transforms, which must be done at deployment
    		 * to know the service unit root path.
    		 * UPDATE: If creating these Templates instances creates memory overhead,
    		 * then lazy-load strategy will be possible w/ Endpoint enhancements.
    		 */
    		if (info.isProvides()) {
    			Collection<Operation> ops = endpt.getOperations();
    			for (Operation op : ops) {
    				ProcessDef def = endpt.getServiceDef(op.getName());
    				for (int i = 0, n = def.countActivities(); i < n; i++) {
    					Activity act = def.getActivity(i);
    					if (act instanceof Transform) {
    					    Transform tr = (Transform) act;
    					    if (!Util.isEmpty(tr.getFile())) {
    					        tr.compile(srvcUnit.getRootPath());
    					    }
    					}
    					else if (act instanceof Invocation) {
    					    for (Iterator<Transform> iter = ((Invocation) act).getFaultHandlers(); iter.hasNext();) {
    					        Transform tr = iter.next();
                                if (!Util.isEmpty(tr.getFile())) {
                                    tr.compile(srvcUnit.getRootPath());
                                }
    					    }
    					}
    				}
    			}
    		}

    		return endpt;
    	} 
    	finally {
    		// switch back to original classloader...
    		getContext().getCustomClassLoaderUtil()
    		        .switchClassLoader(srvcUnit.getName(), SwitchType.context_classloader);
    	}
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager#addServiceUnit(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void addServiceUnit(ServiceUnit srvcUnit) throws DeploymentException {
        if (srvcUnit == null) return;
        
        // register SUs by ServiceEndpoint
        super.addServiceUnit(srvcUnit);
        
        try {
            // parse transformmap.xml and cache for when endpoints are activated
            ProcessDescriptor descriptor = ProcessDescriptor.parse(
                    srvcUnit.getRootPath(), mProcessFactory);
            // validate that defined processes do not have duplicate namespaces
            String tns = descriptor.getTargetNamespace();
            if (mDescriptors.containsKey(tns)) {
                throw error(null,
                            "TRANSL-6054: Failed to initialize {0} service unit {1} - targetNamespace conflict: {2}",
                            mCompName, srvcUnit.getName(), tns);
            }
            
            mDescriptors.register(tns, descriptor);
            mUnitNamespaces.put(srvcUnit.getRootPath(), tns);
        }
        catch (DeploymentException de) {
            super.removeServiceUnit(srvcUnit);
            // already logged...rethrow
            throw de;
        }
        catch (Exception e) {
            super.removeServiceUnit(srvcUnit);
            throw error(e,
                        "TRANSL-6055: {0} Service unit \"{1}\" init failed: {2}", 
            			mCompName, srvcUnit.getName(), e.getMessage());
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager#removeServiceUnit(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void removeServiceUnit(ServiceUnit unit) {
        if (unit == null) return;
        
        super.removeServiceUnit(unit);
        
        String tns = mUnitNamespaces.remove(unit.getRootPath());
        if (tns != null) {
            mDescriptors.remove(tns);
        }
    }

    private DeploymentException error(Exception thrown, String message, Object... params) {
        String err = I18n.loc(message, params);
        if (thrown == null) {
            log().warning(err);
            return new DeploymentException(err);
        }
        else {
            log().log(Level.WARNING, err, thrown);
            return new DeploymentException(err, thrown);
        }
    }
}
