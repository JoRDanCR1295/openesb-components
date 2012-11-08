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
 * @(#)DefaultEndpointManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.endpoint.impl;

import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.EndpointLifeCycle;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;

/**
 * Abstract base implementation of an {@link EndpointManager}.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractEndpointManager implements EndpointManager {
    private EntryRegistry<QName, EntryRegistry<String, Endpoint>> mConsMap =
            new EntryRegistry<QName, EntryRegistry<String,Endpoint>>();
    private EntryRegistry<QName, EntryRegistry<String, Endpoint>> mProvMap =
            new EntryRegistry<QName, EntryRegistry<String,Endpoint>>();
    private EntryRegistry<QName, EntryRegistry<String, ServiceUnit>> mSrvcUnits =
            new EntryRegistry<QName, EntryRegistry<String,ServiceUnit>>();
    private ManagerContext mMgrCtx;
    private EndpointLifeCycle mLifeCycle;
    private Logger mLogger;
    
    protected AbstractEndpointManager(ComponentContext ctx) {
        this(new DefaultEndpointLifeCycle(ctx));
    }
    
    protected AbstractEndpointManager(DefaultEndpointLifeCycle elc) {
        mLifeCycle = elc;
        elc.setEndpointManager(this);
        mLogger = Util.getLogger(elc.getContext(), this.getClass().getName());
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#lookupEndpoint(com.sun.jbi.common.descriptor.EndpointInfo) */
    public Endpoint lookupEndpoint(EndpointInfo info) {
        if (info == null) {
            return null;
        }
        else {
            EntryRegistry<String, Endpoint> imap = (info.isProvides())
                    ? mProvMap.lookup(info.getServiceName()) 
                    : mConsMap.lookup(info.getServiceName());
            return (imap == null) ? null : imap.lookup(info.getEndpointName());
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#registerEndpoint(com.sun.jbi.component.toolkit.endpoint.Endpoint) */
    public void registerEndpoint(Endpoint ept) {
        EndpointInfo info = ept.getInfo();
        if (info.isProvides()) {
            EntryRegistry<String, Endpoint> imap = mProvMap.lookup(info.getServiceName());
            if (imap == null) {
                synchronized (mProvMap) {
                    imap = new EntryRegistry<String, Endpoint>();
                    mProvMap.register(info.getServiceName(), imap);
                }
            }
            imap.register(info.getEndpointName(), ept);
        }
        else {
            EntryRegistry<String, Endpoint> imap = mConsMap.lookup(info.getServiceName());
            if (imap == null) {
                synchronized (mConsMap) {
                    imap = new EntryRegistry<String, Endpoint>();
                    mConsMap.register(info.getServiceName(), imap);
                }
            }
            imap.register(info.getEndpointName(), ept);
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#removeEndpoint(com.sun.jbi.common.descriptor.EndpointInfo) */
    public Endpoint removeEndpoint(EndpointInfo info) {
        if (info == null) {
            return null;
        }
        else {
            EntryRegistry<String, Endpoint> imap = info.isProvides()
                    ? mProvMap.lookup(info.getServiceName()) 
                    : mConsMap.lookup(info.getServiceName());
            return (imap == null) ? null : imap.remove(info.getEndpointName());
        }
    }
    
    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#getLifeCycle() */
    public EndpointLifeCycle getLifeCycle() {
        return mLifeCycle;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#getContext() */
    public ManagerContext getContext() {
        return mMgrCtx;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#setContext(com.sun.jbi.component.toolkit.lifecycle.ManagerContext) */
    public void setContext(ManagerContext ctx) {
        mMgrCtx = ctx;
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#addServiceUnit(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void addServiceUnit(ServiceUnit srvcUnit) throws DeploymentException {
        EndpointInfo[] endpts = srvcUnit.getServices().getProvides();
        
        for (EndpointInfo info : endpts) {
            EntryRegistry<String, ServiceUnit> unitReg = 
                    mSrvcUnits.lookup(info.getServiceName());
            if (unitReg == null) {
                synchronized (mSrvcUnits) {
                    unitReg = new EntryRegistry<String, ServiceUnit>();
                    mSrvcUnits.register(info.getServiceName(), unitReg);
                }
            }
            unitReg.register(info.getEndpointName(), srvcUnit);
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#lookupServiceUnit(javax.jbi.servicedesc.ServiceEndpoint) */
    public ServiceUnit lookupServiceUnit(ServiceEndpoint endpt) {
        if (endpt == null) return null;
        
        EntryRegistry<String, ServiceUnit> unitReg = 
                mSrvcUnits.lookup(endpt.getServiceName());
        return (unitReg != null) ? unitReg.lookup(endpt.getEndpointName()) : null;
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#removeServiceUnit(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void removeServiceUnit(ServiceUnit unit) {
        if (unit != null) {
            EndpointInfo[] endpts = unit.getServices().getProvides();
            
            for (EndpointInfo info : endpts) {
                EntryRegistry<String, ServiceUnit> unitReg = 
                        mSrvcUnits.lookup(info.getServiceName());
                if (unitReg != null) {
                    unitReg.remove(info.getEndpointName());
                }
            }
        }
    }

    /** @see java.lang.Object#toString() */
    public String toString() {
        try {
            StringBuffer buff = new StringBuffer();
            buff.append(this.getClass().getSimpleName()).append("{provisioning={");
            for (QName key : mProvMap.keySet()) {
                EntryRegistry<String, Endpoint> imap = mProvMap.lookup(key);
                if (imap != null && !imap.isEmpty()) {
                    buff.append(key).append("=").append(imap);
                }
            }
            buff.append("},consuming={");
            for (QName key : mConsMap.keySet()) {
                EntryRegistry<String, Endpoint> imap = mConsMap.lookup(key);
                if (imap != null && !imap.isEmpty()) {
                    buff.append(key).append("=").append(imap);
                }
            }
            buff.append("}}");
            return buff.toString();
        }
        catch (Exception e) {
            // avoid concurrent modification issues, however unlikely
            return super.toString();
        }
    }

    /**
     * @return the consMap
     */
    protected EntryRegistry<QName, EntryRegistry<String, Endpoint>> getConsumerRegistry() {
        return mConsMap;
    }

    /**
     * @return the provMap
     */
    protected EntryRegistry<QName, EntryRegistry<String, Endpoint>> getProviderRegistry() {
        return mProvMap;
    }

    protected Logger log() {
        return mLogger;
    }
}
