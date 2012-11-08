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
 * @(#)$Id: WLMSEEndPointManager.java,v 1.1 2010/02/15 19:25:09 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;

public class WLMSEEndPointManager extends AbstractEndpointManager {

    private EntryRegistry<QName, EntryRegistry<String, Endpoint>> mConsMap = new EntryRegistry<QName, EntryRegistry<String, Endpoint>>();

    private EntryRegistry<QName, EntryRegistry<String, Endpoint>> mProvMap = new EntryRegistry<QName, EntryRegistry<String, Endpoint>>();

    private ManagerContext mMgrCtx;

    private Logger mLogger;
    
    public WLMSEEndPointManager(ComponentContext ctx) {
        this(new WLMSEEndPointLifeCycle(ctx));
        // TODO Auto-generated constructor stub
    }
    
    public WLMSEEndPointManager(WLMSEEndPointLifeCycle elc) {
        super (elc);
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#lookupEndpoint(com.sun.jbi.common.descriptor.EndpointInfo) */
    public Endpoint lookupEndpoint(EndpointInfo info) {
        if (info == null) {
            return null;
        } else {
            EntryRegistry<String, Endpoint> imap = (info.isProvides()) ? mProvMap
                    .lookup(info.getServiceName())
                    : mConsMap.lookup(info.getServiceName());
            return (imap == null) ? null : imap.lookup(info.getEndpointName());
        }
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#registerEndpoint(com.sun.jbi.component.toolkit.endpoint.Endpoint) */
    public void registerEndpoint(Endpoint ept) {
        EndpointInfo info = ept.getInfo();
        if (info.isProvides()) {
            EntryRegistry<String, Endpoint> imap = mProvMap.lookup(info
                    .getServiceName());
            if (imap == null) {
                synchronized (mProvMap) {
                    imap = new EntryRegistry<String, Endpoint>();
                    mProvMap.register(info.getServiceName(), imap);
                }
            }
            imap.register(info.getEndpointName(), ept);
        } else {
            EntryRegistry<String, Endpoint> imap = mConsMap.lookup(info
                    .getServiceName());
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
    public Endpoint removeEndpoint(EndpointInfo info, ServiceUnit srvcUnit) {
        if (info == null) {
            return null;
        } else {
            EntryRegistry<String, Endpoint> imap = info.isProvides() ? mProvMap
                    .lookup(info.getServiceName()) : mConsMap.lookup(info
                    .getServiceName());
                    
            WLMSEEndPoint wlmemp = (WLMSEEndPoint) lookupEndpoint(info);
            if (wlmemp != null) {
                wlmemp.getServiceUnitNames().remove(srvcUnit.getName());
                if (wlmemp.getServiceUnitNames().size() == 0) {
                    return (imap == null) ? null : imap.remove(info.getEndpointName());
                }
            }                    
            return null;
        }
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#getContext() */
    public ManagerContext getContext() {
        return mMgrCtx;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#setContext(com.sun.jbi.component.toolkit.lifecycle.ManagerContext) */
    public void setContext(ManagerContext ctx) {
        mMgrCtx = ctx;
    }

    /** @see java.lang.Object#toString() */
    public String toString() {
        try {
            StringBuffer buff = new StringBuffer();
            buff.append(this.getClass().getSimpleName()).append(
                    "{provisioning={");
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
        } catch (Exception e) {
            // avoid concurrent modification issues, however unlikely
            return super.toString();
        }
    }

    protected Logger log() {
        return mLogger;
    }

    //Create Endpoint is called when 
    public Endpoint createEndpoint(EndpointInfo info, ServiceUnit srvcUnit)
            throws DeploymentException {
        // TODO Auto-generated method stub
        Endpoint ep = lookupEndpoint(info);
        WLMSEEndPoint wlmep = null;
        if (ep != null) {
             wlmep = (WLMSEEndPoint) ep;
            if (!wlmep.getServiceUnitNames().contains(srvcUnit.getName())) {
                wlmep.getServiceUnitNames().add(srvcUnit.getName());
            }
        } else {
             wlmep = new WLMSEEndPoint (info, srvcUnit);            
        }
        return wlmep;        
    }

}
