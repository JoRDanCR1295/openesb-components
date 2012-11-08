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

package com.sun.jbi.component.endpoint.impl;

import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.endpoint.Endpoint;
import com.sun.jbi.component.endpoint.EndpointFactory;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.crl.util.EntryRegistry;

/**
 * Default implementation of an {@link EndpointManager}.
 * 
 * @author Kevan Simpson
 */
public class DefaultEndpointManager implements EndpointManager {
    private EntryRegistry<QName, EntryRegistry<String, Endpoint>> mConsMap =
            new EntryRegistry<QName, EntryRegistry<String,Endpoint>>();
    private EntryRegistry<QName, EntryRegistry<String, Endpoint>> mProvMap =
            new EntryRegistry<QName, EntryRegistry<String,Endpoint>>();
    private EndpointFactory mFactory = null;
    private EntryRegistry<QName, EntryRegistry<String, ServiceUnit>> mSrvcUnits =
    		new EntryRegistry<QName, EntryRegistry<String,ServiceUnit>>();
    
    public DefaultEndpointManager(EndpointFactory fac) {
        mFactory = fac;
    }
    
    /** @see com.sun.jbi.component.endpoint.EndpointManager#getEndpointFactory() */
    public EndpointFactory getEndpointFactory() {
        return mFactory;
    }

    /** @see com.sun.jbi.component.endpoint.EndpointManager#initServiceUnit(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void addServiceUnit(ServiceUnit srvcUnit) throws DeploymentException {
    	EndpointInfo[] endpts = srvcUnit.getServices().getProvides();
    	
    	for (EndpointInfo info : endpts) {
            EntryRegistry<String, ServiceUnit> unitReg = 
            		mSrvcUnits.lookup(info.getServiceName());
            if (unitReg == null) {
                synchronized (mSrvcUnits) {
                    unitReg = new EntryRegistry<String, ServiceUnit>();
                    mSrvcUnits.register(info.getServiceName(), unitReg);
//                    mSrvcUnits.register(QName.valueOf(srvcUnit.getName()), unitReg);
                }
            }
            unitReg.register(info.getEndpointName(), srvcUnit);
    	}
    }

    /** @see com.sun.jbi.component.endpoint.EndpointManager#lookupServiceUnit(javax.jbi.servicedesc.ServiceEndpoint) */
    public ServiceUnit lookupServiceUnit(ServiceEndpoint endpt) {
        if (endpt == null) return null;
        
        EntryRegistry<String, ServiceUnit> unitReg = 
                mSrvcUnits.lookup(endpt.getServiceName());
        return (unitReg != null) ? unitReg.lookup(endpt.getEndpointName()) : null;
    }


    /** @see com.sun.jbi.component.endpoint.EndpointManager#removeServiceUnit(com.sun.jbi.common.descriptor.ServiceUnit) */
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

    /** @see com.sun.jbi.component.endpoint.EndpointManager#lookupEndpoint(javax.xml.namespace.QName, java.lang.String, boolean) */
    public Endpoint lookupEndpoint(QName serviceName, String endpointName, boolean provisioning) {
        if (provisioning) {
            EntryRegistry<String, Endpoint> imap = mProvMap.lookup(serviceName);
            return (imap == null) ? null : imap.lookup(endpointName);
        }
        else {
            EntryRegistry<String, Endpoint> imap = mConsMap.lookup(serviceName);
            return (imap == null) ? null : imap.lookup(endpointName);
        }
    }

    /** @see com.sun.jbi.component.endpoint.EndpointManager#registerEndpoint(javax.xml.namespace.QName, javax.xml.namespace.QName, com.sun.jbi.component.endpoint.Endpoint) */
    public void registerEndpoint(QName serviceName, 
                                 String endpointName, 
                                 Endpoint ept) {
        if (ept.getInfo().isProvides()) {
            EntryRegistry<String, Endpoint> imap = mProvMap.lookup(serviceName);
            if (imap == null) {
                synchronized (mProvMap) {
                    imap = new EntryRegistry<String, Endpoint>();
                    mProvMap.register(serviceName, imap);
                }
            }
            imap.register(endpointName, ept);
        }
        else {
            EntryRegistry<String, Endpoint> imap = mConsMap.lookup(serviceName);
            if (imap == null) {
                synchronized (mConsMap) {
                    imap = new EntryRegistry<String, Endpoint>();
                    mConsMap.register(serviceName, imap);
                }
            }
            imap.register(endpointName, ept);
        }
    }

    /** @see com.sun.jbi.component.endpoint.EndpointManager#removeEndpoint(javax.xml.namespace.QName, javax.xml.namespace.QName, boolean) */
    public Endpoint removeEndpoint(QName serviceName, 
                                   String endpointName, 
                                   boolean provisioning) {
        if (provisioning) {
            Endpoint ept = null;
            EntryRegistry<String, Endpoint> imap = mProvMap.lookup(serviceName);
            if (imap != null) ept = imap.remove(endpointName);
            return ept;
        }
        else {
            Endpoint ept = null;
            EntryRegistry<String, Endpoint> imap = mConsMap.lookup(serviceName);
            if (imap != null) ept = imap.remove(endpointName);
            return ept;
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

}
