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
 * @(#)AbstractEndpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.endpoint.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.jbi.JBIException;
import javax.wsdl.Operation;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.component.endpoint.Endpoint;

/**
 * Abstract base class for {@link Endpoint} implementations.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractEndpoint<T> implements Endpoint<T> {
    private EndpointInfo mInfo = null;
    private boolean mStarted = false;
    private Map<String, Operation> mOperations = new HashMap<String, Operation>();
    private Map<String, T> mProcessDefs = new HashMap<String, T>();

//    private String mServiceUnitName = null;
    
    public AbstractEndpoint(EndpointInfo info) {
        setInfo(info);
    }

    /** @see com.sun.jbi.component.endpoint.Endpoint#getInfo() */
    public EndpointInfo getInfo() {
        return mInfo;
    }

	/** @see com.sun.jbi.component.endpoint.Endpoint#isStarted() */
	public boolean isStarted() {
		return mStarted;
	}

	/** @see com.sun.jbi.component.endpoint.Endpoint#start() */
	public void start() throws JBIException {
		mStarted = true;
	}

	/** @see com.sun.jbi.component.endpoint.Endpoint#stop() */
	public void stop() throws JBIException {
		mStarted = false;
	}

    protected void setInfo(EndpointInfo info) {
        mInfo = info;
    }

//    protected String getServiceUnitName() {
//        return mServiceUnitName;
//    }
//
//    public void setServiceUnitName(String serviceUnitName) {
//        mServiceUnitName = serviceUnitName;
//    }

	/** @see com.sun.jbi.component.endpoint.Endpoint#getOperation(java.lang.String) */
    public Operation getOperation(String opName) {
        return mOperations.get(opName);
    }

    /** @see com.sun.jbi.component.endpoint.Endpoint#getOperations() */
	public Collection<Operation> getOperations() {
		return new ArrayList<Operation>(mOperations.values());
	}

    /** @see com.sun.jbi.component.endpoint.Endpoint#getServiceDef(java.lang.String) */
	public T getServiceDef(String opName) {
		return mProcessDefs.get(opName);
	}

	/** @see com.sun.jbi.component.endpoint.Endpoint#setServiceDef(javax.wsdl.Operation, java.lang.Object) */
    public void setServiceDef(Operation op, T def) {
    	mProcessDefs.put(op.getName(), def);
    	mOperations.put(op.getName(), op);
    }

	/** @see java.lang.Object#toString() */
	public String toString() {
		StringBuffer buff = new StringBuffer();
		buff.append(this.getClass().getSimpleName()).append("[srvc=")
			.append(getInfo().getServiceName()).append(",endpt=")
			.append(getInfo().getEndpointName()).append(",")
			.append(getInfo().isProvides() ? "provides]" : "consumes]");
		return buff.toString();
	}
    
}
