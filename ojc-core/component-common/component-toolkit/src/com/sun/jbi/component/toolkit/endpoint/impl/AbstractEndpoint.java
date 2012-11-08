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

package com.sun.jbi.component.toolkit.endpoint.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.JBIException;
import javax.wsdl.Operation;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;

/**
 * Abstract base class for {@link Endpoint} implementations.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractEndpoint<T> implements Endpoint<T> {
    private EndpointInfo mInfo = null;
    private boolean mStarted = false;
    // TODO make these EntryRegistry instead of Map?  or synchronized Map?
    private Map<String, Operation> mOperations = new HashMap<String, Operation>();
    private Map<String, T> mProcessDefs = new HashMap<String, T>();
//    private Map<QName, Schema> mSchemas = new HashMap<QName, Schema>();
    
//    private String mServiceUnitName = null;
    
    public AbstractEndpoint(EndpointInfo info) {
        setInfo(info);
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#getInfo() */
    public EndpointInfo getInfo() {
        return mInfo;
    }

	/** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#isStarted() */
	public boolean isStarted() {
		return mStarted;
	}

	/** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#start() */
	public void start() throws JBIException {
		mStarted = true;
	}

	/** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#stop() */
	public void stop() throws JBIException {
		mStarted = false;
	}

    protected void setInfo(EndpointInfo info) {
        mInfo = info;
    }

	/** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#getOperation(java.lang.String) */
    public Operation getOperation(String opName) {
        return mOperations.get(opName);
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#getOperations() */
	public Collection<Operation> getOperations() {
		return new ArrayList<Operation>(mOperations.values());
	}

    /** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#getServiceDef(java.lang.String) */
	public T getServiceDef(String opName) {
		return mProcessDefs.get(opName);
	}
	
//	/** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#lookupSchema(javax.xml.namespace.QName) */
//    public Schema lookupSchema(QName msgType) {
//        return (msgType == null) ? null : mSchemas.get(msgType);
//    }
//
//    /** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#registerSchema(javax.xml.namespace.QName, org.exolab.castor.xml.schema.Schema) */
//    public void registerSchema(QName msgType, Schema schema) {
//        if (msgType != null && schema != null) {
//            mSchemas.put(msgType, schema);
//        }
//    }

    /** @see com.sun.jbi.component.toolkit.endpoint.Endpoint#setServiceDef(java.lang.Object, javax.wsdl.Operation[]) */
    public void setServiceDef(T def, Operation... ops) {
        for (Operation op : ops) {
            mProcessDefs.put(op.getName(), def);
            mOperations.put(op.getName(), op);
        }
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
