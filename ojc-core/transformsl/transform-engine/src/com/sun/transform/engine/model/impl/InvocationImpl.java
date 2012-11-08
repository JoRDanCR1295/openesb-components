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
 * @(#)InvocationImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model.impl;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.wsdl.Operation;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.Transform;

/**
 * Default implementation of an {@link Invocation} activity.
 * @author Kevan Simpson
 */
public class InvocationImpl extends AbstractActivity implements Invocation {
	private String mInputVar, mOutputVar;
	private EndpointInfo mInfo;
	private Operation mOperation;
	private Map<String, Transform> mTransforms;
	
	public InvocationImpl(String name, EndpointInfo info, Operation op, 
						  String inputVar, String outputVar) {
	    super(name);
		mInfo = info;
		mOperation = op;
		mInputVar = inputVar;
		mOutputVar = outputVar;
		mTransforms = new HashMap<String, Transform>();
	}
	
	/** @see com.sun.transform.engine.model.Invocation#addTransform(com.sun.transform.engine.model.Transform) */
    public void addTransform(Transform tr) {
        if (tr != null) {
            mTransforms.put(tr.getName(), tr);
        }
    }

    /** @see com.sun.transform.engine.model.Invocation#getInfo() */
	public EndpointInfo getInfo() {
		return mInfo;
	}

	/** @see com.sun.transform.engine.model.Invocation#getOperation() */
	public Operation getOperation() {
		return mOperation;
	}

	/** @see com.sun.transform.engine.model.Invocation#getInputVariable() */
	public String getInputVariable() {
		return mInputVar;
	}

	/** @see com.sun.transform.engine.model.Invocation#getOutputVariable() */
	public String getOutputVariable() {
		return mOutputVar;
	}
	
    /** @see com.sun.transform.engine.model.Invocation#getTransform(java.lang.String) */
    public Transform getTransform(String name) {
        return (name == null) ? null : mTransforms.get(name);
    }

	/** @see com.sun.transform.engine.model.Invocation#getFaultHandlers() */
    public Iterator<Transform> getFaultHandlers() {
        return mTransforms.values().iterator();
    }

    /** @see java.lang.Object#toString() */
	public String toString() {
		StringBuffer buff = new StringBuffer();
		buff.append(getName()).append("[endpt=").append(getInfo().getEndpointName())
			.append(", srvc=").append(getInfo().getServiceName())
			.append(", op=").append(getOperation().getName()).append("]");
			
		return buff.toString();
	}
}
