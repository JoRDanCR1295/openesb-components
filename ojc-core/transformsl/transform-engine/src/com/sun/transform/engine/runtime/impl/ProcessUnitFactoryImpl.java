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
 * @(#)ProcessUnitFactoryImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import javax.xml.namespace.QName;

import com.sun.transform.descriptor.TransformEndpoint;
import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.ProcessDef;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.runtime.ActivityUnit;
import com.sun.transform.engine.runtime.InvocationUnit;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessUnitFactory;
import com.sun.transform.engine.runtime.TransformUnit;

/**
 * Default implementation of {@link ProcessUnitFactory}.
 * @author Kevan Simpson
 */
public class ProcessUnitFactoryImpl implements ProcessUnitFactory {

	/** @see com.sun.transform.engine.runtime.ProcessUnitFactory#create(com.sun.transform.engine.model.Activity, com.sun.transform.engine.runtime.ProcessInstance) */
	public ActivityUnit create(Activity act, ProcessInstance proc) {
		if (act != null) {
			if (act instanceof Transform) {
				return create((Transform) act, proc);
			}
			else if (act instanceof Invocation) {
				return create((Invocation) act, proc);
			}
			else if (act instanceof ProcessDef) {
				return create((ProcessDef) act, proc);
			}
		}
		return null;
	}

	/** @see com.sun.transform.engine.runtime.ProcessUnitFactory#create(com.sun.transform.engine.model.Invocation, com.sun.transform.engine.runtime.ProcessInstance) */
	public InvocationUnit create(Invocation inv, ProcessInstance proc) {
		return new InvocationUnitImpl(inv, proc);
	}

	/** @see com.sun.transform.engine.runtime.ProcessUnitFactory#create(javax.xml.namespace.QName, com.sun.transform.descriptor.TransformEndpoint) */
	public ProcessInstance create(QName operation, TransformEndpoint endpt) {
		return new ProcessInstanceImpl(endpt.getServiceDef(operation.getLocalPart()), this, endpt);
	}

	/** @see com.sun.transform.engine.runtime.ProcessUnitFactory#create(com.sun.transform.engine.model.Transform, com.sun.transform.engine.runtime.ProcessInstance) */
	public TransformUnit create(Transform tr, ProcessInstance proc) {
		return new TransformUnitImpl(tr, proc);
	}
}
