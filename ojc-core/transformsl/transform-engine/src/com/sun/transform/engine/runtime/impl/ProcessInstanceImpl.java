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
 * @(#)ProcessInstanceImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.HashMap;
import java.util.Map;

import com.sun.transform.descriptor.TransformEndpoint;
import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.ProcessDef;
import com.sun.transform.engine.runtime.ActivityUnit;
import com.sun.transform.engine.runtime.Engine;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessUnitFactory;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.VariableContext;

/**
 * Default implementation of {@link ProcessInstance}.
 * @author Kevan Simpson
 */
public class ProcessInstanceImpl extends InvocationUnitImpl 
								 implements ProcessInstance {
	private static ProcessUnitFactory mDefaultUnitFactory = new ProcessUnitFactoryImpl();
	
	private int mIndex = -1;
	private VariableContext mVarCtx;
	private ProcessUnitFactory mUnitFactory;
	private Map<Activity, ActivityUnit> mActivityMap;
	private TransformEndpoint mEndpoint;
	
	public ProcessInstanceImpl(ProcessDef def, 
							   ProcessUnitFactory fac, 
							   TransformEndpoint endpt) {
		super(def);
		mUnitFactory = (fac == null) ? mDefaultUnitFactory : fac;
		mVarCtx = new VariableContextImpl();
		mActivityMap = new HashMap<Activity, ActivityUnit>();
		mEndpoint = endpt;
	}

	/** @see com.sun.transform.engine.runtime.ProcessInstance#createUnit(com.sun.transform.engine.model.Activity) */
    public ActivityUnit createUnit(Activity act) {
        return (act == null) ? null : mUnitFactory.create(act, this);
    }

    /** @see com.sun.transform.engine.runtime.ProcessInstance#currentActivity() */
	public ActivityUnit currentActivity() {
		Activity act = getProcessDef().getActivity(mIndex);
		return (act == null) ? null : mActivityMap.get(act);
	}

	// this will be if we want to pool process instances
//	private void reset() throws ProcessingException {
//		mActivityMap.clear();
//		mVarCtx.reset();
//		mExchangeStack.clear();
//		setMessageExchange(null);
//	}

	/** @see com.sun.transform.engine.runtime.ProcessInstance#getProcessDef() */
	public ProcessDef getProcessDef() {
		return (ProcessDef) getActivity();
	}

	/** @see com.sun.transform.engine.runtime.impl.AbstractActivityUnit#getEnclosingProcess() */
	public ProcessInstance getEnclosingProcess() {
		return this;
	}

	/** @see com.sun.transform.engine.runtime.ProcessInstance#getEndpoint() */
	public TransformEndpoint getEndpoint() {
		return mEndpoint;
	}

	/** @see com.sun.transform.engine.runtime.impl.AbstractActivityUnit#getVariableContext() */
	public VariableContext getVariableContext() {
		return mVarCtx;
	}

	/** @see com.sun.transform.engine.runtime.ProcessInstance#isComplete() */
	public boolean isComplete() {
		return (mIndex >= getProcessDef().countActivities());
	}

	/** @see com.sun.transform.engine.runtime.ProcessInstance#nextActivity() */
	public ActivityUnit nextActivity() {
		mIndex += 1;
		Activity act = getProcessDef().getActivity(mIndex);
		if (act == null) {
			return null;
		}
		ActivityUnit unit = createUnit(act);
		mActivityMap.put(act, unit);
		return unit;
	}

	/** @see com.sun.transform.engine.runtime.impl.InvocationUnitImpl#execute(com.sun.transform.engine.runtime.Engine) */
	public boolean execute(Engine eng) throws ProcessingException {
		if (eng != null) {
			return eng.process((ProcessInstance) this);
		}
		return false;
	}
}
