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
 * @(#)AbstractActivityUnit.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.runtime.ActivityUnit;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.VariableContext;

/**
 * Abstract base for {@link ActivityUnit} implementations.
 * @author Kevan Simpson
 */
public abstract class AbstractActivityUnit implements ActivityUnit {
	private Activity mModel;
	private ProcessInstance mProcessInst;	

	public AbstractActivityUnit(Activity act, ProcessInstance proc) {
		mModel = act;
		mProcessInst = proc;
	}
	
	/** @see com.sun.transform.engine.runtime.ActivityUnit#getActivity() */
	public Activity getActivity() {
		return mModel;
	}

	/** @see com.sun.transform.engine.runtime.ActivityUnit#getEnclosingProcess() */
	public ProcessInstance getEnclosingProcess() {
		return mProcessInst;
	}

	/** @see com.sun.transform.engine.runtime.ActivityUnit#getVariableContext() */
	public VariableContext getVariableContext() {
		return getEnclosingProcess().getVariableContext();
	}
	
	/** @see java.lang.Object#toString() */
	public String toString() {
		return String.valueOf(getActivity());
	}
}
