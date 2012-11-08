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
 * @(#)AbstractProcessDecorator.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.ProcessDef;
import com.sun.transform.engine.runtime.ActivityUnit;
import com.sun.transform.engine.runtime.Engine;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.VariableContext;

/**
 * This is a concept class, meaning it's in development.
 * @author Kevan Simpson
 */
public abstract class AbstractProcessDecorator implements ProcessInstance {
	private ProcessInstance mProcess;
	
	public AbstractProcessDecorator(ProcessInstance proc) {
		mProcess = proc;
	}
	
	/** @see com.sun.transform.engine.runtime.ProcessInstance#currentActivity() */
	public ActivityUnit currentActivity() {
		return getProcess().currentActivity();
	}

	/** @see com.sun.transform.engine.runtime.ProcessInstance#getProcessDef() */
	public ProcessDef getProcessDef() {
		return getProcess().getProcessDef();
	}

	/** @see com.sun.transform.engine.runtime.ProcessInstance#nextActivity() */
	public ActivityUnit nextActivity() {
		return getProcess().nextActivity();
	}

	/** @see com.sun.transform.engine.runtime.ActivityUnit#execute(com.sun.transform.engine.runtime.Engine) */
	public boolean execute(Engine eng) throws ProcessingException {
		if (eng != null) {
			eng.process(getProcess());
		}
		return false;
	}

	/** @see com.sun.transform.engine.runtime.ActivityUnit#getActivity() */
	public Activity getActivity() {
		return getProcess().getActivity();
	}

	/** @see com.sun.transform.engine.runtime.ActivityUnit#getVariableContext() */
	public VariableContext getVariableContext() {
		return getProcess().getVariableContext();
	}

	/**
	 * Protected accessor for process instance.
	 * @return the underlying process instance.
	 */
	protected ProcessInstance getProcess() {
		return mProcess;
	}
}
