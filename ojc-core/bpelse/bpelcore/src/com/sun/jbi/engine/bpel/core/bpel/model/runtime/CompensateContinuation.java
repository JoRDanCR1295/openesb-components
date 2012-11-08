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
 * @(#)CompensateContinuation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;

/**
 * The interface that will help the <compensate> and <compensateScope> activities to 
 * resume their next logical iteration. This is need for the scenario where the 
 * compensation handler invoked by these activities have blocking messaging activities
 * whose path of completion would lead to the ActivityUnitImpl.doPassControlToParent() mehod
 * invocation of the resume path for activities. In this case these activities would have to 
 * schedule its enclosing context for its continuation.
 * TODO: have to see if this is redundant and these activities can rely on the StructuredActivityImpl
 * class for the same functionality.
 * 
 * @author Sun Microsystems, Inc. 
 *
 */
public interface CompensateContinuation {
	/**
	 * provision for the path of resumption for the <compensate> and <compensateScope> activities
	 * in the ActivityUnitImpl.doPassControlToParent() iteration logic.
	 * @param childActUnit
	 * @param frame
	 * @param bpit
	 * @param rObjs
	 * @return
	 * @throws Exception
	 */
	boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
			BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception;
}
