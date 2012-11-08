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
 * @(#)InactiveCallframeInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;

public class InactiveCallframeInfo {

	long mTimestamp;
	ICallFrame mCallframe;
	InactivityReason mReason;
	
	public InactiveCallframeInfo(ICallFrame callframe) {
		this.mCallframe = callframe;
	}

	public InactiveCallframeInfo(ICallFrame callframe, InactivityReason reason) {
		this.mCallframe = callframe;
		this.mReason = reason;
		this.mTimestamp = System.currentTimeMillis();
	}

	/**
	 * NOTE: Ideally, the criterion for waiting instances should be inverted compared to
	 * other instances for passivation (instance held in response/request/status pending maps,
	 * as for non-wait type, the logic being select the most aged first, but for instances
	 * with defined expiry we want to select the last expiring instances first).
	 * To keep the code simple, for now same criterion is used for all frames
	 * for passivation, i.e how long they have been idle. 
	 * 
	 * @param idlenessCriterion
	 * @return
	 */
	public boolean passesIdlenssCriterion(long idlenessCriterion) {
		long inactivityTime = System.currentTimeMillis() - mTimestamp;
		if (inactivityTime < idlenessCriterion) {
			return false;
		}
		return true;
	}
	
	public ICallFrame getCallframe() {
		return mCallframe;
	}
	
	public boolean equals(Object obj) {
		ICallFrame frame = ((InactiveCallframeInfo) obj).getCallframe();
		if (this.mCallframe == frame) {
			return true;
		}
		return false;
	}

	public InactivityReason getInactivityReason() {
		return mReason;
	}
}
