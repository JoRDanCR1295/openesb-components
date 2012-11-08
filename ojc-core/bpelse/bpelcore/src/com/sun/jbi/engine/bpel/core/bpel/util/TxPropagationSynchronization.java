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
 * @(#)TxPropagationSynchronization.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import javax.transaction.Status;
import javax.transaction.Synchronization;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;

/**
 *
 *
 * @author Sun Microsystems
 */
public class TxPropagationSynchronization implements Synchronization {
	
	//ICallFrame mFrame;
	
	TxPropagationObject mTxPropObj;

	/**
	 * 
	 */
	public TxPropagationSynchronization(TxPropagationObject txPropObj) {
		super();
		//mFrame = frame;
		mTxPropObj = txPropObj;
	}

	/* (non-Javadoc)
	 * @see javax.transaction.Synchronization#afterCompletion(int)
	 */
	public void afterCompletion(int status) {
		BPELProcessManager processManager = mTxPropObj.getBPELProcessInstance().getBPELProcessManager();
		processManager.addTransactionSyncBPIT(mTxPropObj.getReplyFrame(), mTxPropObj, status);
	}

	/* (non-Javadoc)
	 * @see javax.transaction.Synchronization#beforeCompletion()
	 */
	public void beforeCompletion() {
		// Do nothing.
	}

}
