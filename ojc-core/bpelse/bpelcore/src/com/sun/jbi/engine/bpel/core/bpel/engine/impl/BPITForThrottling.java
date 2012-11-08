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
 * @(#)BPITForThrottling.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
* <code>BPITForThrottling</code>.
* Class scheduled with the ReadyToRunQueue for throttling the outbound 
* invoke activity for an endpoint.
* When the processing count for an endpoint has exceded the configured 
* throttling limit then an instance of this class is created and pushed 
* into the ReadyToRunQueue, where it will reside and be checked periodically
* by the <code>BPELSEInOutThread</code> instances. 
* It will becaome a candidate for processing once the processing count for
* the endpoint has been lowered below the configured throttling count.
*
* @author <a href="mailto:pVarghese@PVARGHESE-HP"></a>
* @version 1.0
*/

@Deprecated
public class BPITForThrottling extends BusinessProcessInstanceThread {
    
    private static Logger LOGGER = Logger.getLogger(BPITForThrottling.class.getName());
    
    
	private EndpointInfo mEndpointInfo = null;
	private int mThrottleCount = 0;
	
	public BPITForThrottling(BPELProcessManager processManager, Engine engine,
			ICallFrame mFrame, EndpointInfo info, int throttleCount) {
		super(processManager, engine, mFrame);
		mEndpointInfo = info;
		mThrottleCount = throttleCount;
		
	}

	@Override
	/**
	 * This call is delegated to the <code>BPELProcessManager</code>
	 * that will query the <code>ThrottlingConfigInfo</code> instance 
	 * associated with this <code>mEndpointInfo</code> to see if 
	 * space is available to process the outbound invoke activity. 
	 */
	public boolean isReady() {
		boolean retVal =  super.isReady();
		
		if (!retVal) {
			return retVal; 
		}
		
		boolean returnVal = mProcessManager.canConsumeService(mEndpointInfo);
        if (!returnVal) {
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, 
                    I18n.loc("BP instance Throttled : {0}", getProcessInstanceId() 
                    ));
            } 
        }
        return returnVal;		
	}
	
	public EndpointInfo getEndpointInfo() {
		return mEndpointInfo;
	}

	public int getThrottleCount() {
		return mThrottleCount;
	}
}
