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
 * @(#)ThrottlingConfigInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * <code>ThrottlingConfigInfo</code>.
 * This class is called by the <code>BPITForThrottling</code> instance
 * that has been put into the ReadyToRunQueue to be scheduled for 
 * execution when space is available to make the outbound invocation.
 * An instance of this class is maintained in the <code>BPELProcessManager</code>
 * in a Map with the key being the <code>EndpointInfo</code> associated with
 * the outbound invoke activity.
 *
 * @author <a href="mailto:pVarghese@PVARGHESE-HP"></a>
 * @version 1.0
 */
public class ThrottlingConfigInfo {
	EndpointInfo mEndpointInfo = null;
	int mProcessingCount = 0;
	int mThrottleCount = 0;
	QName bpName = null;
    private static Logger LOGGER = Logger.getLogger(BPELProcessManagerImpl.class.getName());
	
	public ThrottlingConfigInfo(EndpointInfo info, int throttleCount, QName bpName) {
		mEndpointInfo = info;
		mThrottleCount = throttleCount;
		this.bpName = bpName;
	}
	
	/**
	 * Increment the endpoint processing count.
	 * This is called when a <code>BPITForThrottling</code>
	 * instance for this endpoint is picked up for processing
	 * when space became available. 
	 *
	 */
	public void incrementProcessingCount() {
		mProcessingCount++;
	}
	
	/**
	 * Decrement the throttling endpoint processing count.
	 * This is called when the outbound invoke activity has
	 * either completed successfully or has thrown a 
	 * processing exception.
	 *
	 */
	public void decrementProcessingCount() {
		mProcessingCount--;
	}
	
	/**
	 * Simple check to determine if there is space available to
	 * process a throttling thread invocation for this endpoint.
	 * @return boolean: <code>true</code> or <code>false</code>
	 */
	public boolean isReady() {
	    boolean retValue = false;
		if (mThrottleCount > mProcessingCount) {
		    retValue = true;
		}

		if (LOGGER.isLoggable(Level.FINE)) {
		    LOGGER.log(Level.FINE, 
		        I18n.loc("Invoke for BP Id: {0}, Service Name: {1}, Endpoint Name: {2}, Out standing requests: {3}, Max requests: {4} is throttled: {5},",
		                bpName, mEndpointInfo.getServiceName(), mEndpointInfo.getEndpointName(),  
		                mProcessingCount, mThrottleCount, retValue?"No":"Yes"));
		}
		
		return retValue;
	}
}
