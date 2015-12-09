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
package org.glassfish.openesb.databasebc;

// common-util and qos imports
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;

import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;


import java.util.Map;
import java.util.List;
import java.util.Iterator;

/**
 * <code>ThrottlingConfigInfo</code>.
 * This class is called by the <code>Throttling</code> instance
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
	
	public ThrottlingConfigInfo(EndpointInfo info, int throttleCount) {
		mEndpointInfo = info;
		mThrottleCount = throttleCount;
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
		if (mThrottleCount > mProcessingCount) {
			return true;
		}
		return false;
	}

	public void getQOSConfigurations (EndpointBean jdbcbcEndpoint,
                                       EndpointIdentifier endpointIdentifier,
                                       Map <EndpointInfo, List<ServiceQuality>> qosMap) {
        if (qosMap != null && qosMap.size() > 0) {
            // Until there's a total transitioning to use the common-util, there's a need to
            // create EndpointInfo using EndpointIndentifier
            EndpointInfo endpointInfo = new EndpointInfo (false, 
                                                          endpointIdentifier.getEndpointName(), 
                                                          null,
                                                          endpointIdentifier.getServiceName(),
                                                          null);
            List<ServiceQuality> qoss = qosMap.get(endpointInfo);
            Iterator<ServiceQuality> qossIter = qoss.iterator();
            while (qossIter.hasNext()) {
                ServiceQuality qos = qossIter.next();
                // Gather throttling config
                if (qos instanceof ThrottlingConfig) {
                    ThrottlingConfig throttleConfig = (ThrottlingConfig)qos;
                    jdbcbcEndpoint.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
                }
                // Other services....
            }
        }
    }
}
