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
 * @(#)QoSAssembly.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.ServiceQuality;

/**
 * 
 * @author Kevan Simpson
 */
public class QoSAssembly extends ServiceAssembly {
    private Map<EndpointInfo, List<ServiceQuality>> mQosMap;
    private Connection[] mQosConnections;
    
	/**
	 * @param id
	 * @param units
	 * @param cons
	 */
	public QoSAssembly(ServiceAssembly sa, 
					   Map<EndpointInfo, List<ServiceQuality>> qos,
					   Connection[] cons) {
		super(sa.getIdentification(), sa.getServiceUnits(), sa.getConnections());
		mQosMap = qos;
		mQosConnections = cons;
	}

    public Map<EndpointInfo, List<ServiceQuality>> getServiceQualities() {
        return new HashMap<EndpointInfo, List<ServiceQuality>>(mQosMap);
    }
    
	public Connection[] getQoSConnections() {
		return mQosConnections;
	}

	/**
     * Fetches all consumer endpoints connected to the specified provider endpoint.
     * 
     * @param provider The specified provider endpoint.
     * @return An array of consumers of the specified endpoint.
     * @throws IllegalArgumentException if the specified provider is <code>null</code>.
     */
    public EndpointInfo[] getConsumersByProvider(EndpointInfo provider) {
    	if (mQosMap == null) {
    		return new EndpointInfo[0];
    	}
    	
        if (provider == null) {
            throw new IllegalArgumentException(I18n.loc(
                    "QOS-6018: Provider endpoint cannot be NULL!"));
        }
        
        List<EndpointInfo> consumers = new ArrayList<EndpointInfo>();
        for (Connection con : getConnections()) {
            if (provider.equals(con.getProvider())) {
                consumers.add(con.getConsumer());
            }
        }
        
        EndpointInfo[] endpts = new EndpointInfo[consumers.size()];
        consumers.toArray(endpts);
        return endpts;
    }
}
