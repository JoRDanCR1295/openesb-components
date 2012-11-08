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
 * @(#)QosConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.qos.ServiceQuality;

/**
 * Represents the collection of {@link ServiceQuality} connections
 * defined in a service assembly descriptor.
 * 
 * @deprecated Please use {@link QoSAssembly} instead.
 * @author Kevan Simpson
 */
public class QosConfig {
    private Connection[] mConnections;
    private Map<EndpointInfo, List<ServiceQuality>> mQosMap;
    
    public QosConfig(Connection[] cons, Map<EndpointInfo, List<ServiceQuality>> qos) {
        mConnections = cons;
        mQosMap = qos;
    }

    /**
     * Returns the <code>connection</code> entries in the descriptor.
     * @return the <code>connection</code> entries in the descriptor.
     */
    public Connection[] getConnections() {
        // FindBug warning fix - make copy to avoid exposing internal representation
        // Fix to preserve original semantics.  connections object may be null;
        if (mConnections == null) {
            return new Connection[0];
        }

        int len = mConnections.length;
        Connection[] dest = new Connection[len];
        System.arraycopy(mConnections, 0, dest, 0, len);
        return dest;
    }
    
    public Map<EndpointInfo, List<ServiceQuality>> getServiceQualities() {
        return new HashMap<EndpointInfo, List<ServiceQuality>>(mQosMap);
    }
}
