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
 * @(#)QosServices.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor;

import java.util.Map;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.model.Services;

/**
 * Represents a list of <code>consumes</code> and <code>provides</code>
 * entries in a service unit descriptor, along with any associated
 * <code>ApplicationConfiguration</code>.
 * 
 * @author Kevan Simpson
 */
public class QosServices extends Services {
    private Map<EndpointInfo, String> mAppConfigMap;
    
    /**
     * @param provides
     * @param consumes
     */
    public QosServices(EndpointInfo[] provides, EndpointInfo[] consumes,
                       Map<EndpointInfo, String> appConfig) {
        super(provides, consumes);
        mAppConfigMap = appConfig;
    }

    public Map<EndpointInfo, String> getApplicationConfigurations() {
        return mAppConfigMap;
    }
    
    public String getApplicationConfiguration(EndpointInfo endpt) {
        return mAppConfigMap.get(endpt);
    }
}
