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
 * @(#)TestDeploymentLookup.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.descriptor.model.Identification;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.descriptor.QoSAssembly;

/**
 * 
 * @author Kevan Simpson
 */
public class TestDeploymentLookup extends DeploymentLookup {
    public TestDeploymentLookup() throws DeploymentException {
        super(null);
    }
    public ServiceAssembly getServiceAssembly(String serviceUnitName){
        return new ServiceAssembly(new Identification("testSA", "testSA"), null,null);
    }
    public Map<EndpointInfo, List<ServiceQuality>> lookupServiceQualities(EndpointInfo... consumers){
        return new HashMap<EndpointInfo, List<ServiceQuality>>();
    }
    public EndpointInfo[] getConsumersByProvider(EndpointInfo provider){
        return null;
    }
	@Override
	public QoSAssembly getQoSAssembly(String serviceUnitName)
			throws DeploymentException {
		return new QoSAssembly(getServiceAssembly(serviceUnitName),
							   lookupServiceQualities(), null);
	}
}
