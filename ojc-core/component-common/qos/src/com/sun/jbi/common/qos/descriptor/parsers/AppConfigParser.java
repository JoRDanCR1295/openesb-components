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
 * @(#)AppConfigParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor.parsers;

import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.parsers.ServicesParser;
import com.sun.jbi.common.qos.descriptor.QosServicesDescriptor;

/**
 * 
 * @author Kevan Simpson
 */
public class AppConfigParser extends ServicesParser {
	private Map<EndpointInfo, String> mAppConfigMap;

	/**
	 * 
	 */
	public AppConfigParser() {
		mAppConfigMap = new HashMap<EndpointInfo, String>();
	}

	public Map<EndpointInfo, String> getApplicationConfigurations() {
		return new HashMap<EndpointInfo, String>(mAppConfigMap);
	}

	/** @see com.sun.jbi.common.descriptor.parsers.AbstractJbiParser#resolveEndpoint(org.w3c.dom.Element, boolean) */
	protected EndpointInfo resolveEndpoint(Element elem, boolean isProvides) {
		EndpointInfo info = super.resolveEndpoint(elem, isProvides);
		
		NodeList list = elem.getElementsByTagNameNS(
				QosServicesDescriptor.APP_CONFIG_NS, 
				QosServicesDescriptor.APPLICATION_CONFIGURATION_ELEM);
		if (list != null && list.getLength() == 1) {
            mAppConfigMap.put(info, 
            		((Element) list.item(0)).getAttribute(JbiDescriptor.NAME_ELEM));
        }
	
		return info;
	}
}
