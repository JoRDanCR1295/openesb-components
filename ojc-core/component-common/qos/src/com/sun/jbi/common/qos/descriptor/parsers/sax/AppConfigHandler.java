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
 * @(#)AppConfigHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor.parsers.sax;

import java.util.HashMap;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.parsers.sax.ServicesHandler;
import com.sun.jbi.common.qos.descriptor.QosServicesDescriptor;

/**
 * 
 * @author Kevan Simpson
 */
public class AppConfigHandler extends ServicesHandler {
	private Map<EndpointInfo, String> mConfigMap;
	
	public AppConfigHandler() {
		mConfigMap = new HashMap<EndpointInfo, String>();
	}
	
	public Map<EndpointInfo, String> getApplicationConfigurations() {
		return mConfigMap;
	}
	
//	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#endElement(java.lang.String, java.lang.String, java.lang.String) */
//	@Override
//	public void endElement(String uri, String localName, String name)
//			throws SAXException {
//		// set value
//		if (localName.equals(JbiDescriptor.SERVICES_ELEM)) {
//			mServices.endElement(uri, localName, name);
//			Services endpts = mServices.getValue();
//			setValue(new QosServices(endpts.getProvides(), endpts.getConsumes(), ))
//		}
//	}

	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes) */
	@Override
	public void startElement(String uri, String localName, String name,
							 Attributes attr) throws SAXException {
		if (localName.equals(QosServicesDescriptor.APPLICATION_CONFIGURATION_ELEM)) {
			mConfigMap.put(getCurrentInfo(), attr.getValue(JbiDescriptor.NAME_ELEM));
		}
		else {	// parse consumes and provides
			super.startElement(uri, localName, name, attr);
		}
	}
}
