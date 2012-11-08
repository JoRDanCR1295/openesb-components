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
 * @(#)ConfigObject.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;

/**
 * This class abstracts the config tag and provides an object for the same.
 * 
 * @author karthikeyan s
 */
public class ConfigObject {

	private Map<String, String> properties = new HashMap<String, String>();

	/** Creates a new instance of ConfigObject */
	public ConfigObject() {
	}

	public Map<String, String> getProperties() {
		return properties;
	}

	public void setProperties(String key, String value) {
		properties.put(key, value);
	}

	public String toXMLString() {
		StringBuffer xmlString = new StringBuffer();
		if (properties.isEmpty()) {
			xmlString.append("<" + AspectConstants.CONFIG_TAG + "/>\n");
			return xmlString.toString();
		}
		xmlString.append("<" + AspectConstants.CONFIG_TAG + ">\n");
		Iterator it = properties.keySet().iterator();
		while (it.hasNext()) {
			String name = (String) it.next();
			String value = (String) properties.get(name);
			value = (value == null ? "" : value);
			xmlString.append("<" + AspectConstants.PROPERTY_TAG + " "
					+ AspectConstants.PROPERTY_ATTR_NAME + "=\"" + name + "\" "
					+ AspectConstants.PROPERTY_ATTR_VALUE + "=\"" + value
					+ "\"/>\n");
		}
		xmlString.append("</" + AspectConstants.CONFIG_TAG + ">\n");
		return xmlString.toString();
	}
}
