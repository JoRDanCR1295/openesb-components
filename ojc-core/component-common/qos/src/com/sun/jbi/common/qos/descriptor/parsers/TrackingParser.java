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
 * @(#)DescriptorHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor.parsers;

import java.util.Map;
import java.util.TreeMap;

import javax.jbi.management.DeploymentException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.parsers.AbstractJbiParser;
import com.sun.jbi.common.qos.messaging.tracking.MessageTracking;
import com.sun.jbi.common.qos.messaging.tracking.TrackingConfig;
import com.sun.jbi.common.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * Parses message tracking configuration in a service assembly descriptor.
 * @author Kevan Simpson
 */
public class TrackingParser extends AbstractJbiParser<TrackingConfig> {
	public static final String TRACKING_ELEM 	   = "message-tracking";
	public static final String TRACKING_ATTR 	   = "tracking";
	public static final String STORE_ATTR          = "store-payload";
	public static final String EXTERNALIZE_ATTR    = "externalize-payload";
	public static final String PAYLOAD_QUERY_ELEM  = "payload-query";
	public static final String NAME_ATTR           = "name";
	public static final String XPATH_ATTR          = "xpath";
    public static final String MESSAGE_QUERY_ELEM  = "message-query";
    public static final String PROPERTY_ATTR       = "property";
	
	private static final String MT_PREFIX = "mt";
	

	public TrackingParser(QosConnectionParser cp) {
		super(cp);
		getNSContext().addNamespace(MT_PREFIX, MessageTracking.TRACKING_NS);
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
	public TrackingConfig parse(Element elem) throws DeploymentException {
		try {
			NodeList list = elem.getElementsByTagNameNS(
			        MessageTracking.TRACKING_NS, TRACKING_ELEM);
			if (list != null && list.getLength() == 1) {
				Element mt = (Element) list.item(0);
				String[] attrs = { mt.getAttribute(TRACKING_ATTR),
				                   mt.getAttribute(STORE_ATTR),
				                   mt.getAttribute(EXTERNALIZE_ATTR) };
				boolean[] flags = new boolean[3];
				for (int i = 0; i < 3; i++) {
				    if (Util.isEmpty(attrs[i])) {
				        flags[i] = false;
				    }
				    else {
				        flags[i] = Boolean.valueOf(attrs[i]).booleanValue();
				    }
				}
				
				Map<String, String> payloadQueries = new TreeMap<String, String>(),
				        messageQueries = new TreeMap<String, String>();
				NodeList queryList = mt.getElementsByTagNameNS(
				        MessageTracking.TRACKING_NS, PAYLOAD_QUERY_ELEM);
				if (queryList != null) {
				    for (int i = 0, n = queryList.getLength(); i < n; i++) {
				        Element query = (Element) queryList.item(i);
				        extractQuery(query, XPATH_ATTR, payloadQueries);
				    }
				}
				
                queryList = mt.getElementsByTagNameNS(
                        MessageTracking.TRACKING_NS, MESSAGE_QUERY_ELEM);
                if (queryList != null) {
                    for (int i = 0, n = queryList.getLength(); i < n; i++) {
                        Element query = (Element) queryList.item(i);
                        extractQuery(query, PROPERTY_ATTR, messageQueries);
                    }
                }

                return MessageTracking.createTrackingConfig(
				        flags[0], flags[1], flags[2], 
				        payloadQueries, messageQueries);
			}
		}
		catch (Exception e) {
			throw error(e, I18n.loc(
					"QOS-6014: Failed to parse systemic message tracking configuration: {0}",
					e.getMessage()));
		}
		
		return null;
	}

	protected void extractQuery(Element query, String valueAttr,
	                            Map<String, String> map) {
        String name = query.getAttribute(NAME_ATTR);
        if (!Util.isEmpty(name)) {
            String val = query.getAttribute(valueAttr);
            map.put(name, val);
        }
	}
}
