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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jbi.management.DeploymentException;

import org.w3c.dom.Element;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.descriptor.parsers.ConnectionParser;
import com.sun.jbi.common.descriptor.parsers.JbiParser;
import com.sun.jbi.common.qos.ServiceQuality;

/**
 * Parses JBI connections from a service assembly descriptor.
 * @author Kevan Simpson
 */
public class QosConnectionParser extends ConnectionParser {
	private Map<EndpointInfo, List<ServiceQuality>> mQualityMap;
	private List<JbiParser> mParsers;
	
	public QosConnectionParser(JbiParser... qosParsers) {
		super();
		mQualityMap = new HashMap<EndpointInfo, List<ServiceQuality>>();
		mParsers = new ArrayList<JbiParser>();
		setDefaultPrefix("qos");
		getNSContext().addNamespace("qos", ServiceQuality.QoS_NS);
	}

	public void addSystemicParsers(JbiParser... parsers) {
		if (parsers != null) {
			for (JbiParser jp : parsers) {
				mParsers.add(jp);
			}
		}
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.ConnectionParser#parseConnection(org.w3c.dom.Element) */
	protected Connection parseConnection(Element elem) throws DeploymentException {
		Connection conn = super.parseConnection(elem);
		List<ServiceQuality> qos = new ArrayList<ServiceQuality>();
		
		// parse systemics
		for (JbiParser parser : mParsers) {
			Object obj = parser.parse(elem);
			if (obj != null && obj instanceof ServiceQuality) {
				qos.add((ServiceQuality) obj);
			}
		}
		
		mQualityMap.put(conn.getConsumer(), qos);
		
		return conn;
	}


	public Map<EndpointInfo, List<ServiceQuality>> getServiceQualities() {
		return new HashMap<EndpointInfo, List<ServiceQuality>>(mQualityMap);
	}
}
