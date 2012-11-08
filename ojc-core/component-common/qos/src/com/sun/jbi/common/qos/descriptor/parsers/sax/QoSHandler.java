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
 * @(#)QoSHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor.parsers.sax;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.descriptor.parsers.sax.ServiceAssemblyHandler;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.parsers.RedeliveryParser;
import com.sun.jbi.common.qos.descriptor.parsers.ThrottlingParser;
import com.sun.jbi.common.qos.descriptor.parsers.TrackingParser;
import com.sun.jbi.common.qos.messaging.tracking.MessageTracking;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.Redirect;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.qos.throttling.Throttling;
import com.sun.jbi.common.util.Util;

/**
 * 
 * @author Kevan Simpson
 */
public class QoSHandler extends ServiceAssemblyHandler {
	private Map<EndpointInfo, List<ServiceQuality>> mQualityMap;
	private EndpointInfo mConsumer, mProvider;
	private List<Connection> mQoSConnList;

	// redelivery
	private int mMaxAttempts = 0;
	private long mWaitTime = 0;
	private boolean mOnFailure;
	// tracking
	private boolean[] mTrackingFlags = new boolean[3];
	private Map<String, String> mPayloadQueries, mMessageQueries;

	/**
	 * 
	 */
	public QoSHandler() {
		mQualityMap = new HashMap<EndpointInfo, List<ServiceQuality>>();
		mQoSConnList = new ArrayList<Connection>();
	}

	public Map<EndpointInfo, List<ServiceQuality>> getServiceQualities() {
		return mQualityMap;
	}

	public Connection[] getQoSConnections() {
		return mQoSConnList.toArray(new Connection[mQoSConnList.size()]);
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#endElement(java.lang.String, java.lang.String, java.lang.String) */
	@Override
	public void endElement(String uri, String localName, String name)
			throws SAXException {
		if (uri.equals(ServiceQuality.QoS_NS)) {
			if (localName.equals(JbiDescriptor.CONNECTION_ELEM)) {
				mQoSConnList.add(new Connection(mConsumer, mProvider));
			}
		}
		else if (uri.equals(MessageTracking.TRACKING_NS) &&
				 localName.equals(TrackingParser.TRACKING_ELEM)) {
			// add tracking config
			addServiceQuality(MessageTracking.createTrackingConfig(
				        mTrackingFlags[0], mTrackingFlags[1], mTrackingFlags[2], 
				        mPayloadQueries, mMessageQueries));
		}
		else {
			super.endElement(uri, localName, name);
		}
	}

	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes) */
	@Override
	public void startElement(String uri, String localName, String name,
							 Attributes attr) throws SAXException {
		if (uri.equals(ServiceQuality.QoS_NS)) {
			// parse consumes and provides
			if (localName.equals(JbiDescriptor.CONSUMER_ELEM)) {
				mConsumer = resolveEndpoint(attr, false);
			}
			else if (localName.equals(JbiDescriptor.PROVIDER_ELEM)) {
				mProvider = resolveEndpoint(attr, true);
			}
		}
		else if (uri.equals(Redelivery.REDELIVERY_NS)) {
			parseRedelivery(uri, localName, name, attr);
		}
		else if (uri.equals(Throttling.THROTTLING_NS) &&
				 localName.equals(ThrottlingParser.THROTTLING_ELEM)) {
		    final String alg = attr.getValue(ThrottlingParser.ALGORITHM);
                    if (alg != null && alg.equals("leakyBucket"))
                        addServiceQuality(Throttling.createThrottlingLeakyConfig(Util.parseInt(
					attr.getValue(ThrottlingParser.LEAK_RATE), -1)));
                    else
			addServiceQuality(Throttling.createThrottlingConfig(Util.parseInt(
					attr.getValue(ThrottlingParser.MAX_CONCURRENCY_LIMIT), -1)));
		}
		else if (uri.equals(MessageTracking.TRACKING_NS)) {
			parseTracking(uri, localName, name, attr);
		}
		else {
			super.startElement(uri, localName, name, attr);
		}
	}

	protected void addServiceQuality(ServiceQuality qual) {
		List<ServiceQuality> list = mQualityMap.get(getConsumer());
		if (list == null) {
			list = new ArrayList<ServiceQuality>();
			mQualityMap.put(getConsumer(), list);
		}
		
		list.add(qual);
	}
	
	protected EndpointInfo getConsumer() {
		return mConsumer;
	}

	protected void parseRedelivery(String uri, String localName, String name,
			 					   Attributes attr) {
		if (localName.equals(RedeliveryParser.REDELIVERY_ELEM)) {
			mMaxAttempts = Util.parseInt(attr.getValue(RedeliveryParser.MAX_ATTEMPTS_ATTR), 0);
			mWaitTime = Util.parseLong(attr.getValue(RedeliveryParser.WAIT_TIME_ATTR), 0);
		}
		else if (localName.equals(RedeliveryParser.ON_FAILURE_ELEM)) {
			mOnFailure = true;
		}
		else if (mOnFailure) {
			Redirect redirect = null;
			Failure failure = Failure.error;

			try {
				failure = Failure.valueOf(localName);
			}
			catch (Exception e) { /* ignore */ }

			if (failure == Failure.redirect) {
				redirect = new Redirect(
						resolveEndpoint(attr, false),
						attr.getValue(RedeliveryParser.OPERATION_ATTR));
			}

			addServiceQuality(Redelivery.createRedeliveryConfig(
						mMaxAttempts, mWaitTime, failure, redirect));
		}
	}
	
	protected void parseTracking(String uri, String localName, String name,
			   					 Attributes attr) {
		if (localName.equals(TrackingParser.TRACKING_ELEM)) {
			String[] flags = { 
					TrackingParser.TRACKING_ATTR, 
					TrackingParser.STORE_ATTR,
					TrackingParser.EXTERNALIZE_ATTR
			};
			for (int i = 0; i < 3; i++) {
				mTrackingFlags[i] = resolveBoolean(attr, flags[i]);
			}
			
			mPayloadQueries = new TreeMap<String, String>();
    		mMessageQueries = new TreeMap<String, String>();
		}
		else if (localName.equals(TrackingParser.PAYLOAD_QUERY_ELEM)) {
			extractQuery(attr, TrackingParser.XPATH_ATTR, mPayloadQueries);
		}
		else if (localName.equals(TrackingParser.MESSAGE_QUERY_ELEM)) {
			extractQuery(attr, TrackingParser.PROPERTY_ATTR, mMessageQueries);
		}
	}

	protected void extractQuery(Attributes attr, String valueAttr,
	                            Map<String, String> map) {
        String name = attr.getValue(TrackingParser.NAME_ATTR);
        if (!Util.isEmpty(name)) {
            map.put(name, attr.getValue(valueAttr));
        }
	}

}
