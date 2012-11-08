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
 * @(#)WaitingMessages.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;

/**
 * @author Sun Microsystems
 *
 */

public class RequestLifeSpanHelper {
	
	private TreeMap<Long, InComingEventKeyImpl> mTimeToEventMap;
	
	private Map<InComingEventKeyImpl, Long> mEventToTimeMap;
	
	/**
	 * 
	 * @param engine
	 */
	public RequestLifeSpanHelper() {
		// Has a reference to the engine that this is on.
		mTimeToEventMap = new TreeMap<Long, InComingEventKeyImpl>();
		mEventToTimeMap = new HashMap<InComingEventKeyImpl, Long>(); 	
	}
	
	/**
	 * Adds the request event to the request life span helper along with life span of the 
	 * request in seconds.
	 * 
	 * @param event
	 * @param lifeSpan
	 */
	public void addMessage(InComingEventKeyImpl event, long lifeSpan) {
		if (lifeSpan > 0) {
			long currentTime = System.currentTimeMillis();
			long expiryTimeMillis = currentTime + (lifeSpan * 1000);
			Long expiryTime = new Long(expiryTimeMillis);

			// This is to avoid overwriting of existing keys. We advance the expiry time 
			// by 1 milli second in the case of a conflict.
			while (mTimeToEventMap.containsKey(expiryTime)) {
				expiryTime = expiryTime + 1;
			}

			// Add the event and expiry time to the maps.
			mEventToTimeMap.put(event, expiryTime);
			mTimeToEventMap.put(expiryTime, event);
		}
	}
	
	/**
	 * Removes the request event from the RequestLifeSpanHelper. Generally called when the request is
	 * consumed by a process instance.
	 * @param event
	 */
	public void removeMessage(InComingEventKeyImpl event) {
		Long expiryTime = mEventToTimeMap.remove(event);
		if (expiryTime != null) {
			mTimeToEventMap.remove(expiryTime);
		}
	}
	
	/**
	 * Gets the earliest expiry time. Returns 0 if there are no uncomsumed requests.
	 * @return
	 */
	public long getNextExpiryTime() {
		if (mTimeToEventMap.isEmpty()) {
			return 0;
		}
		return mTimeToEventMap.firstKey().longValue();
	}
	
	/**
	 * Removes the expired events from the helper maps and returns the list of expired request events.
	 * Returns null if there are no expired events.
	 */
	public List<InComingEventKeyImpl> purgeWaitingRequests() {
		long currentTime = System.currentTimeMillis();
		boolean continuePurge = true;
		List<InComingEventKeyImpl> expiredRequestList = null;

		while (continuePurge && (!mTimeToEventMap.isEmpty())) {
			Long expiryTime = mTimeToEventMap.firstKey();
			if (currentTime >= expiryTime.longValue()) {
				InComingEventKeyImpl event = mTimeToEventMap.remove(expiryTime);
				mEventToTimeMap.remove(event);
				if (expiredRequestList == null) {
					expiredRequestList = new ArrayList<InComingEventKeyImpl>();
				}
				expiredRequestList.add(event);
			} else {
				continuePurge = false;
			}
		}
		
		return expiredRequestList;
	}
}
