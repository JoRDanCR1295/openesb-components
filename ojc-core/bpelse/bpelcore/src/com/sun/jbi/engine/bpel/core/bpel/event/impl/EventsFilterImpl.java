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
 * @(#)EventsFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.util.Set;

import com.sun.jbi.engine.bpel.core.bpel.event.EventsFilter;


/**
 *
 * @author Sun Microsystems
 */
public class EventsFilterImpl implements EventsFilter {
	
	/* */
	private boolean filteringOff;
	
	/* */
	private Set<Long> mAllowedVariableIds;
	
	/* */
	private Set<Long> mAllowedActivityIds;
	
	/**
	 * 
	 * @param allowedVarIds
	 * @param allowedActIds
	 */
	public EventsFilterImpl(Set<Long> allowedActIds, Set<Long> allowedVarIds) {
		mAllowedVariableIds = allowedVarIds;
		mAllowedActivityIds = allowedActIds;
		if (allowedVarIds == null && allowedActIds == null) {
			filteringOff = true;
		} else {
			filteringOff = false;
		}
		
	}
	
	/**
	 * 
	 * @param variableId
	 * @return
	 */
	public boolean generateEventsForVariable(long variableId) {
		if (filteringOff) {
			return true;
		} else if (mAllowedVariableIds != null) {
			return mAllowedVariableIds.contains(new Long(variableId));
		} else {
			return false;
		}
	}
	
	/**
	 * 
	 * @param activityId
	 * @return
	 */
	public boolean generateEventsForActivity(long activityId) {
		if (filteringOff) {
			return true;
		} else if (mAllowedActivityIds != null) {
			return mAllowedActivityIds.contains(new Long(activityId));
		} else {
			return false;
		}
	}
}
