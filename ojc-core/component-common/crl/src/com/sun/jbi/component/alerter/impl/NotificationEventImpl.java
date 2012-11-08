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
 * @(#)NotificationEventImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.alerter.impl;

import com.sun.jbi.component.alerter.NotificationEvent;

/**
 * Default implementation of {@link NotificationEvent}. 
 * @author Kevan Simpson
 */
public class NotificationEventImpl extends EventImpl 
								   implements NotificationEvent {
	private String mMessage, mCode;
	private OperationalState mOpState;
	private Severity mSeverity;
	
	/** @see com.sun.jbi.component.alerter.NotificationEvent#getMessage() */
	public String getMessage() {
		return mMessage;
	}

	/** @see com.sun.jbi.component.alerter.NotificationEvent#getMessageCode() */
	public String getMessageCode() {
		return mCode;
	}

	/** @see com.sun.jbi.component.alerter.NotificationEvent#getOperationalState() */
	public OperationalState getOperationalState() {
		return mOpState;
	}

	/** @see com.sun.jbi.component.alerter.NotificationEvent#getSeverity() */
	public Severity getSeverity() {
		return mSeverity;
	}

	/** @see com.sun.jbi.component.alerter.NotificationEvent#setMessage(java.lang.String) */
	public void setMessage(String msg) {
		mMessage = msg;
	}

	/** @see com.sun.jbi.component.alerter.NotificationEvent#setMessageCode(java.lang.String) */
	public void setMessageCode(String code) {
		mCode = code;
	}

	/** @see com.sun.jbi.component.alerter.NotificationEvent#setOperationalState(com.sun.jbi.component.alerter.NotificationEvent.OperationalState) */
	public void setOperationalState(OperationalState state) {
		mOpState = state;
	}

	/** @see com.sun.jbi.component.alerter.NotificationEvent#setSeverity(com.sun.jbi.component.alerter.NotificationEvent.Severity) */
	public void setSeverity(Severity sev) {
		mSeverity = sev;
	}
}
