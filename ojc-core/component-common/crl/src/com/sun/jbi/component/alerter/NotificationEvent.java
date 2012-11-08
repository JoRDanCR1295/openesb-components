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
 * @(#)NotificationEvent.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.alerter;

/**
 * Defines a notification event.
 * @author Kevan Simpson
 */
public interface NotificationEvent extends Event {
	/** Severity enum. */
	public enum Severity { FATAL, CRITICAL, MAJOR, MINOR, WARNING, INFO }
	/** Operational state enum. */
	public enum OperationalState { UNKNOWN, STARTING, STARTED, SUSPENDING, SUSPENDED, 
	                               STOPPING, STOPPED, RUNNING, SHUTTING_DOWN, SHUTDOWN }

	public String getMessage();
	public void setMessage(String msg);

	public String getMessageCode();
	public void setMessageCode(String code);

	public OperationalState getOperationalState();
	public void setOperationalState(OperationalState state);
	
	public Severity getSeverity();
	public void setSeverity(Severity sev);
}
