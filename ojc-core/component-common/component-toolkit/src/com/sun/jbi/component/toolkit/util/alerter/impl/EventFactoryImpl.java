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
 * @(#)EventFactoryImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.alerter.impl;

import com.sun.jbi.component.toolkit.util.alerter.Event;
import com.sun.jbi.component.toolkit.util.alerter.EventDataConverter;
import com.sun.jbi.component.toolkit.util.alerter.EventFactory;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent;

/**
 * Default implementation of {@link EventFactory}.
 * @author Kevan Simpson
 */
public class EventFactoryImpl implements EventFactory {

    /** @see com.sun.jbi.component.toolkit.util.alerter.EventFactory#createEvent() */
	public Event createEvent() {
		return new EventImpl();
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.EventFactory#createDataConverter() */
	public EventDataConverter createDataConverter() {
		return new EventDataConverterImpl(/*this*/);
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.EventFactory#createNotificationEvent() */
	public NotificationEvent createNotificationEvent() {
		return new NotificationEventImpl();
	}
}
