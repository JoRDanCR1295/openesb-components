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
 * @(#)AlerterFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.alerter;

import java.util.List;

import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import com.sun.jbi.component.toolkit.util.I18n;
import com.sun.jbi.component.toolkit.util.alerter.impl.AlerterImpl;

/**
 * Factory to create {@link Alerter} instances.
 * @author Kevan Simpson
 */
public class AlerterFactory {
    /**
     * EventForwarder MBean <code>ObjectName</code>
     */
    private static final String EVENTFORWARDER_MBEAN_NAME =
            "EventManagement:name=EventForwarderMBean";    

	public static Alerter newAlerter() {
		try {
			ObjectName objectName = new ObjectName(EVENTFORWARDER_MBEAN_NAME);
			List mbeanservers = MBeanServerFactory.findMBeanServer(null);
			if(mbeanservers.size() > 0) {
				// get default mbean server and register mBean
				MBeanServer server = (MBeanServer) mbeanservers.get(0);
//				server.registerMBean(null, objectName);
				return new AlerterImpl(server, objectName);
			}
		}
		catch (MalformedObjectNameException mone) {
			throw new IllegalStateException(I18n.loc(
					"COMPTK-6027: Invalid EventForwarder MBean name: {0}", 
					mone.getMessage()));
		}

		return null;
	}
}
