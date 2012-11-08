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
 * @(#)AspectSEEndpointConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.mbean;

import java.io.Serializable;

import javax.management.AttributeChangeNotification;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanNotificationInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.StandardMBean;

/**
 * @author Sujit Biswas
 *
 */
public class AspectSEEndpointConfiguration extends StandardMBean implements
		NotificationEmitter, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected NotificationBroadcasterSupport notificationBroadcaster = new NotificationBroadcasterSupport();

	protected String configFile;

	protected String mConfigurationDisplaySchema;

	protected String mConfigurationDisplayData;

	

	
	
	public AspectSEEndpointConfiguration(Class mbeanInterface) throws NotCompliantMBeanException {
		super(mbeanInterface);

	}



	public MBeanNotificationInfo[] getNotificationInfo() {
		return new MBeanNotificationInfo[] { new MBeanNotificationInfo(
				new String[] { AttributeChangeNotification.ATTRIBUTE_CHANGE },
				AttributeChangeNotification.class.getName(),
				this.getClass().getName()+ "-Attribute_changed") };
	}

	public void addNotificationListener(NotificationListener listener,
			NotificationFilter filter, Object handback) {
		notificationBroadcaster.addNotificationListener(listener, filter,
				handback);
	}

	public void removeNotificationListener(NotificationListener listener)
			throws ListenerNotFoundException {
		notificationBroadcaster.removeNotificationListener(listener);
	}

	public void removeNotificationListener(NotificationListener listener,
			NotificationFilter filter, Object handback)
			throws ListenerNotFoundException {
		notificationBroadcaster.removeNotificationListener(listener, filter,
				handback);
	}

}
