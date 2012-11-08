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
 * @(#)Event.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.alerter;

import javax.jbi.component.ComponentContext;

/**
 * Defines interface for an Alert event. 
 * @author Kevan Simpson
 */
public interface Event {
	/* Event type enum IS NOT USED BECAUSE ONLY ALERTs ARE SUPPORTED FOR NOW. */
//	public enum EventType { Alert, TCA, Event }

	/** Component type enum. */
	public enum ComponentType { BindingComponent, ServiceEngine; 
		public static ComponentType valueOf(ComponentContext ctx) {
			if (ctx != null) {
				String name = ctx.getComponentName().toLowerCase();
				if (name.endsWith("engine")) return ServiceEngine;
				if (name.endsWith("binding")) return BindingComponent;
			}
			
			return null;
		}
	}

	/**
	 * Returns the physical host name. 
	 * @return the physical host name.
	 */
    public String getPhysicalHostName();

    /**
     * Sets the physical host name. 
     * @param physicalHostName The name of the physical host.
     */
    public void setPhysicalHostName(String physicalHostName);

	/**
	 * Returns the deployment name. 
	 * @return the deployment name.
	 */
	public String getDeploymentName();

    /**
     * Sets the deployment name. 
     * @param deploymentName The name of the deployed service unit.
     */
	public void setDeploymentName(String deploymentName);

	/**
	 * Returns the environment name. 
	 * @return the environment name.
	 */
    public String getEnvironmentName();

    /**
     * Sets the environment name. 
     * @param environmentName The name of the environment.
     */
    public void setEnvironmentName(String environmentName);

	/**
	 * Returns the logical host name. 
	 * @return the logical host name.
	 */
    public String getLogicalHostName();

    /**
     * Sets the logical host name. 
     * @param logicalHostName The name of the logical host.
     */
    public void setLogicalHostName(String logicalHostName);

	/**
	 * Returns the server type. 
	 * @return the server type.
	 */
    public String getServerType();

    /**
     * Sets the server type. 
     * @param serverType The server type.
     */
    public void setServerType(String serverType);

	/**
	 * Returns the server name. 
	 * @return the server name.
	 */
    public String getServerName();

    /**
     * Sets the server name. 
     * @param serverName The server name.
     */
    public void setServerName(String serverName);

	/**
	 * Returns the component type. 
	 * @return the component type.
	 */
    public ComponentType getComponentType();

    /**
     * Sets the component type. 
     * @param componentType The type of the component.
     */
    public void setComponentType(ComponentType componentType);

	/**
	 * Returns the component project path. 
	 * @return the component project path.
	 */
    public String getComponentProjectPathName();

    /**
     * Sets the component project path. 
     * @param componentProjectPathName The component project path.
     */
    public void setComponentProjectPathName(String componentProjectPathName);

    /**
     * Returns the component name.
     * @return the component name
     */
    public String getComponentName();

    /**
     * Sets the component name. 
     * @param componentName The component name.
     */
    public void setComponentName(String componentName);

    /**
     * Returns the time stamp.
     * @return the time stamp
     */
    public long getTimeStamp();

    /**
     * Sets the timestamp with the current time.
     */
    public void setTimeStamp();

    /**
     * Sets the timestamp of this event. 
     * @param timeStamp The timestamp of this event.
     */
    public void setTimeStamp(long timeStamp);

    /**
     * Returns the unique id of this event.
     * @return the unique id of this event.
     */
    public long getId();
}
