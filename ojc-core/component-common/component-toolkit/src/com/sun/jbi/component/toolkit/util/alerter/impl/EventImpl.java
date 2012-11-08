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
 * @(#)EventImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.alerter.impl;

import java.util.Date;

import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.util.alerter.Event;

/**
 * 
 * @author Kevan Simpson
 */
public class EventImpl implements Event {
	private String mCompName, mCompProjectPath, mDeploymentName, mEnvironmentName;
	private String mLogicalHost, mPhysicalHost, mServerName, mServerType;
	private ComponentType mCompType;
	private long mId, mTimestamp;
	
	/*					OBJECT METHODS								*/
	
	/** @see java.lang.Object#equals(java.lang.Object) */
	public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        else if (!(obj instanceof Event)) {
            return false;
        }
        
        final Event event = (Event) obj;
        // Put cheapest comparisons first followed by ones must likely to be false.
        return (getTimeStamp() == event.getTimeStamp() &&
                Util.equals(getComponentName(), event.getComponentName()) &&
                Util.equals(getServerName(), event.getServerName()) &&
                Util.equals(getLogicalHostName(), event.getLogicalHostName()) &&
                Util.equals(getEnvironmentName(), event.getEnvironmentName()) &&
                Util.equals(getPhysicalHostName(), event.getPhysicalHostName()) &&
                Util.equals(getServerType(), event.getServerType()) &&
                Util.equals(getComponentType(), event.getComponentType()) &&
                Util.equals(getComponentProjectPathName(), event.getComponentProjectPathName()));
	}

	/** @see java.lang.Object#hashCode() */
	public int hashCode() {
        int result = 29 * Util.hashCode(getPhysicalHostName());
        result = 29 * result + Util.hashCode(getLogicalHostName());
        result = 29 * result + Util.hashCode(getEnvironmentName());
        result = 29 * result + Util.hashCode(getServerType());
        result = 29 * result + Util.hashCode(getServerName());
        result = 29 * result + Util.hashCode(getComponentType());
        result = 29 * result + Util.hashCode(getComponentProjectPathName());
        result = 29 * result + Util.hashCode(getComponentName());
        result = 29 * result + (int) (getTimeStamp() ^ (getTimeStamp() >>> 32));

        return result;
	}

	/** @see java.lang.Object#toString() */
	public String toString() {
	    return super.toString();
	    // TODO implement
//	    StringBuffer buff = new StringBuffer();
//	    
//		return buff.toString();
	}
	
	/*              ACCESSORS + MUTATORS                            */

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getComponentName() */
	public String getComponentName() {
		return mCompName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getComponentProjectPathName() */
	public String getComponentProjectPathName() {
		return mCompProjectPath;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getComponentType() */
	public ComponentType getComponentType() {
		return mCompType;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getDeploymentName() */
	public String getDeploymentName() {
		return mDeploymentName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getEnvironmentName() */
	public String getEnvironmentName() {
		return mEnvironmentName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getId() */
	public long getId() {
		return mId;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getLogicalHostName() */
	public String getLogicalHostName() {
		return mLogicalHost;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getPhysicalHostName() */
	public String getPhysicalHostName() {
		return mPhysicalHost;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getServerName() */
	public String getServerName() {
		return mServerName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getServerType() */
	public String getServerType() {
		return mServerType;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#getTimeStamp() */
	public long getTimeStamp() {
		return mTimestamp;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setComponentName(java.lang.String) */
	public void setComponentName(String componentName) {
		mCompName = componentName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setComponentProjectPathName(java.lang.String) */
	public void setComponentProjectPathName(String componentProjectPathName) {
		mCompProjectPath = componentProjectPathName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setComponentType(com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType) */
	public void setComponentType(ComponentType componentType) {
		mCompType = componentType;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setDeploymentName(java.lang.String) */
	public void setDeploymentName(String deploymentName) {
		mDeploymentName = deploymentName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setEnvironmentName(java.lang.String) */
	public void setEnvironmentName(String environmentName) {
		mEnvironmentName = environmentName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setLogicalHostName(java.lang.String) */
	public void setLogicalHostName(String logicalHostName) {
		mLogicalHost = logicalHostName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setPhysicalHostName(java.lang.String) */
	public void setPhysicalHostName(String physicalHostName) {
		mPhysicalHost = physicalHostName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setServerName(java.lang.String) */
	public void setServerName(String serverName) {
		mServerName = serverName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setServerType(java.lang.String) */
	public void setServerType(String serverType) {
		mServerType = serverType;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setTimeStamp() */
	public void setTimeStamp() {
		setTimeStamp((new Date()).getTime());
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Event#setTimeStamp(long) */
	public void setTimeStamp(long timeStamp) {
		mTimestamp = timeStamp;
	}
}
