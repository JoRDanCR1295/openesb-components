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
 *
 * @(#)Event.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.alerter;


/**
 * An interface representing an event.
 *
 * @author bgebbie
 *
 */

public interface Event {

    /**
     * The RCS ID.
     */
    static String RCS_ID = "$Id: Event.java,v 1.2 2007/10/04 00:07:07 reuven_damir Exp $";

    //
    // Server Types
    //
    public static final String SERVER_TYPE_INTEGRATION = "INTEGRATION";
    public static final String SERVER_TYPE_MESSAGE = "MESSAGE";

    //
    // Component Types
    //
    public static final String COMPONENT_TYPE_COLLABORATION = "COLLABORATION";
    public static final String COMPONENT_TYPE_STCMS = "JMS";
    public static final String COMPONENT_TYPE_EWAY= "EWAY";
    public static final String COMPONENT_TYPE_BPEL = "BPEL";


    /**
     * The getter method for the physical host name.
     *
     * @return the physical host name
     * @hibernate.property
     */
    public String getPhysicalHostName();

    /**
     * The setter method for the physical host name.
     *
     * @param physicalHostName the name of the physical host
     */
    public void setPhysicalHostName(String physicalHostName);

	/**
	 * The getter method for the deployment name.
	 *
	 * @return the deployment name
	 *
	 * @hibernate.property
	 */
	public String getDeploymentName();

	/**
	 * The setter method for the deployment name.
	 *
	 * @param deploymentName the name of the deployment
	 */
	public void setDeploymentName(String deploymentName);

    /**
     * The getter method for the environment name.
     *
     * @return the environment name
     *
     * @hibernate.property
     */
    public String getEnvironmentName();

    /**
     * The setter method for the environment name.
     *
     * @param environmentName the name of the environment
     */
    public void setEnvironmentName(String environmentName);

    /**
     * The getter method for the logical host name.
     *
     * @return the logical host name
     *
     * @hibernate.property
     */
    public String getLogicalHostName();

    /**
     * The setter method for the logical host name.
     *
     * @param logicalHostName the name of the logical host
     */
    public void setLogicalHostName(String logicalHostName);

    /**
     * The getter method for the server type.
     *
     * @return the server type
     *
     * @hibernate.property
     */
    public String getServerType();

    /**
     * The setter method for the server type.
     *
     * @param serverType the type of the server
     */
    public void setServerType(String serverType);

    /**
     * The getter method for the server name.
     *
     * @return the server name
     *
     * @hibernate.property
     */
    public String getServerName();

    /**
     * The setter method for the server name.
     *
     * @param serverName the name of the server
     */
    public void setServerName(String serverName);

    /**
     * The getter method for the component type.
     *
     * @return the component type
     *
     * @hibernate.property
     */
    public String getComponentType();

    /**
     * The setter method for the component type.
     *
     * @param componentType the type of the component
     */
    public void setComponentType(String componentType);

    /**
     * The getter method for the component's project.
     *
     * @return the full path name of the project in which the component is in
     *
     * @hibernate.property
     */
    public String getComponentProjectPathName();

    /**
     * The setter method for the full path name of the project in which
     * the component is in.
     * Note the names of the project and its sub-projects, if any, are separated
     * by '/'.
     *
     * @param componentProjectPathName the project
     */
    public void setComponentProjectPathName(String componentProjectPathName);

    /**
     * The getter method for the component name.
     *
     * @return the component name
     *
     * @hibernate.property
     */
    public String getComponentName();

    /**
     * The setter method for the component name.
     *
     * @param componentName the name of the component
     */
    public void setComponentName(String componentName);

    /**
     * The getter method for the time stamp.
     *
     * @return the time stamp
     *
     * @hibernate.property
     */
    public long getTimeStamp();

    /**
     * The setter method for the time stamp.
     */
    public void setTimeStamp();

    /**
     * The setter method for the time stamp.
     *
     * @param timeStamp the time stamp
     */
    public void setTimeStamp(long timeStamp);
    
    /**
     * Compare two Event objects.
     *
     * @param o the other event to compare to
     *
     * @return <code>true</code> if they are equivalent and <code>false</code>
     * if they are not
     */
    public boolean equals(Object o);
   
    /**
     * Generate a hashcode for the event.
     *
     * @return the hashcode for the event
     */
    public int hashCode();

    /**
     * Generate a string representation of the event.
     *
     * @return the string representation of the event
     */
    public String toString();

    /**
     * The notification event id
     *
     * @return the notification event id
     *
     * @hibernate.id generator-class="hilo.long"
     */
    public long getId();

}
