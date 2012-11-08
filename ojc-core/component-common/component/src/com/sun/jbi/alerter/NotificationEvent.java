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
 * @(#)NotificationEvent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.alerter;


/**
 * This class represents a Notification Event
 *
 * @author bgebbie
 *
 */
public interface NotificationEvent
    extends Event {


    static String RCS_ID = "$Id: NotificationEvent.java,v 1.2 2007/10/04 00:07:08 reuven_damir Exp $";

    /////////////
    // Event Type
    /////////////

    /** constant to represent the Alert event type */
    public static final String EVENT_TYPE_ALERT = "Alert";

    /** constant to represent the TCA event type */
    public static final String EVENT_TYPE_TCA = "TCA";

    /** constant to represent the Event event type */
    public static final String EVENT_TYPE_EVENT = "Event";

    /////////////////
    // Severity Types
    /////////////////
    /** constant to represent the FATAL severity type */
    public static final int SEVERITY_TYPE_FATAL = 0;

    /** constant to represent the CRITICAL severity type */
    public static final int SEVERITY_TYPE_CRITICAL = 1;

    /** constant to represent the MAJOR severity type */
    public static final int SEVERITY_TYPE_MAJOR = 2;

    /** constant to represent the MINOR severity type */
    public static final int SEVERITY_TYPE_MINOR = 3;

    /** constant to represent the WARNING severity type */
    public static final int SEVERITY_TYPE_WARNING = 4;

    /** constant to represent the INFO severity type */
    public static final int SEVERITY_TYPE_INFO = 5;

    /////////////////////
    // Operational States
    /////////////////////

    public static final int OPERATIONAL_STATE_UNKNOWN = 0;
    public static final int OPERATIONAL_STATE_STARTING = 1;
    public static final int OPERATIONAL_STATE_STARTED = 2;
    public static final int OPERATIONAL_STATE_SUSPENDING = 3;
    public static final int OPERATIONAL_STATE_SUSPENDED = 4;
    public static final int OPERATIONAL_STATE_STOPPING = 5;
    public static final int OPERATIONAL_STATE_STOPPED = 6;
    public static final int OPERATIONAL_STATE_RUNNING = 7;
    public static final int OPERATIONAL_STATE_SHUTTINGDOWN = 8;
    public static final int OPERATIONAL_STATE_SHUTDOWN = 9;

    ///////////////////////
    // Observational States
    ///////////////////////

    public static final int OBSERVATIONAL_STATE_UNOBSERVED = 0;
    public static final int OBSERVATIONAL_STATE_OBSERVED = 1;
    public static final int OBSERVATIONAL_STATE_RESOLVED = 2;
    public static final int OBSERVATIONAL_STATE_DELETED = 3;  //you don't see this state because the event is deleted.


    /**
     * The notification event id
     *
     * @return the notification event id
     *
     * @hibernate.id generator-class="hilo.long"
     */
    public long getId();

    /**
     * Set the id.  This should never be called explicitly as persistance
     * mechanism will override this value.
     *
     * @param id the id
     */
    public void setId(long id);

    /**
     * The getter method for the notification event type.
     *
     * @return the event type:
     *      EVENT_TYPE_ALERT
     *      EVENT_TYPE_TCA
     *      EVENT_TYPE_EVENT
     *
     * @hibernate.property
     */
    public String getType();


    /**
     * The setter method for the notification event type.
     *
     * @param type the notification event type:
     *      EVENT_TYPE_ALERT
     *      EVENT_TYPE_TCA
     *      EVENT_TYPE_EVENT
     */
    public void setType(String type);


    /**
     * The getter method for the severity.
     *
     * @return the severity:
     *      SEVERITY_TYPE_CRITICAL
     *      SEVERITY_TYPE_MAJOR
     *      SEVERITY_TYPE_MINOR
     *      SEVERITY_TYPE_WARNING
     *      SEVERITY_TYPE_INFO
     *
     * @hibernate.property
     */
    public int getSeverity();


    /**
     * The setter method for the severity.
     *
     * @param severity the severity:
     *      SEVERITY_TYPE_CRITICAL
     *      SEVERITY_TYPE_MAJOR
     *      SEVERITY_TYPE_MINOR
     *      SEVERITY_TYPE_WARNING
     *      SEVERITY_TYPE_INFO
     */
    public void setSeverity(int severity);


    /**
     * The getter method for the details about a notification event.
     *
     * @return  the details about a notification event
     *
     * @hibernate.property
     */
    public String getMessageDetails();


    /**
     * The setter method for the details about a notification event.
     *
     * @param messageDetails the details about a notification event
     */
    public void setMessageDetails(String messageDetails);


    /**
     * The getter method for the operational state
     *
     * @return the operational state:
     *      OPERATIONAL_STATE_UNKNOWN
     *      OPERATIONAL_STATE_STARTING
     *      OPERATIONAL_STATE_SUSPENDING
     *      OPERATIONAL_STATE_SUSPENDED
     *      OPERATIONAL_STATE_STOPPING
     *      OPERATIONAL_STATE_STOPPED
     *      OPERATIONAL_STATE_RUNNING
     *
     * @hibernate.property
     */
    public int getOperationalState();


    /**
     * The setter method for the operational state.
     *
     * @param operationalState the operational state:
     *      OPERATIONAL_STATE_UNKNOWN
     *      OPERATIONAL_STATE_STARTING
     *      OPERATIONAL_STATE_SUSPENDING
     *      OPERATIONAL_STATE_SUSPENDED
     *      OPERATIONAL_STATE_STOPPING
     *      OPERATIONAL_STATE_STOPPED
     *      OPERATIONAL_STATE_RUNNING
     */
    public void setOperationalState(int operationalState);


    /**
     * The getter method to retrieve the observational state.
     *
     * @return the observational state:
     *      OBSERVATIONAL_STATE_UNOBSERVED
     *      OBSERVATIONAL_STATE_OBSERVED
     *      OBSERVATIONAL_STATE_RESOLVED
     *
     * @hibernate.property
     */
    public int getObservationalState();


    /**
     * Set the observational state.
     *
     * @param observationalState the observational state:
     *      OBSERVATIONAL_STATE_UNOBSERVED
     *      OBSERVATIONAL_STATE_OBSERVED
     *      OBSERVATIONAL_STATE_RESOLVED
     */
    public void setObservationalState(int observationalState);


    /**
     * The getter method for whether the listeners interested in the
     * notifications event have been notified.  Since a system failure may
     * occur during this process, all of the notification events will need
     * to be resent when the system is started up again.  This field keeps
     * track of this.  Possible values are:
     *      TRUE - All the listeners have been notified
     *      FALSE - All the listeners may have not been notified
     * Note that it is possible a listener may be called more then once
     * whenever all the listeners could not be notified in an attempt.
     *
     * @return <code>true</code> if all the listeners have been notified and
     * <code>false</code> if they may have not been notified
     *
     * @hibernate.property
     */
    public boolean getListenersNotified();


    /**
     * The setter method for whether the listeners interested in the
     * notification events have been notified.
     *
     * @param listenersNotified <code>true</code> if all the listeners have
     * been notified and <code>false</code> if they may have not been notified
     */
    public void setListenersNotified(boolean listenersNotified);


    /**
     * The getter method for the message code which is a unique integer that
     * is used to identify the root cause of the notification events as
     * specifically as possible.  It can be used by customer support to
     * easily look up common problems and solutions in a knowledge base.
     * It also helps track the cause of issues and helps duplicate issues
     * to be found.  This code will also be used to look up an i18n String
     * from a resource bundle which will have any message code arguments
     * applied to it.
     *
     * @return the message code
     *
     * @hibernate.property
     */
    public String getMessageCode();


    /**
     * The setter method for the message code.
     *
     * @param messageCode the message code
     */
    public void setMessageCode(String messageCode);

    /**
     * Convert a code into String representation
     *
     * @param severity the code
     * @return String representation
     */
    public String getSeverityString(int severity);
    
    /**
     * Convert a code into String representation
     *
     * @param operationalState the code
     * @return String representation
     */    
    public String getOpStateString(int operationalState);
    
    /**
     * Convert a code into String representation
     *
     * @param observationalState the code
     * @return String representation
     */    
    public String getObStateString(int observationalState);    
    
    /**
     * The getter method for the message code arguments.
     *
     * @return an array of Strings that represent the arguments to
     * the message template specified by the message code.  These strings
     * will not be internationalized.
     *
     * @hibernate.array role="messageCodeArgs" table="T_MSG_CODE_ARGS"
     * hibernate.collection-key column="id" generator-class="hilo"
     * @hibernate.collection-key column="id"
     * @hibernate.collection-index column="argIndex" type="integer"
     * @hibernate.collection-element column="arg" type="string"
     */
    public String[] getMessageCodeArgs();


    /**
     * The setter method for the message code arguments.
     *
     * @param messageCodeArgs the arguments to the message template specified
     * by the message code.  These strings will not be internationalized.
     */
    public void setMessageCodeArgs(String[] messageCodeArgs);


    public String createQuery(
        String notificationType,
        String environmentName,
        String logicalHostName,
        String serverType,
        String serverName,
        String componentType,
        String componentProjectPathName,
        String componentName,
        Integer observationalState);
        
	public String createQuery2(
	    String notificationType,
	    String environmentName,
	    String logicalHostName,
	    String serverType,
	    String serverName,
	    String componentType,
	    String componentProjectPathName,
	    String componentName,
	    Integer observationalState,
	    String dateFrom,
	    String dateTo,
	    Integer severity,
	    Integer operationalState,
	    String messageDetail);
	    
	public String createQuery2(
		String notificationType,
		String deploymentName,
		String environmentName,
		String logicalHostName,
		String serverType,
		String serverName,
		String componentType,
		String componentProjectPathName,
		String componentName,
		Integer observationalState,
		String dateFrom,
		String dateTo,
		Integer severity,
		Integer operationalState,
		String messageDetail);
     
}
