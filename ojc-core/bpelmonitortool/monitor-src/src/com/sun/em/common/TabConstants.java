/*
 * TabConstants.java
 * 
 * Created on Aug 1, 2007, 12:09:41 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.em.common;

//import com.sun.em.common.JMSConstants;

/**
 *
 * @author jlee
 */
public interface TabConstants {

    public static final String STATUS_TAB_URL = "/faces/framework/core/status.jsp";
    public static final String TOPICS_TAB_URL = "/faces/jms/destinationLayout.jsp?" + JMSConstants.DESTINATION_TYPE + "=" + JMSConstants.TOPIC;
    public static final String QUEUES_TAB_URL = "/faces/jms/destinationLayout.jsp?" + JMSConstants.DESTINATION_TYPE + "=" + JMSConstants.QUEUE;
    public static final String LOGGING_TAB_URL = "/faces/logging/loggingTab.jsp";
    public static final String ALERTS_TAB_URL = "/faces/alerts/alertsView.jsp";
    public static final String XA_ID_TAB_URL = "/faces/jms/xaidTab.jsp";
    public static final String JMS_SUMMARY_TAB_URL = "/faces/jms/summary.jsp";
    public static final String JMS_MESSAGES_TAB_URL = "/faces/jms/messageList.jsp";
    public static final String JMS_SUBSCRIBERS_TAB_URL = "/faces/jms/subscribers.jsp";
    
    public static final String STATUS_TAB_ID = "StatusTab";
    public static final String TOPICS_TAB_ID = "TopicsTab";
    public static final String QUEUES_TAB_ID = "QueuesTab";
    public static final String LOGGING_TAB_ID = "LoggingTab";
    public static final String ALERTS_TAB_ID = "AlertsTab";
    public static final String XA_ID_TAB_ID = "XAIDTab";
    
    public static final String JMS_SUMMARY_TAB_ID = "SummaryTab";
    public static final String JMS_MESSAGES_TAB_ID = "MessagesTab";
    public static final String JMS_SUBSCRIBERS_TAB_ID = "SubscribersTab";
    
}
