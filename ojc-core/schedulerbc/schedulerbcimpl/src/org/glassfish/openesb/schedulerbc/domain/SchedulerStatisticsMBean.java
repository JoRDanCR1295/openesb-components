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
 * @(#)SchedulerStatisticsMBean.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

/**
 * Describes the Scheduler Statistics MBean.
 * 
 * @author sunsoabi_edwong
 */
public interface SchedulerStatisticsMBean {
    
    public static final String STATISTICS_EXTENSION = "Statistics";     //NOI18N
    
    public static final String COMP_EXT_STATS_SCHED_NAME = "schedName"; //NOI18N
    public static final String COMP_EXT_STATS_SCHED_NAME_DESC =
            "Scheduler Name";
    public static final String COMP_EXT_STATS_STATE = "schedState";     //NOI18N
    public static final String COMP_EXT_STATS_STATE_DESC =
            "State";
    public static final String COMP_EXT_STATS_QUARTZ_API_VERSION =
            "quartzAPIVersion";                                         //NOI18N
    public static final String COMP_EXT_STATS_QUARTZ_API_VERSION_DESC =
            "Quartz API Version";
    public static final String COMP_EXT_STATS_THREAD_POOL_SIZE =
            "schedThreadPoolSize";                                      //NOI18N
    public static final String COMP_EXT_STATS_THREAD_POOL_SIZE_DESC =
            "Scheduler Thread Pool Size";
    public static final String COMP_EXT_STATS_NUM_JOBS_EXECUTED =
            "numJobsExecuted";                                          //NOI18N
    public static final String COMP_EXT_STATS_NUM_JOBS_EXECUTED_DESC =
            "Num Jobs Executed";
    public static final String COMP_EXT_STATS_ACTIVE_TRIGGERS =
            "activeTriggers";                                           //NOI18N
    public static final String COMP_EXT_STATS_ACTIVE_TRIGGERS_DESC =
            "Active Triggers";
    public static final String COMP_EXT_STATS_NEXT_TRIGGER =
            "nextTrigger";                                              //NOI18N
    public static final String COMP_EXT_STATS_NEXT_TRIGGER_DESC =
            "Next Trigger";
    
    String getQuartzAPIVersion();
    
    String getSchedulerName();
    
    Integer getSchedulerThreadPoolSize();
    
    String getSchedulerJobStoreJNDI();
    
    String getSchedulerState();

    Integer getSchedulerNumberJobsExecuted();
    
    String[] getSchedulerGroup_State();
    
    String[] getActiveStaticTriggers();

    String[] getActiveDynamicTriggers();
    
    String getLastUpdate();
    
    String getNextTrigger();

    String[] getSuspendedConsumerEndpoint_Date();
}
