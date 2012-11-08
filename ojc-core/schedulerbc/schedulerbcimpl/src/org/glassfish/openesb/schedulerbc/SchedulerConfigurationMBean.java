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
 * @(#)SchedulerConfigurationMBean.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.schedulerbc;

import com.sun.jbi.common.qos.config.AppVar.VarType;
import com.sun.jbi.common.qos.config.RuntimeConfigurationMBean;
import com.sun.jbi.component.toolkit.lifecycle.impl.AcceptPoller;

/**
 * Defines configurable properties of Scheduler.
 *
 * @author sunsoabi_edwong
 */
public interface SchedulerConfigurationMBean extends RuntimeConfigurationMBean {

    public static final String POLLER_COUNT_PROPERTY =
            AcceptPoller.POLLER_COUNT_PROPERTY;
    public static final Integer DEFAULT_POLLER_COUNT = Integer.valueOf(1);
    
    public static final String QUARTZ_THREAD_COUNT_PROPERTY =
            "QuartzThreadCount";                                        //NOI18N
    public static final Integer DEFAULT_QUARTZ_THREAD_COUNT =
            Integer.valueOf(10);

    public static final String USE_QUARTZ_RAM_JOBSTORE_PROPERTY =
            "UseQuartzRAMJobStore";                                     //NOI18N
    public static final Boolean DEFAULT_USE_QUARTZ_RAM_JOBSTORE =
            Boolean.TRUE;

    public static final String QUARTZ_PERSISTENT_JOBSTORE_JNDI_URL_PROPERTY =
            "QuartzPersistentJobStoreJNDIURL";                          //NOI18N
    public static final String DEFAULT_BAD_JNDI_URL =
            "<Must specify JNDI JDBC URL if not using RAM JobStore>";   //NOI18N
    
    public static final String CONFIGURATION_NAME =
            APPLICATION_CONFIGURATION_NAME;
    
    public static final String START_DATE = "startDate";                //NOI18N
    
    public static final String END_DATE = "endDate";                    //NOI18N
    
    public static final String TIME_ZONE = "timeZone";                  //NOI18N

    void setPollerCount(Integer count);
    Integer getPollerCount();
    
    void setQuartzThreadCount(Integer count);
    Integer getQuartzThreadCount();

    void setUseQuartzRAMJobStore(Boolean use);
    Boolean getUseQuartzRAMJobStore();

    void setQuartzPersistentJobStoreJNDIURL(String url);
    String getQuartzPersistentJobStoreJNDIURL();
    
    void setStartDate(String appConfigName, String startDate);
    String getStartDate(String appConfigName);
    
    void setEndDate(String appConfigName, String endDate);
    String getEndDate(String appConfigName);
    
    void setTimeZone(String appConfigName, String timeZone);
    String getTimeZone(String appConfigName);
    
    void setAppVar(String appVarName, String appVarValue);
    String getAppVar(String appVarName);
    VarType getAppVarType(String appVarName);
}    
