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
 * @(#)SchedulerStatistics.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import com.sun.jbi.common.util.Util;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;
import org.glassfish.openesb.schedulerbc.I18n;
import org.glassfish.openesb.schedulerbc.SchedulerComponentManager;
import org.glassfish.openesb.schedulerbc.SchedulerConfiguration;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerMetaData;
import org.quartz.Trigger;
import org.quartz.core.QuartzScheduler;

/**
 * Implements Scheduler Statistics MBean.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerStatistics implements SchedulerStatisticsMBean,
        SchedulerConstants {
    
    private SchedulerComponentManager schedCompMgr;
    private Logger logger;
    
    private static final String EMPTY_STR = "";                         //NOI18N
    private static final String SHUTDOWN_STR = "SHUTDOWN";              //NOI18N
    private static final String SUSPENDED_STR = "SUSPENDED";            //NOI18N
    private static final String RUNNING_STR = "RUNNING";                //NOI18N

    private static String QUARTZ_API_VERSION = null;
    
    public SchedulerStatistics(SchedulerComponentManager schedCompMgr) {
        super();
        
        this.schedCompMgr = schedCompMgr;
    }

    private Logger log() {
        // try init component logger
        if (null == logger) {
            logger = Util.getLogger(schedCompMgr.getManagerContext()
                            .getComponentContext(), getClass().getName());
        }

        return logger;
    }

    private String readQuartzAPIVersion() {
        // This needs to be implemented here because Quartz is doing a plain
        // getResourceAsStream on "/build.properties" and the ClassLoader
        // is obviously finding the wrong one (one from GlassFish) and thus
        // Quartz version properties are not being read
        if (null == QUARTZ_API_VERSION) {
            Class schedulerClass = QuartzScheduler.class;
            String schedulerClassname = "/" + schedulerClass.getName()  //NOI18N
                    .replace('.', '/') + ".class";                      //NOI18N
            URL schedulerClassnameURL =
                    schedulerClass.getResource(schedulerClassname);
            if (schedulerClassnameURL != null) {
                String buildPropertiesUrl = schedulerClassnameURL.toString();
                int bang = buildPropertiesUrl.lastIndexOf('!');         //NOI18N
                if (bang != -1) {
                    buildPropertiesUrl = buildPropertiesUrl.substring(0,
                            bang + 1) + "/build.properties";            //NOI18N
                    try {
                        URL buildPropertiesURL = new URL(buildPropertiesUrl);
                        InputStream is = buildPropertiesURL.openStream();
                        Properties buildProperties = new Properties();
                        buildProperties.load(is);
                        QUARTZ_API_VERSION = (new StringBuilder())
                                .append(buildProperties.getProperty(
                                        "version.major")).append(".")   //NOI18N
                                .append(buildProperties.getProperty(
                                        "version.minor")).append(".")   //NOI18N
                                .append(buildProperties.getProperty(
                                        "version.iter")).toString();    //NOI18N
                    } catch (Exception ex) {
                        I18n.warning(log(), "SCHEDBC-6010: "            //NOI18N
                                + "Got SchedulerException in {0}: ", ex,//NOI18N
                                "getQuartzAPIVersion");                 //NOI18N
                        QUARTZ_API_VERSION = "UNKNOWN";                 //NOI18N
                    }
                }
            }
        }
        return QUARTZ_API_VERSION;
    }
    
    private List<Trigger> getActiveTriggersAsList(Scheduler sched) {
        try {
            String[] groups = sched.getTriggerGroupNames();
            if ((groups != null) && (groups.length > 0)) {
                List<Trigger> activeTriggers = new ArrayList<Trigger>();
                for (String group : groups) {
                    String[] names = sched.getTriggerNames(group);
                    for (String name : names) {
                        int state = sched.getTriggerState(name, group);
                        if (!((Trigger.STATE_COMPLETE == state)
                                || (Trigger.STATE_ERROR == state)
                                || (Trigger.STATE_NONE == state)
                                || (Trigger.STATE_PAUSED == state))) {
                            activeTriggers.add(sched.getTrigger(name, group));
                        }
                    }
                }
                return activeTriggers;
            }
        } catch (SchedulerException ex) {
            I18n.warning(log(), "SCHEDBC-6010: "                        //NOI18N
                    + "Got SchedulerException in {0}: ", ex,            //NOI18N
                    "getActiveTriggersAsList");                         //NOI18N
        }
        return Collections.EMPTY_LIST;
    }

    private Object[] getNextFiredTrigger(Date earliestDate,
            List<Trigger> triggers) {
        Trigger earliestTrigger = null;
        for (Trigger trig : triggers) {
            Date nextFireAfter = trig.getNextFireTime();
            if (nextFireAfter != null) {
                if ((null == earliestDate)
                        || (earliestDate.compareTo(nextFireAfter) > 0)) {
                    earliestDate = nextFireAfter;
                    earliestTrigger = trig;
                }
            }
        }
        return new Object[] {earliestDate, earliestTrigger};
    }
    
    public String getQuartzAPIVersion() {
        return readQuartzAPIVersion();
    }

    public Integer getSchedulerThreadPoolSize() {
        Scheduler scheduler = schedCompMgr.getScheduler();
        if (scheduler != null) {
            try {
                return Integer.valueOf(scheduler.getMetaData()
                        .getThreadPoolSize());
            } catch (SchedulerException ex) {
                I18n.warning(log(), "SCHEDBC-6010: "                    //NOI18N
                        + "Got SchedulerException in {0}: ", ex,        //NOI18N
                        "getSchedulerThreadPoolSize");                  //NOI18N
            }
        }
        return Integer.valueOf(0);
    }

    private SchedAttrValue getTriggerMode(Trigger trig) {
        return (trig.getJobName().endsWith(STATIC_JOB_SUFFIX)
                ? SchedAttrValue.STATIC : SchedAttrValue.DYNAMIC);
    }
    
    private String[] getActiveTriggers(SchedAttrValue mode) {
        Scheduler scheduler = schedCompMgr.getScheduler();
        List<String> infos = new ArrayList<String>();
        if (scheduler != null) {
            List<Trigger> trigs = getActiveTriggersAsList(scheduler);
            for (Trigger trig : trigs) {
                if (mode.equals(getTriggerMode(trig))) {
                    infos.add(trig.getFullName());
                }
            }
        }
        return infos.toArray(new String[0]);
    }

    public String[] getActiveStaticTriggers() {
        return getActiveTriggers(SchedAttrValue.STATIC);
    }

    public String[] getActiveDynamicTriggers() {
        return getActiveTriggers(SchedAttrValue.DYNAMIC);
    }

    public String getLastUpdate() {
        return XSD_STD_SDF.format(new Date());
    }
    
    public String getNextTrigger() {
        Scheduler scheduler = schedCompMgr.getScheduler();
        Date earliestDate = null;
        if (scheduler != null) {
            Object[] results = getNextFiredTrigger(earliestDate,
                    getActiveTriggersAsList(scheduler));
            Date foundDate = (Date) results[0];
            Trigger foundTrigger = (Trigger) results[1];
            if ((foundDate != null) && (foundTrigger != null)) {
                return (new StringBuilder())
                    .append(foundTrigger.getFullName()).append(" @ ")   //NOI18N
                    .append(XSD_STD_SDF.format(foundDate)).toString();
            }
        }
        return EMPTY_STR;
    }

    public String getSchedulerName() {
        Scheduler scheduler = schedCompMgr.getScheduler();
        if (scheduler != null) {
            try {
                return scheduler.getSchedulerName();
            } catch (SchedulerException ex) {
                I18n.warning(log(), "SCHEDBC-6010: "                    //NOI18N
                        + "Got SchedulerException in {0}: ", ex,        //NOI18N
                        "getSchedulerName");                            //NOI18N
            }
        }
        return EMPTY_STR;
    }

    public String getSchedulerJobStoreJNDI() {
        SchedulerConfiguration schedConfig = schedCompMgr
                .getSchedulerConfiguration();
        if (schedConfig != null) {
            if (schedConfig.getUseQuartzRAMJobStore()) {
                return "N/A, using RAMJobStore";                        //NOI18N
            } else {
                return schedConfig.getQuartzPersistentJobStoreJNDIURL();
            }
        }
        return EMPTY_STR;
    }

    public String getSchedulerState() {
        Scheduler scheduler = schedCompMgr.getScheduler();
        if (scheduler != null) {
            try {
                SchedulerMetaData smd = scheduler.getMetaData();
                if (smd.isShutdown()) {
                    return SHUTDOWN_STR;
                } else if (smd.isInStandbyMode()) {
                    return SUSPENDED_STR;
                } else if (smd.isStarted()) {
                    return RUNNING_STR;
                }
            } catch (SchedulerException ex) {
                I18n.warning(log(), "SCHEDBC-6010: "                    //NOI18N
                        + "Got SchedulerException in {0}: ", ex,        //NOI18N
                        "getSchedulerState");                           //NOI18N
            }
        }
        return EMPTY_STR;
    }

    public Integer getSchedulerNumberJobsExecuted() {
        Scheduler scheduler = schedCompMgr.getScheduler();
        if (scheduler != null) {
            try {
                return Integer.valueOf(
                        scheduler.getMetaData().numJobsExecuted());
            } catch (SchedulerException ex) {
                I18n.warning(log(), "SCHEDBC-6010: "                    //NOI18N
                        + "Got SchedulerException in {0}: ", ex,        //NOI18N
                        "getNumberExecutedJobs");                       //NOI18N
            }
        }
        return Integer.valueOf(0);
    }

    public String[] getSchedulerGroup_State() {
        Scheduler scheduler = schedCompMgr.getScheduler();
        List<String> infos = new ArrayList<String>();
        if (scheduler != null) {
            String before = null;
            try {
                before = "getPausedTriggerGroups";                      //NOI18N
                Set<String> pausedGroups = scheduler.getPausedTriggerGroups();
                before = "getTriggerGroupNames";                        //NOI18N
                String[] groupNames = scheduler.getTriggerGroupNames();
                for (String groupName : groupNames) {
                    infos.add(new StringBuilder(groupName).append(": ") //NOI18N
                            .append(pausedGroups.contains(groupName)
                                    ? SUSPENDED_STR : RUNNING_STR).toString());
                }
            } catch (SchedulerException ex) {
                I18n.warning(log(), "SCHEDBC-6010: "                    //NOI18N
                        + "Got SchedulerException in {0}: ", ex,        //NOI18N
                        before);
            }
        }
        return infos.toArray(new String[0]);
    }

    public String[] getSuspendedConsumerEndpoint_Date() {
        Map<String, Date> suspendedEPs = schedCompMgr.getEndptMgr()
                .getSuspendedEndpointsMap();
        List<String> results = new ArrayList<String>();
        for (Map.Entry<String, Date> me : suspendedEPs.entrySet()) {
            results.add(me.getKey() + " @ "                             //NOI18N
                    + XSD_STD_SDF.format(me.getValue()));
        }
        return results.toArray(new String[0]);
    }
}
