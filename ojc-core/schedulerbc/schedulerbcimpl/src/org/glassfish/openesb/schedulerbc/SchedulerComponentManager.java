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
 * @(#)SchedulerComponentManager.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.schedulerbc;


import com.sun.jbi.common.util.Util;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
import java.util.Properties;
import javax.jbi.component.ServiceUnitManager;
import org.glassfish.openesb.schedulerbc.domain.SchedulerAdministration;
import org.glassfish.openesb.schedulerbc.domain.SchedulerAdministrationMBean;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants;
import org.glassfish.openesb.schedulerbc.domain.SchedulerStatistics;
import org.glassfish.openesb.schedulerbc.domain.SchedulerStatisticsMBean;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.impl.StdSchedulerFactory;
import org.quartz.impl.jdbcjobstore.JobStoreTX;
import org.quartz.impl.jdbcjobstore.StdJDBCDelegate;
import org.quartz.simpl.RAMJobStore;
import org.quartz.simpl.SimpleThreadPool;

/**
 * Scheduler service engine implementation.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerComponentManager extends AbstractComponentManager
        implements SchedulerConstants {
    
    private SchedulerConfiguration mSchedulerConfiguration;
    private SchedulerStatistics mSchedulerStatistics;
    private SchedulerAdministration mSchedulerAdministration;
    private Scheduler mScheduler;
    
    public static final String ENDPOINT_INFO_PROP = "EndpointInfo";     //NOI18N
    
    public static final String SCHED_TRIGGER_DETAILS_PROP =
            "SchedTriggerDetails";                                      //NOI18N
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.impl
     * .AbstractComponentManager#init(javax.jbi.component.ComponentContext)
     */
    @Override
    public void init(ComponentContext ctx) throws JBIException {
        super.init(ctx);

        if (null == mScheduler) {
            try {
                mScheduler = createScheduler();
            } catch (SchedulerException se) {
                String errMsg = I18n.severe(log(), "SCHEDBC-7001: "     //NOI18N
                        + "Cannot create Quartz Scheduler {0}",         //NOI18N
                        se, COMPONENT_NAME);
                throw new JBIException(errMsg, se);
            }
        }
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#start() */
    @Override
    public void start() throws JBIException {
        I18n.info(log(), "SCHEDBC-5001: "                               //NOI18N
                + "Start Scheduler consumer endpoints...");             //NOI18N

        // starts NMR polling threads
        super.start();

        String schedName = null;
        if (mScheduler != null) {
            try {
                schedName = mScheduler.getSchedulerName();
                mScheduler.start();
                I18n.info(log(), "SCHEDBC-5005: Quartz Scheduler "      //NOI18N
                        + "{0} has been started/resumed", schedName);   //NOI18N
            } catch (SchedulerException se) {
                I18n.warning(log(), "SCHEDBC-6004: Cannot start/resume "//NOI18N
                        + "Quartz Scheduler {0}", se, schedName);       //NOI18N
                throw new JBIException(se);
            }
        }
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#stop() */
    @Override
    public void stop() throws JBIException {
        // stop NMR polling threads
        super.stop();

        String schedName = null;
        if (mScheduler != null) {
            try {
                schedName = mScheduler.getSchedulerName();
                if (!mScheduler.isInStandbyMode()) {
                    mScheduler.standby();
                    I18n.info(log(), "SCHEDBC-5007: Quartz Scheduler "  //NOI18N
                            + "{0} has been suspended", schedName);     //NOI18N
                }
            } catch (SchedulerException se) {
                I18n.warning(log(), "SCHEDBC-6005: Cannot suspend "     //NOI18N
                        + "Quartz Scheduler {0}", schedName);           //NOI18N
            }
        }
        
        I18n.info(log(), "SCHEDBC-5002: "                               //NOI18N
                + "Stopped Scheduler consumer endpoints");              //NOI18N
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#shutDown() */
    @Override
    public void shutDown() throws JBIException {
        I18n.info(log(), "SCHEDBC-5003: "                               //NOI18N
                + "Shut down Scheduler consumer endpoints...");         //NOI18N

        String schedName = null;
        if (mScheduler != null) {
            try {
                schedName = mScheduler.getSchedulerName();
                deleteStaticJobs();

                if (!mScheduler.isShutdown()) {
                    mScheduler.shutdown();
                    I18n.info(log(), "SCHEDBC-5006: Quartz Scheduler "  //NOI18N
                            + "{0} has been shut down", schedName);     //NOI18N
                }
                mScheduler = null;
            } catch (SchedulerException ex) {
                I18n.warning(log(), "SCHEDBC-6003: Cannot shutdown "    //NOI18N
                        + "Quartz Scheduler {0}", ex, schedName);       //NOI18N
                throw new JBIException(ex);
            }
        }

        super.shutDown();
    }

    @Override
    protected ServiceUnitManager initServiceUnitManager() {
        return new SchedulerServiceUnitManager(getManagerContext(), this);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        return new SchedulerExchangeHandler(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new SchedulerEndpointManager(this, getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    @Override
    protected Object initRuntimeMBean() throws JBIException {
        mSchedulerConfiguration = new SchedulerConfiguration(
                getComponentContext(), getConfig());
        
        mSchedulerStatistics = new SchedulerStatistics(this);
        getCustomMBeans().put(SchedulerStatisticsMBean.STATISTICS_EXTENSION,
                mSchedulerStatistics);

        mSchedulerAdministration = new SchedulerAdministration(this);
        getCustomMBeans().put(
                SchedulerAdministrationMBean.ADMINISTRATION_EXTENSION,
                mSchedulerAdministration);
        
        return mSchedulerConfiguration;
    }

    // typed utility accessors
    public SchedulerEndpointManager getEndptMgr() {
        return (SchedulerEndpointManager) getManagerContext()
                .getEndpointManager();
    }

    public SchedulerExchangeHandler getHandler() {
        return (SchedulerExchangeHandler) getManagerContext()
                .getExchangeHandler();
    }
    
    public SchedulerConfiguration getSchedulerConfiguration() {
        return mSchedulerConfiguration;
    }

    protected Properties getRAMJobStoreProperties() {
        Properties schedProps = new Properties();
        schedProps.setProperty(PROP_SCHED_INSTANCE_NAME, COMPONENT_NAME);
        schedProps.setProperty(PROP_JOB_STORE_CLASS, RAMJobStore.class.getName());
        schedProps.setProperty(PROP_THREAD_POOL_CLASS,
                SimpleThreadPool.class.getName());
        schedProps.setProperty(PROP_THREAD_POOL_THREAD_COUNT,
                getSchedulerConfiguration().getQuartzThreadCount().toString());
        schedProps.setProperty(PROP_THREAD_POOL_THREAD_PRIORITY,
                Integer.toString(Thread.NORM_PRIORITY));
        schedProps.setProperty(PROP_SCHED_JOB_FACTORY_CLASS,
                SchedulerJobFactory.class.getName());

        I18n.config(log(), "SCHEDBC-4023: Using Quartz RAM JobStore");  //NOI18N
        
        return schedProps;
    }

    private String dataSourceKey(String key) {
        return new StringBuilder(PROP_DATASOURCE_PREFIX).append('.')    //NOI18N
                .append(SCHEDBC_DSNAME).append('.').append(key)         //NOI18N
                .toString();
    }

    protected Properties getJobStoreTXProperties() {
        Properties schedProps = new Properties();
        schedProps.setProperty(PROP_SCHED_INSTANCE_NAME, COMPONENT_NAME);
        if (isClustered()) {
            schedProps.setProperty(PROP_JOB_STORE_IS_CLUSTERED,
                    Boolean.TRUE.toString());
            schedProps.setProperty(PROP_SCHED_INSTANCE_ID,
                    AUTO_GENERATE_INSTANCE_ID);
        }
        schedProps.setProperty(PROP_JOB_STORE_CLASS,
                JobStoreTX.class.getName());
        schedProps.setProperty(PROP_THREAD_POOL_CLASS,
                SimpleThreadPool.class.getName());
        schedProps.setProperty(PROP_THREAD_POOL_THREAD_COUNT,
                getSchedulerConfiguration().getQuartzThreadCount().toString());
        schedProps.setProperty(PROP_THREAD_POOL_THREAD_PRIORITY,
                Integer.toString(Thread.NORM_PRIORITY));
        schedProps.setProperty(PROP_SCHED_JOB_FACTORY_CLASS,
                SchedulerJobFactory.class.getName());

        String jdbcDelegate = System.getProperty(
                PROP_JOB_STORE_DRIVER_DELEGATE_CLASS,
                StdJDBCDelegate.class.getName());
        schedProps.setProperty(PROP_JOB_STORE_DRIVER_DELEGATE_CLASS,
                jdbcDelegate);
        schedProps.setProperty(PROP_JOB_STORE_USE_PROPS,
                Boolean.TRUE.toString());
        schedProps.setProperty(PROP_JOB_STORE_DATASOURCE, SCHEDBC_DSNAME);
        schedProps.setProperty(dataSourceKey(PROP_DATASOURCE_JNDI_URL),
                getSchedulerConfiguration().getQuartzPersistentJobStoreJNDIURL());

        if (I18n.configLoggable(log())) {
            I18n.config(log(), "SCHEDBC-4022: Using Quartz "            //NOI18N
                    + "Self-managed JDBC JobStore at JNDI URL [{0}] "   //NOI18N
                    + "with Driver delegate [{1}], in {2}Clustered "    //NOI18N
                    + "mode", getSchedulerConfiguration()               //NOI18N
                        .getQuartzPersistentJobStoreJNDIURL(), jdbcDelegate,
                    (isClustered() ? "" : "Non-"));                     //NOI18N
        }

        return schedProps;
    }
    
    protected Scheduler createScheduler()
            throws SchedulerException {
        Properties schedProps = null;
        if (getSchedulerConfiguration().getUseQuartzRAMJobStore()
                .booleanValue()) {
            // Comment out for GFESBv22: clustering will still not be supported
            //if (!isClustered()) {
                schedProps = getRAMJobStoreProperties();
//            } else {
//                String errMsg = I18n.severe(log(), "SCHEDBC-7006: "     //NOI18N
//                        + "Cannot use Quartz RAM JobStore when "        //NOI18N
//                        + "GlassFish Clustering is in effect");         //NOI18N
//                throw new SchedulerException(errMsg);
//            }
        } else {
            String jndiUrl = getSchedulerConfiguration()
                    .getQuartzPersistentJobStoreJNDIURL();
            if (Util.isEmpty(jndiUrl)) {
                String errMsg = I18n.severe(log(), "SCHEDBC-7005: "     //NOI18N
                        + "JNDI JDBC URL to persistent JobStore "       //NOI18N
                        + "database not configured!");                  //NOI18N

                throw new SchedulerException(errMsg);
            }
            schedProps = getJobStoreTXProperties();
        }
        StdSchedulerFactory stdSchedFac = new StdSchedulerFactory(schedProps);
        Scheduler scheduler = stdSchedFac.getScheduler();

        I18n.config(log(), "SCHEDBC-4020: Created Quartz Scheduler "    //NOI18N
                + "{0} with {1} worker threads",                        //NOI18N
                COMPONENT_NAME,
                getSchedulerConfiguration().getQuartzThreadCount());
        
        return scheduler;
    }

    public boolean isClustered() {
        return Boolean.getBoolean(JBI_IS_CLUSTERED_SYS_PROP);
    }

    public Scheduler getScheduler() {
        return mScheduler;
    }

    private void deleteStaticJobs() throws SchedulerException {
        if (null == mScheduler) {
            return;
        }

        String[] groupNames = mScheduler.getJobGroupNames();
        for (String groupName : groupNames) {
            String[] jobNames = mScheduler.getJobNames(groupName);
            for (String jobName : jobNames) {
                if (jobName.endsWith(STATIC_JOB_SUFFIX)) {
                    Trigger trig = null;
                    if (I18n.finerLoggable(log())) {
                        Trigger[] trigs =
                                mScheduler.getTriggersOfJob(jobName, groupName);
                        if ((trigs != null) && (trigs.length > 0)) {
                            trig = trigs[0];
                        }
                    }

                    mScheduler.deleteJob(jobName, groupName);

                    if (trig != null) {
                        I18n.finer(log(), "SCHEDBC-2008: {0} trigger "  //NOI18N
                                + "{1} and associated job deleted "     //NOI18N
                                + "as Scheduler is being shut down",    //NOI18N
                                SchedAttrValue.STATIC.getDisplay(),
                                trig.getFullName());
                    }
                }
            }
        }
    }
}
