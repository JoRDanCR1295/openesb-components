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
 * @(#)SchedulerSEComponentManager.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scheduler;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import java.util.Map;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.engine.scheduler.domain.SchedulerEndPoint;
import java.util.Hashtable;
import javax.jbi.component.ServiceUnitManager;
import org.quartz.Job;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.DirectSchedulerFactory;
import org.quartz.simpl.RAMJobStore;
import org.quartz.simpl.SimpleThreadPool;
import org.quartz.spi.JobFactory;
import org.quartz.spi.JobStore;
import org.quartz.spi.TriggerFiredBundle;

/**
 * Scheduler service engine implementation.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerSEComponentManager extends AbstractComponentManager {
    
    private SchedulerSEConfiguration mSchedulerConfiguration;
    private Map<String, Scheduler> mSchedulers =
            new Hashtable<String, Scheduler>();
    
    public static final String ENDPOINT_INFO_PROP = "EndpointInfo";     //NOI18N
    
    public static final String SCHED_TRIGGER_PROP = "SchedTrigger";     //NOI18N
    
    private static final String INSTANCE_ID_SUFFIX = "sched-instance";  //NOI18N
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.impl
     * .AbstractComponentManager#init(javax.jbi.component.ComponentContext)
     */
    @Override
    public void init(ComponentContext ctx) throws JBIException {
        super.init(ctx);

        // TODO start provisioning of Scheduler endpts
        if (log().isLoggable(Level.FINE)) {
            log().fine("SCHEDSE-3001: Initializing Scheduler configuration database...");
        }
        // TODO insert Amit's code to init db

        if (log().isLoggable(Level.FINE)) {
            log().fine("SCHEDSE-3002: Start provisioning Scheduler service endpoints...");
        }
    // TODO start provisioning Scheduler endpts
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#start() */
    @Override
    public void start() throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("SCHEDSE-3003: Start Scheduler service endpoints consuming...");
        }
        // TODO start consuming of Scheduler endpts; do they consume?

        // starts NMR polling threads
        super.start();
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#stop() */
    @Override
    public void stop() throws JBIException {
        // stop NMR polling threads
        super.stop();

        if (log().isLoggable(Level.FINE)) {
            log().fine("SCHEDSE-3004: Stop Scheduler service endpoints consuming...");
        }
        // TODO stop consuming Scheduler endpts
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#shutDown() */
    @Override
    public void shutDown() throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("SCHEDSE-3005: Stop provisioning Scheduler service endpts...");
        }
        // TODO stop provisioning Scheduler endpts

        super.shutDown();
    }

    @Override
    protected ServiceUnitManager initServiceUnitManager() {
        return new SchedulerSEServiceUnitManager(getManagerContext(), this);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        return new SchedulerSEExchangeHandler(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new SchedulerSEEndpointManager(this, getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    @Override
    protected Object initRuntimeMBean() throws JBIException {
        mSchedulerConfiguration = new SchedulerSEConfiguration(
                getComponentContext(), getConfig());
        return mSchedulerConfiguration;
    }

    // typed utility accessors
    protected SchedulerSEEndpointManager getEndptMgr() {
        return (SchedulerSEEndpointManager) getManagerContext()
                .getEndpointManager();
    }

    protected SchedulerSEExchangeHandler getHandler() {
        return (SchedulerSEExchangeHandler) getManagerContext()
                .getExchangeHandler();
    }
    
    public SchedulerSEConfiguration getSchedulerConfiguration() {
        return mSchedulerConfiguration;
    }

    public Scheduler getScheduler(String serviceUnitName)
            throws JBIException {
        return getScheduler(serviceUnitName, true);
    }
    
    public Scheduler getScheduler(String serviceUnitName, boolean create)
            throws JBIException {
        DeploymentLookup deploymentLU =
                new DeploymentLookup(getComponentContext());
        try {
            ServiceAssembly serviceAssembly = deploymentLU.getServiceAssembly(
                    serviceUnitName);
            String serviceAssemblyName = serviceAssembly.getIdentification()
                    .getName();
            Scheduler scheduler = mSchedulers.get(serviceAssemblyName);
            if (create && (null == scheduler)) {
                scheduler = createScheduler(serviceAssemblyName);
            }
            return scheduler;
        } catch (Exception ex) {
            log().log(Level.SEVERE, "Cannot procure Quartz scheduler!", ex);
            throw new JBIException("Cannot procure Quartz scheduler!", ex);
        }
    }
    
    protected Scheduler createScheduler(String serviceAssemblyName)
            throws SchedulerException {
        Scheduler scheduler = null;
        synchronized (mSchedulers) {
            scheduler = mSchedulers.get(serviceAssemblyName);
            if (null == scheduler) {
                SimpleThreadPool threadPool = new SimpleThreadPool(
                        getSchedulerConfiguration().getPollerCount()
                        .intValue(), Thread.NORM_PRIORITY);
                threadPool.initialize();
                JobStore jobStore = new RAMJobStore();
                DirectSchedulerFactory.getInstance().createScheduler(
                        serviceAssemblyName, INSTANCE_ID_SUFFIX,
                        threadPool, jobStore);
                scheduler = DirectSchedulerFactory.getInstance()
                        .getScheduler(serviceAssemblyName);
                mSchedulers.put(serviceAssemblyName, scheduler);

                scheduler.setJobFactory(new SchedulerSEJobFactory());
            }
        }
        return scheduler;
    }
    
    public Scheduler removeScheduler(String serviceUnitName) {
        Scheduler scheduler = null;
        DeploymentLookup deploymentLU =
                new DeploymentLookup(getComponentContext());
        try {
            ServiceAssembly sa = deploymentLU.getServiceAssembly(
                    serviceUnitName);
            String saName = sa.getIdentification().getName();
            scheduler = mSchedulers.remove(saName);
        } catch (Exception ex) {
            log().warning("Cannot remove scheduler!");
        }
        return scheduler;
    }
    
    public class SchedulerSEJobFactory implements JobFactory {

        public Job newJob(TriggerFiredBundle bundle) throws SchedulerException {
            EndpointInfo endPtInfo = (EndpointInfo) bundle.getJobDetail()
                    .getJobDataMap().get(ENDPOINT_INFO_PROP);
            if (endPtInfo != null) {
                SchedulerEndPoint schedEndPt = (SchedulerEndPoint)
                        getEndptMgr().lookupEndpoint(endPtInfo);
                if (schedEndPt != null) {
                    return schedEndPt.createSchedulerJob();
                }
            }
            
            throw new SchedulerException(
                    "Cannot find respective Scheduler endpoint!");
        }
    }
}
