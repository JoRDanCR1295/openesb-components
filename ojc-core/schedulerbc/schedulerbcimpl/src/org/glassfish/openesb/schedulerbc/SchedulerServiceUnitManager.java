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
 * @(#)SchedulerServiceUnitManager.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc;

import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.lifecycle.impl.DefaultServiceUnitManager;
import javax.jbi.management.DeploymentException;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;

/**
 * Scheduler binding component's customization of DefaultServiceUnitManager.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerServiceUnitManager extends DefaultServiceUnitManager
        implements SchedulerConstants {

    private SchedulerComponentManager mCompMgr;
    private final Object saMonitor = new Object();

    private enum TriggerAction {
        NONE,
        CREATE,
        UPDATE,
        DELETE,
        SHUTDOWN,
        PAUSE,
        RESUME;
    }
    
    public SchedulerServiceUnitManager(ManagerContext mgrCtx,
            SchedulerComponentManager compMgr) {
        super(mgrCtx);
        mCompMgr = compMgr;
    }

    @Override
    public void shutDown(String serviceUnitName) throws DeploymentException {
        shutdownTriggers(serviceUnitName);
        super.shutDown(serviceUnitName);
    }

    private void shutdownTriggers(String serviceUnitName) {
        try {
            DeploymentLookup deploymentLU = new DeploymentLookup(
                    getManagerContext().getComponentContext());
            ServiceAssembly serviceAssembly = deploymentLU.getServiceAssembly(
                    serviceUnitName);
            String saName = serviceAssembly.getIdentification().getName();
            Scheduler scheduler = mCompMgr.getScheduler();
            if ((scheduler != null) && !scheduler.isShutdown()) {
                shutdownTriggerGroup(saName);
            }
        } catch (Exception ex) {
            // ignore since we're going down anyways
        }
    }
    
    private void undeployTriggers(String serviceUnitName) {
        try {
            DeploymentLookup deploymentLU = new DeploymentLookup(
                    getManagerContext().getComponentContext());
            ServiceAssembly serviceAssembly = deploymentLU.getServiceAssembly(
                    serviceUnitName);
            String saName = serviceAssembly.getIdentification().getName();
            Scheduler scheduler = mCompMgr.getScheduler();
            if ((scheduler != null) && !scheduler.isShutdown()) {
                deleteTriggers(saName);
            }
        } catch (Exception ex) {
            // ignore since we're undeploying anyways
        }
    }

    @Override
    public String undeploy(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        undeployTriggers(serviceUnitName);
        return super.undeploy(serviceUnitName, serviceUnitRootPath);
    }

    private void acquire(String saName, TriggerAction action)
            throws SchedulerException {
        synchronized (saMonitor) {
            executor(saName, action);
        }
    }

    private void executor(String saName, TriggerAction action)
            throws SchedulerException {
        switch (action) {
        case CREATE:
            createTriggersHelper(saName);
            break;
        case UPDATE:
            updateTriggersHelper(saName);
            break;
        case DELETE:
            deleteTriggersHelper(saName);
            break;
        case SHUTDOWN:
            shutdownTriggerGroupHelper(saName);
            break;
        case PAUSE:
            pauseTriggersHelper(saName);
            break;
        case RESUME:
            resumeTriggersHelper(saName);
            break;
        default:
            break;
        }
    }

    public void createTriggers(String saName) throws SchedulerException {
        acquire(saName, TriggerAction.CREATE);
    }

    private void createTriggersHelper(String saName) {
    }

    public void updateTriggers(String saName) throws SchedulerException {
        acquire(saName, TriggerAction.UPDATE);
    }

    private void updateTriggersHelper(String saName) {
    }

    public void deleteTriggers(String saName) throws SchedulerException {
        acquire(saName, TriggerAction.DELETE);
    }

    private void deleteTriggersHelper(String saName)
            throws SchedulerException {
        Scheduler scheduler = mCompMgr.getScheduler();
        String[] jobNames = scheduler.getJobNames(saName);
        for (String jobName : jobNames) {
            scheduler.deleteJob(jobName, saName);
        }
        I18n.info(log(), "SCHEDBC-5009: Trigger Group {0} deleted",     //NOI18N
                saName);
    }

    public void shutdownTriggerGroup(String saName) throws SchedulerException {
        acquire(saName, TriggerAction.SHUTDOWN);
    }

    private void shutdownTriggerGroupHelper(String saName)
            throws SchedulerException {
        Scheduler scheduler = mCompMgr.getScheduler();
        String[] jobNames = scheduler.getJobNames(saName);
        for (String jobName : jobNames) {
            Trigger trig = null;
            if (I18n.finestLoggable(log())) {
                Trigger[] trigs =
                        scheduler.getTriggersOfJob(jobName, saName);
                if ((trigs != null) && (trigs.length > 0)) {
                    trig = trigs[0];
                }
            }
            if (jobName.endsWith(STATIC_JOB_SUFFIX)) {
                scheduler.deleteJob(jobName, saName);
                
                if (trig != null) {
                    I18n.finest(log(), "SCHEDBC-1036: Shutdown {0} "    //NOI18N
                            + "Trigger {1} by deleting",                //NOI18N
                            SchedAttrValue.STATIC.getDisplay(),
                            trig.getFullName());
                }
            } else {
                scheduler.pauseJob(jobName, saName);

                if (trig != null) {
                    I18n.finest(log(), "SCHEDBC-1037: Shutdown {0} "    //NOI18N
                            + "Trigger {1} by suspending",              //NOI18N
                            SchedAttrValue.DYNAMIC.getDisplay(),
                            trig.getFullName());
                }
            }
        }
        I18n.info(log(), "SCHEDBC-5012: Trigger Group {0} shutdown",    //NOI18N
                saName);
    }

    public void pauseTriggers(String saName) throws SchedulerException {
        acquire(saName, TriggerAction.PAUSE);
    }

    private void pauseTriggersHelper(String saName)
            throws SchedulerException {
        mCompMgr.getScheduler().pauseTriggerGroup(saName);
        I18n.info(log(), "SCHEDBC-5010: Trigger Group {0} suspended",   //NOI18N
                saName);
    }

    public void resumeTriggers(String saName) throws SchedulerException {
        acquire(saName, TriggerAction.RESUME);
    }

    private void resumeTriggersHelper(String saName)
            throws SchedulerException {
        mCompMgr.getScheduler().resumeTriggerGroup(saName);
        I18n.info(log(), "SCHEDBC-5011: Trigger Group {0} resumed",     //NOI18N
                saName);
    }
}
