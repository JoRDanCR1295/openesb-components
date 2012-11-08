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
 * @(#)SchedulerSEServiceUnitManager.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.scheduler;

import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.lifecycle.impl.DefaultServiceUnitManager;
import java.util.logging.Level;
import javax.jbi.management.DeploymentException;
import org.quartz.Scheduler;

/**
 * Scheduler service engine's customization of DefaultServiceUnitManager.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerSEServiceUnitManager extends DefaultServiceUnitManager {

    private SchedulerSEComponentManager mCompMgr;
    
    public SchedulerSEServiceUnitManager(ManagerContext mgrCtx,
            SchedulerSEComponentManager compMgr) {
        super(mgrCtx);
        mCompMgr = compMgr;
    }

    @Override
    public void shutDown(String serviceUnitName) throws DeploymentException {
        shutdownScheduler(serviceUnitName);
        super.shutDown(serviceUnitName);
    }
    
    private void shutdownScheduler(String serviceUnitName) {
        try {
            Scheduler scheduler = mCompMgr.removeScheduler(serviceUnitName);
            if ((scheduler != null) && !scheduler.isShutdown()) {
                scheduler.shutdown();
                log().info("Quartz scheduler has been shut down.");
            }
        } catch (Exception ex) {
            log().log(Level.WARNING, "Cannot shutdown Quartz scheduler!", ex);
        }
    }

    @Override
    public String undeploy(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        shutdownScheduler(serviceUnitName);
        return super.undeploy(serviceUnitName, serviceUnitRootPath);
    }
}
