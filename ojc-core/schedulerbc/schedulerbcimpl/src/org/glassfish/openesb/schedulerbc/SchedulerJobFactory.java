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
 * @(#)SchedulerJobFactory.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc;

import com.sun.jbi.common.descriptor.EndpointInfo;
import java.util.logging.Logger;
import org.glassfish.openesb.schedulerbc.domain.SchedulerEndPoint;
import org.glassfish.openesb.schedulerbc.domain.SchedulerEndPointInfo;
import org.quartz.Job;
import org.quartz.SchedulerException;
import org.quartz.spi.JobFactory;
import org.quartz.spi.TriggerFiredBundle;

/**
 * JobFactory to create SchdedulerJob.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerJobFactory implements JobFactory {

    private Logger logger;

    private static SchedulerEndpointManager endpointMgr;

    static void setEndpointManager(SchedulerEndpointManager endpointMgr) {
        SchedulerJobFactory.endpointMgr = endpointMgr;
    }

    public SchedulerEndpointManager getEndpointManager() {
        return endpointMgr;
    }

    public Job newJob(TriggerFiredBundle bundle) throws SchedulerException {
        String endPtInfoStr = (String) bundle.getJobDetail().getJobDataMap()
                .get(SchedulerComponentManager.ENDPOINT_INFO_PROP);
        if (endPtInfoStr != null) {
            EndpointInfo endPtInfo =
                    SchedulerEndPointInfo.valueOf(endPtInfoStr);
            SchedulerEndPoint schedEndPt = (SchedulerEndPoint)
                    getEndpointManager().lookupEndpoint(endPtInfo);
            if (schedEndPt != null) {
                return schedEndPt.createSchedulerJob();
            }
        }

        String errMsg = I18n.severe(log(), "SCHEDBC-7002: "             //NOI18N
                + "Cannot find respective Scheduler endpoint");         //NOI18N
        throw new SchedulerException(errMsg);
    }

    private Logger log() {
        if (null == logger) {
            logger = Logger.getLogger(getClass().getName());
        }
        return logger;
    }
}
