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
 * @(#)SchedulerAdministration.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import com.sun.jbi.common.util.Util;
import java.util.logging.Logger;
import javax.management.MBeanException;
import org.glassfish.openesb.schedulerbc.I18n;
import org.glassfish.openesb.schedulerbc.SchedulerComponentManager;

/**
 * Implements "Administration" (of end points) MBean.
 *
 * @author sunsoabi_edwong
 * @since 2.1.1
 */
public class SchedulerAdministration implements SchedulerAdministrationMBean {

    private SchedulerComponentManager schedCompMgr;
    private Logger logger;

    public SchedulerAdministration(SchedulerComponentManager schedCompMgr) {
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

    public boolean suspend(String consumingEndpointName) throws MBeanException {
        boolean result = schedCompMgr.getEndptMgr().suspendEndpoint(
                consumingEndpointName);
        if (result) {
            I18n.info(log(), "SCHEDBC-5013: Endpoint {0} has been "     //NOI18N
                    + "suspended via JMX", consumingEndpointName);      //NOI18N
        } else {
            I18n.warning(log(), "SCHEDBC-6027: Active endpoint {0} not "//NOI18N
                    + "found; none suspended", consumingEndpointName);  //NOI18N
        }
        return result;
    }

    public boolean resume(String consumingEndpointName) throws MBeanException {
        boolean result = schedCompMgr.getEndptMgr().activateEndpoint(
                consumingEndpointName);
        if (result) {
            I18n.info(log(), "SCHEDBC-5014: End point {0} has been "    //NOI18N
                    + "resumed via JMX", consumingEndpointName);        //NOI18N
        } else {
            I18n.warning(log(), "SCHEDBC-6028: Suspended endpoint {0} " //NOI18N
                    + "not found; none resumed", consumingEndpointName);//NOI18N
        }
        return result;
    }

    public boolean isEndpointActive(String consumingEndpointName)
            throws MBeanException {
        return schedCompMgr.getEndptMgr().isEndpointActivated(
                consumingEndpointName);
    }

    public String[] listActiveEndpoints() {
        return schedCompMgr.getEndptMgr().getActivatedEndpoints();
    }

    public String[] listInactiveEndpoints() {
        return schedCompMgr.getEndptMgr().getSuspendedEndpoints();
    }
}
