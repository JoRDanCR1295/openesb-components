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
 * @(#)SchedulerBootstrap.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.schedulerbc;

import com.sun.jbi.common.util.Util;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.component.toolkit.lifecycle.AbstractBootstrap;

/**
 * Bootstrap implementation for Scheduler Binding Component.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerBootstrap extends AbstractBootstrap {

    public SchedulerBootstrap() {
    }

    protected Object initInstallMBean(ComponentContext ctx)
            throws JBIException {
        try {
            // initialize config to avoid broadcasting changes unnecessarily
            if (Util.isEmpty(getConfig().getProperty(
                    SchedulerConfigurationMBean.POLLER_COUNT_PROPERTY)
                            .getValue())) {
                getConfig().getProperty(SchedulerConfigurationMBean
                        .POLLER_COUNT_PROPERTY).setValue(String.valueOf(
                                SchedulerConfigurationMBean
                                        .DEFAULT_POLLER_COUNT));
            }
            if (Util.isEmpty(getConfig().getProperty(
                    SchedulerConfigurationMBean.QUARTZ_THREAD_COUNT_PROPERTY)
                            .getValue())) {
                getConfig().getProperty(SchedulerConfigurationMBean
                        .QUARTZ_THREAD_COUNT_PROPERTY).setValue(String.valueOf(
                                SchedulerConfigurationMBean
                                        .DEFAULT_QUARTZ_THREAD_COUNT));
            }
            
            // pass config to avoid duplicate initialization from descriptor
            // by AcceptConfig
            return new SchedulerConfiguration(ctx, getConfig());
        } catch (Exception e) {
            throw error(e, "SCHEDBC-6001: "                             //NOI18N
                    + "SchedulerBootstrap installation MBean failed "   //NOI18N
                    + "to initialize: {0}",                             //NOI18N
                    e.getMessage());
        }
    }

    @Override
    protected JBIException error(Exception e, String msg, Object... params) {
        String err = I18n.loc(msg, params);
        if (e == null) {
            log().warning(err);
            return new JBIException(err);
        }
        else {
            log().log(Level.WARNING, err, e);
            return new JBIException(err, e);
        }
    }
    
}
