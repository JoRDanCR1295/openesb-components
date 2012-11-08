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
 * @(#)SchedulerSEConfiguration.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scheduler;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.qos.config.AbstractConfigMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;

/**
 * Default implementation of {@link SchedulerSEConfigurationMBean}.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerSEConfiguration extends AbstractConfigMBean
        implements SchedulerSEConfigurationMBean {

    public SchedulerSEConfiguration(ComponentContext ctx, ComponentConfig config)
            throws DeploymentException {
        super(ctx, config);
    }

    /** @see com.sun.jbi.component.config.PollerConfigMBean#getPollerCount() */
    public Integer getPollerCount() {
        return Integer.valueOf(getConfig().getProperty(POLLER_COUNT_PROPERTY)
                .getValue());
    }

    /** @see com.sun.jbi.component.config.PollerConfigMBean#setPollerCount(java.lang.Integer) */
    public void setPollerCount(Integer count) {
        int val = (count == null) ? DEFAULT_POLLER_COUNT : count.intValue();
        getConfig().getProperty(POLLER_COUNT_PROPERTY).setValue(
                String.valueOf(val));
    }
}
