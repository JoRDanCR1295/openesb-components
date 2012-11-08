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
 * @(#)AleSEConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.lifecycle.impl;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.MBeanException;
import com.sun.jbi.common.qos.config.AbstractConfigMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.common.util.Util;

/**
 * Default implementation of an MBean that has a <code>PollerCount</code> and
 * (optionally) a <code>ExchangeThreading</code> property.
 * 
 * @author Kevan Simpson
 */
public class PollerConfig extends AbstractConfigMBean implements PollerConfigMBean {
    
    public PollerConfig(ComponentContext ctx, ComponentConfig config) 
            throws DeploymentException {
        super(ctx, config);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#getExchangeThreading() */
    public String getExchangeThreading() {
        return getConfig().getProperty(
                ThreadedExchangeHandler.EXCHANGE_THREADING_PROPERTY).getValue();
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#getPollerCount() */
    public Integer getPollerCount() {
        return Integer.valueOf(
                getConfig().getProperty(AcceptPoller.POLLER_COUNT_PROPERTY).getValue());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#setExchangeThreading(java.lang.String) */
    public void setExchangeThreading(String cfg) throws MBeanException {
        validateExchangeThreading(cfg); // will throw exception if invalid
        
        getConfig().modifyAndBroadcast(
                ThreadedExchangeHandler.EXCHANGE_THREADING_PROPERTY, cfg);
        persistConfiguration();
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#setPollerCount(java.lang.Integer) */
    public void setPollerCount(Integer count) throws MBeanException {
        validatePollerCount(count); // will throw exception if invalid
        getConfig().modifyAndBroadcast(AcceptPoller.POLLER_COUNT_PROPERTY, count);
        persistConfiguration();
    }
    
    private void validateExchangeThreading(String cfg) throws MBeanException {
        boolean valid = true;
        if (Util.isEmpty(cfg)) {
            valid = false;
        }
        else {
            String[] tkns = Util.tokenize(cfg, ",");
            for (String str : tkns) {
                int x = Util.parseInt(str, -1);
                if (x < 1) valid = false;
            }
        }
        
        if (!valid) {
            throw invalidProperty(
                    ThreadedExchangeHandler.EXCHANGE_THREADING_PROPERTY, cfg);
        }
    }

    private void validatePollerCount(Integer count) throws MBeanException {
        Property pc = getConfig()
                .getProperty(AcceptPoller.POLLER_COUNT_PROPERTY);
        if (count == null || !pc.conforms(count)) {
            throw invalidProperty(
                    AcceptPoller.POLLER_COUNT_PROPERTY, String.valueOf(count));
        }
    }
}
