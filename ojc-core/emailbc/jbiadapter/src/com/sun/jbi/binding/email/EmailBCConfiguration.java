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
 * @(#)EmailBCConfiguration.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.MBeanException;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.RuntimeConfiguration;
import com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfig;

/**
 * Default implementation of {@link EmailBCConfigurationMBean}.
 * 
 * @author CDK
 */
//Harry: public class EmailBCConfiguration extends AbstractConfigMBean implements EmailBCConfigMBean {
//public class EmailBCConfiguration extends PollerConfig implements EmailBCConfigMBean {
public class EmailBCConfiguration extends RuntimeConfiguration implements EmailBCConfigurationMBean {
    private PollerConfig mPollerConfig;
    
    public EmailBCConfiguration(ComponentContext ctx, ComponentConfig config)
    throws DeploymentException {
        super(ctx, config);
        this.mPollerConfig = new PollerConfig(ctx, config);
    }

    /**
     * @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#getExchangeThreading()
     */
    public String getExchangeThreading() {
        return this.mPollerConfig.getExchangeThreading();
    }

    /**
     * @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#getPollerCount()
     */
    public Integer getPollerCount() {
        return this.mPollerConfig.getPollerCount();
    }

    /**
     * @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#setExchangeThreading(java.lang.String)
     */
    public void setExchangeThreading(String cfg) throws MBeanException {
        I18n.config(log(), "EMAILBC-4002: ExchangeThreading Value : [New={0}, Old={1}]", cfg, getExchangeThreading());
        this.mPollerConfig.setExchangeThreading(cfg);
    }

    /**
     * @see com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfigMBean#setPollerCount(java.lang.Integer)
     */
    public void setPollerCount(Integer count) throws MBeanException {
        I18n.config(log(), "EMAILBC-4001: PollerCount Value : [New='{0}', Old='{1}']", count, getPollerCount());
        this.mPollerConfig.setPollerCount(count);
    }

    // TODO (optional) Add additional properties here...
}
