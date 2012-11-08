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
 * @(#)ScreenScrapingseConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.engine.screenscrapingse.jbiadapter;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.qos.config.AbstractConfigMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;

public class ScreenScrapingseConfiguration extends AbstractConfigMBean implements ScreenScrapingseConfigurationMBean {

    public ScreenScrapingseConfiguration(ComponentContext ctx, ComponentConfig config) 
            throws DeploymentException {
        super(ctx, config);
    }
    
     public String getSomeProperty() {
        return getConfig().getProperty("SomeProperty").getValue();
    }
     
     public void setSomeProperty(String someValue)
     {
          getConfig().getProperty("SomeProperty").setValue(someValue);
     }
      /** @see com.sun.jbi.component.config.PollerConfigMBean#getPollerCount() */
    public Integer getPollerCount() {
        return Integer.valueOf(getConfig().getProperty(POLLER_COUNT_PROPERTY).getValue());
    }

    /** @see com.sun.jbi.component.config.PollerConfigMBean#setPollerCount(java.lang.Integer) */
    public void setPollerCount(Integer count) {
        int val = (count == null) ? DEFAULT_POLLER_COUNT : count.intValue();
        getConfig().getProperty(POLLER_COUNT_PROPERTY).setValue(String.valueOf(val));
    }
}
