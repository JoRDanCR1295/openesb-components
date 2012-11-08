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

package com.sun.jbi.engine.xslt;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.MBeanException;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfig;
import com.sun.transform.engine.EngineConfig;
import com.sun.transform.engine.EngineConfig.TransformEngine;

/**
 * Default implementation of {@link XsltSEConfigMBean}.
 * 
 * @author Kevan Simpson
 */
public class XsltSEConfig extends PollerConfig implements XsltSEConfigMBean {
    
    public XsltSEConfig(ComponentContext ctx, ComponentConfig config) 
            throws DeploymentException {
        super(ctx, config);
    }

    /** @see com.sun.transform.engine.runtime.TransformConfiguration#getTransformEngine() */
    public String getTransformEngine() {
        return getConfig().getProperty(EngineConfig.TRANSFORM_ENGINE_PROPERTY).getValue();
    }

    /** @see com.sun.transform.engine.runtime.TransformConfiguration#setTransformEngine(java.lang.String) */
    public void setTransformEngine(String eng) throws MBeanException {
        TransformEngine te = null;
        
        if (!Util.isEmpty(eng)) {
            try {
                te = TransformEngine.valueOf(eng);
            }
            catch (Exception e) { /* ignore */ }
        }

        if (te == null) {
            throw mbeanError(null, I18n.loc(
                    "XSLTSE-6004: Invalid Transform Engine value: {0}", eng));
        }
        
        getConfig().getProperty(EngineConfig.TRANSFORM_ENGINE_PROPERTY).setValue(eng);
        persistConfiguration();
    }
}
