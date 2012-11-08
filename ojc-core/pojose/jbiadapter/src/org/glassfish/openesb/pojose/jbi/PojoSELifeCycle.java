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
 * @(#)PojoSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.qos.config.RuntimeConfigurationMBean;
import com.sun.jbi.common.util.MBeanHelper;
import com.sun.jbi.common.util.Util;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.messaging.DeliveryChannel;
import javax.management.ObjectName;

/**
 *
 * @author gpatil
 */
public class PojoSELifeCycle implements ComponentLifeCycle {
    private POJOComponentContext pojoCompCtx;
    private Logger compLifeCycleLogger;
    private ComponentConfig compCfg;
    private MBeanHelper mbeanHelper;
    
    public PojoSELifeCycle(POJOComponentContext pc){
        assert pc != null;
        pojoCompCtx = pc;
    }

    // *****
    // * ComponentLifeCycle methods 
    // *****  
    public void init(ComponentContext ctx) throws JBIException {
        pojoCompCtx.setJBIComponentContext(ctx);
        compLifeCycleLogger = Util.getLogger(ctx, this.getClass().getName());
        compCfg = ComponentConfig.parse(ctx.getInstallRoot());
        ConfigPersistence.loadConfig(compCfg, ctx.getWorkspaceRoot());
        PojoSEConfigurationMBean cfgMbean = new PojoSEConfiguration(ctx, compCfg);
        
        mbeanHelper = new MBeanHelper(ctx);
        mbeanHelper.registerMBean(RuntimeConfigurationMBean.CONFIGURATION_EXTENSION, cfgMbean);

        pojoCompCtx.setConfigMbean(cfgMbean);        
        
        // send alert
        String[] info = I18n.locStr("POJOSE-5501: Initialized {0} successfully!",
                ctx.getComponentName());
        compLifeCycleLogger.info(info[2]);
        I18n.alertInfo(info);
    }

    public void start() throws JBIException {
        this.pojoCompCtx.startComponent();
        String[] info = I18n.locStr("POJOSE-5502: Started {0} successfully!",
                pojoCompCtx.getJBIComponentContext().getComponentName());
        compLifeCycleLogger.info(info[2]);
        I18n.alertInfo(info);
    }

    public ObjectName getExtensionMBeanName() {
        return null;
    }
    
    public void stop() throws JBIException {
        // stop NMR polling threads
        this.pojoCompCtx.stopComponent();
        String[] info = I18n.locStr("POJOSE-5503: Stopped {0} successfully!",
                pojoCompCtx.getJBIComponentContext().getComponentName());
        compLifeCycleLogger.info(info[2]);
        I18n.alertInfo(info);
    }

    public void shutDown() throws JBIException {
        try {
            DeliveryChannel channel = pojoCompCtx.getJBIComponentContext().getDeliveryChannel();
            if (channel != null) {
                channel.close();
            }
        } catch (Exception e) {
            String compName = pojoCompCtx.getJBIComponentContext().getComponentName();
            String msg = I18n.loc("POJOSE-6501: {0} failed to close DeliveryChannel during shutdown: {1}",
                    compName, e.getMessage());
            compLifeCycleLogger.log(Level.WARNING, msg, e);
            throw new JBIException(msg, e);
        }

        // shut down polling threads
        //getManagerContext().getExchangeHandler().shutDown();

        String[] info = I18n.locStr("POJOSE-5504: Shut down {0} successfully!",
                pojoCompCtx.getJBIComponentContext().getComponentName());
        compLifeCycleLogger.info(info[2]);
        I18n.alertInfo(info);
    }

    // *****
    // * POJO specific methods
    // *****

    public ComponentConfig getCompCfg() {
        return compCfg;
    }

    public Logger getCompLifeCycleLogger() {
        return compLifeCycleLogger;
    }
}
