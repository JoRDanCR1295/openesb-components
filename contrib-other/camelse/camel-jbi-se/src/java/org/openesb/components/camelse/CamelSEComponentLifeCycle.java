/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/*
 * CamelSEComponentLifeCycle.java
 */
package org.openesb.components.camelse;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import org.openesb.components.camelse.CamelSEConfigMBean.CamelSEConfigMBeanImpl;
import org.openesb.components.camelse.common.BasicComponentLifeCycle;
import org.openesb.components.camelse.common.DefaultMessageExchangeReceiver;
import org.openesb.components.camelse.common.MessageExchangeReceiver;
import javax.jbi.JBIException;
import javax.jbi.component.Component;

/**
 * This class extends the basic ComponentLifeCycle implementation to provide component
 * specific implementation of the ComponentLifeCycle.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @see com.sun.jbi.sample.component.common.BasicComponentLifeCycle
 * @author chikkala
 */
public class CamelSEComponentLifeCycle extends BasicComponentLifeCycle {

    private CamelSEConfigMBeanImpl mConfigImpl;

    /** constructor */
    public CamelSEComponentLifeCycle(Component compRuntime) {
        super(compRuntime);
    }

    /**
     * creates DefaultMessageExchangeReceiver to handles receiving and processing
     * the message exchanges from the delivery channel. 
     */
    @Override
    protected MessageExchangeReceiver createMessageExchangeReceiver() {
        return new DefaultMessageExchangeReceiver();
    }

    /**
     * chance to extended classes to do the component specific init
     * @throws javax.jbi.JBIException
     */
    @Override
    protected void doInit() throws JBIException {
        super.doInit();
    //TODO: create and register addtional mbeans if needed();  
    }

    @Override
    protected void doShutDown() throws JBIException {
        super.doShutDown();
    //TODO: unregister addtional mbeans created and clean the resources if needed.
    }

    @Override
    protected void doStart() throws JBIException {

        super.doStart();
    }

    @Override
    protected void doStop() throws JBIException {
        super.doStop();
    }

    @Override
    protected StandardMBean createExtensionMBean() {
        try {
            ComponentContext ctx = this.getComponentContext();
            if ( this.mConfigImpl == null ) {
                this.mConfigImpl = new CamelSEConfigMBeanImpl(ctx.getInstallRoot(), ctx.getWorkspaceRoot());
            }
            StandardMBean configBean = new StandardMBean(this.mConfigImpl, CamelSEConfigMBean.class);
            return configBean;
        } catch (NotCompliantMBeanException ex) {
            Logger.getLogger(CamelSEComponentLifeCycle.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
    }

    @Override
    protected ObjectName createExtensionMBeanName() {
        MBeanNames mbeanNames = this.getComponentContext().getMBeanNames();
        // return mbeanNames.createCustomComponentMBeanName(mbeanNames.COMPONENT_LIFE_CYCLE_EXTENSION);
        // return the configuration mbean as the extension mbean for now. 
        //TODO: change this to return null and manage add/remove configuration mbean 
        // as additional mbeans in doInit and doShutDown methods?
        
        return mbeanNames.createCustomComponentMBeanName("Configuration");
    }

    protected CamelSEConfigMBean getConfigMBeanImpl() {
        return this.mConfigImpl;
    }
    
}
