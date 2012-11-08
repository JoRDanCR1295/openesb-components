/*
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
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * DefaultComponentLifeCycle.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import javax.jbi.JBIException;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 * Default ComponentLifeCycle implementation.
 *
 * @author Sun Microsystems, Inc.
 */
public class DefaultComponentLifeCycle extends AbstractComponentLifeCycle {
    
    public DefaultComponentLifeCycle(ComponentRuntime compRuntime) {
        super(compRuntime);
    }
    
    protected void activateServiceProviders() throws JBIException {
        // NOOP
    }
    
    protected void deactivateServiceProviders() throws JBIException {
        // NOOP
    }
    
    protected void activateServiceConsumers() throws JBIException {
        // NOOP
    }
    
    protected void deactivateServiceConsumers() throws JBIException {
        // NOOP
    }
    
    protected void initMessageExchangeHandlerFactory() throws JBIException {
        RuntimeContext.getInstance().setMessageExchangeHandlerFactory(
                new MessageExchangeHandlerFactory.DefaultMessageExchangeHandlerFactory());
    }
    
    protected MessageExchangeReceiver createMessageExchangeReceiver() throws Exception {
        return new MessageExchangeReceiver();
    }
    
    /**
     * no extension mbean
     */
    protected ObjectName createExtensionMBeanName() {
        return null;
    }
    
    /**
     * no extension mbean
     */
    protected StandardMBean createExtensionMBean() {
        return null;
    }
    
}
