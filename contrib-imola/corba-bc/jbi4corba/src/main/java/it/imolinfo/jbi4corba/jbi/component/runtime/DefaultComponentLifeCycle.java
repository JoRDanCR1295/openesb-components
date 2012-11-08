 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * DefaultComponentLifeCycle.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import javax.jbi.JBIException;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 * Default ComponentLifeCycle implementation.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
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
