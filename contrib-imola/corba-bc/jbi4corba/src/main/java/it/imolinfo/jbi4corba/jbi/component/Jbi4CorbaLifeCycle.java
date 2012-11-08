 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.AbstractComponentLifeCycle;
import it.imolinfo.jbi4corba.jbi.component.runtime.ComponentRuntime;
import it.imolinfo.jbi4corba.jbi.component.runtime.MessageExchangeReceiver;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeConfiguration;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;

import javax.jbi.JBIException;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import com.sun.jbi.eManager.provider.StatusProviderHelper;

/**
 * Jbi4Corba <code>ComponentLifeCycle</code> implementation. 
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaLifeCycle extends AbstractComponentLifeCycle {
    
    /** The logger. */
    private static final Logger LOG = LoggerFactory.getLogger(Jbi4CorbaLifeCycle.class);
    private static final Messages MESSAGES = Messages.getMessages(Jbi4CorbaLifeCycle.class);

           
    /**
     * Instantiates a new jbi4corba life cycle.
     * 
     * @param compRuntime the component runtime
     */
    public Jbi4CorbaLifeCycle(ComponentRuntime compRuntime) {
        super(compRuntime);                        
    }       
        
    /**
     * Init the Message Exchange Handler factory.
     * @throws JBIException if some problem occurs
     * @see it.imolinfo.jbi4corba.jbi.component.runtime.AbstractComponentLifeCycle#initMessageExchangeHandlerFactory()
     */
    protected void initMessageExchangeHandlerFactory() throws JBIException {
                
        if (! ( this.getComponentRuntime().getServiceUnitManager() instanceof Jbi4CorbaSUManager)) {
        	String msg=MESSAGES.getString("CRB000007_Service_Unit_Manager_wrong_type");
        	LOG.error(msg);
            throw new JBIException(msg);
        }
        Jbi4CorbaSUManager suManager = (Jbi4CorbaSUManager)this.getComponentRuntime().getServiceUnitManager();
        RuntimeContext.getInstance().setMessageExchangeHandlerFactory(                
                new Jbi4CorbaMessageExchangeHandlerFactory(suManager));
    }
    
    /**
     * Create the message exchange receiver.
     * @see it.imolinfo.jbi4corba.jbi.component.runtime.AbstractComponentLifeCycle#createMessageExchangeReceiver()
     * @see it.imolinfo.jbi4corba.jbi.component.runtime.MessageExchangeReceiver
     * @throws Exception if some problem occurs
     * @return the <code>MessageExchangeReceiver</code>
     */
    protected MessageExchangeReceiver createMessageExchangeReceiver() throws Exception {
        // JMXBinding component sample only provides synchornous isnbound message exchange.
        // so no need to have the message receiver to get the message exchanges from delivery channel.
        // return null;
        return new MessageExchangeReceiver();
    }
        
    /**
     * Do nothing.
     * @throws JBIException if some problem occurs
     *  @see it.imolinfo.jbi4corba.jbi.component.runtime.AbstractComponentLifeCycle#activateServiceConsumers()
     */
    protected void activateServiceConsumers() throws JBIException {       
        // DO nothing: no consumer for this component
    }
        
    /**
     * Do nothing.
     * @throws JBIException if some problem occurs
     * @see it.imolinfo.jbi4corba.jbi.component.runtime.AbstractComponentLifeCycle#deactivateServiceConsumers()
     */
    protected void deactivateServiceConsumers() throws JBIException {
        // DO nothing: no consumer for this component
    }
    
    /**
     * Do nothing.
     * @throws JBIException if some problem occurs
     * @see it.imolinfo.jbi4corba.jbi.component.runtime.AbstractComponentLifeCycle#activateServiceProviders()
     */    
    protected void activateServiceProviders() throws JBIException {        
        // Do nothing
    }
    
    /**
    * Do nothing.
    * @throws JBIException if some problem occurs
    * @see it.imolinfo.jbi4corba.jbi.component.runtime.AbstractComponentLifeCycle#deactivateServiceProviders()
    */        
    protected void deactivateServiceProviders() throws JBIException {
        // Do nothing
    }
    
    /**
     * no extension mbean.
     * 
     * @return alway null
     */
    protected ObjectName createExtensionMBeanName() {
        return null;
    }
    
    /**
     * no extension mbean.
     * 
     * @return always null
     */
    protected StandardMBean createExtensionMBean() {
        return null;
    }
            
    /**
     * get the RuntimeConfigurationMbean object
     */
    public RuntimeConfiguration getRuntimeConfiguration(){
        return mRuntimeConfig;
    }
    
    StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }    
}
