/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.component;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle;
import it.imolinfo.jbi4ejb.jbi.component.runtime.ComponentRuntime;
import it.imolinfo.jbi4ejb.jbi.component.runtime.MessageExchangeReceiver;
import it.imolinfo.jbi4ejb.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4ejb.jbi.xfire.EjbTransport;

import javax.jbi.JBIException;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import org.codehaus.xfire.DefaultXFire;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.transport.Transport;

/**
 * Jbi4Ejb <code>ComponentLifeCycle</code> implementation. 
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbLifeCycle extends AbstractComponentLifeCycle {
    
    /** The logger. */
    private static final Logger LOG = LoggerFactory.getLogger(Jbi4EjbLifeCycle.class);    
    private static final Messages MESSAGES = Messages.getMessages(Jbi4EjbLifeCycle.class);
    
    /** xfire. */
    private XFire xfire;
        
    /**
     * Instantiates a new jbi4 ejb life cycle.
     * 
     * @param compRuntime the component runtime
     */
    public Jbi4EjbLifeCycle(ComponentRuntime compRuntime) {
        super(compRuntime);                        
    }       
    
    /**
     * chance to extended classes to do the component specific init.
     * @throws javax.jbi.JBIException if some problem occurs
     */
    protected void onInit() throws JBIException {        
        xfire = createXFire();
    }
    
    /**
     * Init the Message Exchange Handler factory.
     * @throws JBIException if some problem occurs
     * @see it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle#initMessageExchangeHandlerFactory()
     */
    protected void initMessageExchangeHandlerFactory() throws JBIException {
                
        if (! ( this.getComponentRuntime().getServiceUnitManager() instanceof Jbi4EjbSUManager)) {
        	String msg=MESSAGES.getString("EJB000101_Service_Unit_Manager_wrong_type");
            LOG.error(msg);
            throw new JBIException(msg);
        }
        Jbi4EjbSUManager suManager = (Jbi4EjbSUManager)this.getComponentRuntime().getServiceUnitManager();
        RuntimeContext.getInstance().setMessageExchangeHandlerFactory(                
                new Jbi4EjbMessageExchangeHandlerFactory(suManager));
    }
    
    /**
     * Create the message exchange receiver.
     * @see it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle#createMessageExchangeReceiver()
     * @see it.imolinfo.jbi4ejb.jbi.component.runtime.MessageExchangeReceiver
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
     *  @see it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle#activateServiceConsumers()
     */
    protected void activateServiceConsumers() throws JBIException {       
        // DO nothing: no consumer for this component
    }
        
    /**
     * Do nothing.
     * @throws JBIException if some problem occurs
     * @see it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle#deactivateServiceConsumers()
     */
    protected void deactivateServiceConsumers() throws JBIException {
        // DO nothing: no consumer for this component
    }
    
    /**
     * Do nothing.
     * @throws JBIException if some problem occurs
     * @see it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle#activateServiceProviders()
     */    
    protected void activateServiceProviders() throws JBIException {        
        // Do nothing
    }
    
    /**
    * Do nothing.
    * @throws JBIException if some problem occurs
    * @see it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle#deactivateServiceProviders()
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
     * Gets xfire.
     * 
     * @return the xfire
     */
    public XFire getXfire() {
        return xfire;
    }
    
    /**
     * Creates XFire.
     * @return xfire, correctly configured
     */
    private static XFire createXFire() {
        XFire xfire = new DefaultXFire();
        Object[] transports = xfire.getTransportManager().getTransports().toArray();
        for (int i = 0; i < transports.length; i++) {
          xfire.getTransportManager().unregister((Transport) transports[i]);
        }
        xfire.getTransportManager().register(new EjbTransport());        
        return xfire;
    }      
    
}
