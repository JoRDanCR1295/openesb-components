/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: ProviderEndpoint.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common.deployment;

import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Service;
import javax.jbi.JBIException;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;

public class ProviderEndpoint extends Endpoint {
    /**
     * This constructor initializes the endpoint with PROVIDER role and makes sure that the service
     * description passed to it is of provider description.
     */
    protected ProviderEndpoint(SUDescriptor.Provides provides, Definition wsdlDef, ServiceUnit su) {
        super(Role.PROVIDER, provides, wsdlDef, su);
    }
    /**
     * constructor that does not need service unit information. useful for creating the endpoint for
     * static services provided by the component.
     */
    protected ProviderEndpoint(SUDescriptor.Provides provides, Definition wsdlDef) {
        this(provides, wsdlDef, null);
    }
    /**
     * initializes the endpoint and creates and registers the MessageExchangeListener with
     * MessageExchangeSupport.
     */
    public final void init() throws JBIException {
        getLogger().fine("ProviderEndpiont: init called");
        doInit();                       //1. initialize the endpiont resources
        addMessageExchangeListener();   //2. register message exchange linster.
    }
    /**
     * calls activatesServiceEndpoint for the Provider.
     */
    public final void activate() throws JBIException {
        getLogger().fine("ProviderEndpiont: activate called");
        activateServiceEndpoint();  //1. activate service endpoint in NMR
        doActivate();               //2. do any other activation related tasks.
    }
    /**
     * calls deactivateServiceEndpoint for the Provider.
     */
    public final void deactivate() throws JBIException {
        getLogger().fine("ProviderEndpiont: deactivate called");
        deactivateServiceEndpoint();    //1. deactivates the service endpoint in NMR
        doDeactivate();                 //2. do any other deactivation related tasks.
    }
    /**
     * removes the message exchange listener. cleans up other resources
     */
    public final void clean() throws JBIException {
        getLogger().fine("ProviderEndpiont: clean called");
        removeMessageExchangeListener();    //1. remove message exchange listener
        doClean();                          //2. clean up any other resources.
    }
    /**
     * Activates the ServiceEndpoint with NMR
     */
    private void activateServiceEndpoint() throws JBIException {
        Service service = this.getService();
        ServiceEndpoint svcEP = this.getComponentContext().activateEndpoint(
            service.getServiceName(), service.getEndpointName());
        this.setServiceEndpoint(svcEP);
    }
    /**
     * Deactivates ServiceEndpoint in NMR
     */
    private void deactivateServiceEndpoint()  throws JBIException {
        this.getComponentContext().deactivateEndpoint(this.getServiceEndpoint());
        this.setServiceEndpoint(null);
    }
    
    protected void doInit() throws JBIException {
        //NOOP
    }
    protected void doActivate() throws JBIException {
        //NOOP
    }
    protected void doDeactivate() throws JBIException {
        //NOOP
    }
    protected void doClean() throws JBIException {
        //NOOP
    }
    
}