 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import org.apache.cxf.jaxws.JaxWsClientFactoryBean;
import org.apache.cxf.service.Service;

/**
 * The Class ProviderCXFServiceCreator.
 */
public class ProviderServiceCreator {

    /** Logger. */
    private static final Logger LOG
    = LoggerFactory.getLogger(ProviderServiceCreator.class);  
    /**
     * Empty Constructor.
     */
    public ProviderServiceCreator() {
        // NOP
    }

    /**
     * Creates the CXF service. 
     * If the interface name is null, get it from the created service.
     * 
     * @param serviceDescriptor
     * @param interfaceName
     * 
     * @return the service
     * 
     * @throws IOException
     * @throws WSDLException
     * @throws Jbi4CorbaException
     */
    public Service createService(ProviderServiceDescriptor serviceDescriptor, QName interfaceName) throws IOException, WSDLException, 
    Jbi4CorbaException {
        
        LOG.debug(">>>>> createService - begin");  
        LOG.debug("InterfaceName: " + interfaceName);
        LOG.debug("Service Interface: " + serviceDescriptor.getServiceInterface());       

        // Creates the endpoints
        JaxWsClientFactoryBean endpointFactory = CXFUtils.getJaxWsClientFactoryBean();                  
        
        // The wrapper must be anonymous to avoid name collisions in jaxb-jaxws, for example:
        // Foo foo();
        // With CXF 2.1.3 does not work!!!!!!!!
        // See https://issues.apache.org/jira/browse/CXF-1930        
        endpointFactory.getServiceFactory().setAnonymousWrapperTypes(true);
        // To qualify schema elements
        endpointFactory.getServiceFactory().setQualifyWrapperSchema(true);     
        endpointFactory.getServiceFactory().setWrapped(true);        
                
        // Sets the service class    
        endpointFactory.setServiceClass(serviceDescriptor.getServiceInterface());
        endpointFactory.setServiceName(interfaceName);
                       
        // Creates the endpoint
        endpointFactory.create();              
       
        // Gets the service model
        Service service = endpointFactory.getServiceFactory().getService();

        LOG.debug("service: " + service);
        LOG.debug("service.getName(): " + service.getName());       

        LOG.debug("Found services: " + service.getServiceInfos().size());
        if (LOG.isDebugEnabled()) {
            LOG.debug("Service=" + service);
        }    

        // Sets the service invoker
        service.setInvoker(new ProviderServiceInvoker(serviceDescriptor));    

        if (LOG.isDebugEnabled()) {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();        
            CXFUtils.writeDefinitionOnOutputStream(service, baos);        
            LOG.debug(baos.toString());        
        }

        LOG.debug("<<<<< createService - end");
        return service;
    }
    
    /**
     * Creates the CXF service no specifying the interface name (gets it from the created service). 
     * 
     * @param serviceDescriptor
     * 
     * @return the service
     * 
     * @throws IOException
     * @throws WSDLException
     * @throws Jbi4CorbaException
     */
    public Service createService(ProviderServiceDescriptor serviceDescriptor) throws IOException, 
        WSDLException, Jbi4CorbaException {        
        return createService(serviceDescriptor, null);
    }   

}
