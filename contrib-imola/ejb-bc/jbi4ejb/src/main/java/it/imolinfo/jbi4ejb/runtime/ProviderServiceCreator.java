/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.runtime;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4ejb.jbi.xfire.EjbTransport;

import java.util.HashMap;
import java.util.Map;

import org.codehaus.xfire.XFire;
import org.codehaus.xfire.aegis.AegisBindingProvider;
import org.codehaus.xfire.aegis.type.TypeMapping;
import org.codehaus.xfire.annotations.AnnotationServiceFactory;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.service.binding.ObjectServiceFactory;
import org.codehaus.xfire.soap.SoapConstants;

/**
 * The ProviderServiceCreator.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class ProviderServiceCreator {

    /** Logger. */
    private static final Logger LOG = LoggerFactory
            .getLogger(ProviderServiceCreator.class);

    /**
     * Empty Constructor.
     */
    public ProviderServiceCreator() {}

    /**
     * Creates the xfire service.
     * 
     * @param serviceDescriptor the servcie descriptr
     * @param xfire Xfire engine
     * 
     * @return the service
     */
    @SuppressWarnings("unchecked")
    public Service createJbiService(
            ProviderServiceDescriptor serviceDescriptor, XFire xfire) {
        
        LOG.debug("Creating xfire service: " + serviceDescriptor.getServiceName());

        ObjectServiceFactory factory = new ObjectServiceFactory(xfire.getTransportManager(), null);

        Map<String, Object> props = new HashMap<String, Object>();

        props.put(ObjectServiceFactory.PORT_TYPE, serviceDescriptor
                .getPortTypeName());
        props.put(ObjectServiceFactory.STYLE, SoapConstants.STYLE_WRAPPED);
        props.put(ObjectServiceFactory.USE, SoapConstants.USE_LITERAL);

        props.put(AnnotationServiceFactory.ALLOW_INTERFACE, Boolean.TRUE);

        factory.getSoap11Transports().clear();
        factory.getSoap12Transports().clear();
        factory.getSoap11Transports().add(EjbTransport.EJB_BINDING);

        Service service = null;

        service = factory.create(serviceDescriptor.getEjbProxy()
                .getRemoteInterfaceClass(), serviceDescriptor.getServiceName()
                .getLocalPart(), serviceDescriptor.getServiceName()
                .getNamespaceURI(), props);

        // Adds the invoker
        service.setInvoker(new ProviderServiceInvoker(serviceDescriptor));
        

        // XFire bug : http://jira.codehaus.org/browse/XFIRE-462 - BEGIN

        AegisBindingProvider abp = (AegisBindingProvider) factory
                .getBindingProvider();

        TypeMapping typeMapping = abp.getTypeMapping(service);
        typeMapping.register(new ByteType());
        // XFire bug --------------------------------------------- END

        return service;
    }

}
