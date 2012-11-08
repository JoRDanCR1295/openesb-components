/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.endpoint;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4ejb.exception.Jbi4EjbException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.component.runtime.AbstractComponentLifeCycle;
import it.imolinfo.jbi4ejb.jbi.component.runtime.RuntimeHelper;
import it.imolinfo.jbi4ejb.processor.ProviderExchangeProcessor;
import it.imolinfo.jbi4ejb.runtime.ProviderServiceCreator;
import it.imolinfo.jbi4ejb.runtime.ejbproxy.StatelessEJBProxy;
import it.imolinfo.jbi4ejb.runtime.ejbproxy.StatelessEJBProxyFactory;
import it.imolinfo.jbi4ejb.webservice.generator.Util;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.List;

import javax.xml.namespace.QName;

import org.codehaus.xfire.XFire;
import org.codehaus.xfire.service.Service;

/**
 * The Class Jbi4EjbProviderEndpoint.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */
public class Jbi4EjbProviderEndpoint extends Jbi4EjbEndpoint {
    
    private static final long serialVersionUID = -7829120677780408268L;

    /** The Logger. */
    private static final Logger LOG
    = LoggerFactory.getLogger(Jbi4EjbProviderEndpoint.class);
    final Messages MESSAGES = Messages.getMessages(Jbi4EjbProviderEndpoint.class);

    /** The service descriptor. */
    private ProviderServiceDescriptor serviceDescriptor;

    /** The xfire service. */
    private Service xfireService;
    
    /**
     * Instantiates a new jbi4 ejb provider endpoint.
     * 
     * @param serviceName the service name 
     * @param endpointName the endpoint name
     * 
     * @throws Jbi4EjbException
     *             if some problem occurs
     */
    public Jbi4EjbProviderEndpoint(QName serviceName, String endpointName) throws Jbi4EjbException {
        super(serviceName, endpointName);
        this.setExchangeProcessor(new ProviderExchangeProcessor(this));
    }    
    

    /* (non-Javadoc)
     * @see it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbEndpoint#registerService()
     */
    
    /**
     * Creates the EJB proxy and registers the xfire service.
     * @throws Jbi4EjbException if some problem occurs
     */
    public void registerService() throws Jbi4EjbException {
        LOG.debug("Registering xfire service for: " + this.getServiceName());
        
        XFire xfire = this.getSuManager().getLifeCycle().getXfire();

        File wsdl = this.getEndpointWSDL();

        // Creates the deploy directory
        String deployDirectoryName = serviceDescriptor.getComponentRootPath() + "/" + serviceDescriptor.hashCode();
        LOG.debug("Starting deploy using root path: " + deployDirectoryName);
        File rootDir = new File(deployDirectoryName);
        if (!rootDir.exists()) {
            boolean result = rootDir.mkdir();
            if (!result) {
            	String msg=MESSAGES.getString("EJB000301_Error_creating_working_directory", new Object[] {rootDir.getAbsolutePath()});
                LOG.error(msg);
                throw new Jbi4EjbException(msg);

            } else {
                LOG.debug("created working directory: " + rootDir.getAbsolutePath());
            }
        }

        // Creates the jar list to compile the sources                
        List<String> jarFilesName = Util.prepareClassPath(RuntimeHelper.getComponentContext().getInstallRoot());

        // Creates the EJB proxy class        
        StatelessEJBProxy ejbProxy = StatelessEJBProxyFactory.createEJBProxy(wsdl, serviceDescriptor, rootDir, jarFilesName);

        // Sets the ejbProxy on the service descriptor
        serviceDescriptor.setEjbProxy(ejbProxy);        

        // Generates the xfire service
        ProviderServiceCreator serviceCreator = new ProviderServiceCreator();

        // Generate the WSDL using xfire. The xfire service MUST be created
        xfireService = serviceCreator.createJbiService(this.serviceDescriptor,
                xfire);
        xfire.getServiceRegistry().register(xfireService);
                
        // Logs out the xfire service WSDL
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        LOG.debug("xfireService:" + xfireService);
        xfire.generateWSDL(xfireService.getSimpleName(), baos);
        if (LOG.isDebugEnabled()) {
          LOG.debug("WSDL:\n------------------\n"
                  + baos.toString()
                  + "\n------------------");
        }

    }


    /**
     * Unregisters the service.
     * @see it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbEndpoint#unregisterService()
     * @throws Jbi4EjbException if some problem occurs
     */
    public void unregisterService() throws Jbi4EjbException {
        // DO nothing
    	LOG.info("EJB000302_unregister_Service", new Object[]{this.getServiceName()}, new Object[]{this.getEndpointName()});
    }      

    /**
     * Validate the endpoint.
     * (now do noting)
     * @throws Jbi4EjbException if some problem occurs
     */
    public void validate() throws Jbi4EjbException {
        // Do nothing
    }

    /**
     * Gets the service descriptor.
     * 
     * @return the service descriptor
     */
    public ProviderServiceDescriptor getServiceDescriptor() {
        return serviceDescriptor;
    }

    /**
     * Sets the service descriptor.
     * 
     * @param serviceDescriptor
     *            the new service descriptor
     */
    public void setServiceDescriptor(ProviderServiceDescriptor serviceDescriptor) {
        this.serviceDescriptor = serviceDescriptor;
    }   

    /**
     * Gets the xfire service.
     * 
     * @return the xfire service
     */
    public Service getXfireService() {
        return xfireService;
    }

}
