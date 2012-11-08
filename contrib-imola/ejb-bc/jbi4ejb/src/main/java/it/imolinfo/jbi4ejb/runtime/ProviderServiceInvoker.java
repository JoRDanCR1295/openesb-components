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
import it.imolinfo.jbi4ejb.exception.EJBInvokeException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.processor.transform.StringSource;
import it.imolinfo.jbi4ejb.runtime.ejbproxy.StatelessEJBProxy;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.codehaus.xfire.MessageContext;
import org.codehaus.xfire.fault.XFireFault;
import org.codehaus.xfire.service.invoker.Invoker;

/**
 * The Service invoker, wraps th eEJBProxy call.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class ProviderServiceInvoker implements Invoker {

    /** The Logger. */
    private static final Logger LOG
        = LoggerFactory.getLogger(ProviderServiceInvoker.class);
    private static final Messages MESSAGES
    = Messages.getMessages(ProviderServiceInvoker.class);

    /** The service descriptor. */
    private ProviderServiceDescriptor serviceDescriptor;

    /**
     * Creates the ProviderServiceInvoker.
     * 
     * @param serviceDescriptor the service descriptor
     */
    public ProviderServiceInvoker(
            final ProviderServiceDescriptor serviceDescriptor) {
        this.serviceDescriptor = serviceDescriptor;
    }

    /**
     * The service class invocation.
     * 
     * @param method
     *            The method to invoke.
     * @param args
     *            The method params
     * @param messageContext
     *            The message context
     * @return the result Object
     * 
     * @throws XFireFault the service fault
     */
    public Object invoke(final Method method, final Object[] args,
            final MessageContext messageContext) throws XFireFault {

        LOG.debug(">>>>> invoke - begin");
        
        StatelessEJBProxy ejbProxy = serviceDescriptor.getEjbProxy();        

        Object result = null;

        try {
            // Invocation
            result = ejbProxy.invokeMethod(method, args);
            if (LOG.isDebugEnabled()) {
                LOG.debug("Result: " + ReflectionToStringBuilder.toString(result));                
            }
        } catch (IllegalAccessException e) {
            // TODO i18n
            //String msg = "Error in invoking the method: " + method + " on the remote object: " + ejbProxy.getRemoteInterfaceClassName();  
            //LOG.error(e.getMessage());
            //throw new XFireFault(msg, e.getCause(), XFireFault.SOAP11_SERVER);
        	
        } catch (InvocationTargetException e) {
            // TODO i18n
            String msg = "Error in invoking the method: " + method + " on the remote object: " + ejbProxy.getRemoteInterfaceClassName();  
            LOG.error(e.getMessage());
            throw new XFireFault(msg, e.getCause(), XFireFault.SOAP11_SERVER); 

        } catch (EJBInvokeException ex) {
            // This exception should be thrown only if the proxy is NOT correctly initialized
            // TODO i18n
            String msg = "Error in invoking the method: " + method + " on the remote object: " + ejbProxy.getRemoteInterfaceClassName();  
            LOG.error(ex.getMessage());
            throw new XFireFault(msg, ex.getCause(), XFireFault.SOAP11_SERVER);                           
        } finally {
            LOG.debug(">>>>> invoke - end");
        }
        return result;
    }

}
