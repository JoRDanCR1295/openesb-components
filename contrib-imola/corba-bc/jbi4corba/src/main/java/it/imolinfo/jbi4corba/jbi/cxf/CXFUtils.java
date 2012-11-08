 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.cxf;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

import org.apache.cxf.Bus;
import org.apache.cxf.BusException;
import org.apache.cxf.BusFactory;
import org.apache.cxf.binding.soap.SoapBindingConstants;
import org.apache.cxf.binding.soap.interceptor.RPCInInterceptor;
import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.interceptor.BareInInterceptor;
import org.apache.cxf.interceptor.BareOutInterceptor;
import org.apache.cxf.interceptor.DocLiteralInInterceptor;
import org.apache.cxf.interceptor.FaultOutInterceptor;
import org.apache.cxf.interceptor.StaxInInterceptor;
import org.apache.cxf.interceptor.StaxOutInterceptor;
import org.apache.cxf.interceptor.WrappedInInterceptor;
import org.apache.cxf.interceptor.WrappedOutInterceptor;
import org.apache.cxf.jaxws.JaxWsClientFactoryBean;
import org.apache.cxf.jaxws.interceptors.HolderInInterceptor;
import org.apache.cxf.jaxws.interceptors.HolderOutInterceptor;
import org.apache.cxf.jaxws.interceptors.WrapperClassInInterceptor;
import org.apache.cxf.jaxws.interceptors.WrapperClassOutInterceptor;
import org.apache.cxf.phase.PhaseInterceptorChain;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.factory.AbstractServiceConfiguration;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.service.model.MessageInfo;
import org.apache.cxf.service.model.MessagePartInfo;
import org.apache.cxf.wsdl11.ServiceWSDLBuilder;
import org.w3c.dom.Document;

/**
 * Utility methods to manage CXF classes.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>s
 */
public final class CXFUtils {
    
    private static Bus bus;
   
    /** The logger. */
    private static final Logger LOG = 
    	LoggerFactory.getLogger(CXFUtils.class);
    
    /** The messages. */
    private static final Messages MESSAGES = 
    	Messages.getMessages(CXFUtils.class);

          
    public  static Bus setupBus() throws BusException {
        
        if (bus == null) {            
            bus = createBus();
        }
        
        return bus;
    }
    
    /**
     * The Bus can be null...
     * @return
     * @throws BusException
     */
    public static Bus getBus() {        
        return bus;
    }
    
    /**
     * Creates the CXF "Bus".
     */
    protected static Bus createBus() throws BusException {     
        Bus bus = null;
        // Get the classloader (to avoid OpenESB classloading problems)
        ClassLoader threadClassLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader componentClassLoader = new CXFUtils().getClass().getClassLoader();
                       
        try {            
            Thread.currentThread().setContextClassLoader(componentClassLoader);            
            bus = BusFactory.newInstance().createBus();                       
            
        } finally {
            // Sets the thread classloader
            Thread.currentThread().setContextClassLoader(threadClassLoader);
        }
              
        return bus; 
    }
    /**
     * Creates the CXF Service factory.
     */
    public static JaxWsClientFactoryBean getJaxWsClientFactoryBean() throws Jbi4CorbaException {        
        if (bus == null) {
            try {
                setupBus();
            } catch (BusException bex) {
            	String msg=MESSAGES.getString("CRB000900_Error_in_CXF_Bus_init");
                LOG.error(msg,bex);
                throw new Jbi4CorbaException(msg,bex);
            }
        }
        JaxWsClientFactoryBean factory = new JaxWsClientFactoryBean();    
        factory.setBus(bus);
        
        // Sets the configuration to force message part minOccurs/maxOccurs to 1
        List<AbstractServiceConfiguration> customServiceConfigurations = 
            new ArrayList<AbstractServiceConfiguration>();
        customServiceConfigurations.add(new Jbi4CorbaServiceConfiguration());
        factory.getServiceFactory().setServiceConfigurations(customServiceConfigurations);
        
        return factory;
    }        
                   
    /**
     * Gets the WSDL document.
     * 
     * @param service
     * 
     * @return the WSDL document
     * 
     * @throws WSDLException
     */
    protected static Document getWSDLDocument(Service service) throws WSDLException {                
        ServiceWSDLBuilder wsdlBuilder = 
            new ServiceWSDLBuilder(bus, service.getServiceInfos().get(0));
        wsdlBuilder.setUseSchemaImports(true);
        Definition definition = wsdlBuilder.build();
        WSDLWriter writer = WSDLFactory.newInstance().newWSDLWriter();        
        return writer.getDocument(definition);
    }
    
    /**
     * Gets the WSDL definition.
     * 
     * @param service
     * 
     * @return the WSDL definition
     * 
     * @throws WSDLException
     */
    public static Definition getWSDLDefinition(Service service) throws WSDLException {
        LOG.debug("BUS: " + bus);
        
        ServiceWSDLBuilder wsdlBuilder = new ServiceWSDLBuilder(bus, service.getServiceInfos());
        // wsdlBuilder.setUseSchemaImports(true);        
        
        Definition def = wsdlBuilder.build(); 
        return def;
    }
    
    /**
     * Writes out the service definition on the output stream 
     * @param out
     * @throws WSDLException 
     */
    public static void writeDefinitionOnOutputStream(Service service, OutputStream out) throws WSDLException {        
        Definition def = getWSDLDefinition(service);
        WSDLWriter wsdlWriter = WSDLFactory.newInstance().newWSDLWriter();
        wsdlWriter.writeWSDL(def, out);        
    }
        
    /**
     * Gets THE FIRST ENDPOINT interface name.
     * @param service
     * @return
     */
    public static QName getInterfaceNameFromFirstEndpoint(Service service) {
        Collection<Endpoint> serviceEndpoints = (service.getEndpoints()).values();
        Endpoint endpoint = (Endpoint)serviceEndpoints.iterator().next();
        QName interfaceName = endpoint.getEndpointInfo().getBinding().getInterface().getName();
        return interfaceName;
    }
        
    /**
     * Gets the EndpointInfo for the FIRST (should be the only...) Endpoint.
     * @param service
     * @return
     * @throws IOException
     */
    public static EndpointInfo getEndpointInfo(Service service) throws IOException {        
        Collection<Endpoint> serviceEndpoints = (service.getEndpoints()).values();
        LOG.debug("Endpoints of service: " + serviceEndpoints.size());
        Endpoint endpoint = (Endpoint)serviceEndpoints.iterator().next();
        EndpointInfo ei = endpoint.getEndpointInfo();        
        return ei;
    }       
            
    /**
     * Gets the endpoint from service endpoint.
     */
    public static Endpoint getEndpoint(Service service, EndpointInfo ei) {
        for(int i=0;i<service.getEndpoints().size();i++){
            LOG.debug("ENP ==> "+service.getEndpoints().keySet().toArray()[i].toString());
        }
        return service.getEndpoints().get(ei.getName());        
    }
    
    /**
     * Return the correct binding style for the binding, Document o RPC.
     * We don'thave  SOAP binding extensions, so we have to calculate that 
     * from the message part. See Section 7 of the SOAP 1.1 specification.
     * (notice that the WSDL is always Document/literal).
     * @param bio
     * @return <code>SoapBindingConstants.PARAMETER_STYLE_WRAPPED</code> or <code>SoapBindingConstants.PARAMETER_STYLE_BARE</code>
     */
    public static String getBindingStyle(BindingOperationInfo bio) {                	
        String bindingStyle = SoapBindingConstants.BINDING_STYLE_DOC;     
        if (bio.getOperationInfo() != null) {
           MessageInfo mi = bio.getOperationInfo().getInput();
            MessagePartInfo mpi = mi.getMessagePart(0);
            // There
            if ((mi.getMessageParts().size() != 1)  || 
            	(!mpi.isElement()) ) {
            	bindingStyle = SoapBindingConstants.BINDING_STYLE_RPC;        	
            } 
        }
        return bindingStyle;
    }    
  	
    /**
     * This method creates the CXF OUT Interceptors chain and process the message.
     * 
     * @param parameterStyle
     * @param bindingStyle
     * @param isProviderOrConsumerFromIDL
     */
  	public static void populateOutInterceptors(PhaseInterceptorChain outInterceptorChain, 
  			String parameterStyle, String bindingStyle, boolean isProviderOrConsumerFromIDL) {
  	  		
          outInterceptorChain.add(new StaxOutInterceptor());                                 
          outInterceptorChain.add(new HolderOutInterceptor());
       
          if (isProviderOrConsumerFromIDL) {
    		  // In the "from IDL" case the WSDL is always wrapped
              // For Jax-ws
              outInterceptorChain.add(new WrapperClassOutInterceptor());        	  
        	  outInterceptorChain.add(new Jbi4CorbaWrappedOutInterceptor());
        	  outInterceptorChain.add(new Jbi4CorbaBareOutInterceptor());
          } else {         
        	  // Must check if the WDSL is wrapped
              outInterceptorChain.add(new BareOutInterceptor());              
              if (parameterStyle.equals(SoapBindingConstants.PARAMETER_STYLE_WRAPPED)) {            
                  outInterceptorChain.add(new WrappedOutInterceptor());
              }	
          }
           		
  	} 
  	
    /**
     * This method creates the CXF OUT Interceptors chain and process the message.
     * 
     * @param parameterStyle
     * @param message
     */
	public static void populateFaultInterceptors(PhaseInterceptorChain faultInterceptorChain, 
			String parameterStyle, String bindingStyle) {
		
        // The FaultOutInterceptor creates and fills the XML details from the cause Exception                 
        faultInterceptorChain.add(new StaxOutInterceptor());
        // faultInterceptorChain.add(new Soap12FaultOutInterceptor());
        faultInterceptorChain.add(new FaultOutInterceptor());						
	}     
  	

	
    /**
     * This method creates the CXF OUT Interceptors chain and process the message.
     * The interceptor order is important! First the Stream is created, than the message is unwrapped 
     * and then 
     * converted into objects. Notice that with a custom binding, we could fix the interceptor 
     * execution order statically
     * 
     * @param parameterStyle
     * @param bindingStyle
     * @param message
     */
  	public static void populateInInterceptorsForConsumer(PhaseInterceptorChain inInterceptorChain, 
  			String parameterStyle, String bindingStyle, boolean isFromIDL) {
  		
  		/**
  		 * DON'T CHANGE THE INTERCEPTORE DECLARATION ORDER (IS A CXF ISSUE!)
  		 */
  		inInterceptorChain.add(new StaxInInterceptor());          				
  		
  		inInterceptorChain.add(new HolderInInterceptor());
 
  		if (parameterStyle.equals(SoapBindingConstants.PARAMETER_STYLE_WRAPPED)) {
  			inInterceptorChain.add(new WrapperClassInInterceptor());  	    	
  	    } 
  		// If it's from IDL, we generate the WSDL (is't doc-literal).
  		if (isFromIDL) {
  			inInterceptorChain.add(new WrappedInInterceptor());	
  			inInterceptorChain.add(new DocLiteralInInterceptor());
  		} else {
  	      if (parameterStyle.equals(SoapBindingConstants.PARAMETER_STYLE_WRAPPED)) {
  	          inInterceptorChain.add(new WrappedInInterceptor());
  	      }
  	      inInterceptorChain.add(new BareInInterceptor());
  		}

  	} 	
  	
    /**
     * This method creates the CXF Interceptors chain and process the message.
     *                
     * @param parameterStyle
     * @param message
     */
	public static void populateInInterceptorsForProvider(PhaseInterceptorChain inInterceptorChain, 
			String parameterStyle, String bindingStyle) { 
		
		inInterceptorChain.add(new StaxInInterceptor());
		// For jax-ws
		inInterceptorChain.add(new WrapperClassInInterceptor());			
		
		inInterceptorChain.add(new HolderInInterceptor());
		inInterceptorChain.add(new Jbi4CorbaServiceInvokerInterceptor());
		
		if (bindingStyle.equals(SoapBindingConstants.BINDING_STYLE_RPC)) {
			inInterceptorChain.add(new RPCInInterceptor());
		} else {            	
		    inInterceptorChain.add(new DocLiteralInInterceptor());
		} 		
	}  
  	
  	/**
     * Return the correct parameter style for the binding, Wrapped or Bare 
     * (notice that the WSDL is always Document/literal).
     * @param bio
     * @return <code>SoapBindingConstants.PARAMETER_STYLE_WRAPPED</code> or <code>SoapBindingConstants.PARAMETER_STYLE_BARE</code>
     */
    public static String getParameterStyle(BindingOperationInfo bio) {
            
        String parameterStyle = SoapBindingConstants.PARAMETER_STYLE_WRAPPED;
        if (bio!= null) {
            if (bio.getUnwrappedOperation() == null) {
                parameterStyle = SoapBindingConstants.PARAMETER_STYLE_BARE;
            }
        }        
        return parameterStyle;
    }

    
}
