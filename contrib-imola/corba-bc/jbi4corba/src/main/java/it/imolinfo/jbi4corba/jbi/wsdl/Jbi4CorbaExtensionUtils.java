 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.jbi.wsdl;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

/**
 * Jbi4Corba wsdl extensions helper class.
 * @author marco
 *
 */
public class Jbi4CorbaExtensionUtils {
    
    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4CorbaExtensionUtils.class);
    
    @SuppressWarnings("unused")
    private static final Messages MESSAGES = 
    	Messages.getMessages(Jbi4CorbaExtensionUtils.class);
    
    /**
     * Default constructor.
     */
    public Jbi4CorbaExtensionUtils(){
    }
    
    /**
     * Removes all  The Jbi4Corba elements from a definition .
     *
     * @param   def  The WSDL to add SOAP elements. Must be not
     *                         <code>null</code>.
     * @throws  WSDLException  In case of error manipulating the
     *                         <code>Definition</code>.
     */
    @SuppressWarnings("unchecked")
	public static void removeCorbaElements(Definition def)
      throws WSDLException {
      LOG.debug("removeCorbaElements - Definition=" + def);

      Port port = null;
      WSDLFactory factory = WSDLFactory.newInstance();        

      ExtensionRegistry registry = factory.newPopulatedExtensionRegistry();
      Jbi4CorbaExtension.register(registry);

      // removes the jbi4corba namespace        
      String jbi4CorbaNamespace = null;
      for (Object namespaceKey: def.getNamespaces().keySet()) {
        LOG.debug("namespaceKey=" + namespaceKey);

        String value = (String) def.getNamespaces().get(namespaceKey);
        LOG.debug("NameSpace[" + namespaceKey + "]=" + value);

        if (value.equalsIgnoreCase(Jbi4CorbaExtension.NS_URI_JBI4CORBA)) {
          jbi4CorbaNamespace = (String) namespaceKey;
          LOG.debug("jbi4CorbaNamespace=" + jbi4CorbaNamespace);
        }
      }

      if (jbi4CorbaNamespace != null) {
        LOG.debug("FOUND jbi4corba namespace.");
        def.getNamespaces().remove(jbi4CorbaNamespace);
      } else {
        LOG.debug("jbi4corba namespace NOT found.");
      }
        
      LOG.debug("Start removing the Jbi4Corba extensibility elements");

      // Removes the Jbi4Corba Prt and bindings        
      for (Object serviceAsObject : def.getServices().values()) {
        Service service = (Service) serviceAsObject;
        LOG.debug("Service=" + service + "; Service.QName=" + service.getQName());

        for (Object obj : service.getPorts().values()) {
          port = (Port) obj;             
          Collection<Jbi4CorbaAddress> toRemoveFromPort 
          	= new ArrayList<Jbi4CorbaAddress>();

          LOG.debug("Port=" + port + "; Port.name" + port.getName());

          for (Object element : port.getExtensibilityElements()) {

            // Removes the jbi4corba address
            if (element instanceof Jbi4CorbaAddress) {
              LOG.debug("The element IS a Jbi4CorbaAddress. Element=" + element);
              Jbi4CorbaAddress jbi4CorbaAddress = (Jbi4CorbaAddress) element;
              LOG.debug("Found address (It will be removed):" + jbi4CorbaAddress);
              toRemoveFromPort.add(jbi4CorbaAddress);
            } else {
              LOG.debug("The element is NOT a Jbi4CorbaAddress. Element=" + element);
            }

            Collection<Jbi4CorbaBinding> toRemoveFromBinding = new ArrayList<Jbi4CorbaBinding>();
            // Removes the jbi4corba binding
            for (Object bindingEstension : port.getBinding().getExtensibilityElements()) {
              if (bindingEstension instanceof Jbi4CorbaBinding) {
                LOG.debug("bindingEstension instanceof Jbi4CorbaBinding.bindingEstension=" + bindingEstension);
                Jbi4CorbaBinding jbi4CorbaBinding = (Jbi4CorbaBinding)bindingEstension;
                LOG.debug("Found binding (It will be removed):" + jbi4CorbaBinding);
                toRemoveFromBinding.add(jbi4CorbaBinding);
              } else {
                LOG.debug("bindingEstension instanceof Jbi4CorbaBinding.bindingEstension=" + bindingEstension);
              }
            }

            LOG.debug("toRemoveFromBinding.size=" + toRemoveFromBinding.size());
            port.getBinding().getExtensibilityElements().removeAll(toRemoveFromBinding);
          }

          LOG.debug("toRemoveFromPort.size=" + toRemoveFromPort.size());
          port.getExtensibilityElements().removeAll(toRemoveFromPort);
        }
      }

    }
    
    
    /**
     * Gets the <code>Binding</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the service name
     * @param endpointName the endpoint name
     * 
     * @return the Binding
     */
    public static Binding getBinding(final Definition def,
            final String serviceName, final String endpointName) {
        final Service svc = def.getService(QName.valueOf(serviceName));
        
        if (LOG.isDebugEnabled()) {
            LOG.debug("Looking for service: " + serviceName + " in wsdl " + getWSDLStringFromDefinition(def));            
        }
        
        if (svc == null) {
        	LOG.debug("CRB000301_Service_not_found_in_WSDL_for_serviceName", 
            		new Object[]{serviceName});

            return null;
        }

        final Port port = svc.getPort(endpointName);

        if (port == null) {
        	LOG.debug("CRB000302_Port_not_found_in_WSDL_for_endpointName", 
            		new Object[]{endpointName});
            return null;
        } else {
            return port.getBinding();
        }
    }
    
    /**
     * Gets the <code>Service</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the service name
     * 
     * @return the Service
     */
    public static Service getService(final Definition def,
            final String serviceName) {
        final Service svc = def.getService(QName.valueOf(serviceName));        

        if (svc == null) {
            return null;
        }
        
        return svc;
    }    
    
    /**
     * Gets the <code>Jbi4CorbaBinding</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the service name
     * @param endpointName the endpoint name
     * 
     * @return the <code>Jbi4CorbaBinding</code>
     */
    @SuppressWarnings("unchecked")
    public static Jbi4CorbaBinding getCorbaBinding(final Definition def,
            final String serviceName, final String endpointName) {
        Jbi4CorbaBinding corbaBinding = null;
        final Binding binding = getBinding(def, serviceName, endpointName);

        if (binding != null) {
            final List extElems = binding.getExtensibilityElements();

            Iterator extIter = null;
            if (extElems != null) {
                extIter = extElems.iterator();
            }            

            while ((extIter != null) && extIter.hasNext() &&
                    (corbaBinding == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter
                .next();

                if (Jbi4CorbaBinding.class.isInstance(ee)) {
                    corbaBinding = (Jbi4CorbaBinding) ee;
                }
            }
        }

        return corbaBinding;
    }

     /**
     * Gets the <code>Jbi4CorbaAddress</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the service name
     * @param endpointName the endpoint name
     * 
     * @return the <code>Jbi4CorbaAddress</code>
     */
    @SuppressWarnings("unchecked")
    public static Jbi4CorbaAddress getCorbaAddress(final Definition def,
            final String serviceName, final String endpointName) {
        Jbi4CorbaAddress address = null;
        final Service svc = def.getService(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(QName.valueOf(endpointName)
                .getLocalPart());

        if (port != null) {
            final List extElems = port.getExtensibilityElements();

            Iterator extIter = null;
            
            if (extElems != null) {
                extIter = extElems.iterator();
            }

            while ((extIter != null) && extIter.hasNext() && (address == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter
                .next();

                if (Jbi4CorbaAddress.class.isInstance(ee)) {
                    address = (Jbi4CorbaAddress) ee;
                }
            }
        }
        return address;
    }    
    
    /**
     * Gets the <code>PortType</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the Service Name
     * @param endpointName the Endpoint name
     * 
     * @return the WSDL port type
     */
    public static PortType getPortType(final Definition def,
            final String serviceName, final String endpointName) {
        final Service svc = def.getService(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(QName.valueOf(endpointName)
                .getLocalPart());

        Binding binding = null;
        if (port == null) {
            return null;
        } else {
            binding = port.getBinding();
        }
        PortType portType = null;
        if (binding != null) {           
            portType = binding.getPortType();
        }
        return portType;
    }        

    
    /**
     * Gets the Jbi4Ejb extended WSDL writer.
     * 
     * @return the extended WSDL writer
     * 
     * @throws WSDLException
     */
    public static WSDLWriter getExtendedWSDLWriter() throws WSDLException {    
        WSDLFactory factory = WSDLFactory.newInstance();
        WSDLWriter writer = factory.newWSDLWriter();  
        ExtensionRegistry registry = factory.newPopulatedExtensionRegistry(); 
        Jbi4CorbaExtension.register(registry);              
        return writer;
    }
    
    /**
     * Gets the WSDL string from the WSDL definition.
     * USE THIS METHOD ONLY FOR LOGGING!
     * 
     * @param wsdl
     * 
     * @return the WSDL string from definition
     */
    public static String getWSDLStringFromDefinition(Definition wsdl) {
        // Asserts...
        StringWriter strWriter = new StringWriter();   
        try {
            getExtendedWSDLWriter().writeWSDL(wsdl, strWriter);
        } catch (WSDLException e) {
        	LOG.warn("CRB000303_Warning_in_getting_WSDL_string_from_WSDL_definition", 
            		new Object[]{e.getMessage()});
        }
        return strWriter.toString();                
    }
    
 
}
