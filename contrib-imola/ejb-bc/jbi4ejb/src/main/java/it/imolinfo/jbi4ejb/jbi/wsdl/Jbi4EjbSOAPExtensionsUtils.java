/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.wsdl;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.EJBDeployException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.component.runtime.DefaultMessageExchangeHandler;
import it.imolinfo.jbi4ejb.runtime.ejbproxy.EJBProxyUtils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap.SOAPOperation;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import java.io.StringWriter;

/**
 * Utility class to add the SOAP extensions (needed by xfire to correctly generate the sources).
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>s
 */
public final class Jbi4EjbSOAPExtensionsUtils {      

    /** The logger. */
    private static final Logger LOG
    = LoggerFactory.getLogger(EJBProxyUtils.class);    
    private static final Messages MESSAGES 
    = Messages.getMessages(Jbi4EjbSOAPExtensionsUtils.class);   

    /**
     * The standard SOAP namespace URI used in WSDL files.
     */
    private static final String SOAP_NAMESPACE_URI
    = "http://schemas.xmlsoap.org/wsdl/soap/";

    /**
     * The standard SOAP address element name used in WSDL files.
     */
    private static final QName SOAP_ADDRESS_QNAME
    = new QName(SOAP_NAMESPACE_URI, "address");

    /**
     * The standard SOAP binding element name used in WSDL files.
     */
    private static final QName SOAP_BINDING_QNAME
    = new QName(SOAP_NAMESPACE_URI, "binding");

    /**
     * The standard SOAP body element name used in WSDL files.
     */
    private static final QName SOAP_BODY_QNAME
    = new QName(SOAP_NAMESPACE_URI, "body");

    /**
     * The standard SOAP operation element name used in WSDL files.
     */
    private static final QName SOAP_OPERATION_QNAME
    = new QName(SOAP_NAMESPACE_URI, "operation");

    /**
     * Instantiates a new jbi4 ejb SOAP extensions utils.
     */
    private Jbi4EjbSOAPExtensionsUtils() {}

    
    /**
     * Creates a wsdl file, adding the SOAP extension elements.
     * 
     * @param wsdl the wsdl file
     * 
     * @return the file
     * 
     * @throws EJBDeployException if some problem occurs
     */
    public static File addSoapElements(final File wsdl) throws EJBDeployException {
        
        File wsdlWithSoap = new File(wsdl.getAbsolutePath()+"_soap_");       

        Definition wsdlDef = null;
        try {
            // Reads the definition
            wsdlDef = readWsdl(wsdl);

            // Adds the SOAP elements
            addSoapElements(wsdlDef);

            // Wrotes the definition on the file
            writeWsdl(wsdlDef, wsdlWithSoap);


        } catch (WSDLException wex) {
        	String msg=MESSAGES.getString("EJB000402_Jbi4EjbSOAPExtensionsUtils", new Object[]{wex.getMessage()});
            LOG.error(msg);
            throw new EJBDeployException(msg);   

        }        

        return wsdlWithSoap;
    }


    /**
     * Adds SOAP elements to the specified WSDL if they are not present.
     *
     * @param   def            the WSDL to add SOAP elements. Must be not
     *                         <code>null</code>.
     * @throws  WSDLException  in case of error manipulating the
     *                         <code>Definition</code>.
     */
    public static void addSoapElements(final Definition def)
    throws WSDLException {
        
         WSDLFactory factory = WSDLFactory.newInstance(); 
         
        StringWriter strWriter = new StringWriter();   
         WSDLWriter writer = factory.newWSDLWriter();  
         
        ExtensionRegistry registry = factory.newPopulatedExtensionRegistry();            
        Jbi4EjbExtension.register(registry);
        writer.writeWSDL(def, strWriter);
        String wsdlString = strWriter.toString();
       LOG.debug("wsdlString: " + wsdlString);
        
        boolean soapFound = false;
        Port port = null;
              
        LOG.debug("definition: "+def);
        LOG.debug("services: "+def.getServices().size());
        LOG.debug("bindings: "+def.getBindings().size());
        
        for (Object service : def.getServices().values()) {
            LOG.debug("ports: "+((Service) service).getPorts());
            for (Object obj : ((Service) service).getPorts().values()) {
                port = (Port) obj;
                LOG.debug("port found:" + port.getName()); 
                
                for (Object element : port.getExtensibilityElements()) {
                    if (element instanceof SOAPAddress) {
                        soapFound = true;
                        break;
                    }
                }
            }
        }
        
            LOG.debug("port: "+port);
            LOG.debug("port.getBinding(): "+port.getBinding());
            LOG.debug("port.getBinding().getBindingOperations(): "+port.getBinding().getBindingOperations());
            LOG.debug("port.getBinding().getBindingOperations().get(0): "+port.getBinding().getBindingOperations().get(0));

        if (!soapFound) {
           // ExtensionRegistry registry
            //= factory.newPopulatedExtensionRegistry();
            SOAPAddress soapAddress = (SOAPAddress)
            registry.createExtension(Port.class, SOAP_ADDRESS_QNAME);
            SOAPBinding soapBinding = (SOAPBinding)
            registry.createExtension(Binding.class, SOAP_BINDING_QNAME);
            SOAPBody inputBody = (SOAPBody) registry.createExtension(
                    BindingInput.class, SOAP_BODY_QNAME);
            SOAPOperation soapOperation
            = (SOAPOperation) registry.createExtension(
                    BindingOperation.class, SOAP_OPERATION_QNAME);
            
            // XXX Take first BindingOperation: what if they're more than one?
            LOG.debug("port: "+port);
            LOG.debug("port.getBinding(): "+port.getBinding());
            LOG.debug("port.getBinding().getBindingOperations(): "+port.getBinding().getBindingOperations());
            LOG.debug("port.getBinding().getBindingOperations().get(0): "+port.getBinding().getBindingOperations().get(0));
            BindingOperation bo = (BindingOperation)
            port.getBinding().getBindingOperations().get(0);

            soapAddress.setLocationURI("http://localhost/fake_location");
            port.addExtensibilityElement(soapAddress);

            soapBinding.setTransportURI("http://schemas.xmlsoap.org/soap/http");
            soapBinding.setStyle("document");
            port.getBinding().addExtensibilityElement(soapBinding);

            soapOperation.setSoapActionURI("");
            bo.addExtensibilityElement(soapOperation);

            inputBody.setUse("literal");
            addSoapBodyIfNotPresent(bo.getBindingInput(), inputBody);
            addSoapBodyIfNotPresent(bo.getBindingOutput(), inputBody);
        }

    }

    /**
     * Adds the SOAP body if not present.
     * @param bindingInput the binding input
     * @param soapBody the soap body
     */
    private static void addSoapBodyIfNotPresent(
            final BindingInput bindingInput, final SOAPBody soapBody) {
        for (Object obj : bindingInput.getExtensibilityElements()) {
            if (obj instanceof SOAPBody) {
                return;
            }
        }
        bindingInput.addExtensibilityElement(soapBody);
    }

    /**
     * Adds the SOAP body if not present.
     * @param bindingOutput the binding output
     * @param soapBody the soap body
     */
    private static void addSoapBodyIfNotPresent(
            final BindingOutput bindingOutput, final SOAPBody soapBody) {
        for (Object obj : bindingOutput.getExtensibilityElements()) {
            if (obj instanceof SOAPBody) {
                return;
            }
        }
        bindingOutput.addExtensibilityElement(soapBody);
    }    

    /**
     * Reads a <code>Definition</code> from a <code>File</code>.
     * @param wsdlFile the WSDL file
     * @throws javax.wsdl.WSDLException if some problem occurs
     * @return the WSDL Definitions
     */
    private static Definition readWsdl(final File wsdlFile) throws WSDLException {
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        ExtensionRegistry registry = wsdlFactory
            .newPopulatedExtensionRegistry();
        final WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory)
        .newWSDLReader();
        reader.setFeature(Constants.FEATURE_VERBOSE, false);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        
        Jbi4EjbExtension.register(registry);
        LOG.debug("Extension QName: " + Jbi4EjbExtension.NS_URI_JBI4EJB);
        reader.setExtensionRegistry(registry);
       
        final Definition def = reader.readWSDL(wsdlFile.getAbsolutePath());
        return def;
    }
    
    /**
     * Writes a <code>Definition</code> to a <code>File</code>.
     * @param def the WSDL definition
     * @param wsdlFile the file  where the WSDL is written
     * @return
     * @throws javax.wsdl.WSDLException if soem problem occurs in reading the WSDL definition
     * @throws EJBDeployException if some generic problem occurs
     */
    private static void writeWsdl(Definition def, final File wsdlFile) throws WSDLException, EJBDeployException {
        
        FileWriter fr = null;
        try {
            fr = new FileWriter(wsdlFile);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000402_Jbi4EjbSOAPExtensionsUtils", new Object[]{e.getMessage()});
            LOG.error(msg);
            throw new EJBDeployException(msg); 
        }
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        ExtensionRegistry registry = wsdlFactory
            .newPopulatedExtensionRegistry();
        final WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory)
            .newWSDLReader();
        reader.setFeature(Constants.FEATURE_VERBOSE, false);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        Jbi4EjbExtension.register(registry);
        LOG.debug("Extension QName: " + Jbi4EjbExtension.NS_URI_JBI4EJB);
        reader.setExtensionRegistry(registry);               
        
        WSDLWriter writer = wsdlFactory.newWSDLWriter();
        LOG.debug("Writing WSDL: " + wsdlFile.getAbsolutePath());
        writer.writeWSDL(def, fr);
        try {
            fr.flush();
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000402_Jbi4EjbSOAPExtensionsUtils", new Object[]{e.getMessage()});
            LOG.error(msg);
            throw new EJBDeployException(msg); 
        }
        
    }

}
