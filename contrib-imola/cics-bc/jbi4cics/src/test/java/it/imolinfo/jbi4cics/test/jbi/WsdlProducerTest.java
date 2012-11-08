/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4cics.test.jbi;

import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsAddress;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsBinding;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension;

import java.io.File;
import java.io.FileWriter;
import java.io.StringWriter;

import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

import junit.framework.TestCase;

/**
 * Test the Wsdl extension serializer
 * @author amedeocannone, marcopiraccini
 */
public class WsdlProducerTest extends TestCase {
        
    
    public static final String wsdlDir = "src/test/etc/wsdl";
    
    public static final String rootPath = "target/test-wsdl-ext";
    
    public static final String serviceUnitName = "testServiceUnit";      
    
    public WSDLFactory factory = null;
    
    public WSDLReader reader = null;
    
    public WSDLWriter writer = null;
    
    public ExtensionRegistry registry;
    
    String tns = "it.imolinfo.Jbi4Cics.test.webservice.generator";
    
    public WsdlProducerTest() {}

    public WsdlProducerTest(String name) {
        super(name);
        File dir = new File(rootPath);
        dir.mkdir();
    }
        
    
    public void setUp() {
        try {
            factory = WSDLFactory.newInstance();
            reader = factory.newWSDLReader();
            writer = factory.newWSDLWriter();
            registry = factory.newPopulatedExtensionRegistry();        
            WSDLReader reader = factory.newWSDLReader();
            Jbi4CicsExtension.register(registry);      
            reader.setExtensionRegistry(registry);
            
        } catch (WSDLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    public void testWSDLSerializer() {          

        try {
            Definition def = reader.readWSDL(wsdlDir,"EchoService.wsdl");            
            
            // Sets the registry
            def.setExtensionRegistry(registry);
           
            // Adds the namespace
            def.addNamespace("imolacics", Jbi4CicsExtension.NS_URI_JBI4CICS);
            
            // Adds a new Service with my estension            
            Service myService = def.createService();
            myService.setQName(new QName(tns, "MyService"));
            def.addService(myService);
            
            // Adds the port
            Port myPort = def.createPort();
            myPort.setName("myPort");
            myService.addPort(myPort);
            
            // Adds the extended address
            Jbi4CicsAddress log4jAddress = new Jbi4CicsAddress();
            log4jAddress.setElementType(
                    Jbi4CicsExtension.Q_ELEM_JBI4CICS_ADDRESS);               
            log4jAddress.setUsername("imola");
            log4jAddress.setPassword("imola");          
            log4jAddress.setConnectionType("CICS");
            log4jAddress.setJNDIConnectionName("");
            log4jAddress.setProgramName("imola");
            log4jAddress.setTransactionName("imola");
            log4jAddress.setTpn(new Boolean("false"));
            myPort.addExtensibilityElement(log4jAddress);                  
            
            // Adds the binding (using an exixtent porttype)
            PortType portType = def.getPortType(
                    new QName(tns,"EchoServicePortType"));
            Binding myBinding = def.createBinding();
            myBinding.setUndefined(false);
            myBinding.setQName(new QName(tns, "MyBinding"));
            myBinding.setPortType(portType);              
            myPort.setBinding(myBinding); 
            def.addBinding(myBinding);            
            BindingOperation myOp = def.createBindingOperation();                       
            myOp.setName("myOp");            
            
            myOp.setOperation((Operation)portType.getOperations().get(0));
            myBinding.addBindingOperation(myOp);

            // Adds the extended binding
            Jbi4CicsBinding log4jBinding = new Jbi4CicsBinding();            
            log4jBinding.setElementType(
                    Jbi4CicsExtension.Q_ELEM_JBI4CICS_BINDING);            
            
            // Attributes
            log4jBinding.setServicePackageName(
                    "it.imolinfo.jbi4cics.test.nested");
            log4jBinding.setCodePage("CP037");            
            
            // copy cobol
            String copyCobol = 
                          "02 CA-RETCODE1 PIC XXXXX        ." + "/n"
			+ "02 CA-RETCODE2                  ." + "/n"
			+ "03 CA-RETCODE3 PIC XXXXX        ." + "/n"
			+ "02 CA-RETCODE4 PIC XXXXX        .";
            log4jBinding.setCopyCobol(copyCobol);
                      
            myBinding.addExtensibilityElement(log4jBinding);  
                                 
            String wsdlFileName = rootPath + File.separator 
                    + "TestWSDLwriter.wsdl";            
            File wsdlFile = new File(wsdlFileName);
            FileWriter fileWriter = new FileWriter(wsdlFile);
                                 
            writer.writeWSDL(def, fileWriter);            
            
            // Reads the WSDL:            
            reader.setExtensionRegistry(registry);
            Definition def2 = reader.readWSDL(rootPath,"TestWSDLwriter.wsdl");
            StringWriter strWriter = new StringWriter();   
            writer.writeWSDL(def2, strWriter);
            System.out.println(strWriter);            
            //TODO Uncomment the following instruction 
            //wsdlFile.delete();
            
            // Reads the extended elements;
            Binding extBinding = def2.getBinding(new QName(tns, "MyBinding"));            
            Jbi4CicsBinding readenJbi4CicsBinding = 
                (Jbi4CicsBinding) extBinding.getExtensibilityElements().get(0);
            Service extService = def2.getService(new QName(tns, "MyService"));            
            Port extPort = (Port)extService.getPort("myPort");                        
            Jbi4CicsAddress readenJbi4CicsAddress = 
                (Jbi4CicsAddress) extPort.getExtensibilityElements().get(0);           
            
            // The extensibility element must be equals            
            assertTrue("Cics binding assert failed",readenJbi4CicsBinding.equals(log4jBinding));
            assertTrue("Cics address assert failed",readenJbi4CicsAddress.equals(log4jAddress));                                                
            
        } catch (Exception e) {
            // TODO Auto-generated catch block
            System.out.print(e.getMessage());            
            e.printStackTrace();
            fail(e.getMessage());
        }
    }    
    
    

}
