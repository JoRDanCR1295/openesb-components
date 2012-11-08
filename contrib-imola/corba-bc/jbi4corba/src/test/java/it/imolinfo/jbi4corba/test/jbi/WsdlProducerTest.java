 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.jbi;

import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaAddress;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaBinding;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtension;

import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaIDLEntry;
import java.io.File;
import java.io.FileWriter;
import java.io.StringWriter;
import java.util.Properties;

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
 * @author marcopiraccini
 */
public class WsdlProducerTest extends TestCase {
        
    
    public static final String wsdlDir = "src/test/etc/wsdl";
    
    public static final String rootPath = "target/test-wsdl-ext";
    
    public static final String serviceUnitName = "testServiceUnit";      
    
    public WSDLFactory factory = null;
    
    public WSDLReader reader = null;
    
    public WSDLWriter writer = null;
    
    public ExtensionRegistry registry;
    
    String tns = "it.imolinfo.Jbi4Corba.test.webservice.generator";
    
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
            Jbi4CorbaExtension.register(registry);      
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
            def.addNamespace("imolacorba", Jbi4CorbaExtension.NS_URI_JBI4CORBA);
            
            // Adds a new Service with my estension            
            Service myService = def.createService();
            myService.setQName(new QName(tns, "MyService"));
            def.addService(myService);
            
            // Adds the port
            Port myPort = def.createPort();
            myPort.setName("myPort");
            myService.addPort(myPort);
            
            // Adds the extended address
            Jbi4CorbaAddress log4jAddress = new Jbi4CorbaAddress();
            log4jAddress.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_ADDRESS);               
            log4jAddress.setName("jbi4CorbaName");
            log4jAddress.setLocalizationType("NameService");  
            // ORB PRoperties
            Properties orbProperties = new Properties();
            orbProperties.setProperty("org.omg.CORBA.ORBInitialPort", "1050");
            orbProperties.setProperty("org.omg.CORBA.ORBInitialHost", "localhost");
            log4jAddress.setOrbProperties(orbProperties);
            myPort.addExtensibilityElement(log4jAddress);                  
            
            // Adds the binding (using an exixtent porttype)
            PortType portType = def.getPortType(new QName(tns,"EchoServicePortType"));
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

            // Adds the extended address
            Jbi4CorbaBinding log4jBinding = new Jbi4CorbaBinding();            
            log4jBinding.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_BINDING);            
            
            // Attributes
            //log4jBinding.setRole("provider");
            //log4jBinding.setTargetEndpoint("TargetEndpoint");
            //log4jBinding.setTargetService(new QName(tns,"TargetService"));
            //log4jBinding.setTargetInterfaceName(new QName(tns,"TargetInterfaceName"));
            //log4jBinding.setLoadEndpointURL(Boolean.TRUE);
            //log4jBinding.setWsdlURL("wsdlUrl");
            
            // IDL
            String idl = "      module it{ "+
            "module imolinfo{ module jbi4corba{  module test{ " +
               " module webservice{  module generator{ " +
                 "       interface Echo { " +
                  "          string echo(in string msg); " +
                   "      };};};};};};};";
            Jbi4CorbaIDLEntry jbi4CorbaIDLEntry=new Jbi4CorbaIDLEntry();
            jbi4CorbaIDLEntry.setIDL(idl);
            log4jBinding.getJbi4CorbaDLEntryList().add(jbi4CorbaIDLEntry);
            
            myBinding.addExtensibilityElement(log4jBinding);  
                     
            
            String wsdlFileName = rootPath + File.separator + "TestWSDLwriter.wsdl";            
            File wsdlFile = new File(wsdlFileName);
            FileWriter fileWriter = new FileWriter(wsdlFile);
                                 
            writer.writeWSDL(def, fileWriter);            
            
            // Reads the WSDL:            
            reader.setExtensionRegistry(registry);
            Definition def2 = reader.readWSDL(rootPath,"TestWSDLwriter.wsdl");
            StringWriter strWriter = new StringWriter();   
            writer.writeWSDL(def2, strWriter);
            System.out.println(strWriter);            
            wsdlFile.delete();
            
            // Reads the extended elements;
            Binding extBinding = def2.getBinding(new QName(tns, "MyBinding"));            
            Jbi4CorbaBinding readenJbi4CorbaBinding = (Jbi4CorbaBinding) extBinding.getExtensibilityElements().get(0);
            Service extService = def2.getService(new QName(tns, "MyService"));            
            Port extPort = (Port)extService.getPort("myPort");                        
            Jbi4CorbaAddress readenJbi4CorbaAddress = (Jbi4CorbaAddress) extPort.getExtensibilityElements().get(0);           
            
            // The extensibility element must be equals            
            assertTrue(readenJbi4CorbaBinding.equals(log4jBinding));
            assertTrue(readenJbi4CorbaAddress.equals(log4jAddress));
                                    
            
        } catch (Exception e) {
            // TODO Auto-generated catch block
            System.out.print(e.getMessage());            
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

        public void testWSDLSerializerMultipleIDL() {

        try {
            Definition def = reader.readWSDL(wsdlDir,"EchoService.wsdl");

            // Sets the registry
            def.setExtensionRegistry(registry);

            // Adds the namespace
            def.addNamespace("imolacorba", Jbi4CorbaExtension.NS_URI_JBI4CORBA);

            // Adds a new Service with my estension
            Service myService = def.createService();
            myService.setQName(new QName(tns, "MyService"));
            def.addService(myService);

            // Adds the port
            Port myPort = def.createPort();
            myPort.setName("myPort");
            myService.addPort(myPort);

            // Adds the extended address
            Jbi4CorbaAddress log4jAddress = new Jbi4CorbaAddress();
            log4jAddress.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_ADDRESS);
            log4jAddress.setName("jbi4CorbaName");
            log4jAddress.setLocalizationType("NameService");
            // ORB PRoperties
            Properties orbProperties = new Properties();
            orbProperties.setProperty("org.omg.CORBA.ORBInitialPort", "1050");
            orbProperties.setProperty("org.omg.CORBA.ORBInitialHost", "localhost");
            log4jAddress.setOrbProperties(orbProperties);
            myPort.addExtensibilityElement(log4jAddress);

            // Adds the binding (using an exixtent porttype)
            PortType portType = def.getPortType(new QName(tns,"EchoServicePortType"));
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

            // Adds the extended address
            Jbi4CorbaBinding log4jBinding = new Jbi4CorbaBinding();
            log4jBinding.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_BINDING);

            // Attributes
            //log4jBinding.setRole("provider");
            //log4jBinding.setTargetEndpoint("TargetEndpoint");
            //log4jBinding.setTargetService(new QName(tns,"TargetService"));
            //log4jBinding.setTargetInterfaceName(new QName(tns,"TargetInterfaceName"));
            //log4jBinding.setLoadEndpointURL(Boolean.TRUE);
            //log4jBinding.setWsdlURL("wsdlUrl");

            // IDL
            String idl = "      module it{ "+
            "module imolinfo{ module jbi4corba{  module test{ " +
               " module webservice{  module generator{ " +
                 "       interface Echo { " +
                  "          string echo(in string msg); " +
                   "      };};};};};};};";

            //first entry
            Jbi4CorbaIDLEntry jbi4CorbaIDLEntry=new Jbi4CorbaIDLEntry();
            jbi4CorbaIDLEntry.setIDL(idl);
            jbi4CorbaIDLEntry.setIdlFilename("root1.idl");
            jbi4CorbaIDLEntry.setRelativePath("a_relative/PATH");
            jbi4CorbaIDLEntry.setRoot(true);
            log4jBinding.getJbi4CorbaDLEntryList().add(jbi4CorbaIDLEntry);
            //second entry
            jbi4CorbaIDLEntry=new Jbi4CorbaIDLEntry();
            jbi4CorbaIDLEntry.setIDL(idl);
            jbi4CorbaIDLEntry.setIdlFilename("another.idl");
            jbi4CorbaIDLEntry.setRelativePath("a_relative/path/path");
            jbi4CorbaIDLEntry.setRoot(false);
            log4jBinding.getJbi4CorbaDLEntryList().add(jbi4CorbaIDLEntry);

            myBinding.addExtensibilityElement(log4jBinding);


            String wsdlFileName = rootPath + File.separator + "TestWSDLwriterMultipleIDL.wsdl";
            File wsdlFile = new File(wsdlFileName);
            FileWriter fileWriter = new FileWriter(wsdlFile);

            writer.writeWSDL(def, fileWriter);

            // Reads the WSDL:
            reader.setExtensionRegistry(registry);
            Definition def2 = reader.readWSDL(rootPath,"TestWSDLwriterMultipleIDL.wsdl");
            StringWriter strWriter = new StringWriter();
            writer.writeWSDL(def2, strWriter);
            System.out.println(strWriter);
            wsdlFile.delete();

            // Reads the extended elements;
            Binding extBinding = def2.getBinding(new QName(tns, "MyBinding"));
            Jbi4CorbaBinding readenJbi4CorbaBinding = (Jbi4CorbaBinding) extBinding.getExtensibilityElements().get(0);
            Service extService = def2.getService(new QName(tns, "MyService"));
            Port extPort = (Port)extService.getPort("myPort");
            Jbi4CorbaAddress readenJbi4CorbaAddress = (Jbi4CorbaAddress) extPort.getExtensibilityElements().get(0);

            // The extensibility element must be equals
            assertTrue(readenJbi4CorbaBinding.equals(log4jBinding));
            assertTrue(readenJbi4CorbaAddress.equals(log4jAddress));

            
        } catch (Exception e) {
            // TODO Auto-generated catch block
            System.out.print(e.getMessage());
            e.printStackTrace();
            fail(e.getMessage());
        }
    }
    
    /**
     * Tests if the writer can manage non xml-characters in IDL.
     *
     */
    public void testWSDLSerializerIDLWithNOXMLCharacters() {          

        try {
            Definition def = reader.readWSDL(wsdlDir,"EchoService.wsdl");            
            
            // Sets the registry
            def.setExtensionRegistry(registry);
           
            // Adds the namespace
            def.addNamespace("imolacorba", Jbi4CorbaExtension.NS_URI_JBI4CORBA);
            
            // Adds a new Service with my estension            
            Service myService = def.createService();
            myService.setQName(new QName(tns, "MyService"));
            def.addService(myService);
            
            // Adds the port
            Port myPort = def.createPort();
            myPort.setName("myPort");
            myService.addPort(myPort);
            
            // Adds the extended address
            Jbi4CorbaAddress log4jAddress = new Jbi4CorbaAddress();
            log4jAddress.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_ADDRESS);
            log4jAddress.setRequired(Boolean.TRUE);            
            log4jAddress.setName("jbi4CorbaName");
            log4jAddress.setLocalizationType("NameService");            
            myPort.addExtensibilityElement(log4jAddress);                  
            
            // Adds the binding (using an exixtent porttype)
            PortType portType = def.getPortType(new QName(tns,"EchoServicePortType"));
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

            // Adds the extended address
            Jbi4CorbaBinding log4jBinding = new Jbi4CorbaBinding();            
            log4jBinding.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_BINDING);
            log4jBinding.setRequired(Boolean.TRUE);
            
            // Attributes
//            log4jBinding.setRole("provider");
//            log4jBinding.setTargetEndpoint("TargetEndpoint");
//            log4jBinding.setTargetService(new QName(tns,"TargetService"));
//            log4jBinding.setTargetInterfaceName(new QName(tns,"TargetInterfaceName"));
//            log4jBinding.setLoadEndpointURL(Boolean.TRUE);
//            log4jBinding.setWsdlURL("wsdlUrl");
            
            // IDL
            String idl = "myIDL with NOXML Characters >> < <!--";
            Jbi4CorbaIDLEntry jbi4CorbaIDLEntry=new Jbi4CorbaIDLEntry();
            jbi4CorbaIDLEntry.setIDL(idl);
            log4jBinding.getJbi4CorbaDLEntryList().add(jbi4CorbaIDLEntry);
          
            // ORB PRoperties
            Properties orbProperties = new Properties();
            orbProperties.setProperty("org.omg.CORBA.ORBInitialPort", "1050");
            orbProperties.setProperty("org.omg.CORBA.ORBInitialHost", "localhost");
            log4jAddress.setOrbProperties(orbProperties);
            
            myBinding.addExtensibilityElement(log4jBinding);  
                                 
            String wsdlFileName = rootPath + File.separator + "TestWSDLwriter.wsdl";            
            File wsdlFile = new File(wsdlFileName);
            FileWriter fileWriter = new FileWriter(wsdlFile);
                                 
            writer.writeWSDL(def, fileWriter);            
            
            // Reads the WSDL:            
            reader.setExtensionRegistry(registry);
            Definition def2 = reader.readWSDL(rootPath,"TestWSDLwriter.wsdl");
            
            // Reads the binding;
            Binding extBinding = def2.getBinding(new QName(tns, "MyBinding"));            
            Jbi4CorbaBinding readJbi4CorbaBinding = (Jbi4CorbaBinding) extBinding.getExtensibilityElements().get(0);
            
            assertEquals(log4jBinding.getJbi4CorbaDLEntryList().get(0).getIDL(), readJbi4CorbaBinding.getJbi4CorbaDLEntryList().get(0).getIDL());
            wsdlFile.delete();            
            
        } catch (Exception e) {
            // TODO Auto-generated catch block
            System.out.print(e.getMessage());            
            e.printStackTrace();
            fail(e.getMessage());
        }
    }        
    

}
