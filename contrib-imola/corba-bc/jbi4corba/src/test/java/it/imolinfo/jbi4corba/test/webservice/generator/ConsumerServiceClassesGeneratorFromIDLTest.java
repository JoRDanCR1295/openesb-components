 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.test.mock.MockComponentContext;
import it.imolinfo.jbi4corba.utils.HelperFileUtil;
import it.imolinfo.jbi4corba.webservice.descriptor.ConsumerServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.ConsumerServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.generator.ServerCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.generator.Util;
import it.imolinfo.jbi4corba.webservice.runtime.ConsumerServiceCreator;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import junit.framework.TestCase;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;

/**
 * This class is used to test the code generation for the component in consumer
 * mode starting from IDL (if found inside WSDL)
 * 
 */
public class ConsumerServiceClassesGeneratorFromIDLTest extends TestCase {

	/**
	 * Logger.
	 */
	private static Logger log = LoggerFactory.getLogger(ConsumerServiceClassesGeneratorFromIDLTest.class);
	
	
	/**
	 * The directory where the WSDLs are located.
	 */
	public static final File WSDLDIR = new File("src/test/etc/wsdl/");
	
	
	/**
	 * The directory where the WSDLs are located.
	 */
	public static final File OUTPUTDIR = new File("target/consumer/testEchoClassesGenerationFromIDL/");
 
	/**
	 * The list of jars for the classpath.
	 */
	private List<String> jarFilesName = new ArrayList<String>();

        @Override
	public void setUp() {

		String repodir = System.getProperty("localRepository");
		log.debug("repodir=" + repodir);

		assertNotNull(repodir);
		assertFalse("".equals(repodir));

		jarFilesName = JarListHelper.extractJarList(repodir);

	}

	/**
	 * TestCase Construtor.
	 * 
	 * @param arg0
	 */
	public ConsumerServiceClassesGeneratorFromIDLTest(String arg0) {
		super(arg0);
	}

//	public void testEchoClassesGenerationWSDLwithIDLandXSD() {
//	
//            File outputDir = new File("target/consumer/ConsumerXsd/");
//            File wsdlDir = new File("src/test/etc/wsdl/ConsumerXsd/");
//            try {
//			JbiServiceDescriptor jbiServiceDescriptor = createJbiServiceDescriptor(
//					wsdlDir.getAbsolutePath() + "/AlarmIRP.wsdl",
//					"echo.idl");
//			
//			ConsumerServiceClassesGenerator serviceGenerator = new ConsumerServiceClassesGenerator();
//			ServerCorbaClassesHolder serverCorbaClassesHolder = serviceGenerator
//					.generateConsumerServiceClassesDirect(jbiServiceDescriptor
//							.getWsdlURL(),
//							outputDir.getAbsolutePath(),
//							jarFilesName,jbiServiceDescriptor);
//
//			log.debug("classes generated: " + serverCorbaClassesHolder);
//		} catch (Exception e) {
//			log.error("Error in testEchoClassesGeneration", e);
//			fail(e.getMessage());
//		}
//	}	
	
	public void testEchoClassesGenerationWSDLwithIDL() {
		try {
			JbiServiceDescriptor jbiServiceDescriptor = createJbiServiceDescriptor(
					WSDLDIR.getAbsolutePath() + "/Echo_with_IDL.wsdl",
					"echo.idl");
			
			ConsumerServiceClassesGenerator serviceGenerator = new ConsumerServiceClassesGenerator();
			ServerCorbaClassesHolder serverCorbaClassesHolder = serviceGenerator
					.generateConsumerServiceClassesDirect(jbiServiceDescriptor
							.getWsdlURL(),
							OUTPUTDIR.getAbsolutePath(),
							jarFilesName,jbiServiceDescriptor);

			log.debug("classes generated: " + serverCorbaClassesHolder);
		} catch (Exception e) {
			log.error("Error in testEchoClassesGeneration", e);
			fail(e.getMessage());
		}
	}

    public void testEchoServiceCreation() {
        try {
      	  /* ------------------------------------
      	   *                   [01] WSDL to Corba
      	   * ------------------------------------
      	   */
      	  String serviceName = "echo";
			JbiServiceDescriptor jbiServiceDescriptor = createJbiServiceDescriptor(
					WSDLDIR.getAbsolutePath() + "/Echo_with_IDL.wsdl",
					"echoservice.idl");


      	  ConsumerServiceClassesGenerator serviceGenerator
      	      = new ConsumerServiceClassesGenerator();

      	  ServerCorbaClassesHolder serverCorbaClassesHolder = serviceGenerator
					.generateConsumerServiceClassesDirect(jbiServiceDescriptor
							.getWsdlURL(), OUTPUTDIR.getAbsolutePath(),
							jarFilesName, jbiServiceDescriptor);
      	  log.debug("classes generated: " + serverCorbaClassesHolder);

          ConsumerServiceDescriptor consumerServiceDescriptor
                  = new ConsumerServiceDescriptor();

          consumerServiceDescriptor.setServerCorbaClassesHolder(
                  serverCorbaClassesHolder);

          Properties props = new Properties();

          props.put("org.omg.CORBA.ORBInitialPort", "1050");
          props.put("org.omg.CORBA.ORBInitialHost", "localhost");
          consumerServiceDescriptor.setCorbaServiceName("echo");

          consumerServiceDescriptor.setOrbProperties(props);
          
          

          ConsumerServiceCreator consumerServiceCreator
                  = new ConsumerServiceCreator();
                  
          MockComponentContext context = new MockComponentContext();
          
          RuntimeContext.getInstance().setComponentContext(context);  
          
          consumerServiceCreator.createJbiService(consumerServiceDescriptor);

          log.debug("consumer descriptor: " + consumerServiceDescriptor);

          consumerServiceDescriptor.setServiceName(serviceName);
          consumerServiceCreator.registerAndActivateService(
                  consumerServiceDescriptor);

          log.debug("service activated, activating orb...");

          new Thread(new OrbRunner(consumerServiceDescriptor.getOrb())).start();

          Class corbaInterface = serverCorbaClassesHolder.getCorbaOperations();
          Class helper = serverCorbaClassesHolder.getCorbaHelper();

          Method narrowMethod = helper.getDeclaredMethod("narrow",
                  new Class [] { Object.class });

          Method echoMethod = corbaInterface.getDeclaredMethod("echo",
                  new Class [] { String.class });

          // corba client
          // create and initialize the ORB
          ORB orb = ORB.init((String[])null, props);

          // get the root naming context
          org.omg.CORBA.Object objRef
                  = orb.resolve_initial_references("NameService");

          // Use NamingContextExt instead of NamingContext. This is
          // part of the Interoperable naming Service.
          NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

          // resolve the Object Reference in Naming
          org.omg.CORBA.Object echoRef=ncRef.resolve_str(serviceName);

          //Echo echoImpl = EchoHelper.narrow(ncRef.resolve_str(serviceName));
          org.omg.CORBA.Object narrowedObject = (Object) narrowMethod.invoke(
                  helper,
                  new java.lang.Object [] { echoRef });

          log.debug("corbaInterface.isInstance(narrowedObject):"
                  + corbaInterface.isInstance(narrowedObject));

          log.debug("calling corba service with client: "
                  + narrowedObject);

          java.lang.Object response = echoMethod.invoke(narrowedObject,
                  new java.lang.Object[]{"Hello World"});

          log.debug("response: "+response);

        } catch (Exception e) {
            log.error("Unexpected Error:" + e.getMessage());
            e.printStackTrace();
            fail(e.getMessage());
        }

      }
   
	private JbiServiceDescriptor createJbiServiceDescriptor( String wsdlUrl, String idlFileName) {
		
		JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();
		try {
			jbiServiceDescriptor.setWsdlURL(wsdlUrl);

			String idlFromWsdl = null;
		    File idlFile = null;
		    try {
		          idlFromWsdl = HelperFileUtil.readIdlFromFile(new File(jbiServiceDescriptor.getWsdlURL()));
		          if ((idlFromWsdl != null) && (idlFromWsdl.length() > 0)) {
		              idlFile=Util.saveFile(idlFromWsdl, OUTPUTDIR.getAbsolutePath(), idlFileName);
		          }
		      } catch (IOException ioe) {
		          ioe.printStackTrace();
		          log.error("Error accessing the WSDL directory");
		      }
 	    	jbiServiceDescriptor.setIdlFileName(idlFile.getName());
 	    	jbiServiceDescriptor.setIdlFileNameDirectory(OUTPUTDIR.getAbsolutePath());
 	    	jbiServiceDescriptor.setRole(JbiServiceDescriptor.CONSUMER);
 	
		} catch (Exception e) {
			log.error("Error in creating jbiServiceDescriptor", e);
		}
		return jbiServiceDescriptor;
	}
    /**
     * Inner Class.
     */
    public static class OrbRunner implements Runnable {
        ORB orb;

        public OrbRunner(ORB orb){
            this.orb=orb;
        }

        public void run() {
            orb.run();
        }
    }

    
}
