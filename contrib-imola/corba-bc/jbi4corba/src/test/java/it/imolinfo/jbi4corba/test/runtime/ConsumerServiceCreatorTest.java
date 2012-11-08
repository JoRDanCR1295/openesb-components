 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.test.mock.MockComponentContext;
import it.imolinfo.jbi4corba.webservice.descriptor.ConsumerServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.ConsumerServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.generator.ServerCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.runtime.ConsumerServiceCreator;

import java.io.File;
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
 * XXX javadoc
 * Note:
 * 1) We need the corba name service.
 */
public class ConsumerServiceCreatorTest extends TestCase {

    /**
     * Logger.
     */
    private static Logger log = LoggerFactory.getLogger(ConsumerServiceCreatorTest.class);

    /**
     * Where the WSDLs are located.
     */
    public static final File wsdlDir = new File("src/test/etc/wsdl/");

    /**
     * The list of jars for the classpath.
     */
    private List<String> jarFilesName = new ArrayList<String>();

    /**
     * TestCase Construtor.
     */
    public ConsumerServiceCreatorTest(String arg0) {
        super(arg0);

        String repodir = System.getProperty("localRepository");
        log.debug("repodir=" + repodir);

        
        assertNotNull(repodir);
        assertFalse("".equals(repodir));

        jarFilesName = JarListHelper.extractJarList(repodir);
    }


    public void testEchoServiceCreation() {
      try {
    	  /* ------------------------------------
    	   *                   [01] WSDL to Corba
    	   * ------------------------------------
    	   */
    	  String serviceName = "echo";
    	  JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();
    	  jbiServiceDescriptor.setWsdlURL(
                wsdlDir.getAbsolutePath() + "/EchoService.wsdl");

    	  ConsumerServiceClassesGenerator serviceGenerator
    	      = new ConsumerServiceClassesGenerator();

    	  ServerCorbaClassesHolder serverCorbaClassesHolder
              = serviceGenerator.generateConsumerServiceClassesDirect(
                  jbiServiceDescriptor.getWsdlURL(),
                  "target/producer/testEchoClassesGeneration",
                  jarFilesName,null);

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
