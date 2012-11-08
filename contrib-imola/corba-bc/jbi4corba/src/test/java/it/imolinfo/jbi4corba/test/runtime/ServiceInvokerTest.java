 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.runtime;


/**
 * This test is can be subtituted by the more complete <code>CXFProviderTest</code>
 * @author marco
 *
 */
public class ServiceInvokerTest {
}

//public class ServiceInvokerTest extends AbstractXFireTest {
//  private static Log log = LogFactory.getLog(ServiceInvokerTest.class);
//
//  public static final String idlDir = "src/test/etc/idl";
//
//  public static String nameServerPort = "1050";
//
//  public void setUp() {
////    // start the echo service and register into name service
////    /*
////     * Thread thread=new Thread(new Runnable(){ public void run(){ ORBD.main(new
////     * String[]{"-ORBInitialPort",nameServerPort,"-ORBInitialHost","raffaele"});
////     * log.debug("orbd partito"); } }); thread.start(); try {
////     * Thread.currentThread().sleep(3000); } catch (InterruptedException e) {
////     * fail(e.getMessage()); }
////     */
////    Thread thread2 = new Thread(new Runnable() {
////      public void run() {
////        EchoImpl.main(new String[] { nameServerPort });
////      }
////    });
////    thread2.start();
////    try {
////      Thread.currentThread().sleep(2000);
////    } catch (InterruptedException e) {
////      fail(e.getMessage());
////    }
//
//  }
//
//  public void tearDown() {
//    // stop the echo service
//
//  }
//
//  public void testEchoServiceInvokation() {
//    try {
//      JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();
//      jbiServiceDescriptor.setIdlFileName("Echo.idl");
//      jbiServiceDescriptor.setIdlFileNameDirectory(idlDir);
//      ProviderServiceClassesGenerator serviceGenerator = new ProviderServiceClassesGenerator();
//
//      List<String> jars = null;
//      List<ClientCorbaClassesHolder> classes
//        = serviceGenerator.generateProviderServiceClasses(
//          jbiServiceDescriptor,
//          "target/testEchoClassesGeneration",
//          jars); // new params
//      log.debug("classes: " + classes);
//
//      // we know there is just on class
//      ClientCorbaClassesHolder corbaServiceClasses = classes.get(0);
//      ProviderServiceDescriptor serviceDescriptor = new ProviderServiceDescriptor();
//      serviceDescriptor.setServiceInterface(corbaServiceClasses.getOperationsClass());
//      serviceDescriptor.setCorbaHelperClass(corbaServiceClasses.getHelperClass());
//      ProviderCXFServiceCreator serviceCreator = new ProviderCXFServiceCreator();
//      serviceDescriptor.setServiceName("EchoService");
//      serviceDescriptor.setServiceNameSpace(serviceDescriptor.getServiceInterface().getPackage().getName());
//      log.debug("service namespace: " + serviceDescriptor.getServiceNameSpace());
//      
//      Service service = serviceCreator.createService(serviceDescriptor);
//      log.info("service created: " + service);
//
//
//      // create and initialize the ORB
//      Properties props = new Properties();
//      props.put("org.omg.CORBA.ORBInitialPort", nameServerPort);
//      props.put("org.omg.CORBA.ORBInitialHost", "localhost");
//      ORB orb = ORB.init(new String[] {}, props);
//
//      // get the root naming context
//      org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
//
//      // Use NamingContextExt instead of NamingContext. This is
//      // part of the Interoperable naming Service.
//      NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
//
//      // resolve the Object Reference in Naming
//      String name = "Echo";
//      Class helperClass = serviceDescriptor.getCorbaHelperClass();
//
//      Method narrowMethod
//        = helperClass.getDeclaredMethod("narrow", new Class[] { Object.class });
//
//      Object corbaObjectReference = (Object) narrowMethod.invoke(
//        helperClass, new java.lang.Object[] { ncRef.resolve_str(name) });
//
//      serviceDescriptor.setCorbaObjectReference(corbaObjectReference);
//      log.debug("service reference found: " + corbaObjectReference);
//
//      // invoco il servizio
//      Document response = invokeService(service.getName().getLocalPart(),
//                                        "/xmlmessages/testEchoMessage.xml");
//      printNode(response);
//
//    } catch (Exception e) {
//      e.printStackTrace();
//      fail(e.getMessage());
//    }
//  }


