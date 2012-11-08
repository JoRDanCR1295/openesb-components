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
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.ClientCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.Param;
import it.imolinfo.jbi4corba.webservice.generator.ProviderServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceCreator;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.wsdl.WSDLException;

import junit.framework.TestCase;

import org.apache.cxf.service.Service;


public class ParamInformationCollectionTest  extends TestCase {

  /**
   * Logger
   */
  private static Logger log = LoggerFactory.getLogger(ServiceCreatorTest.class);

  /**
   * The directory where we place the IDLs used to generate the java code.
   */
  public static final String IDLDIR = "src/test/etc/idl";

  private List<String> jarFilesName = new ArrayList<String>();

  public ParamInformationCollectionTest(String arg0) {
        
    super(arg0);

    String repodir = System.getProperty("localRepository");
    log.debug("repodir=" + repodir);

    assertNotNull(repodir);
    assertFalse("".equals(repodir));

    jarFilesName = JarListHelper.extractJarList(repodir);
  }  
  

    /**
     * This is a test where we use a complex idl with sequences and inout parameters.
     * The idl is:
     * module it{
     *   module imolinfo{
     *   module jbi4corba{
     *   module test{
     *   module webservice{
     *   module generator{
     *   // struct
     *  struct EchoStruct {
     *           boolean fieldBoolean;
     *           string fieldString;
     *   };

     *   typedef sequence<long> SeqLong;
     *   typedef sequence<string> SeqString;                                     
     *  typedef sequence<EchoStruct> SeqEchoStruct;

     *   interface foo {
     *           string barSeqStr(in SeqString str);
     *           string barSeqLong(in SeqLong e);
     *           string barSeqStruct(in SeqEchoStruct seq);
     *           string barSeqStructSeqLong(in SeqEchoStruct seq, in SeqLong integ, inout string str2);
     *           string barSeqStrInOut(inout SeqString str);
     *           string barSeqLongInOut(inout SeqLong e);
     *           string barSeqStructInOut(inout SeqEchoStruct seq);
     *           };};};};};};};
     * 
     */
    public void testEchoSequence() {
      final String idlToTest = "EchoSequence.idl";
      final String targetDir = "target/testEchoSequence";

      log.debug(">>>>> testEchoSequence - begin");
      ClientCorbaClassesHolder holder = null;
      try {

          holder = testBody(idlToTest, targetDir, "testEchoSequence",0);
          holder.getMethodSignatures();
          System.err.println("Returned: " + holder.getMethodSignatures().size());
          // 7 methods must be returned
          assertEquals(7, holder.getMethodSignatures().size());
          
          for (MethodSignature sig: holder.getMethodSignatures()) {              
              System.err.println("*****************************");
              assertNotNull(sig.getMethod());
              assertNotNull(sig.getChangedMethod());
              System.err.println("Signature found:" + sig.getMethodName() + " / contains holder: " + sig.isContainsHolder());
              System.err.println("Original method: " + sig.getMethod().toGenericString());
              System.err.println("Changed method: " + sig.getChangedMethod().toGenericString());
              
              for (Param param: sig.getParameters()) {
                  System.err.println("----");
                  System.err.println("   " + param.getName());
                  System.err.println("   " + param.getType());
                  System.err.println("   isHolder:" + param.isHolder());
                  System.err.println("   isHolder of:" + param.getHolderValueType());
                  System.err.println("   new Holder Type Name:" + param.getTypeName());                  
                  System.err.println("   isArray:" + param.isArray());
                  System.err.println("   array dimension:" + param.getArrayDimension());
                  System.err.println("----");
                  
                  if (sig.getMethodName().equals("barSeqStr")) {
                      assertEquals(1, sig.getParameters().size());
                      assertEquals("str", param.getName());
                      assertEquals(java.lang.String.class, param.getType());
                      assertEquals(false, param.isHolder());
                      assertEquals(true, param.isArray());
                      assertEquals(false, sig.isContainsHolder());    
                  }
                  
                  if (sig.getMethodName().equals("barSeqLong")) {
                      assertEquals(1, sig.getParameters().size());
                      assertEquals("e", param.getName());
                      assertEquals(Integer.TYPE, param.getType());
                      assertEquals(false, param.isHolder());
                      assertEquals(true, param.isArray());
                      assertEquals(false, sig.isContainsHolder());                          
                  }         

                  if (sig.getMethodName().equals("barSeqStruct")) {
                      assertEquals(1, sig.getParameters().size());
                      assertEquals("seq", param.getName());
                      assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.EchoStruct", param.getType().getName());
                      assertEquals(false, param.isHolder());
                      assertEquals(true, param.isArray());
                      assertEquals(false, sig.isContainsHolder());                          
                  }  
                  
                  if (sig.getMethodName().equals("barSeqStructSeqLong")) {
                      assertEquals(3, sig.getParameters().size());
                      assertEquals(true, sig.isContainsHolder());                          
                      if (param.getName().equals("seq")) {                      
                          assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.EchoStruct", param.getType().getName());
                          assertEquals(false, param.isHolder());
                          assertEquals(true, param.isArray());
                      } else if (param.getName().equals("integ")) {
                          assertEquals(Integer.TYPE, param.getType());
                          assertEquals(false, param.isHolder());
                          assertEquals(true, param.isArray());                              
                      } else if (param.getName().equals("str2")) {                          
                          assertEquals(org.omg.CORBA.StringHolder.class, param.getType());                          
                          assertEquals(java.lang.String.class, param.getHolderValueType());
                          assertEquals("javax.xml.ws.Holder<java.lang.String>", param.getTypeName());
                          assertEquals(true, param.isHolder());
                          assertEquals(false, param.isArray());                           
                      } else {
                          fail("Parameter with name:" + param.getName() + " is wrong.");
                      }
                  }    
                  
                  if (sig.getMethodName().equals("barSeqStrInOut")) {
                      assertEquals(true, sig.isContainsHolder());                       
                      assertEquals(1, sig.getParameters().size());
                      assertEquals("str", param.getName());
                      assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.SeqStringHolder", param.getType().getName());
                      assertEquals("java.lang.String[]", param.getHolderValueType().getCanonicalName());
                      assertEquals("javax.xml.ws.Holder<java.lang.String[]>", param.getTypeName());
                      assertEquals(true, param.isHolder());
                      assertEquals(false, param.isArray());                     
                  }  
                  
                  if (sig.getMethodName().equals("barSeqLongInOut")) {
                      assertEquals(true, sig.isContainsHolder());                       
                      assertEquals(1, sig.getParameters().size());
                      assertEquals("e", param.getName());
                      assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.SeqLongHolder", param.getType().getName());
                      assertEquals("int[]", param.getHolderValueType().getCanonicalName());
                      assertEquals("javax.xml.ws.Holder<int[]>", param.getTypeName());
                      assertEquals(true, param.isHolder());
                      assertEquals(false, param.isArray());                     
                  }
                          
                  
                  if (sig.getMethodName().equals("echoMatrix")) {
                      assertEquals(false, sig.isContainsHolder()); 
                      assertEquals(1, sig.getParameters().size());
                      assertEquals("e", param.getName());
                      assertEquals("java.lang.String", param.getType().getName());                      
                      assertEquals(false, param.isHolder());
                      assertEquals(true, param.isArray());
                      assertEquals(2, param.getArrayDimension());
                  }                                  
                  
              }
              System.err.println("*****************************");
          }

      } catch (Exception e) {
        String m = "Error in ... testEchoSequence:"
                + e.getMessage();

        log.error(m, e);
        fail(m);
      }
      log.debug("<<<<< testEchoSequence - end");
    }    
    
   

//     /**
//     * This is a test where we use a complex AlarmIRP idl to test HolderUtil Class 
//     * this Test generate 10 class, 
//     * Change 
//     **/
//    public void testAlarmIrp() {
//      final String idlToTest = "AlarmIRP.idl";
//      final String targetDir = "target/testAlarmIrp";
//
//      log.debug(">>>>> testEchoSequence - begin");
//      ClientCorbaClassesHolder holder = null;
//      try {
//
//          holder = testBody(idlToTest, targetDir, "testAlarmIRP",7);
//          System.out.println(holder.getAllInterfaceTypes().size());
//          holder.getMethodSignatures();
//          System.err.println("Returned: " + holder.getMethodSignatures().size());
//          // 7 methods must be returned
//          assertEquals(9, holder.getMethodSignatures().size());
//          
//          for (MethodSignature sig: holder.getMethodSignatures()) {              
//              System.err.println("*****************************");
//              
//            
//              System.err.println("Signature found:" + sig.getMethodName() + " / contains holder: " + sig.isContainsHolder());
//             
//              assertNotNull(sig.getMethod());
//              assertNotNull(sig.getChangedMethod());
//              System.err.println("Original method: " + sig.getMethod().toGenericString());
//              System.err.println("Changed method: " + sig.getChangedMethod().toGenericString());
//              
//              for (Param param: sig.getParameters()) {
//                  System.err.println("----");
//                  System.err.println("   " + param.getName());
//                  System.err.println("   " + param.getType());
//                  System.err.println("   isHolder:" + param.isHolder());
//                  System.err.println("   isHolder of:" + param.getHolderValueType());
//                  System.err.println("   new Holder Type Name:" + param.getTypeName());                  
//                  System.err.println("   isArray:" + param.isArray());
//                  System.err.println("   array dimension:" + param.getArrayDimension());
//                  System.err.println("----");
//                  
//                  if (sig.getMethodName().equals("barSeqStr")) {
//                      assertEquals(1, sig.getParameters().size());
//                      assertEquals("str", param.getName());
//                      assertEquals(java.lang.String.class, param.getType());
//                      assertEquals(false, param.isHolder());
//                      assertEquals(true, param.isArray());
//                      assertEquals(false, sig.isContainsHolder());    
//                  }
//                  
//                  if (sig.getMethodName().equals("barSeqLong")) {
//                      assertEquals(1, sig.getParameters().size());
//                      assertEquals("e", param.getName());
//                      assertEquals(Integer.TYPE, param.getType());
//                      assertEquals(false, param.isHolder());
//                      assertEquals(true, param.isArray());
//                      assertEquals(false, sig.isContainsHolder());                          
//                  }         
//
//                  if (sig.getMethodName().equals("barSeqStruct")) {
//                      assertEquals(1, sig.getParameters().size());
//                      assertEquals("seq", param.getName());
//                      assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.EchoStruct", param.getType().getName());
//                      assertEquals(false, param.isHolder());
//                      assertEquals(true, param.isArray());
//                      assertEquals(false, sig.isContainsHolder());                          
//                  }  
//                  
//                  if (sig.getMethodName().equals("barSeqStructSeqLong")) {
//                      assertEquals(3, sig.getParameters().size());
//                      assertEquals(true, sig.isContainsHolder());                          
//                      if (param.getName().equals("seq")) {                      
//                          assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.EchoStruct", param.getType().getName());
//                          assertEquals(false, param.isHolder());
//                          assertEquals(true, param.isArray());
//                      } else if (param.getName().equals("integ")) {
//                          assertEquals(Integer.TYPE, param.getType());
//                          assertEquals(false, param.isHolder());
//                          assertEquals(true, param.isArray());                              
//                      } else if (param.getName().equals("str2")) {                          
//                          assertEquals(org.omg.CORBA.StringHolder.class, param.getType());                          
//                          assertEquals(java.lang.String.class, param.getHolderValueType());
//                          assertEquals("javax.xml.ws.Holder<java.lang.String>", param.getTypeName());
//                          assertEquals(true, param.isHolder());
//                          assertEquals(false, param.isArray());                           
//                      } else {
//                          fail("Parameter with name:" + param.getName() + " is wrong.");
//                      }
//                  }    
//                  
//                  if (sig.getMethodName().equals("barSeqStrInOut")) {
//                      assertEquals(true, sig.isContainsHolder());                       
//                      assertEquals(1, sig.getParameters().size());
//                      assertEquals("str", param.getName());
//                      assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.SeqStringHolder", param.getType().getName());
//                      assertEquals("java.lang.String[]", param.getHolderValueType().getCanonicalName());
//                      assertEquals("javax.xml.ws.Holder<java.lang.String[]>", param.getTypeName());
//                      assertEquals(true, param.isHolder());
//                      assertEquals(false, param.isArray());                     
//                  }  
//                  
//                  if (sig.getMethodName().equals("barSeqLongInOut")) {
//                      assertEquals(true, sig.isContainsHolder());                       
//                      assertEquals(1, sig.getParameters().size());
//                      assertEquals("e", param.getName());
//                      assertEquals("it.imolinfo.jbi4corba.test.webservice.generator.SeqLongHolder", param.getType().getName());
//                      assertEquals("int[]", param.getHolderValueType().getCanonicalName());
//                      assertEquals("javax.xml.ws.Holder<int[]>", param.getTypeName());
//                      assertEquals(true, param.isHolder());
//                      assertEquals(false, param.isArray());                     
//                  }
//                          
//                  
//                  if (sig.getMethodName().equals("echoMatrix")) {
//                      assertEquals(false, sig.isContainsHolder()); 
//                      assertEquals(1, sig.getParameters().size());
//                      assertEquals("e", param.getName());
//                      assertEquals("java.lang.String", param.getType().getName());                      
//                      assertEquals(false, param.isHolder());
//                      assertEquals(true, param.isArray());
//                      assertEquals(2, param.getArrayDimension());
//                  }                                  
//                  
//              }
//              System.err.println("*****************************");
//          }
//
//      } catch (Exception e) {
//        String m = "Error in ... testEchoSequence:"
//                + e.getMessage();
//
//        log.error(m, e);
//        fail(m);
//      }
//      log.debug("<<<<< testEchoSequence - end");
//    }    
    
    /**
     * This is a test where we use a simple idl with sequences and inout parameters.   
     * 
     */
    public void testEchoSimpleInOut() {
      final String idlToTest = "EchoInOut.idl";
      final String targetDir = "target/testEchoInOut";

      log.debug(">>>>> testEchoSequence - begin");
      ClientCorbaClassesHolder holder = null;
      try {

          holder = testBody(idlToTest, targetDir, "testEchoInOut",0);
          MethodSignature signature = holder.getMethodSignatures().get(0);
          System.err.println("Original Method:" + signature.getMethod());         
          System.err.println("Actual Method:" + signature.getChangedMethod());
          assertNotNull( signature.getMethod());
          assertNotNull(signature.getChangedMethod());
          assertEquals(1, holder.getMethodSignatures().size());
          System.err.println("Returned: " + holder.getMethodSignatures().size());          

      } catch (Exception e) {
        String m = "Error in ... testEchoSequence:"
                + e.getMessage();

        log.error(m, e);
        fail(m);
      }
      log.debug("<<<<< testEchoSequence - end");
    }        
    
    /**
     * This is a test where we use a complex idl with sequences and inout parameters.
     * The idl is:
     * module it{
     *   module imolinfo{
     *   module jbi4corba{
     *   module test{
     *   module webservice{
     *   module generator{
     *   // struct
     *  struct EchoStruct {
     *           boolean fieldBoolean;
     *           string fieldString;
     *   };

     *   typedef sequence<long> SeqLong;
     *   typedef sequence<string> SeqString;                                     
     *  typedef sequence<EchoStruct> SeqEchoStruct;

     *   interface foo {
     *           string barSeqStr(in SeqString str);
     *           string barSeqLong(in SeqLong e);
     *           string barSeqStruct(in SeqEchoStruct seq);
     *           string barSeqStructSeqLong(in SeqEchoStruct seq, in SeqLong integ, inout string str2);
     *           string barSeqStrInOut(inout SeqString str);
     *           string barSeqLongInOut(inout SeqLong e);
     *           string barSeqStructInOut(inout SeqEchoStruct seq);
     *           };};};};};};};
     * 
     */
    public void testEchoComplex2InOut() {
      final String idlToTest = "EchoComplexInOut2.idl";
      final String targetDir = "target/testEchoComplexInOut2";

      log.debug(">>>>> testEchoComplexInOut2 - begin");
      ClientCorbaClassesHolder holder = null;
      try {

          holder = testBody(idlToTest, targetDir, "testEchoComplexInOut2",0);

          assertEquals(15, holder.getMethodSignatures().size());
          
          for (int i = 0; i < holder.getMethodSignatures().size(); i++) {
              MethodSignature signature = holder.getMethodSignatures().get(i);
              System.err.println("***********************************************"); 
              System.err.println("Original Method:" + signature.getMethod());         
              System.err.println("Actual Method:" + signature.getChangedMethod());
              System.err.println("***********************************************\n");
          }          
          

      } catch (Exception e) {
        String m = "Error in ... testEchoComplexInOut2:"
                + e.getMessage();

        log.error(m, e);
        fail(m);
      }
      log.debug("<<<<< testEchoComplexInOut2 - end");
    }     

    private void createService(ClientCorbaClassesHolder corbaServiceClasses,
      String serv,java.lang.String serviceNameSpace) throws IOException, WSDLException, Jbi4CorbaException {

      ProviderServiceDescriptor serviceDescriptor
        = new ProviderServiceDescriptor();

      serviceDescriptor.setServiceInterface(
        corbaServiceClasses.getOperationsClass());
      serviceDescriptor.setCorbaHelperClass(
        corbaServiceClasses.getHelperClass());

      serviceDescriptor.setServiceName(serv);
      if (serviceNameSpace==null){
      serviceDescriptor.setServiceNameSpace(
        serviceDescriptor.getServiceInterface().getPackage().getName());
      }
      else {
          serviceDescriptor.setServiceNameSpace(serviceNameSpace);
      }

      log.debug("service namespace:" + serviceDescriptor.getServiceNameSpace());

      ProviderServiceCreator serviceCreator = new ProviderServiceCreator();
      Service service = serviceCreator.createService(serviceDescriptor);

      log.info("service created: " + service);
    }

    /**
     * IDL -> Corba (java/classes) -> WSDL
     *
     * @param idlToTest 
     * @param targetDir 
     * @param serv 
     * @param classNumber class to test
     * 
     * @throws java.io.IOException 
     * @throws Jbi4CorbaException 
     * @throws WSDLException 
     */
    private ClientCorbaClassesHolder testBody(String idlToTest, String targetDir, String serv,int classNumber)
        throws IOException, WSDLException, Jbi4CorbaException {

        log.debug(">>>>> testBody - begin");

        JbiServiceDescriptor jbiServiceDescriptor
            = new JbiServiceDescriptor();

        jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR);
        jbiServiceDescriptor.setIdlFileName(idlToTest);

        //jbiServiceDescriptor.setServiceNameSpace("urn:unitTestNameSpace");
        jbiServiceDescriptor.setServiceNameSpace("http://echocomplex.generator.webservice.test.jbi4corba.imolinfo.it/");
        
        
        ProviderServiceClassesGenerator serviceGenerator
            = new ProviderServiceClassesGenerator();

        //List<String> jars = null;
        List<ClientCorbaClassesHolder> classes
            = serviceGenerator.generateProviderServiceClasses(
                    jbiServiceDescriptor,
                    targetDir,
                    jarFilesName, null); // new params

        log.debug("classes: " + classes);

        assertNotNull("classes must be not null", classes);
        assertFalse("classes.isEmpty() must be false", classes.isEmpty());

        // we know there is just one class
        ClientCorbaClassesHolder corbaServiceClasses = classes.get(classNumber);
        
        createService(corbaServiceClasses, serv,jbiServiceDescriptor.getServiceNameSpace());
                
        log.debug(">>>>> testBody - end");
        return corbaServiceClasses;
    }

 
}
