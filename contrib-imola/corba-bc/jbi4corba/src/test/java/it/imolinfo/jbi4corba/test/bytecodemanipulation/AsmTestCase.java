 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.bytecodemanipulation;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.UnionType;
import it.imolinfo.jbi4corba.webservice.generator.Util;
import it.imolinfo.jbi4corba.webservice.generator.UtilJavaSourceParsing;
import it.imolinfo.jbi4corba.webservice.generator.bcm.CorbaEnumAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.CorbaOnewayAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.IdlToWsdlAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.SerializableDecorationAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.SerializableInspectorAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.ValueTypeAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.WebServiceAnnotationAdapter;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceCreator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.MessagePartInfo;
import org.apache.cxf.service.model.OperationInfo;
import org.apache.cxf.service.model.SchemaInfo;
import org.apache.cxf.service.model.ServiceInfo;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaSequence;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ibm.wsdl.util.xml.DOMUtils;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;

/**
 * This class is used to test the bytecode manipulation.
 */
public class AsmTestCase extends TestCase {

  /**
   * Logger.
   */
  private static final Log log = LogFactory.getLog(AsmTestCase.class);

  /**
   * The directory of the test.
   */
  protected File testClasses = new File("target/test-classes");

  /**
   * The source directory of the test
   */
  protected File testSources = new File("src/test/java");

  /**
   * Constructor.
   * @throws ClassNotFoundException
   */
  public AsmTestCase(String arg0) throws ClassNotFoundException {
    super(arg0);

    Class.forName("org.omg.CORBA.portable.OutputStream");
    Class.forName("org.omg.CORBA.portable.InputStream");
    Class.forName("org.omg.CORBA.portable.StreamableValue");
  }

  /**
   * setUp.
   */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /**
   * tearDown.
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  // ==========================================
  //                                      TESTs
  // ==========================================

    /**
     * The test verifies if the class manipulated contains the getter and
     * the setter.
     * The class manipulated contains an attribute and an array for each
     * primitive java data.
     *
     * @see it.imolinfo.jbi4corba.webservice.generator.bcm.IdlToWsdlAdapter
     */
     public void testInitVar2() throws Exception {
         try {
             String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/TReqSeqInitVar2.class";

             byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

             tweakGetterSetterClasses(c, testClasses.getAbsolutePath(), false);

             Class tested = Class.forName("it.imolinfo.jbi4corba.test.bytecodemanipulation.TReqSeqInitVar2");

             Method [] methods = tested.getMethods();
             if (methods != null) {
                for (Method m : methods) {
                    log.debug("Method=" + m.getName());
                }
             }

             Object r = null;
             //Object o = tested.newInstance();

             r = tested.getMethod("getDataBoolean", (Class []) null);
             assertNotNull(r);

             r = tested.getMethod("setDataBoolean", new Class [] {boolean.class});
             assertNotNull(r);

             // replace the new bytecode with the old bytecode
             // to make the test repeatable.
             saveAsJavaClass(testClasses.getAbsolutePath(), c, old);
         } catch (Exception e) {
             log.error("testInitVar2", e);
             fail("ERROR:" + e.getMessage());
         }
     }

    /**
     * The test is used to verify the class ValueTypeAdapter
     *
     * @see it.imolinfo.jbi4corba.webservice.generator.bcm.ValueTypeAdapter
     */
    public void testValueTypeAdapter() throws Exception {
        try {
            String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/FakeJavaBean.class";

            byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

            tweakValueTypeAdapter(c, testClasses.getAbsolutePath());

            Class tested = Class.forName("it.imolinfo.jbi4corba.test.bytecodemanipulation.FakeJavaBean");

            assertNotNull(tested.getMethod("get", (Class []) null));
            assertNotNull(tested.getMethod("getString", String.class));
            assertNotNull(tested.getMethod("set", (Class []) null));

            try {
                tested.getMethod("isBar", (Class []) null);
                fail("I'm expecting an exception");
            } catch (NoSuchMethodException e) {
                // ok
            }

            try {
                assertNull(tested.getMethod("getFoo", (Class []) null));
                fail("I'm expecting an exception");
            } catch (NoSuchMethodException e) {
                // ok
            }

            try {
                assertNull(tested.getMethod("setBar", boolean.class));
                fail("I'm expecting an exception");
            } catch (NoSuchMethodException e) {
                // ok
            }

            try {
                assertNull(tested.getMethod("setFoo", String.class));
                fail("I'm expecting an exception");
            } catch (NoSuchMethodException e) {
                // ok
            }

            // replace the new bytecode with the old bytecode
            // to make the test repeatable.
            saveAsJavaClass(testClasses.getAbsolutePath(), c, old);
        } catch (Exception e) {
            log.error("testValueTypeAdapter", e);
            fail("ERROR:" + e.getMessage());
        }
    }

    /**
    * The test is used to verify the class ValueTypeAdapter
    *
    * @see it.imolinfo.jbi4corba.webservice.generator.bcm.ValueTypeAdapter
    */
    public void testValueTypeAdapter2() throws Exception {
      log.debug(">>>>> testValueTypeAdapter2 - begin");
        try {
            String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/TReqSeq.class";

            byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

            tweakValueTypeAdapter(c, testClasses.getAbsolutePath());

            Class tested = Class.forName("it.imolinfo.jbi4corba.test.bytecodemanipulation.TReqSeq");
            assertNotNull("Loading class failure.", tested);

            try {
                tested.getMethod("getDataBoolean", (Class []) null);
                fail("I'm expecting an exception");
            } catch (NoSuchMethodException e) {
                // ok
            }

            try {
                assertNull(tested.getMethod("setDataBoolean", boolean.class));
                fail("I'm expecting an exception");
            } catch (NoSuchMethodException e) {
                // ok
            }

            // replace the new bytecode with the old bytecode
            // to make the test repeatable.
            saveAsJavaClass(testClasses.getAbsolutePath(), c, old);

        } catch (Throwable e) {
            log.error("testValueTypeAdapter", e);
            fail("ERROR:" + e.getMessage());
        }
    }

    /**
    * The test verifies if the class manipulated contains the getter and
    * the setter.
    *
    * @see it.imolinfo.jbi4corba.webservice.generator.bcm.IdlToWsdlAdapter
    */
    public void testOctet() throws Exception {

        String [] ss = new String [] {//getter
                                    "getFieldBoolean",
                                    "getFieldChar",
                                    "getFieldWChar",
                                    "getFieldOctet",
                                    "getFieldString",
                                    "getFieldWString",
                                    "getFieldShort",
                                    "getFieldUnsignedShort",
                                    "getFieldLong",
                                    "getFieldUnsignedLong",
                                    "getFieldLongLong",
                                    "getFieldUnsignedLongLong",
                                    "getFieldFloat",
                                    "getFieldDouble",
                                    //setter
                                    "setFieldBoolean",
                                    "setFieldChar",
                                    "setFieldWChar",
                                    "setFieldOctet",
                                    "setFieldString",
                                    "setFieldWString",
                                    "setFieldShort",
                                    "setFieldUnsignedShort",
                                    "setFieldLong",
                                    "setFieldUnsignedLong",
                                    "setFieldLongLong",
                                    "setFieldUnsignedLongLong",
                                    "setFieldFloat",
                                    "setFieldDouble",
                                    // utility methods: toString(), equals()
                                    "toString",
                                    "equals"};

        Set<String> gsName = new HashSet<String>();

        gsName.addAll(Arrays.asList(ss));

        try {
            String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/EchoStructOctet.class";

            byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

            tweakGetterSetterClasses(
                    c,
                    testClasses.getAbsolutePath(), false);

            Class tested = Class.forName("it.imolinfo.jbi4corba.test.bytecodemanipulation.EchoStructOctet");

            Method [] mets = tested.getMethods();

            Set<String> metName = new HashSet<String>();
            for (int i = 0; i < mets.length; i++) {
                log.debug("METHOD[" + i + "]<" + mets[i].getName() + ">");
                metName.add(mets[i].getName());
            }

            assertTrue(metName.containsAll(gsName));

            log.debug("POST - TOSTRING[" + tested.newInstance() + "]");

            // replace the new bytecode with the old bytecode
            // to make the test repeatable.
            saveAsJavaClass(testClasses.getAbsolutePath(), c, old);

        } catch (Exception e) {
            log.error("testInitVar", e);
            fail("ERROR:" + e.getMessage());
        }
    }

  /**
   * The test verifies if the class manipulated contains
   * the initialized array.
   *
   * @see it.imolinfo.jbi4corba.webservice.generator.bcm.IdlToWsdlAdapter
   */
  public void testCorbaException() throws Exception {
    String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/MyCorbaException.class";
    try {
      byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

      tweakGetterSetterClasses(c, testClasses.getAbsolutePath(), true);

      Class tested = Class.forName(
        "it.imolinfo.jbi4corba.test.bytecodemanipulation.MyCorbaException");

      MyCorbaException mye = (MyCorbaException) tested.newInstance();

      assertNotNull(mye.getArray());
      assertEquals(0, mye.getArray().length);

      // replace the new bytecode with the old bytecode
      // to make the test repeatable.
      saveAsJavaClass(testClasses.getAbsolutePath(), c, old);
    } catch (Exception e) {
      log.error("testInitVar", e);
      fail("ERROR:" + e.getMessage());
    }
  }

    /**
    * The test verifies if the class manipulated contains the getter and
    * the setter.
    * The class manipulated contains an attribute and an array for each
    * primitive java data.
    *
    * @see it.imolinfo.jbi4corba.webservice.generator.bcm.IdlToWsdlAdapter
    */
    public void testInitVar() throws Exception {
        try {
            String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/FakeClassToManipulate.class";

            byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

            tweakGetterSetterClasses(c, testClasses.getAbsolutePath(), false);

            Class tested = Class.forName("it.imolinfo.jbi4corba.test.bytecodemanipulation.FakeClassToManipulate");

            Class [] cs = null;
            Object [] as = null;

            Object r = null;
            Object o = tested.newInstance();

            //public boolean barBoolean[];
            r = tested.getMethod("getBarBoolean", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((boolean []) r).length == 0);

            //public byte barByte[];
            r = tested.getMethod("getBarByte", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((byte []) r).length == 0);

            //public char barChar[];
            r = tested.getMethod("getBarChar", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((char []) r).length == 0);

            //public double barDouble[];
            r = tested.getMethod("getBarDouble", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((double []) r).length == 0);

            //public float barFloat[];
            r = tested.getMethod("getBarFloat", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((float []) r).length == 0);

            //public int barInt[];
            r = tested.getMethod("getBarInt", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((int []) r).length == 0);

            //public long barLong[];
            r = tested.getMethod("getBarLong", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((long []) r).length == 0);

            //public short barShort[];
            r = tested.getMethod("getBarShort", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((short []) r).length == 0);

            //public String barString[];
            r = tested.getMethod("getBarString", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((String []) r).length == 0);

            //public Object barObject[];
            r = tested.getMethod("getBarObject", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((Object []) r).length == 0);

            //public FakeObject barFakeObject[];
            r = tested.getMethod("getBarFakeObject", cs).invoke(o, as);
            assertNotNull(r);
            assertTrue(((FakeObject []) r).length == 0);

            // replace the new bytecode with the old bytecode
            // to make the test repeatable.
            saveAsJavaClass(testClasses.getAbsolutePath(), c, old);
        } catch (Exception e) {
            log.error("testInitVar", e);
            fail("ERROR:" + e.getMessage());
        }
    }


    /**
     * Adding the annotation (JSR181) to a class for WebService customization.
     *
     * @throws Exception
     */
    public void testAnnotationJsr181() throws Exception {
      // source file
      String absPath = testSources.getAbsolutePath() + File.separator
                     + "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
                     + "AnnotationJsr181.java";

      // class file
      String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
               + "AnnotationJsr181.class";

      // java name
      String qualifiedJavaName
        = "it.imolinfo.jbi4corba.test.bytecodemanipulation.AnnotationJsr181";

      try {
        //================================== PARSING
        UtilJavaSourceParsing parseUtil = new UtilJavaSourceParsing();

        List<MethodSignature> methodSignatureList
          = parseUtil.extractMethodSignature(absPath, testSources.getAbsolutePath());

        assertNotNull("I'm expecting one or more methods", methodSignatureList);

        for (int i = 0; i < methodSignatureList.size(); i++) {
          log.info("MethodSignature[" + i + "]=" + methodSignatureList.get(i));
        }

        assertEquals("I'm expecting 1 method", 1, methodSignatureList.size());

        MethodSignature signatureEchoString = methodSignatureList.get(0);

        assertEquals("echoString", signatureEchoString.getMethodName());
        assertEquals("String", signatureEchoString.getReturnType());

        assertEquals(1, signatureEchoString.getParameters().size());        

        assertEquals("param1", signatureEchoString.getParameters().get(0).getName());
        assertEquals("String", signatureEchoString.getParameters().get(0).getTypeName());

        //================================== BYTECODE
        byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

        /* The method to add the annotations !!
         */        
        tweakCorbaOperation(qualifiedJavaName,
                            testClasses.getAbsolutePath(),
                            methodSignatureList);

        Class tested = Class.forName(qualifiedJavaName);

        // XXX assert on the tested class ?

        // annotation on the class and on the methods
        debug("CLASS", tested.getAnnotations());
        debug(tested.getMethods());

        //================================== Java To Wsdl
        Service service = createService(tested, "CRB108");
        
       
        assertEquals("I'm expecting 1 operation",
                    1, service.getServiceInfos().get(0).getInterface().getOperations().size());
        
        Collection<OperationInfo> opInfos =  service.getServiceInfos().get(0).getInterface().getOperations();
                
        OperationInfo opInfo = opInfos.iterator().next();
        assertEquals(opInfo.getName().getLocalPart(), "echoString");
        assertNotNull("I'm expecting an operation named 'echoString'", opInfo);
                            
        // Gets the schema info to control if the annotation has worked.
        // Asserts the element name in the complex type message element 
        ServiceInfo si = (ServiceInfo)service.getServiceInfos().get(0);        
        
        // Gets the parameter name 
        String paramName = getParameterNameForOperationWithOneParameter(si, opInfo);
        
        assertEquals("param1", paramName);

        // replace the new bytecode with the old bytecode
        // to make the test repeatable.
        saveAsJavaClass(testClasses.getAbsolutePath(), c, old);
      } catch (Exception e) {
        log.error("testAnnotationJsr181", e);
        e.printStackTrace();
        fail("testAnnotationJsr181 - ERROR:" + e.getMessage());
      }
    }

    /**
     * Adding the annotation (JSR181) to a class for WebService customization.
     *
     * @throws Exception
     */
    public void testInterfaceJsr181() throws Exception {
      // source file
      String absPath = testSources.getAbsolutePath() + File.separator
                     + "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
                     + "InterfaceJsr181.java";

      // class file
      String c = "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
               + "InterfaceJsr181.class";

      // java name
      String qualifiedJavaName
        = "it.imolinfo.jbi4corba.test.bytecodemanipulation.InterfaceJsr181";

      try {
        //================================== PARSING
        UtilJavaSourceParsing parseUtil = new UtilJavaSourceParsing();

        List<MethodSignature> methodSignatureList
          = parseUtil.extractMethodSignature(absPath, testSources.getAbsolutePath());

        assertNotNull("I'm expecting one or more methods", methodSignatureList);

        for (int i = 0; i < methodSignatureList.size(); i++) {
          log.debug("MethodSignature[" + i + "]=" + methodSignatureList.get(i));
        }

        assertEquals("I'm expecting 1 method", 1, methodSignatureList.size());

        MethodSignature signatureEchoString = methodSignatureList.get(0);

        assertEquals("echoString", signatureEchoString.getMethodName());
        assertEquals("String", signatureEchoString.getReturnType());

        assertEquals(1, signatureEchoString.getParameters().size());        

        assertEquals("param1", signatureEchoString.getParameters().get(0).getName());
        assertEquals("String", signatureEchoString.getParameters().get(0).getTypeName());

        //================================== BYTECODE
        byte [] old = getOldBytecode(c, testClasses.getAbsolutePath());

        /* The method to add the annotations !!
         */
        tweakCorbaOperation(qualifiedJavaName,
                            testClasses.getAbsolutePath(),
                            methodSignatureList);

        System.err.println("Qualified java name: " + qualifiedJavaName);
        Class tested = Class.forName(qualifiedJavaName);

        // XXX assert on the tested class ?

        // annotation on the class and on the methods
        debug("CLASS", tested.getAnnotations());
        debug(tested.getMethods());

        //================================== Java To Wsdl
        Service service = createService(tested, "CRB108");

        Collection<OperationInfo> opInfos =  service.getServiceInfos().get(0).getInterface().getOperations();
                
        OperationInfo opInfo = opInfos.iterator().next();
        assertEquals(opInfo.getName().getLocalPart(), "echoString");
        assertNotNull("I'm expecting an operation named 'echoString'", opInfo);

        MessagePartInfo mpInfo
        = (MessagePartInfo) opInfo.getInput().getMessageParts().get(0);
                        
        
        System.err.println("**************: " + mpInfo.getName().getLocalPart());
        
        // Gets the parameter name 
        
        // Gets the schema info to control if the annotation has worked.
        // Asserts the element name in the cmplex type message element 
        ServiceInfo si = (ServiceInfo)service.getServiceInfos().get(0);
        
        String paramName = getParameterNameForOperationWithOneParameter(si, opInfo);
        
        assertEquals("param1", paramName);

        // replace the new bytecode with the old bytecode
        // to make the test repeatable.
        saveAsJavaClass(testClasses.getAbsolutePath(), c, old);
      } catch (Exception e) {
        log.error("testInterfaceJsr181", e);
        fail("testInterfaceJsr181 - ERROR:" + e.getMessage());
      }
    }

  public void testSerializableDecoration() {

    final String absPath = testClasses.getAbsolutePath() + File.separator
                        + "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
                        + "FakeNotYetSerial.class";

    // first ... check the situation
    try {
      tweakSerializableInspection(absPath, false, null);
    } catch (Exception e) {
      log.error("testSerializableDecoration:" + e.getMessage(), e);
      fail("testSerializableDecoration: I do not expect an exception.");
    }

    // ... ok the class is not yet serializable ...
    try {
      tweakSerializableDecoration(absPath, new Long(123456789L));
    } catch (Exception e) {
      log.error("testSerializableDecoration:" + e.getMessage(), e);
      fail("testSerializableDecoration: I do not expect an exception.");
    }

  }

  public void testSerializableInspection() {

    final String absPath = testClasses.getAbsolutePath() + File.separator
                        + "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
                        + "FakeSerial.class";

    try {
      tweakSerializableInspection(absPath,true,new Long(4105365034861528295L));
    } catch (Exception e) {
      log.error("testSerializableInspection:" + e.getMessage(), e);
      fail("testSerializableInspection: I do not expect an exception.");
    }

  }

  public void testCorbaEnumInspection() {

    final String absPath = testClasses.getAbsolutePath() + File.separator
                        + "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
                        + "CorbaEnum.class";

    List<String> expectedLabel = new ArrayList<String>();
    expectedLabel.add("E1");
    expectedLabel.add("E2");
    expectedLabel.add("E3");

    try {
      tweakCorbaEnumInspection(absPath, true, expectedLabel);
    } catch (Exception e) {
      log.error("testCorbaEnumInspection:" + e.getMessage(), e);
      fail("testCorbaEnumInspection: I do not expect an exception.");
    }

  }

  public void testOnewayInspection() {

    final String absPath = testClasses.getAbsolutePath() + File.separator
                        + "it/imolinfo/jbi4corba/test/bytecodemanipulation/"
                        + "_EchoOnewayInterfaceStub.class";

    try {
      List<String> onewayop = new ArrayList<String>();
      onewayop.add("echoOneway");

      tweakOnewayInspection(absPath, onewayop);

    } catch (Exception e) {
      log.error("testCorbaEnumInspection:" + e.getMessage(), e);
      fail("testCorbaEnumInspection: I do not expect an exception.");
    }

  }

    // ==========================================
    //                            Utility methods
    // ==========================================

  private Service createService(Class clazz, String serv)
    throws IOException, WSDLException, Jbi4CorbaException {

    ProviderServiceDescriptor sd = new ProviderServiceDescriptor();

    sd.setServiceInterface(clazz);

    //sd.setServiceName(serv);
    sd.setServiceNameSpace(sd.getServiceInterface().getPackage().getName());

    log.debug("service namespace: " + sd.getServiceNameSpace());

    ProviderServiceCreator serviceCreator = new ProviderServiceCreator();
    Service service = serviceCreator.createService(sd);

    log.info("service created: " + service);
    return service;
  }

  private void debug(Method [] methodArray) {
    // annotation on the methods
    if (methodArray == null) {
      log.debug("methodArray is null.");
    } else if (methodArray.length == 0) {
      log.debug("methodArray is empty.");
    } else {
      for (int i = 0; i < methodArray.length; i++) {
        String met = "METHOD[" + i + "]{" + methodArray[i].getName() + "}";

        Annotation [] annotationArray = methodArray[i].getAnnotations();
        debug(met, annotationArray);

        Annotation [] [] parameterAnnotationArray
          = methodArray[i].getParameterAnnotations();

        debug("PARAMETER", parameterAnnotationArray);
      }
    }
  }

  private void debug(String message, Annotation [][] annotationArray) {
    if (annotationArray == null) {

      log.debug("Message=" + message + "; Annotation is null.");
      return;
    }
    // else
    int iSize = annotationArray.length;
    log.debug("Message=" + message + "; iSize=" + iSize);

    for (int i = 0; i < iSize; i++) {
      log.debug("Message=" + message + "; annotationArray[" + i + "].len="
        + annotationArray[i].length);

      for (int j = 0; j < annotationArray[i].length; j++) {
        Annotation a = annotationArray[i][j];
        log.debug("Message=" + message
          + "; Annotation[" + i + "][" + j + "]=" + a);
      }
    }
  }

  private void debug(String message, Annotation [] annotationArray) {
    if (annotationArray == null) {

      log.debug("Message=" + message + "; Annotation is null.");

    } else if (annotationArray.length == 0) {

      log.debug("Message=" + message + "; Annotation is empty.");

    } else {
      for (int i = 0; i < annotationArray.length; i++) {
        log.debug("Message=" + message
          + "; Annotation[" + i + "]=" + annotationArray[i]);
      }
    }
  }

  private void tweakSerializableDecoration(
    String absPath,
    Long serialVersionUidExpected) throws ClassGenerationException {

    ClassWriter  cw = new ClassWriter(true); // visitMaxs
    ClassVisitor cc = new CheckClassAdapter(cw);
    StringWriter sw = new StringWriter();
    ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

    SerializableDecorationAdapter cv
      = new SerializableDecorationAdapter(tv, serialVersionUidExpected);

    ClassReader cr = Util.getAsmCLassReader(absPath);

    cr.accept(cv, true);
    log.debug("ClassReader.accept ... done");

    // asserts
    assertTrue("The class is serializable=" + true,
      cv.getClassMetaInfo().isSerializable());

    assertEquals("The serialVersionUID is " + serialVersionUidExpected,
      serialVersionUidExpected,
      cv.getClassMetaInfo().getClassSerialVersionUid());
  }

  private void tweakSerializableInspection(
    String absPath,
    boolean serializableExpected,
    Long serialVersionUidExpected) throws ClassGenerationException {

    ClassWriter  cw = new ClassWriter(true); // visitMaxs
    ClassVisitor cc = new CheckClassAdapter(cw);
    StringWriter sw = new StringWriter();
    ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

    SerializableInspectorAdapter cv = new SerializableInspectorAdapter(tv);

    ClassReader cr = Util.getAsmCLassReader(absPath);

    cr.accept(cv, true);
    log.debug("ClassReader.accept ... done");

    // asserts
    assertEquals("The class is serializable=" + serializableExpected,
      serializableExpected, cv.getClassMetaInfo().isSerializable());

    assertEquals("The serialVersionUID is " + serialVersionUidExpected,
      serialVersionUidExpected,
      cv.getClassMetaInfo().getClassSerialVersionUid());
  }

  private void tweakCorbaEnumInspection(
    String absPath,
    boolean expectedIsCorbaEnum,
    List<String> expectedLabel) throws ClassGenerationException {

    ClassWriter  cw = new ClassWriter(true); // visitMaxs
    ClassVisitor cc = new CheckClassAdapter(cw);
    StringWriter sw = new StringWriter();
    ClassVisitor cv = new TraceClassVisitor(cc, new PrintWriter(sw));

    CorbaEnumAdapter ca = new CorbaEnumAdapter(cv);

    ClassReader cr = Util.getAsmCLassReader(absPath);

    cr.accept(ca, true);
    log.debug("ClassReader.accept ... done");

    // asserts
    assertEquals(expectedIsCorbaEnum, ca.isCorbaEnum());

    assertEquals(expectedLabel.size(), ca.getEnumLabelList().size());

    for (int i = 0; i < expectedLabel.size(); i++) {
      assertEquals(expectedLabel.get(i), ca.getEnumLabelList().get(i));
    }

  }

  private void tweakOnewayInspection(
    String absPath,
    List<String> onewayop) throws ClassGenerationException {

    ClassWriter  cw = new ClassWriter(true); // visitMaxs
    ClassVisitor cc = new CheckClassAdapter(cw);
    StringWriter sw = new StringWriter();
    ClassVisitor cv = new TraceClassVisitor(cc, new PrintWriter(sw));

    CorbaOnewayAdapter ca = new CorbaOnewayAdapter(cv, sw);

    ClassReader cr = Util.getAsmCLassReader(absPath);

    cr.accept(ca, true);
    log.debug("ClassReader.accept ... done");

    // asserts
    List<String> onewayopObtained = ca.getOnewayOperationList();

    assertEquals(onewayop.size(), onewayopObtained.size());
    assertEquals(onewayop.get(0), onewayopObtained.get(0));
  }

  private void tweakCorbaOperation(String qualifiedJavaName, String dir,
    List<MethodSignature> methodSignatureList) throws ClassGenerationException {

    ClassWriter  cw = new ClassWriter(true); // visitMaxs
    ClassVisitor cc = new CheckClassAdapter(cw);
    StringWriter sw = new StringWriter();
    ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

    WebServiceAnnotationAdapter cv
      = new WebServiceAnnotationAdapter(tv, cw, methodSignatureList, null, null, null, null,null, new HashSet<Class>());

    String absPath = dir + File.separator
      + qualifiedJavaName.replace('.', File.separatorChar) + ".class";

    ClassReader cr = Util.getAsmCLassReader(absPath);

    cr.accept(cv, true);
    log.debug("ClassReader.accept ... done");

    log.debug("output of tracer during creation of class: " + absPath + "\n"
      + sw.toString());

    byte [] newBytecode = cw.toByteArray();

    log.debug("absPath=" + absPath);
    Util.saveAsJavaClass(absPath, newBytecode);
  }


    /**
    * This method if used to extract the original bytecode.
    *
    * @param    className            The class to save.
    * @param    classesDirName        Where the class is located.
    *
    * @return    The class.
    *
    * @throws    ClassGenerationException
    */
    private byte [] getOldBytecode(
        String className,
        String classesDirName) throws ClassGenerationException {

        log.debug(">>>>> getOldBytecode - begin");

        ClassWriter  cw = new ClassWriter(true); // visitMaxs
        ClassAdapter cv = new ClassAdapter(cw);
        ClassReader  cr = getAsmCLassReader(classesDirName, className);

        cr.accept(cv, true);

        log.debug("<<<<< getOldBytecode - end");
        return cw.toByteArray();
    }

    /**
    * XXX description
    *
    * @param className            The class to manipulate.
    * @param classesDirName    The directory where the class is located.
    *
    * @return    the full class name.
    *
    * @throws    ClassGenerationException
    */
    private String tweakValueTypeAdapter(
            String className,
            String classesDirName) throws ClassGenerationException {

        log.debug(">>>>>>>>>> tweakValueTypeAdapter - begin");

        log.debug("tweakValueTypeAdapter; "
                + "class: " + className + " in dir: " + classesDirName);

        ClassWriter  cw = new ClassWriter(true); // visitMaxs
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        ValueTypeAdapter cv = new ValueTypeAdapter(tv, cw, className);

        ClassReader cr = getAsmCLassReader(classesDirName, className);

        log.debug("getAsmCLassReader ... done");

        cr.accept(cv, true);
        log.debug("ClassReader.accept ... done");

        log.debug("output of tracer during creation of class: "
                + className + "\n" + sw.toString());

        byte [] newBytecode = cw.toByteArray();

        // write class in the right place

        String relativeFileName = className.replace('/', File.separatorChar);

        saveAsJavaClass(classesDirName, relativeFileName, newBytecode);

        log.debug("<<<<<<<<<< tweakValueTypeAdapter - end");
        return className.replace('/', '.');
    }


    /**
    * Bytecode manipulation.
    * Adding the getter and setter methods for all the public members.
    *
    * @param className            The class to manipulate.
    * @param classesDirName    The directory where the class is located.
    * @param isException        The class is an Exception
    *
    * @return    the full class name.
    *
    * @throws    ClassGenerationException
    */
    private String tweakGetterSetterClasses(
            String className,
            String classesDirName, boolean isException) throws ClassGenerationException {

        log.debug(">>>>>>>>>> tweakGetterSetterClasses - begin");

        log.debug("tweakGetterSetterClasses; "
                + "class: " + className + " in dir: " + classesDirName);

        ClassWriter  cw = new ClassWriter(true); // visitMaxs
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        IdlToWsdlAdapter cv = new IdlToWsdlAdapter(tv, cw, className, 
                new HashMap<String, UnionType>(), 
                new HashMap<String, String>(),  new HashMap<String, InterfaceType>(),new HashMap<String, String>(),
                isException,false);

        ClassReader cr = getAsmCLassReader(classesDirName, className);

        log.debug("getAsmCLassReader ... done");

        cr.accept(cv, true);
        log.debug("ClassReader.accept ... done");

        log.debug("output of tracer during creation of class: "
                + className + "\n" + sw.toString());

        byte [] newBytecode = cw.toByteArray();

        // write class in the right place

        String relativeFileName = className.replace('/', File.separatorChar);

        saveAsJavaClass(classesDirName, relativeFileName, newBytecode);

        log.debug("<<<<<<<<<< tweakGetterSetterClasses - end");
        return className.replace('/', '.');
    }

    /**
    * This method load a class.
    *
    * @param    classesDirName    Where the class is located.
    * @param    className        The class name.
    *
    * @return    The object used to access the class.
    *
    * @throws    ClassGenerationException
    */
    private ClassReader getAsmCLassReader(String classesDirName,
        String className) throws ClassGenerationException {
        log.debug(">>>>> getAsmCLassReader - begin");
        ClassReader cr = null;
        try {

            cr = new ClassReader(
                    new FileInputStream(
                        classesDirName + File.separator + className));

        } catch (IOException e) {
            String msg = "could not instantiate class reader for class: "
                        + className + " in dir: " + classesDirName;
            log.error(msg, e);
            throw new ClassGenerationException(msg, e);
        }
        log.debug("<<<<< getAsmCLassReader - end. ClassReader=" + cr);
        return cr;
    }

    /**
    * This method is used to save a class.
    *
    * @param    classesDirName        Where the class is located.
    * @param    relativeFileName    The class to save.
    * @param    newBytecode            The bytecode of the class.
    *
    * @throws    ClassGenerationException
    */
    private void saveAsJavaClass(
        String classesDirName,
        String relativeFileName,
        byte [] newBytecode) throws ClassGenerationException {

        log.debug(">>>>> saveAs - begin");
        try {
            FileOutputStream fos = new FileOutputStream(
                classesDirName + File.separator + relativeFileName);

            fos.write(newBytecode);
            fos.close();
        } catch (FileNotFoundException e) {
            String m = "could not instantiate file writer for class: "
                    + relativeFileName + " in dir: " + classesDirName;
            log.error(m, e);
            throw new ClassGenerationException(m, e);
        } catch (IOException e) {
            String m = "could not save class: " + relativeFileName + " in dir: "
                    + classesDirName;
            log.error(m, e);
            throw new ClassGenerationException(m, e);
        }
        log.debug("<<<<< saveAs - end");
    }
    
    /**
     * Return the parameter name for the operation (the WSDL is in the Wrapped Document literal form).
     * The operation must have only one parameter (the parameter returned is the first).
     * 
     * @param si The service info
     * @param oi The operation info
     * @return the param name
     */
    private String getParameterNameForOperationWithOneParameter(ServiceInfo si, OperationInfo oi) {        
        String paramName = null;
        SchemaInfo schema = (SchemaInfo)si.getSchemas().iterator().next();        
        NodeList list  = schema.getElement().getChildNodes();
          
        QName messagePartElementQName = oi.getInput().getMessagePart(0).getElementQName();
                
        XmlSchemaElement elementOfMessage = schema.getElementByQName(messagePartElementQName);
        Object schemaType = elementOfMessage.getSchemaType();
        if (schemaType instanceof XmlSchemaComplexType) {
        	XmlSchemaComplexType schemaComplexType = (XmlSchemaComplexType)schemaType;
        	Object par = schemaComplexType.getParticle();
        	if (par instanceof XmlSchemaSequence) {
        		XmlSchemaSequence schemaSequence = (XmlSchemaSequence) par;
        		XmlSchemaElement element = (XmlSchemaElement) schemaSequence.getItems().getItem(0);
        		paramName = element.getName();
        	}        	        	
        }

        return paramName;
    }

}
