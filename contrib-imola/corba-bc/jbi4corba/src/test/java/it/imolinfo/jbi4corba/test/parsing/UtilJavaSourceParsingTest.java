 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.parsing;

import junit.framework.TestCase;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.UtilJavaSourceParsing;

import java.io.File;
import java.util.List;

public class UtilJavaSourceParsingTest extends TestCase {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(UtilJavaSourceParsingTest.class);

  /**
   * The directory where we place the java source files to parse.
   */
  public static final String JAVADIR = "src/test/etc/javasource/";

  /**
   * Testing for ... "Echo.idl"
   */
  public void testExtractMethodSignature() {
    final String fileToTest = "TargetInterface.java";

    LOG.debug(">>>>> testExtractMethodSignature - begin");
    try {

      UtilJavaSourceParsing util = new UtilJavaSourceParsing();

      String absPath = new File(JAVADIR + fileToTest).getAbsolutePath();      
      String srcPath = new File(JAVADIR).getAbsolutePath();
      
      LOG.debug("absPath=" + absPath);

      List<MethodSignature> msList = util.extractMethodSignature(absPath, srcPath);

      assertNotNull("The list must be not null", msList);
      assertTrue("The list must contain 7 methods", msList.size() == 7);

      // METHOD # 0 # public abstract Integer echoInterger();
      MethodSignature m = msList.get(0);
      LOG.debug("Method 0:" + m);

      assertEquals("echoInterger", m.getMethodName());
      assertEquals("Integer", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      // METHOD # 1 # public String echoCharString(char charParam,String stringParam);
      m = msList.get(1);
      LOG.debug("Method 1:" + m);

      assertEquals("echoCharString", m.getMethodName());
      assertEquals("String", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 2 parameters name",
                 m.getParameters().size() == 2);

      assertEquals("charParam", m.getParameters().get(0).getName());
      assertEquals("stringParam", m.getParameters().get(1).getName());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 2 parameters data types",
                 m.getParameters().size() == 2);

      assertEquals("char", m.getParameters().get(0).getTypeName());
      assertEquals("String", m.getParameters().get(1).getTypeName());

      // METHOD # 2 # public FakeClass echoFake(FakeClass fake);
      m = msList.get(2);
      LOG.debug("Method 2:" + m);

      assertEquals("echoFake", m.getMethodName());
      assertEquals("FakeClass", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 1 parameters name",
                 m.getParameters().size() == 1);
      assertEquals("fake", m.getParameters().get(0).getName());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 1 parameters data types",
                 m.getParameters().size() == 1);
      assertEquals("FakeClass", m.getParameters().get(0).getTypeName());

      // METHOD # 3 # public int echoIntDouble(int intParam,double doubleParam);
      m = msList.get(3);
      LOG.debug("Method 3:" + m);

      assertEquals("echoIntDouble", m.getMethodName());
      assertEquals("int", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 2 parameters name",
                 m.getParameters().size() == 2);
      assertEquals("intParam", m.getParameters().get(0).getName());
      assertEquals("doubleParam", m.getParameters().get(1).getName());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 1 parameters data types",
                 m.getParameters().size() == 2);
      assertEquals("int", m.getParameters().get(0).getTypeName());
      assertEquals("double", m.getParameters().get(1).getTypeName());

      // METHOD # 4 # public void echoVoid();
      m = msList.get(4);
      LOG.debug("Method 4:" + m);

      assertEquals("echoVoid", m.getMethodName());
      assertEquals("void", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters data types",
                 m.getParameters().size() == 0);

      // METHOD # 5 # public void echoVoid2();
      m = msList.get(5);
      LOG.debug("Method 5:" + m);

      assertEquals("echoVoid2", m.getMethodName());
      assertEquals("void", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters data types",
                 m.getParameters().size() == 0);

      // METHOD # 6 # public int echoInt2();
      m = msList.get(6);
      LOG.debug("Method 6:" + m);

      assertEquals("echoInt2", m.getMethodName());
      assertEquals("int", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters data types",
                 m.getParameters().size() == 0);

    } catch (Exception e) {
        String m = "Error in ... testExtractMethodSignature:" + e.getMessage();
        LOG.error(m, e);
        fail(m);
    }
    LOG.debug("<<<<< testExtractMethodSignature - end");
  }
  
  /**
   * Testing for method signature with only ona package
   */
  public void testExtractMethodSignatureOnePackage() {
    final String fileToTest = "TargetInterfaceOnePackage.java";

    LOG.debug(">>>>> testExtractMethodSignatureOnePackage - begin");
    try {

      UtilJavaSourceParsing util = new UtilJavaSourceParsing();

      String absPath = new File(JAVADIR + fileToTest).getAbsolutePath();      
      String srcPath = new File(JAVADIR).getAbsolutePath();
      
      LOG.debug("absPath=" + absPath);

      List<MethodSignature> msList = util.extractMethodSignature(absPath, srcPath);

      assertNotNull("The list must be not null", msList);
      assertTrue("The list must contain 7 methods", msList.size() == 7);

      // METHOD # 0 # public abstract Integer echoInterger();
      MethodSignature m = msList.get(0);
      LOG.debug("Method 0:" + m);

      assertEquals("echoInterger", m.getMethodName());
      assertEquals("Integer", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      // METHOD # 1 # public String echoCharString(char charParam,String stringParam);
      m = msList.get(1);
      LOG.debug("Method 1:" + m);

      assertEquals("echoCharString", m.getMethodName());
      assertEquals("String", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 2 parameters name",
                 m.getParameters().size() == 2);

      assertEquals("charParam", m.getParameters().get(0).getName());
      assertEquals("stringParam", m.getParameters().get(1).getName());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 2 parameters data types",
                 m.getParameters().size() == 2);

      assertEquals("char", m.getParameters().get(0).getTypeName());
      assertEquals("String", m.getParameters().get(1).getTypeName());

      // METHOD # 2 # public FakeClass echoFake(FakeClass fake);
      m = msList.get(2);
      LOG.debug("Method 2:" + m);

      assertEquals("echoFake", m.getMethodName());
      assertEquals("FakeClassOnePackage", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 1 parameters name",
                 m.getParameters().size() == 1);
      assertEquals("fake", m.getParameters().get(0).getName());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 1 parameters data types",
                 m.getParameters().size() == 1);
      assertEquals("freestyle.FakeClassOnePackage", m.getParameters().get(0).getTypeName());

      // METHOD # 3 # public int echoIntDouble(int intParam,double doubleParam);
      m = msList.get(3);
      LOG.debug("Method 3:" + m);

      assertEquals("echoIntDouble", m.getMethodName());
      assertEquals("int", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 2 parameters name",
                 m.getParameters().size() == 2);
      assertEquals("intParam", m.getParameters().get(0).getName());
      assertEquals("doubleParam", m.getParameters().get(1).getName());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 1 parameters data types",
                 m.getParameters().size() == 2);
      assertEquals("int", m.getParameters().get(0).getTypeName());
      assertEquals("double", m.getParameters().get(1).getTypeName());

      // METHOD # 4 # public void echoVoid();
      m = msList.get(4);
      LOG.debug("Method 4:" + m);

      assertEquals("echoVoid", m.getMethodName());
      assertEquals("void", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters data types",
                 m.getParameters().size() == 0);

      // METHOD # 5 # public void echoVoid2();
      m = msList.get(5);
      LOG.debug("Method 5:" + m);

      assertEquals("echoVoid2", m.getMethodName());
      assertEquals("void", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters data types",
                 m.getParameters().size() == 0);

      // METHOD # 6 # public int echoInt2();
      m = msList.get(6);
      LOG.debug("Method 6:" + m);

      assertEquals("echoInt2", m.getMethodName());
      assertEquals("int", m.getReturnType());

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters name",
                 m.getParameters().size() == 0);

      assertNotNull("The list must be not null", m.getParameters());
      assertTrue("The list must contain 0 parameters data types",
                 m.getParameters().size() == 0);

    } catch (Exception e) {
        String m = "Error in ... testExtractMethodSignatureOnePackage:" + e.getMessage();
        LOG.error(m, e);
        fail(m);
    }
    LOG.debug("<<<<< testExtractMethodSignatureOnePackage - end");
  }

}
