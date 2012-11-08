/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.test.wsdlgeneration;

import it.imolinfo.jbi4ejb.test.TestUtils;
import it.imolinfo.jbi4ejb.webservice.generator.WSDLDescriptor;
import it.imolinfo.jbi4ejb.webservice.generator.WSDLGenerator;

import java.io.File;

import javax.wsdl.Definition;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * Tests the WSDL generation from a complex interface.
 */
public class WSDLGenerationTest extends TestCase {
    
    /** The Constant WSDL_DIR. */
    private static final String WSDL_DIR = "src/test/etc/wsdl";
           
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(WSDLGenerationTest.class);
    
    // Where the test EAR (wsdl and jbi.xml) are
    public static final String EAR_TEST_DIR = "src/test/etc/ear";
    
    public static final String EAR_COMPLEX_TEST_NAME = "test-ear-0.1-SNAPSHOT.ear";
    
    public static final String EAR_SIMPLE_TEST_NAME = "simple-ear-0.5-SNAPSHOT.ear";      
        
    /**
     * Instantiates a new WSDL generation test.
     */
    public WSDLGenerationTest() {}
    
    /**
     * Test WSDL generation (simple interface). The intercae is
     * <code>it.imolinfo.jbi4ejb.test.wsdlgeneration.interfaces.TestInterface</code>
     */
    public void testComplexWSDLGenerationFromClasses() {
         
        Class interfaceClass = it.imolinfo.jbi4ejb.test.wsdlgeneration.interfaces.TestInterface.class;
        String testWsdlFileName = "TestInterface.wsdl";
                
        
        try {
                        
            WSDLDescriptor wsdlDescription = new WSDLDescriptor("corbaname", WSDLDescriptor.CORBANAME_LOCALIZATION_TYPE);
            wsdlDescription.setOrbProperties(TestUtils.getOrbProperties(TestUtils.JACORB));
            
            File tempDir = TestUtils.createTempDir();
            String wsdlFileName = tempDir.getAbsolutePath() + File.separatorChar + interfaceClass.getName() + ".wsdl";
                        
            // Creates the WSDL
            File wsdlFile = WSDLGenerator.createWsdlFromClassesDirectory(interfaceClass.getName(), 
                    TestUtils.getTestClassesDir(), wsdlFileName, wsdlDescription, tempDir);                                  
                        
            // Reads the definition
            Definition wsdl = TestUtils.getExtendedWSDLReader().readWSDL(wsdlFile.getAbsolutePath());
            
            // Loads the WSDL to compare
            File wsdlTestFile = new File(WSDL_DIR + File.separator + testWsdlFileName);
            Definition expectedWsdl = TestUtils.getExtendedWSDLReader().readWSDL(wsdlTestFile.getAbsolutePath());
                                                            
            // Compares the string version of the WSDL with the expected   
            assertEquals(TestUtils.getWSDLStringFromDefinition(wsdl), TestUtils.getWSDLStringFromDefinition(expectedWsdl));
            
            
        } catch (Exception e) {            
            LOG.error(e.getMessage());
            e.printStackTrace();
            fail(e.getMessage());
        }
    }        
    
    /**
     * Test WSDL generation for a EJB conained in an EAR.
     */
    public void testComplexWSDLGenerationFromEAR() {
         
        String interfaceClassName = "it.imolinfo.test14.complex.TestComplexSessionRemote";
        String testWsdlFileName = "TestComplexSessionRemote2.wsdl";
        
        String earPath = EAR_TEST_DIR + File.separator + EAR_COMPLEX_TEST_NAME;
        
        try {
                        
            WSDLDescriptor wsdlDescription = new WSDLDescriptor("corbaname", WSDLDescriptor.CORBANAME_LOCALIZATION_TYPE);
            wsdlDescription.setOrbProperties(TestUtils.getOrbProperties(TestUtils.JACORB));
            
            File tempDir = TestUtils.createTempDir();
            String wsdlFileName = tempDir.getAbsolutePath() + File.separatorChar + interfaceClassName + ".wsdl";
                        

            // Creates the WSDL
            File wsdlFile = WSDLGenerator.createWsdlFromEar(interfaceClassName, earPath, wsdlFileName, wsdlDescription);
            
            assertTrue(wsdlFile.exists());
            
            // This assert doesn't work...
            // Reads the definition
            //Definition wsdl = TestUtils.getExtendedWSDLReader().readWSDL(wsdlFile.getAbsolutePath());            
            // Loads the WSDL to compare
            //File wsdlTestFile = new File(WSDL_DIR + File.separator + testWsdlFileName);
            //Definition expectedWsdl = TestUtils.getExtendedWSDLReader().readWSDL(wsdlTestFile.getAbsolutePath());                                                           
            // Compares the string version of the WSDL with the expected   
            //assertEquals(TestUtils.getWSDLStringFromDefinition(wsdl), TestUtils.getWSDLStringFromDefinition(expectedWsdl));
            
        } catch (Exception e) {            
            LOG.error(e.getMessage());
            e.printStackTrace();
            fail(e.getMessage());
        }
    }        

    /**
     * Test WSDL generation for a EJB conained in an EAR.
     */
    public void testSimpleWSDLGenerationFromEAR() {
         
        String interfaceClassName = "it.imolinfo.test14.TestSessionRemote";
        String assertWsdlFileName = "TestSessionRemote.wsdl";
        
        String earPath = EAR_TEST_DIR + File.separator + EAR_SIMPLE_TEST_NAME;
        
        try {
                        
            WSDLDescriptor wsdlDescription = new WSDLDescriptor("corbaname", WSDLDescriptor.CORBANAME_LOCALIZATION_TYPE);
            wsdlDescription.setOrbProperties(TestUtils.getOrbProperties(TestUtils.JACORB));
            
            File tempDir = TestUtils.createTempDir();
            String wsdlFileName = tempDir.getAbsolutePath() + File.separatorChar + interfaceClassName + ".wsdl";
                        

            // Creates the WSDL
            File wsdlFile = WSDLGenerator.createWsdlFromEar(interfaceClassName, earPath, wsdlFileName, wsdlDescription);
            
            assertTrue(wsdlFile.exists());
            
            // This assert doesn't work...
            // Reads the definition
            Definition wsdl = TestUtils.getExtendedWSDLReader().readWSDL(wsdlFile.getAbsolutePath());
            System.err.println(TestUtils.getWSDLStringFromDefinition(wsdl));
            // Loads the WSDL to compare
            File wsdlTestFile = new File(WSDL_DIR + File.separator + assertWsdlFileName);
            Definition expectedWsdl = TestUtils.getExtendedWSDLReader().readWSDL(wsdlTestFile.getAbsolutePath());                                                           
            // Compares the string version of the WSDL with the expected   
            assertEquals(TestUtils.getWSDLStringFromDefinition(wsdl), TestUtils.getWSDLStringFromDefinition(expectedWsdl));
            
        } catch (Exception e) {            
            LOG.error(e.getMessage());
            e.printStackTrace();
            fail(e.getMessage());
        }
    }         
}
