/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.test.classesgeneration;

import it.imolinfo.jbi4ejb.runtime.ejbproxy.EJBProxyUtils;
import it.imolinfo.jbi4ejb.test.TestUtils;

import java.io.File;
import java.util.Properties;

import junit.framework.TestCase;

/**
 * Tests the classes generation and the service invocation.
 * 
 * @author marco 
 */
public class ClassesGenerationTest extends TestCase {
   
    // The test working path
    public static final String WORKING_PATH = "target/test_classes_generation";
    
    // The test wsdl working path
    public static final String WSDL_DIR = "src/test/etc/wsdl";
    
    // The complex test wsdl
    public static final String WSDL_COMPLEX_FILE_NAME = "TestComplexSessionRemote.wsdl";
    
    // The simple test wsdl
    public static final String WSDL_SIMPLE_FILE_NAME = "TestSessionRemote.wsdl";    

    public ClassesGenerationTest() {}

    public ClassesGenerationTest(String name) {
        super(name);
    }

    public void setUp() {
        // Creates the working path directory
        File dirTemp = new File(WORKING_PATH);        
        dirTemp.mkdir();
        if (System.getProperty("localRepository") == null) {
            fail("Set the localRepository variable to the maven repository");
        }
    }

    /**
     * Tests the classes generation from the <code>TestComplexSessionRemote.wsdl</code>.
     */
    public void testComplexClassesGeneration() {        
        String remoteInterfaceName = "it.imolinfo.test14.complex.TestComplexSessionRemote";
        String corbaName = "corbaname:iiop:127.0.0.1:3700#ejb/TestComplexSessionBean";
        Properties classesId = new Properties();
        classesId.put("it.imolinfo.test14.complex.UserProfile", new Long("8891581763048162223"));
        classesId.put("it.imolinfo.test14.complex.UserProfileException", new Long("-6098976502009828357"));
        classesGeneration(WSDL_COMPLEX_FILE_NAME, remoteInterfaceName, corbaName, classesId);            
    }        
    
    /**
     * Tests the classes generation from the <code>TestComplexSessionRemote.wsdl</code>.
     */
    public void testSimpleClassesGeneration() {        
        String remoteInterfaceName = "it.imolinfo.test14.TestSessionRemote";
        String corbaName = "corbaname:iiop:127.0.0.1:3700#ejb/TestSessionBean";
        Properties classesId = new Properties();        
        classesGeneration(WSDL_SIMPLE_FILE_NAME, remoteInterfaceName, corbaName, classesId);            
    }   
    
    /**
     * Generates the classes.
     * @param WSDLClassName
     * @param remoteInterfaceClassName
     * @param corbaName
     * @param classesId
     */
    private String classesGeneration(String WSDLClassName, String remoteInterfaceClassName, String corbaName, Properties classesId) {
        try {
            String wsdlComplexFileName = WSDL_DIR + File.separator + WSDLClassName;
            // Deploy-time: creates dinamically the ejb client classes
            
            // Creates the EJB Classes
            String classesDir = EJBProxyUtils.createEJBClasses(wsdlComplexFileName, remoteInterfaceClassName, null, classesId,  TestUtils.getJarFilesName("1.2.2"));                
            return classesDir;
            
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
            return null;
        }
    }
    
     
    /**
     * Remove all the created files in the working path.
     */
    public void tearDown() {
        System.out.println("Removing the directory:" + WORKING_PATH);
        File dir = new File(WORKING_PATH);
        TestUtils.deleteDir(dir);
    }    
    
}

