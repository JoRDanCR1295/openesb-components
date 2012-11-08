/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.test.deploy;

import it.imolinfo.jbi4ejb.jbi.component.Jbi4EjbRuntime;
import it.imolinfo.jbi4ejb.jbi.component.Jbi4EjbSUManager;
import it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbEndpoint;
import it.imolinfo.jbi4ejb.test.TestUtils;

import java.io.File;
import java.util.List;

import javax.jbi.management.DeploymentException;
import javax.xml.namespace.QName;

import junit.framework.TestCase;

/**
 * Tests the SU deploy, copiying the deploy files in the src/test/etc/test-sus/<su-name> directory in 
 * a temporary directory and trying to deploy. At the end, removes the temporary directory.
 * 
 * @author marco 
 */
public class WSDLDeployTest extends TestCase {


    // Where the test SUS (wsdl and jbi.xml) are
    public static final String SUS_TEST_DIR = "src/test/etc/test-sus";

    // The test working path
    public static final String WORKING_PATH = "target/test-deploy";

    public WSDLDeployTest() {}

    public WSDLDeployTest(String name) {
        super(name);
    }

    public void setUp() {
        // Creates the working path directory
        File dirTemp = new File(WORKING_PATH);        
        dirTemp.mkdir();        
    }

    /**
     * Tests the <code>su-complex</code> SU deploy.
     */
    public void testSuComplexDeployTest() {        
        String serviceUnitName = "su-complex";
        String suWorkingPath = WORKING_PATH + File.separator + serviceUnitName;
        try {                 
            Jbi4EjbSUManager suManager = getTestSUManager(serviceUnitName, suWorkingPath);
            
            try {
                suManager.deploy(serviceUnitName, suWorkingPath);
                suManager.init(serviceUnitName, suWorkingPath);
            } catch (DeploymentException e) {      
                fail(e.getMessage());
            }
                        
            List<Jbi4EjbEndpoint> endpoints = suManager.getDeployedEndpoints();
            assertEquals(1, endpoints.size());
            assertEquals(endpoints.get(0).getServiceName(), new QName("http://complex.test14.imolinfo.it","TestComplexSessionRemote"));
            assertEquals(endpoints.get(0).getEndpointName(), "TestComplexSessionRemotePort");
  
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }

        // TODO assert the  enpoint loaded
    }      
    
    /**
     * Tests the <code>su-complex</code> SU deploy.
     */
    public void testSuSimpleDeployTest() {        
        String serviceUnitName = "su-simple";
        String suWorkingPath = WORKING_PATH + File.separator + serviceUnitName;
        try {                 
            Jbi4EjbSUManager suManager = getTestSUManager(serviceUnitName, suWorkingPath);
            
            try {
                suManager.deploy(serviceUnitName, suWorkingPath);
                suManager.init(serviceUnitName, suWorkingPath);
            } catch (DeploymentException e) {      
                fail(e.getMessage());
            }
            
            List<Jbi4EjbEndpoint> endpoints = suManager.getDeployedEndpoints();
            assertEquals(1, endpoints.size());
            assertEquals(endpoints.get(0).getServiceName(), new QName("http://test14.imolinfo.it","TestSessionRemote"));
            assertEquals(endpoints.get(0).getEndpointName(), "TestSessionRemotePort");
            
        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.getMessage()); 
        }
        // TODO assert the  enpoint loaded
    }  


    /**
     * Create the <code>Jbi4EjbSUManager</code> of the given SU and inits the component context (with a Mock).
     * @param suName
     */
    private Jbi4EjbSUManager getTestSUManager(String suName, String suWorkingPath) throws Exception {
        File susTestDirFile = new File(SUS_TEST_DIR + File.separator + suName);
        File suDeployFile = new File(suWorkingPath);        
        System.out.println("Copying files from: " + susTestDirFile.getAbsolutePath() + " to :" + suDeployFile.getAbsolutePath());
        TestUtils.copyDirectory(susTestDirFile, suDeployFile);        
        Jbi4EjbRuntime ejbComponent = new Jbi4EjbRuntime();          
        Jbi4EjbSUManager suManager = new Jbi4EjbSUManager(ejbComponent);      
        return suManager;
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

