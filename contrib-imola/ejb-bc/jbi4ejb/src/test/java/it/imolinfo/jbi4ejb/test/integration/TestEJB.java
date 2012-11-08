/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.test.integration;

import it.imolinfo.jbi4ejb.runtime.ejbproxy.StatelessEJBProxy;
import it.imolinfo.jbi4ejb.runtime.ejbproxy.StatelessEJBProxyFactory;
import it.imolinfo.jbi4ejb.test.TestUtils;
import it.imolinfo.jbi4ejb.webservice.generator.DynamicEJBWSDLGenerator;
import it.imolinfo.jbi4ejb.webservice.generator.WSDLDescriptor;

import java.util.Properties;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Tests EJB call versus the Differnts AS.
 * This is an integration test.
 */
public class TestEJB extends TestCase {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(TestEJB.class);   
    

    /**
     * Test user profile.
     */
    @SuppressWarnings("unchecked")
    public void testUserProfile() {
        testEJBRemoteInterface(
                "it.imolinfo.test14.complex.TestComplexSessionRemote",
                "/home/marco/NetBeansProjects/EJBModule14/dist/EJBModule14.jar",
                "corbaname:iiop:127.0.0.1:3700#ejb/TestComplexSessionBean",
                "getUserProfile", new Object[] { "pippo" }, TestUtils.GLASSFISH_v2);
    }
    
    /**
     * Test user profile.
     */
    @SuppressWarnings("unchecked")
    public void _testThrowException() {
        testEJBRemoteInterface(
                "it.imolinfo.test14.complex.TestComplexSessionRemote",
                "/home/marco/NetBeansProjects/EJBModule14/dist/EJBModule14.jar",
                "corbaname:iiop:127.0.0.1:3700#ejb/TestComplexSessionBean",
                "throwException", new Object[] {}, TestUtils.GLASSFISH_v2);
    }
    
    /**
     * Test user profile.
     */
    @SuppressWarnings("unchecked")
    public void _testGetBalance() {
        testEJBRemoteInterface(
                "it.imolinfo.test14.TestSessionRemote",
                "/home/marco/NetBeansProjects/EJBModule14/dist/EJBModule14.jar",
                "corbaname:iiop:127.0.0.1:3700#ejb/TestSessionBean",
                "getBalance", new Object[] { "pippo" }, TestUtils.GLASSFISH_v2);
    }

    /**
     * Test user profile-JBoss
     */
    @SuppressWarnings("unchecked")
    public void _testUserProfileJBoss() {
        testEJBRemoteInterface(
                "it.imolinfo.test14.complex.TestComplexSessionRemote",
                "/home/marco/NetBeansProjects/EJBModule14/dist/EJBModule14.jar",
                "corbaname:iiop:127.0.0.1:3528/NameService#ejb/TestComplexSessionBean",                
                "getUserProfile", new Object[] { "rollback" }, TestUtils.JBOSS_V4);
    }
    
    /**
     * Test user profile.
     */
    @SuppressWarnings("unchecked")
    public void _testIBMUserProfile() {
        testEJBRemoteInterface(
                "it.imolinfo.test14.complex.TestComplexSessionRemote",
                "/home/marco/IBM/rationalsdp7.0/workspace/TestEJBClient/TestEJBClient.jar",
                "corbaname:iiop:127.0.0.1:2809/NameServiceServerRoot#ejb/TestComplexSessionBeanWS",
                "getUserProfile", new Object[] { "pippo" }, TestUtils.WEBSPHERE_6);
    }

    /**
     * Test EJB remote interface.
     * 
     * @param remoteInterfaceName
     * @param jarPath
     * @param corbaName
     * @param methodName
     * @param args
     */
    @SuppressWarnings("unchecked")
    private void testEJBRemoteInterface(String remoteInterfaceName,
            String jarPath, String corbaName, String methodName, Object[] args, String asType) {

        if (System.getProperty("localRepository") == null) {
            fail("Set the localRepository variable to the maven repository");
        }

        try {
            
            Properties classesId = TestUtils.getClassesIdFromJar(remoteInterfaceName, jarPath);
            
            // Design-time. Creates the WSDL and gets the classesId from the value types, exception and return classes
            WSDLDescriptor descriptor = new WSDLDescriptor(corbaName, "corbaname");
            descriptor.setOrbProperties(TestUtils.getOrbProperties(asType));
            
            // Creates the WSDL
            String wsdlFileName = DynamicEJBWSDLGenerator
            .generateWSDLFromRemoteInterface(remoteInterfaceName,
                    jarPath, descriptor);
            
            // Deploy-time: creates dinamically the ejb client classes
            StatelessEJBProxy ejbProxy = StatelessEJBProxyFactory.getEJBFromCorbaname(wsdlFileName,
                    remoteInterfaceName, descriptor.getCorbaServiceName(), classesId, TestUtils.getJarFilesName("1.2.2"), descriptor.getOrbProperties());            

            // Runtime: invoke the method
            Object res = ejbProxy.invokeMethod(methodName, args);
            System.out.println("Result: " + res);

        } catch (Throwable ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }
    
    
    


}
