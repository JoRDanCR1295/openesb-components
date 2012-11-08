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
 * The Class TestEJB.
 */
public class TestEJBJNDI extends TestCase {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(TestEJBJNDI.class);   
    
    /**
     * Test user profile.
     */
    @SuppressWarnings("unchecked")
    public void _testGFUserProfile() {
        // TODO remove local dependencies ("marco")
        testEJBJNDIRemoteInterface(
                "it.imolinfo.test14.complex.TestComplexSessionRemote",
                "/home/marco/NetBeansProjects/EJBModule14/dist/EJBModule14.jar",
                "ejb/TestComplexSessionBean",                
                "getUserProfile", new Object[] { "pippo" }, TestUtils.GLASSFISH_v2);
    }


    /**
     * Test user profile.
     */
    @SuppressWarnings("unchecked")
    public void _testIBMUserProfile() {
        // TODO remove local dependencies ("marco")
        testEJBJNDIRemoteInterface(
                "it.imolinfo.test14.complex.TestComplexSessionRemote",
                "/home/marco/IBM/rationalsdp7.0/workspace/TestEJBClient/TestEJBClient.jar",
                "ejb/TestComplexSessionBeanWS",
                "getUserProfile", new Object[] { "pippo" }, TestUtils.WEBSPHERE_6);
    }
    
    /**
     * Test user profile.
     */
    @SuppressWarnings("unchecked")
    public void _testUserProfileJBoss() {
        // TODO remove local dependencies ("marco")
        testEJBJNDIRemoteInterface(
                "it.imolinfo.test14.complex.TestComplexSessionRemote",
                "/home/marco/NetBeansProjects/EJBModule14/dist/EJBModule14.jar",
                "ejb/TestComplexSessionBean",                   
                "getUserProfile", new Object[] { "pippo" }, TestUtils.JBOSS_V4);
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
    private void testEJBJNDIRemoteInterface(String remoteInterfaceName,
            String jarPath, String jndiName, String methodName, Object[] args, String asType) {

        if (System.getProperty("localRepository") == null) {
            fail("Set the localRepository variable to the maven repository");
        }

        try {
            
         // Gets the SUIDs
            Properties classesId = TestUtils.getClassesIdFromJar(remoteInterfaceName, jarPath);
            
            // Design-time. Creates the WSDL and gets the classesId from the value types, exception and return classes
            WSDLDescriptor descriptor = new WSDLDescriptor(jndiName, "jndi");
            descriptor.setJndiProperties(TestUtils.getJndiProperties(asType));
            
            // Design-time. Creates the WSDL and gets the classesId from the value types, exception and return classes
            String wsdlFileName = DynamicEJBWSDLGenerator
            .generateWSDLFromRemoteInterface(remoteInterfaceName,
                    jarPath, descriptor);
            
            // Deploy-time: creates dinamically the ejb client classes
            StatelessEJBProxy ejbProxy = StatelessEJBProxyFactory.getEJBFromJNDIName(wsdlFileName,
                    remoteInterfaceName, jndiName, TestUtils.getJndiProperties(asType), TestUtils.getOrbProperties(asType),
                    classesId, TestUtils.getJarFilesName("1.2.2"));
            

            // Runtime: invoke the method
            Object res = ejbProxy.invokeMethod(methodName, args);

            LOG.debug(res);

        } catch (Throwable ex) {
            ex.printStackTrace();
            fail(ex.getMessage());
        }
    }
    
    
    


}
