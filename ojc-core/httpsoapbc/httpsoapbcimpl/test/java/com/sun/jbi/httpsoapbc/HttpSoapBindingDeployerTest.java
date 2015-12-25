/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)HttpSoapBindingDeployerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import junit.framework.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.HashMap;
import java.util.HashSet;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.component.ComponentContext;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.internationalization.Messages;
import org.jmock.*;
import org.jmock.core.constraint.*;
import org.jmock.core.stub.*;

/**
 *
 * 
 */
public class HttpSoapBindingDeployerTest extends MockObjectTestCase {
    
    private JBITaskMessageBuilder mMsgBuilder;
    private String mComponentName;
    
    public HttpSoapBindingDeployerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mComponentName = "comp";
        mMsgBuilder = new DefaultJBITaskMessageBuilder();
        mMsgBuilder.setComponentName(mComponentName);
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of deploy method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testSuccessfulDeploy() throws Exception {
        
        String suId = "aSuId";
        String asaFilePath = "aFilePath";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("deploy").with(new StringContains(asaFilePath)).isVoid();
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            String result = instance.deploy(suId, asaFilePath);
        
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects
//            String expResult = mMsgBuilder.createSuccessMessage("deploy");
//            assertEquals(expResult, result);
        } catch (Exception ex) {
            fail("Call to deploy() method failed with exception msg: " + ex.getMessage());
        }
    }
  
   /**
     * Test of deploy method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testFailedDeploy() throws Exception {
        String suId = "aSuId";
        String asaFilePath = "aFilePath";
        String errorMsg = "error";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("deploy").with(new StringContains(asaFilePath)).will(throwException(new DeploymentException(errorMsg)));
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            String result = instance.deploy(suId, asaFilePath);
            fail("Call to deploy() should have failed but it didn't");
        } catch (Exception ex) {
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();

            ex.printStackTrace();
            
            // Do state verification of our objects
            assertTrue(ex instanceof DeploymentException);
        }
    }

    /**
     * Test of init method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testSuccessfulInit() throws Exception {
        String suId = "aSuId";
        String asaFilePath = "aFilePath";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("init").with(new StringContains(asaFilePath)).isVoid();
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.init(suId, asaFilePath);
        
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects

        } catch (Exception ex) {
            fail("Call to init() method failed with exception msg: " + ex.getMessage());
        }
    }

    /**
     * Test of init method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testFailedInit() throws Exception {
        String suId = "aSuId";
        String asaFilePath = "aFilePath";
        String errorMsg = "error";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("init").with(new StringContains(asaFilePath)).will(throwException(new DeploymentException(errorMsg)));
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.init(suId, asaFilePath);
            fail("Call to init() should have failed but it didn't");
        } catch (Exception ex) {
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects
            assertTrue(ex instanceof DeploymentException);
        }
    }
    
    /**
     * Test of start method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testSuccessfulStart() throws Exception {
        String suId = "aSuId";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("start").withNoArguments().isVoid();
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.start(suId);
        
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects

        } catch (Exception ex) {
            fail("Call to start() method failed with exception msg: " + ex.getMessage());
        }
    }

    /**
     * Test of start method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testFailedStart() throws Exception {
        String suId = "aSuId";
        String errorMsg = "error";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("start").withNoArguments().will(throwException(new DeploymentException(errorMsg)));
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.start(suId);
            fail("Call to start() should have failed but it didn't");
        } catch (Exception ex) {
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects
            assertTrue(ex instanceof DeploymentException);
        }
    }
    
    /**
     * Test of stop method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testSuccessfulStop() throws Exception {
        String suId = "aSuId";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("stop").withNoArguments().isVoid();
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.stop(suId);
        
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects

        } catch (Exception ex) {
            fail("Call to stop() method failed with exception msg: " + ex.getMessage());
        }
    }

    /**
     * Test of stop method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testFailedStop() throws Exception {
        String suId = "aSuId";
        String errorMsg = "error";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("stop").withNoArguments().will(throwException(new DeploymentException(errorMsg)));
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.stop(suId);
            fail("Call to stop() should have failed but it didn't");
        } catch (Exception ex) {
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects
            assertTrue(ex instanceof DeploymentException);
        }
    }
    
    /**
     * Test of shutDown method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testSuccessfulShutDown() throws Exception {
        String suId = "aSuId";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("shutdown").withNoArguments().isVoid();
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.shutDown(suId);
        
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects

        } catch (Exception ex) {
            fail("Call to shutdown() method failed with exception msg: " + ex.getMessage());
        }
    }
    
    
    /**
     * Test of shutDown method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testFailedShutDown() throws Exception {
        String suId = "aSuId";
        String errorMsg = "error";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnit.expects(exactly(1)).method("shutdown").withNoArguments().will(throwException(new DeploymentException(errorMsg)));
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            instance.shutDown(suId);
            fail("Call to shutdown() should have failed but it didn't");
        } catch (Exception ex) {
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects
            assertTrue(ex instanceof DeploymentException);
        }
    }

    /**
     * Test of undeploy method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testSuccessfulUndeploy() throws Exception {
        String suId = "aSuId";
        String asaFilePath = "aFilePath";
        
        // Create our Mock objects
        HashMap serviceUnits = new HashMap();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock serviceUnit = mock(ServiceUnit.class);
        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
        
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        new ArrayList());
        
        // Now test our method
        try {
            String result = instance.undeploy(suId, asaFilePath);
        
            // Verify that our mock object was called correctly
            serviceUnit.verify();
            componentContext.verify();
        
            // Do state verification of our objects
            String expResult = mMsgBuilder.createSuccessMessage("undeploy");
            assertEquals(expResult, result);
            assertNull(serviceUnits.get(suId));
        } catch (Exception ex) {
            fail("Call to shutdown() method failed with exception msg: " + ex.getMessage());
        }
    }

    // removing invalid test case. When the SU is undeployed, it's entry in the SU map will be removed, otherwise, we will end up with a memory leak
//    public void testFailedUndeploy() throws Exception {
//        String suId = "aSuId";
//        String asaFilePath = "aFilePath";
//        String errorMsg = "error";
//        
//        // Create our Mock objects
//        HashMap serviceUnits = new HashMap();
//        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
//        Mock serviceUnit = mock(ServiceUnit.class);
//        serviceUnits.put(suId, (ServiceUnit)serviceUnit.proxy());
//        
//        Mock componentContext = mock(ComponentContext.class);
//        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
//        
//        HttpSoapBindingDeployer instance = 
//            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
//                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
//                                        serviceUnits,
//                                        new ArrayList());
//        
//        // Now test our method
//        try {
//            String result = instance.undeploy(suId, asaFilePath);
//            fail("Call to deploy() should have failed but it didn't");
//        } catch (Exception ex) {
//            // Verify that our mock object was called correctly
//            serviceUnit.verify();
//            componentContext.verify();
//        
//            // Do state verification of our objects
//            assertTrue(ex instanceof DeploymentException);
//        }
//    }
    
    /**
     * Test of addEndpointChangeListener method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testAddEndpointChangeListener() {
        
        // Create our Mock objects
        ArrayList listeners = new ArrayList();
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        HashMap serviceUnits = new HashMap();
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        listeners);
        EndpointChangeListener listener = new EndpointChangeListener() {
            public void endpointInitialized(Endpoint endpoint) {}
            public void endpointActivated(Endpoint endpoint) {}
            public void endpointDeactivated(Endpoint endpoint) {}
            public void endpointShutdown(Endpoint endpoint) {}
            public void endpointDeployed(Endpoint endpoint) {}
        };
        
        instance.addEndpointChangeListener(listener);
        
        assertEquals(1, listeners.size());
        assertEquals(listener, listeners.get(0));
    }
    
    /**
     * Test of removeEndpointChangeListener method, of class com.sun.jbi.httpsoapbc.HttpSoapBindingDeployer.
     */
    public void testRemoveEndpointChangeListener() {
        // Create our Mock objects
        ArrayList listeners = new ArrayList();
        EndpointChangeListener listener = new EndpointChangeListener() {
            public void endpointInitialized(Endpoint endpoint) {}
            public void endpointActivated(Endpoint endpoint) {}
            public void endpointDeactivated(Endpoint endpoint) {}
            public void endpointShutdown(Endpoint endpoint) {}
            public void endpointDeployed(Endpoint endpoint) {}
        };
        listeners.add(listener);
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        HashMap serviceUnits = new HashMap();
        Mock componentContext = mock(ComponentContext.class);
        componentContext.expects(atLeastOnce()).method("getComponentName").withNoArguments().will(returnValue(mComponentName));
        
        HttpSoapBindingDeployer instance = 
            new HttpSoapBindingDeployer((ComponentContext)componentContext.proxy(),
                                        (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                                        serviceUnits,
                                        listeners);

        
        instance.removeEndpointChangeListener(listener);
        
        assertEquals(0, listeners.size());
    }
   
}
