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
 * @(#)SAPBindingDeployerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import javax.jbi.component.ComponentContext;
import javax.management.MBeanServer;
import java.util.HashMap;
import javax.jbi.messaging.DeliveryChannel;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class SAPBindingDeployerTest extends org.jmock.cglib.MockObjectTestCase {
    SAPBindingDeployer instance = null;
    
    Mock componentContext = mock(ComponentContext.class);
    Mock lifeCycle = mock(SAPBindingLifeCycle.class);
    
    public SAPBindingDeployerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new SAPBindingDeployer((ComponentContext) componentContext.proxy(),
                                           (SAPBindingLifeCycle) lifeCycle.proxy());
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SAPBindingDeployerTest.class);
        
        return suite;
    }

    /**
     * Test of deploy method, of class com.sun.jbi.sapbc.SAPBindingDeployer.
     */
    public void testDeploy() throws Exception {
        System.out.println("Testing deploy");
        
        RuntimeConfiguration runtimeConfig = new RuntimeConfiguration("test/com/sun/jbi/sapbc/testDir", "", "");
        StatusProviderHelper statusHelper = 
            new StatusProviderHelper("shortName", "componentType", "componentName", (MBeanServer)mock(MBeanServer.class).proxy());
        InboundReceiver inboundReceiver = new InboundReceiver((ComponentContext) componentContext.proxy(),
                                                              (DeliveryChannel) mock(DeliveryChannel.class).proxy(),
                                                               runtimeConfig);
        HashMap serviceUnits = new HashMap();
        componentContext.expects(once()).method("getComponentName").will(returnValue("someComponentName"));
        lifeCycle.expects(atLeastOnce()).method("getStatusProviderHelper").will(returnValue(statusHelper));
        lifeCycle.expects(once()).method("getInboundReceiver").will(returnValue(inboundReceiver));
        lifeCycle.expects(once()).method("getRuntimeConfigurationMBean").will(returnValue(runtimeConfig));
        String result = instance.deploy("testSUId", "test/com/sun/jbi/sapbc/packaging/descriptors");
        assertNotNull(result);
    }

    /**
     * Test of init method, of class com.sun.jbi.sapbc.SAPBindingDeployer.
     */
    public void testInit() throws Exception {
        System.out.println("Testing init");
        
        // 1. testing the scenario when a new service unit is to be initialized.
        RuntimeConfiguration runtimeConfig = new RuntimeConfiguration("test/com/sun/jbi/sapbc/testDir", "", "");
        StatusProviderHelper statusHelper = 
            new StatusProviderHelper("shortName", "componentType", "componentName", (MBeanServer)mock(MBeanServer.class).proxy());
        InboundReceiver inboundReceiver = new InboundReceiver((ComponentContext) componentContext.proxy(),
                                                              (DeliveryChannel) mock(DeliveryChannel.class).proxy(),
                                                               runtimeConfig);
        HashMap serviceUnits = new HashMap();
        lifeCycle.expects(atLeastOnce()).method("getStatusProviderHelper").will(returnValue(statusHelper));
        lifeCycle.expects(once()).method("getInboundReceiver").will(returnValue(inboundReceiver));
        lifeCycle.expects(once()).method("getRuntimeConfigurationMBean").will(returnValue(runtimeConfig));
        try {
            instance.init("testSUId", "test/com/sun/jbi/sapbc/packaging/descriptors");
            System.out.println("Successfully tested init for a new service unit");
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test init for a new service unit");
        }
        
        // 2. testing the scenario when the service unit is already initialized
        serviceUnits.put("testSUId", mock(ServiceUnit.class).proxy());
        try {
            instance.init("testSUId", "testSURoot");
            System.out.println("Successfully tested init for an existing service unit");
        } catch (Exception e) {
            fail("Failed to test init for an existing service unit");
        }
    }

    /**
     * Test of start method, of class com.sun.jbi.sapbc.SAPBindingDeployer.
     */
    public void testStart() throws Exception {
        System.out.println("Testing start");
        
        // setting up service units
        Mock aServiceUnit = mock(ServiceUnit.class);
        HashMap serviceUnits = instance.getServiceUnits();
        serviceUnits.put("testSUId", (ServiceUnit)aServiceUnit.proxy());
        aServiceUnit.expects(once()).method("start");
        componentContext.expects(once()).method("getComponentName").will(returnValue("testStartComponentName"));
        
        // 1. testing the scenario when the service unit is initialized
        try {
            aServiceUnit.expects(once()).method("isInitialized").will( returnValue(true));
            instance.start("testSUId");
            System.out.println("Successfully tested start when the service unit is initialized");
            aServiceUnit.verify();
        } catch (Exception e) {
            fail("Failed to test start when the service unit is initialized due to: " + e.getMessage());
        }
        
        // 2. testing the scenario when the service unit is not initialized
        try {
            instance.start("noMatchSUId");
            System.out.println("Successfully tested start when the service unit is not initialized");
            fail("Failed to throw exception for a non-existant service unit noMatchSUId");
        } catch (Exception e) {
            System.out.println("Failed to test start when the service unit is not initialized due to: " + e.getMessage());
        }
    }

    /**
     * Test of stop method, of class com.sun.jbi.sapbc.SAPBindingDeployer.
     */
    public void testStop() throws Exception {
        System.out.println("Testing stop");
        
        // setting up service units
        Mock aServiceUnit = mock(ServiceUnit.class);
        HashMap serviceUnits = instance.getServiceUnits();
        serviceUnits.put("testSUId", (ServiceUnit)aServiceUnit.proxy());
        aServiceUnit.expects(once()).method("stop");
        componentContext.expects(once()).method("getComponentName").will(returnValue("testStopComponentName"));
        
        // 1. testing the scenario when the service unit is initialized
        try {
            instance.stop("testSUId");
            System.out.println("Successfully tested stop when the service unit is initialized");
            aServiceUnit.verify();
        } catch (Exception e) {
            fail("Failed to test stop when the service unit is initialized due to: " + e.getMessage());
        }
        
        // 2. testing the scenario when the service unit is not initialized
        try {
            instance.start("noMatchSUId");
            System.out.println("Successfully tested start when the service unit is not initialized");
            fail("Failed to throw exception for a non-existant service unit noMatchSUId");
        } catch (Exception e) {
            System.out.println("Failed to test start when the service unit is not initialized due to: " + e.getMessage());
        }
    }

    /**
     * Test of shutDown method, of class com.sun.jbi.sapbc.SAPBindingDeployer.
     */
    public void testShutDown() throws Exception {
        System.out.println("Testing shutDown");
        
        // setting up service units
        Mock aServiceUnit = mock(ServiceUnit.class);
        HashMap serviceUnits = instance.getServiceUnits();
        serviceUnits.put("testSUId", (ServiceUnit)aServiceUnit.proxy());
        aServiceUnit.expects(once()).method("shutdown");
        componentContext.expects(once()).method("getComponentName").will(returnValue("testShutDownComponentName"));
        
        // 1. testing the scenario when the service unit is initialized
        try {
            instance.shutDown("testSUId");
            System.out.println("Successfully tested shutDown when the service unit is initialized");
            aServiceUnit.verify();
        } catch (Exception e) {
            fail("Failed to test shutDown when the service unit is initialized due to: " + e.getMessage());
        }
        
        // 2. testing the scenario when the service unit is not initialized
        try {
            instance.start("noMatchSUId");
            System.out.println("Successfully tested start when the service unit is not initialized");
            fail("Failed to throw exception for a non-existant service unit noMatchSUId");
        } catch (Exception e) {
            System.out.println("Failed to test start when the service unit is not initialized due to: " + e.getMessage());
        }
    }

    /**
     * Test of undeploy method, of class com.sun.jbi.sapbc.SAPBindingDeployer.
     */

/*    public void testUndeploy() throws Exception {
        System.out.println("Testing undeploy");
        
        componentContext.expects(once()).method("getComponentName").will(returnValue("someComponentName"));
        
         // setting up service units
        Mock aServiceUnit = mock(ServiceUnit.class);
        HashMap serviceUnits = instance.getServiceUnits();
        serviceUnits.put("testSUId", (ServiceUnit)aServiceUnit.proxy());
        aServiceUnit.expects(once()).method("isShutdown").will( returnValue(true));
        
        instance.undeploy("testSUId", "testRoot");
        assertNull(instance.getServiceUnits().get("testSUId"));
    }
*/
    public void testUndeploy() throws Exception {
        System.out.println("Testing undeploy");
        
        Mock aServiceUnit = mock(ServiceUnit.class);
        componentContext.expects(once()).method("getComponentName").will(returnValue("someComponentName"));
        aServiceUnit.expects(once()).method("isShutdown").will( returnValue(true));
        aServiceUnit.expects(once()).method("shutdown");
        
        HashMap serviceUnits = instance.getServiceUnits();
        serviceUnits.put("testSUId", (ServiceUnit)aServiceUnit.proxy());
        
        instance.undeploy("testSUId", "testRoot");
        assertNull(instance.getServiceUnits().get("testSUId"));
    }

}
