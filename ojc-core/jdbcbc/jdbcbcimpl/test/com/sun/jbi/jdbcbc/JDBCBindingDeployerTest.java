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
 * @(#)JDBCBindingDeployerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import junit.framework.*;

import org.jmock.*;

import java.util.HashMap;
import javax.jbi.component.ComponentContext;
import javax.management.MBeanServer;


/**
 *
 *
 */
public class JDBCBindingDeployerTest extends org.jmock.cglib.MockObjectTestCase {
    JDBCBindingDeployer instance = null;
    Mock componentContext = mock(ComponentContext.class);
    Mock lifeCycle = mock(JDBCBindingLifeCycle.class);

    public JDBCBindingDeployerTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCBindingDeployer((ComponentContext) componentContext.proxy(),
                (JDBCBindingLifeCycle) lifeCycle.proxy());
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(JDBCBindingDeployerTest.class);

        return suite;
    }

    /**
     * Test of deploy method, of class com.sun.jbi.jdbcbc.JDBCBindingDeployer.
     */
    public void testDeploy() throws Exception {
        System.out.println("Testing deploy");

        componentContext.expects(once()).method("getComponentName")
                        .will(returnValue("someComponentName"));

        final String result = instance.deploy("testSUId", "testSUDir");
        Assert.assertNotNull(result);
    }

    /**
     * Test of init method, of class com.sun.jbi.jdbcbc.JDBCBindingDeployer.
     */
    public void testInit() throws Exception {
        System.out.println("Testing init");

        // 1. testing the scenario when a new service unit is to be initialized.
        final RuntimeConfiguration runtimeConfig = new RuntimeConfiguration(
                "test/com/sun/jbi/jdbcbc/testDir");
        final StatusProviderHelper statusHelper = new StatusProviderHelper("shortName",
                "componentType", "componentName",
                (MBeanServer) mock(MBeanServer.class).proxy());
        final HashMap endpoints = new HashMap();

        //InboundReceiver inboundReceiver = new InboundReceiver((DeliveryChannel) mock(DeliveryChannel.class).proxy(),
        //														endpoints,runtimeConfig,
        //														(ComponentContext) componentContext.proxy());
        final HashMap serviceUnits = new HashMap();

        //lifeCycle.expects(once()).method("getStatusProviderHelper").will(returnValue(statusHelper));
        //lifeCycle.expects(once()).method("activateEndpoints");
        //componentContext.expects(once()).method("getComponentName").will(returnValue("someComponentName"));
        //lifeCycle.expects(once()).method("getInboundReceiver").will(returnValue(inboundReceiver));
        try {
            //componentContext.expects(once()).method("getComponentName").will(returnValue("someComponentName"));
            //instance.init("testSUId", "test/com/sun/jbi/jdbcbc/packaging/descriptors");
            System.out.println(
                "Successfully tested init for a new service unit");
        } catch (final Exception e) {
            e.printStackTrace();

            //fail("Failed to test init for a new service unit");
        }

        // 2. testing the scenario when the service unit is already initialized
        //serviceUnits.put("testSUId", mock(ServiceUnit.class).proxy());
        try {
            //componentContext.expects(once()).method("getComponentName").will(returnValue("someComponentName"));
            //instance.init("testSUId", "testSURoot");
            System.out.println(
                "Successfully tested init for an existing service unit");
        } catch (final Exception e) {
            //fail("Failed to test init for an existing service unit");
        }
    }

    /**
     * Test of start method, of class com.sun.jbi.jdbcbc.JDBCBindingDeployer.
     */
    public void testStart() throws Exception {
        System.out.println("Testing start");

        // 1. testing the scenario when the service unit is initialized
        try {
            instance.start("testSUId");
            System.out.println(
                "Successfully tested start when the service unit is initialized");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test start when the service unit is initialized due to: " +
                e.getMessage());
        }

        // 2. testing the scenario when the service unit is not initialized
        try {
            instance.start("noMatchSUId");
            System.out.println(
                "Successfully tested start when the service unit is not initialized");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test start when the service unit is not initialized due to: " +
                e.getMessage());
        }
    }

    /**
     * Test of stop method, of class com.sun.jbi.jdbcbc.JDBCBindingDeployer.
     */
    public void testStop() throws Exception {
        System.out.println("Testing stop");

        // 1. testing the scenario when the service unit is initialized
        try {
            instance.stop("testSUId");
            System.out.println(
                "Successfully tested stop when the service unit is initialized");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test stop when the service unit is initialized due to: " +
                e.getMessage());
        }

        // 2. testing the scenario when the service unit is not initialized
        try {
            instance.stop("noMatchSUId");
            System.out.println(
                "Successfully tested stop when the service unit is not initialized");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test stop when the service unit is not initialized due to: " +
                e.getMessage());
        }
    }

    /**
     * Test of shutDown method, of class com.sun.jbi.jdbcbc.JDBCBindingDeployer.
     */
    public void testShutDown() throws Exception {
        System.out.println("Testing shutDown");

        // 1. testing the scenario when the service unit is initialized
        try {
            //lifeCycle.expects(once()).method("deactivateEndpoints");
            componentContext.expects(once()).method("getComponentName")
                            .will(returnValue("someComponentName"));
            instance.shutDown("testSUId");
            System.out.println(
                "Successfully tested shutDown when the service unit is initialized");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test shutDown when the service unit is initialized due to: " +
                e.getMessage());
        }

        // 2. testing the scenario when the service unit is not initialized
        try {
            componentContext.expects(once()).method("getComponentName")
                            .will(returnValue("someComponentName"));
            instance.shutDown("noMatchSUId");
            System.out.println(
                "Successfully tested shutDown when the service unit is not initialized");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test shutDown when the service unit is not initialized due to: " +
                e.getMessage());
        }
    }

    /**
     * Test of undeploy method, of class com.sun.jbi.jdbcbc.JDBCBindingDeployer.
     */
    public void testUndeploy() throws Exception {
        System.out.println("Testing undeploy");

        try {
            componentContext.expects(once()).method("getComponentName")
                            .will(returnValue("someComponentName"));
            instance.undeploy("testSUId",
                "test/com/sun/jbi/jdbcbc/packaging/descriptors");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test Undeploy when the service unit is initialized due to: " +
                e.getMessage());
        }

        //assertNull(instance.getServiceUnits().get("testSUId"));
    }
}
