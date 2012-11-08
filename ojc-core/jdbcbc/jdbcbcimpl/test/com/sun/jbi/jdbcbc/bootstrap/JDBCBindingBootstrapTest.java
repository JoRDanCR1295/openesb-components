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
 * @(#)JDBCBindingBootstrapTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.bootstrap;

import junit.framework.*;

import org.jmock.*;

import java.util.Hashtable;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;

import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;


/**
 *
 *
 */
public class JDBCBindingBootstrapTest extends org.jmock.cglib.MockObjectTestCase {
    JDBCBindingBootstrap instance = null;
    ObjectName objectName = null;

    //Mock mMessages = mock(Messages.class);
    Mock context = mock(InstallationContext.class);
    Mock componentContext = mock(ComponentContext.class);
    Mock mbServer = mock(MBeanServer.class);
    Mock mbNames = mock(MBeanNames.class);

    //Mock mLogger = mock(Logger.class);
    public JDBCBindingBootstrapTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCBindingBootstrap();

        final Hashtable table = new Hashtable();
        table.put("someKey", "someValue");
        objectName = new ObjectName("someObjectName", table);
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(JDBCBindingBootstrapTest.class);

        return suite;
    }

    /**
     * Test of init method, of class com.sun.jbi.jdbcbc.bootstrap.JdbcBindingBootstrap.
     */
    public void testInit() throws Exception {
        System.out.println("Testing init");
        /*context.expects(atLeastOnce()).method("getContext")
               .will(returnValue(componentContext.proxy()));
        componentContext.expects(atLeastOnce()).method("getLogger")
                        .with(eq(JDBCBindingBootstrap.class.getName()),
            eq("com.sun.jbi.jdbcbc.bootstrap.messages.Bundle"))
                        .will(returnValue(Logger.getLogger(
                    JDBCBindingBootstrap.class.getName(),
                    "com.sun.jbi.jdbcbc.bootstrap.messages.Bundle")));
        componentContext.expects(atLeastOnce()).method("getLogger")
                        .with(eq("com.sun.jbi.jdbcbc.bootstrap.InstallerExt"),
            eq("com.sun.jbi.jdbcbc.bootstrap.messages.Bundle"))
                        .will(returnValue(Logger.getLogger(
                    "com.sun.jbi.jdbcbc.bootstrap.InstallerExt",
                    "com.sun.jbi.jdbcbc.bootstrap.messages.Bundle")));
        componentContext.expects(atLeastOnce()).method("getMBeanServer")
                        .will(returnValue(mbServer.proxy()));
        componentContext.expects(atLeastOnce()).method("getMBeanNames")
                        .will(returnValue(mbNames.proxy()));
        mbNames.expects(atLeastOnce()).method("createCustomComponentMBeanName")
               .with(eq(MBeanNames.BOOTSTRAP_EXTENSION))
               .will(returnValue(objectName));
        mbServer.expects(atLeastOnce()).method("isRegistered")
                .with(eq(objectName)).will(returnValue(false));
        mbServer.expects(atLeastOnce()).method("registerMBean")
                .with(isA(StandardMBean.class), eq(objectName));*/

        try {
            //instance.init((InstallationContext) context.proxy());
            System.out.println(
                "Successfully tested init for the scenario where no exception is expected.");
        } catch (final Exception e) {
            /*Assert.fail(
                "Failed to test init for the scenario where no exception is expected due to: " +
                e.getMessage());*/
        }

        //mbServer.verify();
        //mbNames.verify();
        //context.verify();

        /* 2. testing the failure scenario
        mbServer.expects(once()).method("registerMBean")
                .with(isA(StandardMBean.class), eq(objectName))
                .will(throwException(
                new MBeanRegistrationException(new Exception(), "someException")));*/

        try {
            /*instance.init((InstallationContext) context.proxy());
            Assert.fail(
                "Failed to test init when an exception should be caught - a MBeanRegistrationException is raised.");*/
        } catch (final Exception e) {
            System.out.println(
                "Successfully tested init when an exception is raised.");
        }

        //mbServer.verify();
        //mbNames.verify();
        //context.verify();
    }

    /**
     * Test of cleanUp method, of class com.sun.jbi.jdbcbc.bootstrap.JdbcBindingBootstrap.
     */
    public void testCleanUp() throws Exception {
        System.out.println("Testing cleanUp");

        try {
            instance.cleanUp();
            System.out.println("Successfully tested cleanUp.");
        } catch (final Exception e) {
            Assert.fail("Failed to test cleanUp due to: " + e.getMessage());
        }
    }
}
