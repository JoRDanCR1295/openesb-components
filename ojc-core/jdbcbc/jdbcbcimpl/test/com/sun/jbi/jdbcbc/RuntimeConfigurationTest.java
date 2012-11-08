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
 * @(#)RuntimeConfigurationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import junit.framework.*;

import org.jmock.*;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;


/**
 *
 * @author temp
 */
public class RuntimeConfigurationTest extends org.jmock.cglib.MockObjectTestCase {
    RuntimeConfiguration instance = null;
    Mock nbs = null;
    Mock nListener = null;
    Mock nFilter = null;

    //Mock broadcasterSupport = null;
    public RuntimeConfigurationTest(final String testName) {
        super(testName);
    }

    //@Override
	protected void setUp() throws Exception {
        instance = new RuntimeConfiguration("test/com/sun/jbi/jdbcbc/testDir");
        nbs = mock(NotificationBroadcasterSupport.class);
        nListener = mock(NotificationListener.class);
        nFilter = mock(NotificationFilter.class);

        //broadcasterSupport = mock(BroadcasterSupport.class);
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(RuntimeConfigurationTest.class);

        return suite;
    }

    /**
     * Test of getThreads method, of class com.sun.jbi.jdbcbc.RuntimeConfiguration.
     */
    public void testSetGetThreads()
        throws InvalidAttributeValueException, MBeanException {
        System.out.println("getThreads");

        //RuntimeConfiguration instance = null;
        final Integer expResult = 10;
        instance.setThreads(expResult);

        final Integer result = instance.getThreads();
        Assert.assertEquals(expResult, result);
    }

    /**
    * Test of persistConfiguration method, of class com.sun.jbi.jdbcbc.RuntimeConfiguration.
    */
    public void testPersistConfiguration() throws Exception {
    }

    /**
     * Test of getNotificationInfo method, of class com.sun.jbi.jdbcbc.RuntimeConfiguration.
     */
    public void testGetNotificationInfo() {
        System.out.println("getNotificationInfo");

        //RuntimeConfiguration instance = null;
    }

    /**
     * Test of addNotificationListener method, of class com.sun.jbi.jdbcbc.RuntimeConfiguration.
     */
    public void testAddNotificationListener() {
        try {
            System.out.println("addNotificationListener");

            final Object o = new Object();
            //nbs.expects(atLeastOnce()).method("addNotificationListener");
            instance.addNotificationListener((NotificationListener) nListener.proxy(),
                (NotificationFilter) nFilter.proxy(), o);
        } catch (final Exception e) {
            System.out.println("test addNotificationListener failed");
        }
    }

    /**
     * Test of removeNotificationListener method, of class com.sun.jbi.jdbcbc.RuntimeConfiguration.
     */
    public void testRemoveNotificationListener() throws Exception {
        try {
            System.out.println("removeNotificationListener");
            //nbs.expects(atLeastOnce()).method("removeNotificationListener");
            instance.removeNotificationListener((NotificationListener) nListener.proxy());
        } catch (final Exception e) {
            System.out.println("test removeNotificationListener failed");
        }
    }
}
