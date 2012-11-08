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
 * @(#)EndpointBeanTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import com.sun.jbi.eManager.provider.EndpointStatus;

import junit.framework.*;

import org.jmock.*;


/**
 *
 *
 */
public class EndpointBeanTest extends MockObjectTestCase {
    EndpointBean instance = null;

    public EndpointBeanTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        instance = new EndpointBean();
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(EndpointBeanTest.class);

        return suite;
    }

    /**
    * Test of setEndpointStatus and getEndpointStatus method, of class com.sun.jbi.filebc.EndpointImpl.
    */
    public void testSetGetEndpointStatus() {
        System.out.println("Testing setEndpointStatus and getEndpointStatus");

        final Mock endpointStatus = mock(EndpointStatus.class);
        instance.setEndpointStatus((EndpointStatus) endpointStatus.proxy());

        final EndpointStatus result = instance.getEndpointStatus();
        Assert.assertTrue(result instanceof EndpointStatus);
        System.out.println(
            "Successfully tested setEndpointStatus and getEndpointStatus");
    }

    public void testSetGetValue() {
        System.out.println("Testing setValue and getValue");

        instance.setValue("servicename", "servicenameValue");

        final String result = instance.getValue("servicename");
        Assert.assertEquals("servicenameValue", result);
        System.out.println("Successfully tested setvalue and getValue");
    }

    /**
     * Test of setDeploymentId method, of class com.sun.jbi.jdbcbc.EndpointBean.
     */

    /**
     * Test of getDeploymentId method, of class com.sun.jbi.jdbcbc.EndpointBean.
     */
    public void testSetGetDeploymentId() {
        System.out.println("Testing Set Get DeploymentId");

        final String asId = "depID";
        instance.setDeploymentId(asId);

        final String expResult = "depID";
        final String result = instance.getDeploymentId();
        Assert.assertEquals(expResult, result);
    }

    /**
     * Test of getUniqueName method, of class com.sun.jbi.jdbcbc.EndpointBean.
     */
    public void testGetUniqueName() {
        System.out.println("getUniqueName");

        final String servicename = "servicename";
        final String endpointName = "endpoint";
        final String endpointtype = "endpointtype";

        final String expResult = "servicename" + "," + "endpoint" + "," +
            "endpointtype";
        final String result = EndpointBean.getUniqueName(servicename, endpointName,
                endpointtype);
        Assert.assertEquals(expResult, result);
    }
}
