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
 * @(#)FaultExceptionTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import junit.framework.*;

import javax.jbi.messaging.MessagingException;


/**
 *
 *
 */
public class FaultExceptionTest extends TestCase {
    FaultException instance = new FaultException(new MessagingException(
                "Testing exception message"));

    public FaultExceptionTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(FaultExceptionTest.class);

        return suite;
    }

    /**
     * Test of getFaultCode method, of class org.glassfish.openesb.databasebc.FaultException.
     */
    public void testSetGetFaultCode() {
        System.out.println("Testing getFaultCode");

        final String expResult = "Server";
        final String result = instance.getFaultCode();
        Assert.assertEquals(expResult, result);
    }

    /**
     * Test of getDetail method, of class org.glassfish.openesb.databasebc.FaultException.
     */
    public void testGetDetail() {
        System.out.println("Testing getDetail");

        final String expResult = "Testing exception message";
        final String result = instance.getDetail();
        Assert.assertEquals(expResult, result);
    }
}
