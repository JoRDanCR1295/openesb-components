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
 * @(#)InstallerExtTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.bootstrap;

import junit.framework.*;


/**
 *
 * @author sweng
 */
public class InstallerExtTest extends TestCase {
    InstallerExt instance = new InstallerExt();

    public InstallerExtTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(InstallerExtTest.class);

        return suite;
    }

    /**
     * Test of setThreads and getThreads method, of class com.sun.jbi.jdbcbc.bootstrap.InstallerExt.
     */
    public void testSetGetThreads() {
        System.out.println("Testing setThreads and getThreads");

        final String threads = "someNumberOfThreads";
        instance.setThreads(threads);

        final String result = instance.getThreads();
        Assert.assertEquals(threads, result);
    }
}
