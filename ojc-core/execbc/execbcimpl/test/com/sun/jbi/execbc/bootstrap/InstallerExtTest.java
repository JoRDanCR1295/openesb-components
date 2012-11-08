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

package com.sun.jbi.execbc.bootstrap;

import junit.framework.*;

import com.sun.jbi.execbc.bootstrap.InstallerExt;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Logger;

/**
 *
 * @author sweng
 */
public class InstallerExtTest extends TestCase {
    InstallerExt instance = new InstallerExt();
            
    public InstallerExtTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InstallerExtTest.class);
        
        return suite;
    }

    /**
     * Test of setThreads and getThreads method, of class com.sun.jbi.execbc.bootstrap.InstallerExt.
     */
    public void testSetGetThreads() {
        System.out.println("Testing setThreads and getThreads");
        
        String numOfSessions = "someNumberOfSessions";
        instance.setMaximumSessions(numOfSessions);
        String result = instance.getMaximumSessions();
        assertEquals(numOfSessions, result);
    }
}
