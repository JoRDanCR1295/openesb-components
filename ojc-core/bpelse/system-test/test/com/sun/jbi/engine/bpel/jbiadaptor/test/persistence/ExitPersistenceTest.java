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
 * @(#)FHPersistenceTesterTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


/**
 *
 * @author Sun Microsystems
 */
public class ExitPersistenceTest extends TestCase {

    /** Creates a new instance of ScopeTerminationTest */
    public ExitPersistenceTest(String testname) {
        super(testname);
    }

    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ExitPersistenceTest.class);
        return suite;
    }

    public void testExitInCH() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInCH", "Test.properties");
    }

    public void testExitInEHOnAlarm() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInEHOnAlarm", "Test.properties");
    }

    public void testExitInFH() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInFH", "Test.properties");
    }

    public void testExitInScopeTerminateOnAlarm() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInScopeTerminateOnAlarm", "Test.properties");
    }

    public void testExitWithinFlow() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitWithinFlow", "Test.properties");
    }

    public void testExitInPickOnAlarm() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInPickOnAlarm", "Test.properties");
    }

    public void testExitInRepeatUntil() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInRepeatUntil", "Test.properties");
    }

    public void testExitInIf() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInIf", "Test.properties");
    }

    public void testExitInTH() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInTH", "Test.properties");
    }

    public void testExitInPickOnMessage() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInPickOnMessage", "Test.properties");
    }

    public void testExitInEHOnMessage() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInEHOnMessage", "Test.properties");
    }

    public void testExitInForEach() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/Exit/ExitInForEach", "Test.properties");
    }

}
