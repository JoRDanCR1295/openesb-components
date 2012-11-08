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
public class RethrowPersistenceTest extends TestCase {

    /** Creates a new instance of RethrowTest */
    public RethrowPersistenceTest(String testname) {
        super(testname);
    }

    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(RethrowPersistenceTest.class);
        return suite;
    }

    public void testRethrow_FirstLevelScopeCatch() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "FirstLevelScopeCatch.properties");
    }

    public void testRethrow_FirstLevelScopeCatchAll() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "FirstLevelScopeCatchAll.properties");
    }

    public void testRethrow_NestedScopeCatch() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "NestedScopeCatch.properties");
    }

    public void testRethrow_NestedScopeCatchAll() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "NestedScopeCatchAll.properties");
    }

    /**public void testWithinForEachCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinForEach", "Catch.properties");
    }

    public void testWithinForEachCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinForEach", "CatchAll.properties");
    }

    public void testWithinWhileCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinWhile", "Catch.properties");
    }

    public void testWithinWhileCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinWhile", "CatchAll.properties");
    }

    public void testWithinRepeatUntilCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinRepeatUntil", "Catch.properties");
    }

    public void testWithinRepeatUntilCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinRepeatUntil", "CatchAll.properties");
    }

    public void testWithinIfCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinIf", "Catch.properties");
    }

    public void testWithinIfCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinIf", "CatchAll.properties");
    }

    public void testWithinEHOnAlarmCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinEHOnAlarm", "Catch.properties");
    }

    public void testWithinEHOnAlarmCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinEHOnAlarm", "CatchAll.properties");
    }

    public void testWithinEHOnEventCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinEHOnEvent", "Catch.properties");
    }

    public void testWithinEHOnEventCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinEHOnEvent", "CatchAll.properties");
    }

    public void testWithinPickOnAlarmCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinPickOnAlarm", "Catch.properties");
    }

    public void testWithinPickOnAlarmCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinPickOnAlarm", "CatchAll.properties");
    }

    public void testWithinPickOnMessageCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinPickOnMessage", "Catch.properties");
    }

    public void testWithinPickOnMessageCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinPickOnMessage", "CatchAll.properties");
    }

    public void testWithinCHCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinCH", "Catch.properties");
    }

    public void testWithinCHCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinCH", "CatchAll.properties");
    }

    public void testWithinTHCatch() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinTH", "Catch.properties");
    }

    public void testWithinTHCatchAll() throws Exception {
	        CommonPersistenceTesterHelper.commonCode("bpels/rethrow/WithinTH", "CatchAll.properties");
    }*/

}
