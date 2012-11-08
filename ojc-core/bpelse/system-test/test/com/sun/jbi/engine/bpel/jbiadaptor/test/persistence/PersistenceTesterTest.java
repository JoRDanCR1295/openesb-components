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
 * @(#)PersistenceTesterTest.java 
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
public class PersistenceTesterTest extends TestCase {
    /*
     * This flag enables/disables persistence tests using Wait activities
     * as persistent points. Any test making use of this flag should
     * ALWAYS have a duplicate version using Invoke in place of Wait.
     */
    private boolean mRunWaitTests = false;
    
    /** Creates a new instance of PersistenceTesterTest */
    public PersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        mRunWaitTests = CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(PersistenceTesterTest.class);
        return suite;
    }
    
    public void testAssign() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/assign1", "assign.properties");
    }
    
    public void testInvoke() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/invoke", "invoke.properties");
    }
    
    public void test2WayInvoke() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/2wayInvoke", "invoke.properties");
    }
    
    public void testSimpleWait() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/simpleWait", "simpleWait.properties");
    }
    
    public void testWhileWait() throws Exception {
        if (mRunWaitTests) {
            CommonPersistenceTesterHelper.commonCode("bpels/whileTest", "WhileWait.properties");
        }
    }
    
    public void testWhileInvoke() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/whileTest/invoke", "WhileInvoke.properties");
    }
    
    public void testRepeatUntilWait() throws Exception {
        if (mRunWaitTests) {
            CommonPersistenceTesterHelper.commonCode("bpels/repeatUntil", "RepeatUntilWait.properties");
        }
    }

    public void testRepeatUntilInvoke() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/repeatUntil/invoke", "RepeatUntilInvoke.properties");
    }

    public void testPickIfWhileRepeatUntil() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/pickifwhilerepeatuntil", "pickifwhilerepeatuntil.properties");
    }
 
    public void testReceiveInWhile() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/IMAinWhile/ReceiveInWhile", "ReceiveInWhile.properties");
    }
    
    public void testPickInWhile() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/IMAinWhile/PickInWhile", "PickInWhile.properties");
    }

    /*
     *Tests added for the bug: 510, where the invocation returns with an ERROR on the ME and the engine ran
     * into NullPointerException in the StateImpl.
     * testInvokeError() 
     * testInOutInvokeError()
     */
     public void testInvokeError() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/InovkeWithError/OneWayError", "OneWayError.properties");
     }
    
    public void testInOutInvokeErrorResponse() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/InovkeWithError/TwoWayError", "TwoWayError.properties");
    }

}
