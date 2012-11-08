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
 * @(#)DebuggerTesterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


/**
 *
 * @author Sun Microsystems
 */
public class DebuggerTesterTest extends TestCase {
    
    /** Creates a new instance of PersistenceTesterTest */
    public DebuggerTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(DebuggerTesterTest.class);
        return suite;
    }
    
    public void testInvoke() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/invoke", "invoke.properties");
    }
    
    public void testAssign() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/assign1", "assign.properties");
    }
    
    public void testSimpleWait() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/simpleWait", "simpleWait.properties");
    }
    
    public void testWhileWait() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/whileTest", "WhileWait.properties");
    }
    
    public void testWhileInvoke() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/whileTest/invoke", "WhileInvoke.properties");
    }
    
    public void testRepeatUntilWait() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/repeatUntil", "RepeatUntilWait.properties");
    }

    public void testRepeatUntilInvoke() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/repeatUntil/invoke", "RepeatUntilInvoke.properties");
    }

    public void testPickIfWhileRepeatUntil() throws Exception {
    	setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/pickifwhilerepeatuntil", "pickifwhilerepeatuntil.properties");
    }  
    
    public void testRethrow_FirstLevelScopeCatch() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "FirstLevelScopeCatch.properties");
    }
    public void testRethrow_FirstLevelScopeCatchAll() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "FirstLevelScopeCatchAll.properties");
    }
    public void testRethrow_NestedScopeCatch() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "NestedScopeCatch.properties");
    }
    public void testRethrow_NestedScopeCatchAll() throws Exception {
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/rethrow", "NestedScopeCatchAll.properties");
    }    
    
    private void setWait(int i) {
		// TODO Auto-generated method stub
		System.setProperty("ToWait", new Integer(i).toString());
	}    
}
