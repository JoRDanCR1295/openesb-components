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
 * @(#)FlowPersistenceTesterTest.java 
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
 * @author Sun Inc
 * Jan 17, 2007
 */
public class FlowPersistenceTesterTest extends TestCase {

    /** Creates a new instance of PersistenceTesterTest */
    public FlowPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public void testFlowFirstActivityAndAnotherFlow() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/flowFirstActivityAndAnotherFlow", "flowFirstActivityAndAnotherFlow.properties");
    }

    public void testFlowTree() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/flowTree", "flowTree.properties");
    }

    public void testFlowVarInsertCopy() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/FlowTest/varInsertCopy", 
                                                 "FlowVarInsertCopy.properties");
    }

    public void testFlowInsideIf() throws Exception {
    	CommonPersistenceTesterHelper.commonCode("bpels/FlowTest/flowInsideIf", 
    											 "FlowInsideIf.properties");
    }
    
    public void testFlowWait() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/flowWait", "flowWait.properties");
    }

    public void testAssignBeforeFlow() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/flow/assignbeforeflow", "assignbeforeflow.properties");
    }
    
    public void testAssignInFlowBeforeFlow() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/flow/assigninflowbeforeflow", "assigninflowbeforeflow.properties");
    }

    public void testAssignBeforeFlow3() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/flow/assignbeforeflow3", "assignbeforeflow3.properties");
    }

    public void testForEachInFlow() throws Exception {
        // This test is valid when run in multi thread mode. Even though it doesn't add much value here in 
        // terms of a junit, this helps in sort of documenting what will be persisted in this kind of 
        // BPEL.
        CommonPersistenceTesterHelper.commonCode("bpels/flow/foreachinflowbranch", "foreachinflowbranch.properties");
    }
    
    public void testFlowInWhile() throws Exception {
    	CommonPersistenceTesterHelper.commonCode("bpels/flow/FlowInWhile", "FlowInWhile.properties");
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FlowPersistenceTesterTest.class);
        return suite;
    }
    
}
