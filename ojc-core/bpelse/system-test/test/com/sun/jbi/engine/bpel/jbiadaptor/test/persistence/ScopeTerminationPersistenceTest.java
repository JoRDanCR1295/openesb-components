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
public class ScopeTerminationPersistenceTest extends TestCase {

    /** Creates a new instance of ScopeTerminationTest */
    public ScopeTerminationPersistenceTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ScopeTerminationPersistenceTest.class);
        return suite;
    }

    public void testReceiveTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/ReceiveTerminationInScope", "Test.properties");
    }
    
    public void testTerminationInProcessAndScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/TerminationInProcessAndScope", "Test.properties");
    }
    
    public void testFaultFromEH() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/FaultFromEH", "Test.properties");
    }
    public void testWaitTerminationInProcess() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/WaitTerminationInProcess", "Test.properties");
    }

    public void testPickAndOnAlarmTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/PickAndOnAlarmTerminationInScope", "Test.properties");
    }

    public void testForEachTerminationInProcess() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/ForEachTerminationInProcess", "Test.properties");
    }

    public void testIfTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/IfTerminationInScope", "Test.properties");
    }

    public void testCompensateTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/CompensateTerminationInScope", "Test.properties");
    }

    public void testCompensateScopeTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/CompensateScopeTerminationInScope", "Test.properties");
    }

    public void testEHOnAlarmTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/EHOnAlarmTerminationInScope", "Test.properties");
    }

    public void testEHOnAlarmScopeTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/EHOnAlarmScopeTerminationInScope", "Test.properties");
    }

    public void testEHonEventScopeTerminationInScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTermination/EHonEventScopeTerminationInScope", "Test.properties");
    }
}
