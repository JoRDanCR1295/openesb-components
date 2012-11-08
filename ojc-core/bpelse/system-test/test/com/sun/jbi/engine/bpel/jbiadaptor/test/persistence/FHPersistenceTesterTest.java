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
public class FHPersistenceTesterTest extends TestCase {

    /** Creates a new instance of PersistenceTesterTest */
    public FHPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FHPersistenceTesterTest.class);
        return suite;
    }

    
    public void testFHSimpleThrow() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/faulthandler/bpsimplethrow", "invoke.properties");
    }
    
    public void testProcessFH() throws Exception {
        // this causes the fault to be caught at the process level fault handlers
        CommonPersistenceTesterHelper.commonCode("bpels/faulthandler/throwAndSimpleCatch", "processFH.properties");
    }
    
    public void testScopeFH() throws Exception {
        // this causes the fault to be caught at the scope level fault handlers
        CommonPersistenceTesterHelper.commonCode("bpels/faulthandler/throwAndSimpleCatch", "scopeFH.properties");
    }

    public void testThrowInProcessFH() throws Exception {
        // this causes the fault to be caught at the process level fault handlers 
        // and to be thrown again from the process level fault handlers.
        CommonPersistenceTesterHelper.commonCode("bpels/faulthandler/throwInsideFH", "processFH.properties");
    }
    
    public void testThrowInScopeFH() throws Exception {
        // this causes the fault to be caught at the scope level fault handlers
        // and to be thrown again from the scope level fault handlers
        CommonPersistenceTesterHelper.commonCode("bpels/faulthandler/throwInsideFH", "scopeFH.properties");
    }
    
    public void testInvokeThrowsInProcessScope() throws Exception {
        // this causes the fault to be caught at the scope level fault handlers
        // and to be thrown again from the scope level fault handlers
        CommonPersistenceTesterHelper.commonCode("bpels/faulthandler/invokeThrowsFault", 
                "throwInProcessScope.properties");
    }

    public void testInvokeThrowsInScope() throws Exception {
        // this causes the fault to be caught at the scope level fault handlers
        // and to be thrown again from the scope level fault handlers
        CommonPersistenceTesterHelper.commonCode("bpels/faulthandler/invokeThrowsFault", 
                "throwInScope.properties");
    }
    
}
