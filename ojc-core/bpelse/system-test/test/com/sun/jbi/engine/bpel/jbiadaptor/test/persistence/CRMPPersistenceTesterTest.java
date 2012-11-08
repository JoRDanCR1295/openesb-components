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
 * @(#)CRMPPersistenceTesterTest.java 
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
public class CRMPPersistenceTesterTest extends TestCase {
    
    /** Creates a new instance of PersistenceTesterTest */
    public CRMPPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CRMPPersistenceTesterTest.class);
        return suite;
    }
    
    public void testSubBPRecover() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/subBPRecovery/SubBPRecover", "SubBPRecover.properties");
    }    
 
    public void testSubBPRecoverWhile() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/subBPRecovery/SubBPRecoverWhile", "SubBPRecoverWhile.properties");
    }    
    public void testSubBPRecoverForEach() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/subBPRecovery/SubBPRecoverForEach", "SubBPRecoverForEach.properties");
    } 
}
