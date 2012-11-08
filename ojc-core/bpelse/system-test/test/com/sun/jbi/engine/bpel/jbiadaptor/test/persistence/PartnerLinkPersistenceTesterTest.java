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
public class PartnerLinkPersistenceTesterTest extends TestCase {
    
    /** Creates a new instance of PersistenceTesterTest */
    public PartnerLinkPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(PartnerLinkPersistenceTesterTest.class);
        return suite;
    }
    
     public void testPartnerLink_Literal() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/partnerlink/literal", "literal.properties");
        verifyAllInstancesAreComplete();
    }
     
     public void testPartnerLink_myRoleToPartnerRole() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/partnerlink/myRoleToPartnerRole", "myRoleToPartnerRole.properties");
        verifyAllInstancesAreComplete();
    }
  
     public void test_myRoleToPartnerRole_DocFragWithoutSingleRootElement() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/partnerlink/myRoleToPartnerRole", "myRoleToPartnerRole2.properties");
         verifyAllInstancesAreComplete();
     } 
     
     private static void verifyAllInstancesAreComplete() throws Exception {
        
//        DummyTxManagerAndDataSource tm = (DummyTxManagerAndDataSource) BPELSERegistry.getInstance().lookup(
//                TransactionManager.class.getName());
//        Connection conn = tm.getNonTxConnection();
//        Statement stmt = conn.createStatement();
//        ResultSet rs = stmt.executeQuery("select count(*) from state where status != 'DONE'");
//        rs.next();
//        int incompleteInstanceCount = rs.getInt(1);
//        assertTrue(incompleteInstanceCount == 0);        
    }
}
