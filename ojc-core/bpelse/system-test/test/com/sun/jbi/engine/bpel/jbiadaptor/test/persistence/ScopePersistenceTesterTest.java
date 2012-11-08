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
 * @(#)ScopePersistenceTesterTest.java 
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
public class ScopePersistenceTesterTest extends TestCase {
    
    /** Creates a new instance of PersistenceTesterTest */
    public ScopePersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ScopePersistenceTesterTest.class);
        return suite;
    }
    
    public void testScopeVars() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTest/scopeVars", 
                                                 "scopeVars.properties");
    }
    
    public void testNestedScopeVars() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTest/nestedScopeVars", 
                                                 "nestedScopeVars.properties");
    }

    public void testWhileScope() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTest/whileScope", 
                                                 "whileScope.properties");
    }

    public void testScopeInFaultHandler() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTest/scopeFH", 
                                                 "scopeFH.properties");
    }
    
    public void testIterScopeVars() throws Exception {// exposes bug CR6516089
        CommonPersistenceTesterHelper.commonCode("bpels/ScopeTest/iterScopeVars", 
                                                 "iterScopeVars.properties");
    }
}
