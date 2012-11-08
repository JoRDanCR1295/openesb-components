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
 * @(#)ForEachPersistenceTesterTest.java 
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
public class ForEachPersistenceTesterTest extends TestCase {
    
    /** Creates a new instance of PersistenceTesterTest */
    public ForEachPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ForEachPersistenceTesterTest.class);
        return suite;
    }
    
    public void testForEachSimple() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/simple", 
                                                 "foreachSimple.properties");
    }

    public void testForEachLoop() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/loopPersist", 
                                                 "foreachLoop.properties");
    }

    public void testForEachCompletion() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/completion", 
                                                 "foreachCompletion.properties");
    }

    public void testForEachSuccessOnly() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/successOnly", 
                                                 "foreachSuccessOnly.properties");
    }

    public void testForEachModifiedVars() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/modifiedVars", 
                                                 "forEachModifiedVars.properties");
    }
    
    public void testForEachNested() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/nested", 
                                                 "foreachNested.properties");
    }
    
    public void testForEachWhileNested() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/whileNested", 
                                                 "foreachWhileNested.properties");
    }
    
    public void testForEachIncrementNested() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/incrementNested", 
                                                 "foreachIncrNested.properties");
    }
    
    public void testForEachCounterAssign() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/ForEachTest/counterAssign",
                                                 "foreachCounterAssign.properties");
    }
}
