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
 * @(#)PickPersistenceTesterTest.java 
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
public class PickPersistenceTesterTest extends TestCase {

    /** Creates a new instance of PersistenceTesterTest */
    public PickPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    
    public void testPickNonStartOnAlarmTest2() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/pick", "nonstartTest2.properties");
    }
    
    public void testPickNonStartOnAlarm() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/pick", "nonstart.properties");
    }

    public void testPickNonStartOnMsg() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/pick", "nonstartOnMsg.properties");
    }
    
    public void testPickStartInvoke1() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/pick", "start1.properties");
    }

    public void testPickStartInvoke3() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/pick", "start3.properties");
    }

    public void testPickStartReply2() throws Exception {
    //      commonCode("bpels/pick", "start2.properties");
        }

    public static Test suite() {
        TestSuite suite = new TestSuite(PickPersistenceTesterTest.class);
        return suite;
    }
    
}
