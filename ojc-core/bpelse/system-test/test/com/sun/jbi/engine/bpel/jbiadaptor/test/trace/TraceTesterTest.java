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
 * @(#)TraceTesterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.jbiadaptor.test.trace;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * 
 *
 *
 * @author Sun Microsystems
 */
public class TraceTesterTest extends TestCase {
	
    /** Creates a new instance of TraceTesterTest */
    public TraceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TraceTesterTest.class);
        return suite;
    }
    
    public void testFromEvaluation() throws Exception {
        CommonTraceTesterHelper.commonCode(
        		"/com/sun/jbi/engine/bpel/jbiadaptor/test/trace/bpels/fromEvaluation", 
        		"fromEvaluation.properties");
    }
    
    public void testTraceSanity() throws Exception {
        CommonTraceTesterHelper.commonCode(
        		"/com/sun/jbi/engine/bpel/jbiadaptor/test/trace/bpels/traceSanity", 
        		"traceSanity.properties");
    }
    
    public void testTraceSanityWithWait() throws Exception {
        CommonTraceTesterHelper.commonCode(
        		"/com/sun/jbi/engine/bpel/jbiadaptor/test/trace/bpels/traceSanityWithWait", 
        		"traceSanityWithWait.properties");
    }
}
