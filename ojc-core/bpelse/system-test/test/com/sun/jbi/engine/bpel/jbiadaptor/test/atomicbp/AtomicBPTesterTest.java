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
package com.sun.jbi.engine.bpel.jbiadaptor.test.atomicbp;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * 
 *
 *
 * @author Sun Microsystems
 */
public class AtomicBPTesterTest extends TestCase {
	
    /** Creates a new instance of TraceTesterTest */
    public AtomicBPTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(AtomicBPTesterTest.class);
        return suite;
    }
    
    public void testSimpleInOnly() throws Exception {
        CommonAtomicBPTesterHelper.commonCode(
        		"/com/sun/jbi/engine/bpel/jbiadaptor/test/atomicbp/bpels/simpleInOnly", 
        		"simpleInOnly.properties");
    }
    
    public void testSimpleInOut() throws Exception {
        CommonAtomicBPTesterHelper.commonCode(
        		"/com/sun/jbi/engine/bpel/jbiadaptor/test/atomicbp/bpels/simpleInOut", 
        		"simpleInOut.properties");
    }
    
    public void testInovkeAfterInOut() throws Exception {
    	CommonAtomicBPTesterHelper.commonCode(
    			"/com/sun/jbi/engine/bpel/jbiadaptor/test/atomicbp/bpels/invokeAfterInOut", 
    	"invokeAfterInOut.properties");
    }
    
    public void testMultipleInOnly() throws Exception {
    	CommonAtomicBPTesterHelper.commonCode(
    			"/com/sun/jbi/engine/bpel/jbiadaptor/test/atomicbp/bpels/multipleInOnly", 
    	"multipleInOnly.properties");
    }
}
