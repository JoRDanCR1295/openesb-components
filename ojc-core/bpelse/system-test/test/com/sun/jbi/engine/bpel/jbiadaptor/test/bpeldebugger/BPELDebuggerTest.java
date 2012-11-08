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
 * @(#)BPELDebuggerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.jbiadaptor.test.common.Utility;

/**
 * @author Sun Microsystems
 * Jan 27, 2006
 */
public class BPELDebuggerTest extends TestCase {
	
	Debugger testDebugger;
    /**
     * 
     */
    
    public static Test suite() {
        TestSuite suite = new TestSuite(BPELDebuggerTest.class);
        suite.addTestSuite(BPELLineNoToXpathTestCase.class);
        suite.addTestSuite(BPELLineNoToXpathBySaxTestCase.class);

        return suite;
    }
    
    /**
     * @param arg0
     */
    public BPELDebuggerTest(String arg0) {
        super(arg0);
        testDebugger = new Debugger ();
        // TODO Auto-generated constructor stub
    }
    
    protected void setUp() throws Exception {
        //SetUpHelper.getSetUp().getEngine();
    	Utility.logEnter(getClass().getSimpleName(), "setUp");
    	testDebugger.setUp();
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    	Utility.logEnter(getClass().getSimpleName(), "tearDown");
    	testDebugger.detach();
    	testDebugger.dbEngine.tearDown();
    }
    
    
    public void testSendMessage () throws Exception {
    	Utility.logEnter(getClass().getSimpleName(), "testSendMessage");
    	testDebugger.attach();
    	testDebugger.sendMessage();
    	assertEquals(testDebugger.outputProduced(), true);
    	assertEquals(testDebugger.isAttached(), true);
    	System.out.println(">>>>>Detaching the client >>>>>");
    	testDebugger.detach();
    	
//    	System.out.println(">>>>>Reattaching the client >>>>>");
//    	testDebugger.attach();
//    	assertEquals(testDebugger.isAttached(), true); 	
//    	if (!testDebugger.isAttached()) {
//    		System.out.println("Test failed, unable to connect");
//    		return;
//    	}
//    	testDebugger.sendMessage();
//    	assertEquals(testDebugger.outputProduced(), true);    	
    	
    	Utility.logExit(getClass().getSimpleName(), "testSendMessage");
    }
}
