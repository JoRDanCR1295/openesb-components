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
 * @(#)GenerateEventAttributeTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.test.modelparsing;

import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RReceive;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.impl.RAssignImpl;
import com.sun.bpel.model.meta.impl.RSequenceImpl;
import com.sun.bpel.model.meta.impl.RWaitImpl;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
/**
 * 
 *
 *
 * @author Sun Microsystems
 */
public class GenerateEventAttributeTest extends AbstractTestCase {
	
    //private static Logger mLogger = Logger.getLogger(GenerateEventAttributeTest.class.getName());

	/**
	 * 
	 */
    public GenerateEventAttributeTest(String testName) {
		super(testName);
	}

    /**
     * 
     */
	protected void setUp() throws Exception {
    }

	/**
	 * 
	 */
    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    /**
     * 
     * @return
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(GenerateEventAttributeTest.class);

        return suite;
    }

    /**
     * 
     * @throws Exception
     */
    public void testGenerateEventAttributeOnActivity() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testGenerateEventAttributeOnActivity");

        RBPELProcess process = loadBPELModel("events/activity/EventAttributes.bpel");
 
        // Sequence has a receive, scope and reply
        RActivity sequence = process.getChildActivity();
        assertTrue(sequence instanceof RSequenceImpl);
        assertNull(sequence.getGenerateEvents());
        
        // Receive
        RActivity receive = ((RActivityHolder) sequence).getChildActivity();
        assertTrue(receive instanceof RReceive);
        assertTrue("yes".equals(receive.getGenerateEvents()));
        
        RActivity assign = receive.getNextActivity();
        assertTrue(assign instanceof RAssignImpl);
        assertTrue("no".equals(assign.getGenerateEvents()));
        
        RActivity wait = assign.getNextActivity();
        assertTrue(wait instanceof RWaitImpl);
        assertTrue("yes".equals(wait.getGenerateEvents()));
        
        RActivity reply = wait.getNextActivity();
        assertTrue(reply instanceof RReply);
        assertTrue("yes".equals(reply.getGenerateEvents()));
        
        Utility.logExit(getClass().getSimpleName(), "testGenerateEventAttributeOnActivity");
    }
    
    /**
     * 
     * @throws Exception
     */
    public void testGenerateEventAttributeOnVariable() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testGenerateEventAttributeOnVariable");

        RBPELProcess process = loadBPELModel("events/variable/EventVariables.bpel");
 
        List procChildList = process.getChildren();
        
        // The second child in the given bpel is the variables element.
        Object obj = procChildList.get(2);
        assertTrue(obj instanceof BPELElement);
        BPELElement variables = (BPELElement) obj;
        assertTrue("variables".equals(variables.getLocalName()));
        List childList = variables.getChildren();
        
        Object varObj1 = childList.get(0);
        assertTrue(varObj1 instanceof RVariable);
        RVariable var1 = (RVariable) varObj1;
        assertTrue("EventVariablesOperationOut".equals(var1.getName()));
        assertTrue("yes".equals(var1.getGenerateEvents()));
        
        Object varObj2 = childList.get(1);
        assertTrue(varObj2 instanceof RVariable);
        RVariable var2 = (RVariable) varObj2;
        assertTrue("EventVariablesOperationIn".equals(var2.getName()));
        assertTrue("no".equals(var2.getGenerateEvents()));
        
        Utility.logExit(getClass().getSimpleName(), "testGenerateEventAttributeOnVariable");
    }
}
