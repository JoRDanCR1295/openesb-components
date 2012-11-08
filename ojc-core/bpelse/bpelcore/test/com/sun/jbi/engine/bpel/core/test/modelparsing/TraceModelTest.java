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
 * @(#)TraceModelTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.test.modelparsing;

import java.util.Collection;
import java.util.Iterator;
//import java.util.logging.Logger;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RInvoke;
import com.sun.bpel.model.meta.RReceive;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RThrow;
import com.sun.bpel.model.meta.impl.RAssignImpl;
import com.sun.bpel.model.meta.impl.REmptyImpl;
import com.sun.bpel.model.meta.impl.RFlowImpl;
import com.sun.bpel.model.meta.impl.RForEachImpl;
import com.sun.bpel.model.meta.impl.RIfImpl;
import com.sun.bpel.model.meta.impl.RPickImpl;
import com.sun.bpel.model.meta.impl.RRepeatUntilImpl;
import com.sun.bpel.model.meta.impl.RSequenceImpl;
import com.sun.bpel.model.meta.impl.RTerminateImpl;
import com.sun.bpel.model.meta.impl.RWaitImpl;
import com.sun.bpel.model.meta.impl.RWhileImpl;

import com.sun.jbi.engine.bpel.core.test.common.Utility;
/**
 * 
 *
 *
 * @author Sun Microsystems
 */
public class TraceModelTest extends AbstractTestCase {
	
    //private static Logger mLogger = Logger.getLogger(TraceModelTest.class.getName());

	/**
	 * 
	 */
    public TraceModelTest(String testName) {
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
        TestSuite suite = new TestSuite(TraceModelTest.class);

        return suite;
    }

    /**
     * 
     * @throws Exception
     */
    public void testTraceModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testTraceModel");

        RBPELProcess process = loadBPELModel("trace/Trace.bpel");
 
        // Sequence has a receive, scope and reply
        RActivity sequence = process.getChildActivity();
        assertTrue(sequence instanceof RSequenceImpl);
        checkTrace(sequence);
        
        // Receive
        RActivity receive = ((RActivityHolder) sequence).getChildActivity();
        assertTrue(receive instanceof RReceive);
        checkTrace(receive);
        
        // Scope has a fault handler and sequence.
        RActivity scopeAct = receive.getNextActivity();
        checkTrace(scopeAct);
        
        // Catch - get faulthandler from scope and then catch
        Scope scope = (Scope) scopeAct;
        Collection catchCol = scope.getFaultHandlers().getCatches();
        assertEquals(1, catchCol.size());
        Catch catchAct = (Catch) catchCol.iterator().next();
        checkTrace(catchAct);
        
        // Empty
        RActivity empty = (RActivity) catchAct.getActivity();
        assertTrue(empty instanceof REmptyImpl);
        checkTrace(empty);
        
        // CatchAll - get faulthandler from scope and then catchAll
        CatchAll catchAll = scope.getFaultHandlers().getCatchAll();
        checkTrace(catchAll);
        
        // Exit
        RActivity exit = (RActivity) catchAll.getActivity();
        assertTrue(exit instanceof RTerminateImpl);
        checkTrace(exit);
        
        // Invoke - get the sequence from the scope and then the invoke
        RActivity invoke = ((RActivityHolder)((RActivityHolder) scopeAct).getChildActivity()).getChildActivity();
        assertTrue(invoke instanceof RInvoke);
        checkTrace(invoke);
        
        // Assign
        RActivity assign = invoke.getNextActivity();
        assertTrue(assign instanceof RAssignImpl);
        checkTrace(assign);
        
        // Pick
        RActivity pick = assign.getNextActivity();
        assertTrue(pick instanceof RPickImpl);
        checkTrace(pick);
        
        // If - get the onMessage from Pick and then the if
        assertEquals(1, ((Pick) pick).getOnMessageSize());
        RActivity ifAct = ((RActivityHolder)((Pick) pick).getOnMessage(0)).getChildActivity();
        assertTrue(ifAct instanceof RIfImpl);
        checkTrace(ifAct);
        
        // Throw - get the sequence from if and then the Throw
        RActivity throwAct = ((RActivityHolder)((RActivityHolder) ifAct).getChildActivity()).getChildActivity();
        assertTrue(throwAct instanceof RThrow);
        checkTrace(throwAct);
        
        // Flow - get else from if and then flow
        RActivity flow = ((RActivityHolder)((If) ifAct).getElse()).getChildActivity();
        assertTrue(flow instanceof RFlowImpl);
        checkTrace(flow);
        
        // While - get sequence from flow and then while
        RActivity whileAct = ((RActivityHolder)((RActivityHolder) flow).getChildActivity()).getChildActivity();
        assertTrue(whileAct instanceof RWhileImpl);
        checkTrace(whileAct);
        
        // RepeatUntil
        RActivity repeatUntil = whileAct.getNextActivity();
        assertTrue(repeatUntil instanceof RRepeatUntilImpl);
        checkTrace(repeatUntil);
        
        // ForEach
        RActivity forEach = repeatUntil.getNextActivity();
        assertTrue(forEach instanceof RForEachImpl);
        checkTrace(forEach);
        
        // Wait
        RActivity wait = forEach.getNextActivity();
        assertTrue(wait instanceof RWaitImpl);
        checkTrace(wait);
        
        // Reply
        RActivity reply = scopeAct.getNextActivity();
        assertTrue(reply instanceof RReply);
        checkTrace(reply);
        
        Utility.logExit(getClass().getSimpleName(), "testTraceModel");
    }
    
    /**
     * 
     * @throws Exception
     */
    public void testTraceModelParse() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testTraceModelParse");

        RBPELProcess process = loadBPELModel("trace/parse/Trace.bpel");
 
        // Sequence has a receive, scope and reply
        RActivity sequence = process.getChildActivity();
        assertTrue(sequence instanceof RSequenceImpl);
        
        // Receive
        RActivity receive = ((RActivityHolder) sequence).getChildActivity();
        assertTrue(receive instanceof RReceive);
        
        // Assign 1 - Test multiple logs and alerts for both onStart and onComplete
        RActivity assign1 = receive.getNextActivity();
        assertTrue(assign1 instanceof RAssignImpl);
        Trace trace1 = assign1.getTrace();
        assertNotNull(trace1);
        assertTrue(trace1.hasLogs());
        assertTrue(trace1.hasAlerts());
        
        String[] logLevel = new String[] {Log.FINE, Log.INFO, Log.SEVERE};
        assertTrue(trace1.hasOnStartLogs());
        Collection<Log> osLogCol1 = trace1.getOnStartLogs();
        assertEquals(3, osLogCol1.size());
        Iterator<Log> osLogIter1 = osLogCol1.iterator();
        for (int i = 0; i < 3; i++) {
        	Log log = osLogIter1.next();
        	assertEquals(logLevel[i], log.getLevel());
        	assertEquals(Trace.ON_START, log.getLocation());
        	assertEquals("This is the message for the log os" + (i + 1), log.getFrom().getLiteral().getValue());
        }
        
        assertTrue(trace1.hasOnCompleteLogs());
        Collection<Log> ocLogCol1 = trace1.getOnCompleteLogs();
        assertEquals(3, ocLogCol1.size());
        Iterator<Log> ocLogIter1 = ocLogCol1.iterator();
        for (int i = 0; i < 3; i++) {
        	Log log = ocLogIter1.next();
        	assertEquals(logLevel[i], log.getLevel());
        	assertEquals(Trace.ON_COMPLETE, log.getLocation());
        	assertEquals("This is the message for the log oc" + (i + 1), log.getFrom().getLiteral().getValue());
        }
        
        String[] alertLevel = new String[] {Alert.INFO, Alert.MINOR, Alert.CRITICAL};
        assertTrue(trace1.hasOnStartAlerts());
        Collection<Alert> osAlertCol1 = trace1.getOnStartAlerts();
        assertEquals(3, osAlertCol1.size());
        Iterator<Alert> osAlertIter1 = osAlertCol1.iterator();
        for (int i = 0; i < 3; i++) {
        	Alert alert = osAlertIter1.next();
        	assertEquals(alertLevel[i], alert.getLevel());
        	assertEquals(Trace.ON_START, alert.getLocation());
        	assertEquals("This is the message for the alert os" + (i + 1), alert.getFrom().getLiteral().getValue());
        }
        
        assertTrue(trace1.hasOnCompleteAlerts());
        Collection<Alert> ocAlertCol1 = trace1.getOnCompleteAlerts();
        assertEquals(3, ocAlertCol1.size());
        Iterator<Alert> ocAlertIter1 = ocAlertCol1.iterator();
        for (int i = 0; i < 3; i++) {
        	Alert alert = ocAlertIter1.next();
        	assertEquals(alertLevel[i], alert.getLevel());
        	assertEquals(Trace.ON_COMPLETE, alert.getLocation());
        	assertEquals("This is the message for the alert oc" + (i + 1), alert.getFrom().getLiteral().getValue());
        }

        // Assign 2 - test default values for both log and alert
        RActivity assign2 = assign1.getNextActivity();
        assertTrue(assign2 instanceof RAssignImpl);
        
        Trace trace2 = assign2.getTrace();
        assertNotNull(trace2);
        assertTrue(trace2.hasLogs());
        assertTrue(trace2.hasAlerts());
        
        assertFalse(trace2.hasOnStartLogs());
        assertTrue(trace2.hasOnCompleteLogs());
        assertEquals(0, trace2.getOnStartLogs().size());
        Collection<Log> ocLogCol2 = trace2.getOnCompleteLogs();
        assertEquals(1, ocLogCol2.size());
        Log log2 = ocLogCol2.iterator().next();
        assertEquals(Trace.ON_COMPLETE, Log.DEFAULT_LOCATION);
        assertEquals(Trace.ON_COMPLETE, log2.getLocation());
        assertEquals(Log.INFO, Log.DEFAULT_LEVEL);
        assertEquals(Log.INFO, log2.getLevel());
        
        assertFalse(trace2.hasOnStartAlerts());
        assertTrue(trace2.hasOnCompleteAlerts());
        assertEquals(0, trace2.getOnStartAlerts().size());
        Collection<Alert> ocAlertCol2 = trace2.getOnCompleteAlerts();
        assertEquals(1, ocAlertCol2.size());
        Alert alert2 = ocAlertCol2.iterator().next();
        assertEquals(Trace.ON_COMPLETE, Alert.DEFAULT_LOCATION);
        assertEquals(Trace.ON_COMPLETE, alert2.getLocation());
        assertEquals(Alert.INFO, Alert.DEFAULT_LEVEL);
        assertEquals(Alert.INFO,alert2.getLevel());

        // Assign 3 - test log only
        RActivity assign3 = assign2.getNextActivity();
        assertTrue(assign3 instanceof RAssignImpl);
        
        Trace trace3 = assign3.getTrace();
        assertNotNull(trace3);
        assertTrue(trace3.hasLogs());
        assertFalse(trace3.hasAlerts());
        
        assertTrue(trace3.hasOnStartLogs());
        assertFalse(trace3.hasOnCompleteLogs());
        Collection<Log> osLogCol3 = trace3.getOnStartLogs();
        assertEquals(1, osLogCol3.size());
        assertEquals(0, trace3.getOnCompleteLogs().size());
        Log log3 = osLogCol3.iterator().next();
        assertEquals(Trace.ON_START, log3.getLocation());
        assertEquals(Log.SEVERE, log3.getLevel());
        
        assertFalse(trace3.hasOnStartAlerts());
        assertFalse(trace3.hasOnCompleteAlerts());
        assertEquals(0, trace3.getOnStartAlerts().size());
        assertEquals(0, trace3.getOnCompleteAlerts().size());

        // Assign 4 - test alert only
        RActivity assign4 = assign3.getNextActivity();
        assertTrue(assign4 instanceof RAssignImpl);
        
        Trace trace4 = assign4.getTrace();
        assertNotNull(trace4);
        assertFalse(trace4.hasLogs());
        assertTrue(trace4.hasAlerts());
        
        assertFalse(trace4.hasOnStartLogs());
        assertFalse(trace4.hasOnCompleteLogs());
        assertEquals(0, trace4.getOnStartLogs().size());
        assertEquals(0, trace4.getOnCompleteLogs().size());
        
        assertTrue(trace4.hasOnStartAlerts());
        assertFalse(trace4.hasOnCompleteAlerts());
        Collection<Alert> osAlertCol4 = trace4.getOnStartAlerts();
        assertEquals(1, osAlertCol4.size());
        assertEquals(0, trace4.getOnCompleteAlerts().size());
        Alert alert4 = osAlertCol4.iterator().next();
        assertEquals(Trace.ON_START, alert4.getLocation());
        assertEquals(Alert.CRITICAL, alert4.getLevel());
        
        // Reply
        RActivity reply = assign4.getNextActivity();
        assertTrue(reply instanceof RReply);
        
        Utility.logExit(getClass().getSimpleName(), "testTraceModelParse");
    }
    
    /*
     * 
     */
    private void checkTrace(BPELElement bpelElement) {
    	
    	Trace trace = bpelElement.getTrace();
    	assertNotNull(trace);

    	assertTrue(trace.hasLogs());
    	Collection<Log> logCol = trace.getLogs();
    	assertEquals(1, logCol.size());
    	Log log = logCol.iterator().next();
    	assertEquals(Log.INFO, log.getLevel());
    	assertEquals(Trace.ON_COMPLETE, log.getLocation());
    	assertNotNull(log.getFrom());
    	
    	assertTrue(trace.hasAlerts());
    	Collection<Alert> alertCol = trace.getAlerts();
    	assertEquals(1, alertCol.size());
    	Alert alert = alertCol.iterator().next();
    	assertEquals(Alert.MINOR, alert.getLevel());
    	assertEquals(Trace.ON_COMPLETE, alert.getLocation());
    	assertNotNull(alert.getFrom());
    }
}
