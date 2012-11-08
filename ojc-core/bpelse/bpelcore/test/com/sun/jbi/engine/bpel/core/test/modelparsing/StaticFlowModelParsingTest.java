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
 * @(#)StaticFlowModelParsingTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.modelparsing;


import java.util.logging.Logger;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.While;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 *
 * Tests
 * RActivity getNextActivity();
 * RActivity getChildActivity();
 * RActivity getParentActivity();
 * API of the RActivity interface.
 */
public class StaticFlowModelParsingTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(StaticFlowModelParsingTest.class.getName());

    public StaticFlowModelParsingTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(StaticFlowModelParsingTest.class);

        /*
                TestSuite suite = new TestSuite();
                Test t = new StaticFlowModelParsingTest("testModelWhileParsing");
                suite.addTest(t);
        */
        return suite;
    }

    public void testParseFlowStaticModel() {
        Utility.logEnter(getClass().getSimpleName(), "testParseFlowStaticModel");

        RBPELProcess process = loadBPELModel("modelFlowBased.bpel");

        RActivity rootAct = (RActivity) process.getActivity();
        assertTrue(rootAct instanceof RActivityHolder);

        RActivityHolder root = (RActivityHolder) rootAct;
        assertNotNull(root.getChildActivity());

        RActivity flow1Act = root.getChildActivity();
        assertTrue(flow1Act instanceof Flow);

        RActivity flow1SeqAct = ((RActivityHolder) flow1Act).getChildActivity();
        assertNotNull(flow1SeqAct);
        assertNotNull(((RActivityHolder) flow1SeqAct).getChildActivity());

        RActivity flow1Seq2Act = flow1SeqAct.getNextActivity();
        assertNotNull(flow1Seq2Act);

        RActivity flow1Seq2InvokeAct = ((RActivityHolder) flow1Seq2Act).getChildActivity();
        assertTrue(flow1Seq2InvokeAct instanceof Invoke);

        RActivity rec2Act = flow1Act.getNextActivity();
        assertTrue(rec2Act.getName().equals("Receive2"));

        Utility.logExit(getClass().getSimpleName(), "testParseFlowStaticModel");
    }

    public void testModelIdParsing() {
        Utility.logEnter(getClass().getSimpleName(), "testModelIdParsing");

        BPELProcess process = loadBPELModel("modelIdParsing.bpel");

        RActivity root = (RActivity) process.getActivity();
        assertTrue(root instanceof Sequence);
        assertNotNull(((RActivityHolder) root).getChildActivity());

        RActivity rec1Act = ((RActivityHolder) root).getChildActivity();
        assertTrue(rec1Act instanceof Receive);
        assertTrue(rec1Act.getName().equals("Receive1"));

        RActivity invoke1Act = rec1Act.getNextActivity();
        RActivity wait1Act = invoke1Act.getNextActivity();
        assertTrue(wait1Act instanceof Wait);

        RActivity pickAct = wait1Act.getNextActivity().getNextActivity();
        assertTrue(pickAct instanceof Pick);

        OnMessage onMsg1 = ((Pick) pickAct).getOnMessage(0);
        RActivity onMsg1SeqAct = (RActivity) onMsg1.getActivity();
        assertNull(onMsg1SeqAct.getNextActivity());

        RActivity onMsg1AssignAct = ((RActivityHolder) onMsg1SeqAct).getChildActivity();
        assertTrue(onMsg1AssignAct instanceof Assign);
        assertNull(onMsg1AssignAct.getNextActivity().getNextActivity());

        OnMessage onMsg2 = ((Pick) pickAct).getOnMessage(0);
        RActivity onMsg2SeqAct = (RActivity) onMsg2.getActivity();
        assertNull(onMsg2SeqAct.getNextActivity());

        RActivity onMsg2AssignAct = ((RActivityHolder) onMsg2SeqAct).getChildActivity();
        assertTrue(onMsg2AssignAct instanceof Assign);
        assertNull(onMsg2AssignAct.getNextActivity().getNextActivity());

        OnAlarm onalrm = ((Pick) pickAct).getOnAlarm(0);
        RActivity onalrmSeqAct = (RActivity) onalrm.getActivity();
        assertNull(onalrmSeqAct.getNextActivity());

        RActivity onalrmAssignAct = ((RActivityHolder) onalrmSeqAct).getChildActivity();
        assertTrue(onalrmAssignAct instanceof Assign);
        assertNull(onalrmAssignAct.getNextActivity().getNextActivity());

        RActivity rep2Act = pickAct.getNextActivity();
        assertTrue(rep2Act instanceof Reply);
        assertNull(rep2Act.getNextActivity());

        Utility.logExit(getClass().getSimpleName(), "testModelIdParsing");
    }

    public void testModelWhileParsing() {
        Utility.logEnter(getClass().getSimpleName(), "testModelWhileParsing");

        BPELProcess process = loadBPELModel("modelWhileBased.bpel");

        RActivity root = (RActivity) process.getActivity();
        assertTrue(root instanceof Sequence);
        assertNotNull(root.getChildren());

        RActivity recAct = ((RActivityHolder) root).getChildActivity();
        assertNotNull(recAct);
        assertTrue(recAct instanceof Receive);
        assertTrue(recAct.getName().equals("Receive"));

        RActivity while1Act = recAct.getNextActivity();
        assertTrue(while1Act instanceof While);
        assertTrue(while1Act.getName().equals("While1"));

        RActivity while1SeqAct = ((RActivityHolder) while1Act).getChildActivity();
        assertNull(while1SeqAct.getNextActivity());

        RActivity while1SeqInvokeAct = ((RActivityHolder) while1SeqAct).getChildActivity();
        assertNull(while1SeqInvokeAct.getNextActivity());

        RActivity while2Act = while1Act.getNextActivity();
        assertNull(while2Act.getNextActivity());

        RActivityHolder while2ActChild = (RActivityHolder) ((RActivityHolder) while2Act).getChildActivity();
        RActivity while2_1Act = while2ActChild.getChildActivity();
        RActivityHolder while2_1ActChild = (RActivityHolder) ((RActivityHolder) while2_1Act).getChildActivity();
        RActivity while2_1SeqInvokeAct = while2_1ActChild.getChildActivity();
        assertNotNull(while2_1SeqInvokeAct.getNextActivity());
        assertNull(while2_1SeqInvokeAct.getNextActivity().getNextActivity());

        Utility.logExit(getClass().getSimpleName(), "testModelWhileParsing");
    }
}
