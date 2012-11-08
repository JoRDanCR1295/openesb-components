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
 * @(#)ModelBehaviorTest2.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.modelparsing;


import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Flow;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FlowUnit;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 * tests Flow, Switch, Pick
 */
public class ModelBehaviorTest2 extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(ModelBehaviorTest.class.getName());

    public ModelBehaviorTest2(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ModelBehaviorTest2.class);

        return suite;
    }

    public void testParseFlowModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testParseFlowModel");

        RBPELProcess process = loadBPELModel("modelFlowBased.bpel");
        RActivity rootAct = (RActivity) process.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                                      .createActivityUnit(null, null,
                rootAct, RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
        long rootBranchId = rootActUnit.getBranchId();

        assertEquals(rootActUnit.getStaticModelActivity(), rootAct);
        assertNull(rootActUnit.getEnclosingActivityUnit());
        assertNull(rootActUnit.getEnclosingScopeUnit());
        assertNull(rootActUnit.getPrevActivityUnit());

        // walk the bpel.
        assertNull(rootActUnit.getNextActivityUnit());

        Flow flow1Act = (Flow) ((RActivityHolder) rootAct).getChildActivity();
        ActivityUnit flow1ActUnit = ActivityUnitFactory.getInstance()
                                                       .createActivityUnit(null, rootActUnit,
                (RActivity) flow1Act, rootBranchId);

        assertNotNull(flow1ActUnit);

        ActivityUnit prev = flow1ActUnit.getPrevActivityUnit();
        assertNull(prev);

        ActivityUnit enclosingAct = flow1ActUnit.getEnclosingActivityUnit();
        assertNotNull(enclosingAct);
        assertEquals(enclosingAct.getStaticModelActivity(), rootAct);

        Iterator iter = flow1Act.getActivities().iterator();
        List flow1Frames = new ArrayList();

        if (iter.hasNext()) {
            while (iter.hasNext()) {
                RActivity childAct = (RActivity) iter.next();
                long branchId = childAct.getUniqueId();
                ActivityUnit childActUnit = ActivityUnitFactory.getInstance()
                                                               .createActivityUnit(null, flow1ActUnit,
                        childAct, branchId);
                flow1Frames.add(childActUnit);
            }
        }

        ActivityUnit seqUnit = (ActivityUnit) flow1Frames.get(0);
        Receive rec1 = (Receive) ((RActivityHolder) seqUnit.getStaticModelActivity()).getChildActivity();
        ActivityUnit rec1Unit = ActivityUnitFactory.getInstance()
                                                   .createActivityUnit(null, seqUnit,
                (RActivity) rec1, seqUnit.getBranchId());

        assertNotNull(rec1Unit);

        ActivityUnit rec1UnitEnclosingAct = rec1Unit.getEnclosingActivityUnit();
        assertNotNull(enclosingAct);

        ActivityUnit rec1UnitNext = rec1Unit.getNextActivityUnit();
        assertNull(rec1UnitNext);

        ActivityUnit rec2Unit = flow1ActUnit.getNextActivityUnit();
        assertEquals(rec2Unit.getStaticModelActivity().getName(), "Receive2");

        ActivityUnit flow2ActUnit = rec2Unit.getNextActivityUnit();
        Iterator iter2 = ((Flow) flow2ActUnit.getStaticModelActivity()).getActivities()
                          .iterator();
        List flow2Frames = new ArrayList();

        if (iter2.hasNext()) {
            while (iter2.hasNext()) {
                RActivity childAct = (RActivity) iter2.next();
                long branchId = childAct.getUniqueId();
                ActivityUnit childActUnit = ActivityUnitFactory.getInstance()
                                                               .createActivityUnit(null, flow1ActUnit,
                        childAct, branchId);
                flow2Frames.add(childActUnit);
            }
        }

        ActivityUnit seq2Unit = (ActivityUnit) flow2Frames.get(0);
        Flow flow2_1Act = (Flow) ((RActivityHolder) seq2Unit.getStaticModelActivity()).getChildActivity();
        ActivityUnit flow2_1ActUnit = ActivityUnitFactory.getInstance()
                                                         .createActivityUnit(null, seq2Unit,
                (RActivity) flow2_1Act, seq2Unit.getBranchId());
        assertEquals(flow2_1ActUnit.getEnclosingActivityUnit(), seq2Unit);
        assertNull(flow2_1ActUnit.getNextActivityUnit());

        Iterator iter3 = flow2_1Act.getActivities().iterator();
        List flow2_1Frames = new ArrayList();

        if (iter3.hasNext()) {
            while (iter3.hasNext()) {
                RActivity childAct = (RActivity) iter3.next();
                long branchId = childAct.getUniqueId();
                ActivityUnit childActUnit = ActivityUnitFactory.getInstance()
                                                               .createActivityUnit(null, flow1ActUnit,
                        childAct, branchId);
                flow2_1Frames.add(childActUnit);
            }
        }

        ActivityUnit seq3Unit = (ActivityUnit) flow2_1Frames.get(0);
        Flow flow2_1_1Act = (Flow) ((RActivityHolder) seq3Unit.getStaticModelActivity()).getChildActivity();
        ActivityUnit flow2_1_1ActUnit = ActivityUnitFactory.getInstance()
                                                           .createActivityUnit(null, seq3Unit,
                (RActivity) flow2_1_1Act, seq3Unit.getBranchId());
        assertEquals(flow2_1_1ActUnit.getStaticModelActivity().getName(),
            "Flow2_1_1");
        assertTrue(flow2_1_1ActUnit instanceof FlowUnit);

        ActivityUnit seq2_Unit = (ActivityUnit) flow2Frames.get(1);
        Receive receive2_0Act = (Receive) ((RActivityHolder) seq2_Unit.getStaticModelActivity()).getChildActivity();
        ActivityUnit receive2_0ActUnit = ActivityUnitFactory.getInstance()
                                                            .createActivityUnit(null, seq2_Unit,
                (RActivity) receive2_0Act, seq2_Unit.getBranchId());

        assertEquals(receive2_0ActUnit.getEnclosingActivityUnit(), seq2_Unit);
        assertNotNull(receive2_0ActUnit.getNextActivityUnit());
        assertEquals(receive2_0ActUnit.getStaticModelActivity().getName(),
            "Receive2_0");

        ActivityUnit invoke2_0ActUnit = receive2_0ActUnit.getNextActivityUnit();
        assertEquals(invoke2_0ActUnit.getEnclosingActivityUnit(), seq2_Unit);
        assertNull(invoke2_0ActUnit.getNextActivityUnit());
        assertEquals(invoke2_0ActUnit.getStaticModelActivity().getName(),
            "invoke2_0");

        ActivityUnit invoke2ActUnit = flow2ActUnit.getNextActivityUnit();
        assertEquals(invoke2ActUnit.getStaticModelActivity().getName(),
            "invoke2");
        assertEquals(invoke2ActUnit.getEnclosingActivityUnit(),
            rec2Unit.getEnclosingActivityUnit());

        ActivityUnit reply1ActUnit = invoke2ActUnit.getNextActivityUnit();
        assertEquals(reply1ActUnit.getStaticModelActivity().getName(), "reply1");
        assertEquals(reply1ActUnit.getEnclosingActivityUnit(),
            rec2Unit.getEnclosingActivityUnit());
        assertNull(reply1ActUnit.getNextActivityUnit());

        Utility.logExit(getClass().getSimpleName(), "testParseFlowModel");
    }

    // TODO
    public void testParseSwitchModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testParseSwitchModel");

        RBPELProcess process = loadBPELModel("modelWhileBased.bpel");
        RActivity act = (RActivity) process.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                                      .createActivityUnit(null, null,
                act, RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());

        assertEquals(rootActUnit.getStaticModelActivity(), act);
        assertNull(rootActUnit.getEnclosingActivityUnit());
        assertNull(rootActUnit.getEnclosingScopeUnit());
        assertNull(rootActUnit.getNextActivityUnit());

        Utility.logExit(getClass().getSimpleName(), "testParseSwitchModel");
    }

    // TODO
    public void testParsePickModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testParsePickModel");

        RBPELProcess process = loadBPELModel("modelScopeBased.bpel");
        RActivity act = (RActivity) process.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                                      .createActivityUnit(null, null,
                act, RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());

        Utility.logExit(getClass().getSimpleName(), "testParsePickModel");
    }
}
