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
 * @(#)ModelBehaviorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.modelparsing;


import java.util.logging.Logger;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.While;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 * tests while, scope, sequence. Simulates the execution of the bpel.
 */
public class ModelBehaviorTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(ModelBehaviorTest.class.getName());

    public ModelBehaviorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ModelBehaviorTest.class);

        return suite;
    }

    // TODO
    public void testParseSequenceModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testParseSequenceModel");

        RBPELProcess process = loadBPELModel("modelIdParsing.bpel");
        RActivity act = (RActivity) process.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                                      .createActivityUnit(null, null,
                act, RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue());

        assertEquals(rootActUnit.getStaticModelActivity(), act);
        assertNull(rootActUnit.getEnclosingActivityUnit());
        assertNull(rootActUnit.getEnclosingScopeUnit());

        // walk the bpel.
        assertNull(rootActUnit.getNextActivityUnit());

        Utility.logExit(getClass().getSimpleName(), "testParseSequenceModel");
    }

    public void testParseWhileModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testParseWhileModel");

        RBPELProcess process = loadBPELModel("modelWhileBased.bpel");
        RActivity act = (RActivity) process.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                                      .createActivityUnit(null, null,
                act, RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue());
        long rootBranchId = rootActUnit.getBranchId();

        assertEquals(rootActUnit.getStaticModelActivity(), act);
        assertNull(rootActUnit.getEnclosingActivityUnit());
        assertNull(rootActUnit.getEnclosingScopeUnit());
        assertNull(rootActUnit.getNextActivityUnit());

        // walk the bpel.
        Receive rec = (Receive) ((RActivityHolder) act).getChildActivity();
        ActivityUnit recUnit = ActivityUnitFactory.getInstance()
                                                  .createActivityUnit(null, rootActUnit,
                (RActivity) rec, rootBranchId);

        assertNotNull(recUnit);

        ActivityUnit enclosingAct = recUnit.getEnclosingActivityUnit();
        assertNotNull(enclosingAct);
        assertEquals(enclosingAct.getStaticModelActivity(), act);

        ActivityUnit while1ActUnit = recUnit.getNextActivityUnit();
        assertNotNull(while1ActUnit);
        assertTrue(while1ActUnit.getStaticModelActivity() instanceof While);

        RActivityHolder while_staticModel = (RActivityHolder) while1ActUnit.getStaticModelActivity();
        RActivity while1SeqAct = while_staticModel.getChildActivity();
        assertTrue(while1SeqAct instanceof Sequence);

        ActivityUnit while1SeqActUnit = ActivityUnitFactory.getInstance()
                                                           .createActivityUnit(null, while1ActUnit,
                while1SeqAct, rootBranchId);

        RActivity while1SeqInvAct = ((RActivityHolder) while1SeqAct).getChildActivity();

        ActivityUnit invUnit = ActivityUnitFactory.getInstance()
                                                  .createActivityUnit(null, while1SeqActUnit,
                while1SeqInvAct, rootBranchId);
        assertNotNull(invUnit);

        ActivityUnit enclosingActInv = invUnit.getEnclosingActivityUnit();
        assertNotNull(enclosingActInv);
        assertEquals(enclosingActInv.getEnclosingActivityUnit()
                                    .getStaticModelActivity(),
            while1ActUnit.getStaticModelActivity());

        ActivityUnit while2ActUnit = while1ActUnit.getNextActivityUnit();
        assertNotNull(while2ActUnit);
        assertTrue(while2ActUnit.getStaticModelActivity() instanceof While);
        assertEquals(while2ActUnit.getStaticModelActivity().getName(), "While2");

        ActivityUnit nill = while2ActUnit.getNextActivityUnit();
        assertNull(nill);

        RActivityHolder while2_staticModel = (RActivityHolder) while2ActUnit.getStaticModelActivity();
        RActivity while2SeqAct = while2_staticModel.getChildActivity();
        ActivityUnit while2SeqActUnit = ActivityUnitFactory.getInstance()
                                                           .createActivityUnit(null, while2ActUnit,
                while2SeqAct, rootBranchId);

        RActivity while2_1Act = (RActivity) ((RActivityHolder) while2SeqAct).getChildActivity();

        ActivityUnit while2_1ActUnit = ActivityUnitFactory.getInstance()
                                                          .createActivityUnit(null, while2SeqActUnit,
                while2_1Act, while2SeqAct.getUniqueId());
        assertNotNull(while2_1ActUnit);

        Sequence while2_1SeqAct = (Sequence) ((While) while2_1ActUnit.getStaticModelActivity()).getActivity();
        ActivityUnit while2_1SeqActUnit = ActivityUnitFactory.getInstance()
                                                             .createActivityUnit(null, while2_1ActUnit,
                (RActivity) while2_1SeqAct, rootBranchId);

        Invoke inv2_1 = (Invoke) ((RActivityHolder) while2_1SeqAct).getChildActivity();

        ActivityUnit inv2_1Unit = ActivityUnitFactory.getInstance()
                                                     .createActivityUnit(null, while2_1SeqActUnit,
                (RActivity) inv2_1, rootBranchId);
        assertNotNull(inv2_1Unit);

        ActivityUnit enclosingActInv2_1 = inv2_1Unit.getEnclosingActivityUnit();
        assertNotNull(enclosingActInv2_1);
        assertEquals(enclosingActInv2_1.getEnclosingActivityUnit()
                                       .getStaticModelActivity(),
            while2_1ActUnit.getStaticModelActivity());
        assertNotNull(inv2_1Unit.getNextActivityUnit());
        assertNull(enclosingActInv2_1.getNextActivityUnit());

        assertEquals(inv2_1Unit.getNextActivityUnit().getEnclosingActivityUnit()
                               .getEnclosingActivityUnit()
                               .getEnclosingActivityUnit()
                               .getEnclosingActivityUnit(), while2ActUnit);

        Utility.logExit(getClass().getSimpleName(), "testParseWhileModel");
    }

    // TODO
    public void testParseScopeModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testParseScopeModel");

        RBPELProcess process = loadBPELModel("modelScopeBased.bpel");
        RActivity act = (RActivity) process.getActivity();
        ActivityUnit rootActUnit = ActivityUnitFactory.getInstance()
                                                      .createActivityUnit(null, null,
                act, RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue());
        long rootBranchId = rootActUnit.getBranchId();
        Utility.logExit(getClass().getSimpleName(), "testParseScopeModel");
    }
}
