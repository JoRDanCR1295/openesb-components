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
 * @(#)ModelParsingIDTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;


import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 */
public class ModelParsingIDTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(
            "com.sun.jbi.engine.bpel.core.test.bpelpersist.ModelParsingID");

    public ModelParsingIDTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mEng = OneTimesetUp.getSetUp().getEngine();
        mEngId = mEng.getId();
        mConnProp = OneTimesetUp.getSetUp().getConnectionProperties();
        mStateMgr = mEng.getStateManager();
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ModelParsingIDTest.class);

        return suite;
    }

    public void testParseModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testParseModel");

        RBPELProcess process1 = loadBPELModel("modelIdParsing.bpel");
        RBPELProcess process2 = loadBPELModel("modelIdParsing.bpel");
        Utility.assertArrayEquals("container Id ", getContainerIds(process1),
            getContainerIds(process2));
        Utility.assertArrayEquals("receive activity Id ",
            getActivityIds(process1), getActivityIds(process2));
        Utility.logExit(getClass().getSimpleName(), "testParseModel");
    }

    //    public void testStructuredParseModel() {
    //        logEnter("testStructuredParseModel");
    //        assertNotNull(mEngStateMgr);
    //        RBPELProcess process1 = loadBPELModel("modelIdParsing.bpel");
    //        RBPELProcess process2 = loadBPELModel("modelIdParsing.bpel");
    //        assertArrayEquals("container Id ", getContainerIds(process1), 
    //                getContainerIds(process2)); 
    //        assertArrayEquals("receive activity Id ", getActivityIds(process1), 
    //                getActivityIds(process2)); 
    //        
    //        logExit("testStructuredParseModel");
    //    }
    private long[] getContainerIds(RBPELProcess process) {
        Variables cons = process.getVariables();
        assertNotNull(cons);

        // there are four containers in the test file.
        Collection containers = cons.getVariables();
        assertNotNull(containers);
        assertEquals(4, containers.size());

        long[] retVal = new long[4];
        int i = 0;

        for (Iterator itr = containers.iterator(); itr.hasNext(); i++) {
            RVariable var = (RVariable) itr.next();
            retVal[i] = var.getUniqueId();
            assertIdAssigned(var);
        }

        return retVal;
    }

    private Long[] getActivityIds(RBPELProcess process) {
        RActivity firstActivity = (RActivity) process.getChildActivity();
        List retVals = new ArrayList();
        getActivityIds(firstActivity, retVals);

        return (Long[]) retVals.toArray(new Long[] {  });
    }

    private void getActivityIds(RActivity tree, List retVals) {
        if (tree == null) {
            return;
        }

        retVals.add(new Long(tree.getUniqueId()));

        if (tree instanceof RActivityHolder) {
            RActivity child = ((RActivityHolder) tree).getChildActivity();
            getActivityIds(child, retVals);
        }

        for (RActivity next = tree.getNextActivity(); next != null;
                next = next.getNextActivity()) {
            getActivityIds(next, retVals);
        }
    }

    private long[] getReceiveIds(RBPELProcess process) {
        return null;
    }

    private long[] getInvokeIds(RBPELProcess process) {
        return null;
    }

    private void assertIdAssgined(RActivity act) {
        if (act instanceof Receive || act instanceof OnMessage ||
                act instanceof Invoke || act instanceof Reply ||
                act instanceof OnAlarm || act instanceof Wait) {
            long id = act.getUniqueId();

            // check that it is not default value
            assertTrue(Long.MIN_VALUE != id);

            //check for range
            assertTrue(2000000 <= id);
            assertTrue(id < 3000000);
        }
    }

    private void assertIdAssigned(RVariable con) {
        long id = con.getUniqueId();

        // check that it is not default value
        assertTrue(Long.MIN_VALUE != id);

        //check for range
        assertTrue(1000000 <= id);
        assertTrue(id < 2000000);
    }
}
