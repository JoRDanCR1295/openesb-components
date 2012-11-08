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
public class TransactionAttributeTest extends AbstractTestCase {
	
    //private static Logger mLogger = Logger.getLogger(TraceModelTest.class.getName());

	/**
	 * 
	 */
    public TransactionAttributeTest(String testName) {
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
        TestSuite suite = new TestSuite(TransactionAttributeTest.class);

        return suite;
    }

    /**
     * 
     * @throws Exception
     */
    public void testTransactionAttribute() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testTransactionAttribute");

        RBPELProcess process = loadBPELModel("transaction/Transaction.bpel");
 
        // Check that the attribute atomic is yes.
        assertNotNull(process.getAtomic());
        assertEquals("yes", process.getAtomic());
        assertNotNull(process.getChildActivity());

        
        Utility.logExit(getClass().getSimpleName(), "testTransactionAttribute");
    }
}
