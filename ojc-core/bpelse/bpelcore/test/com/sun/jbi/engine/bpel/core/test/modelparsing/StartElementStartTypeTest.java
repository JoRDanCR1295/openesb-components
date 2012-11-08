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
 * @(#)StartElementStartTypeTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.modelparsing;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Inc
 * May 2, 2006
 */
public class StartElementStartTypeTest extends AbstractTestCase {

    /**
     * @param testName
     */
    public StartElementStartTypeTest(String testName) {
        super(testName);
    }


    public static Test suite() {
        TestSuite suite = new TestSuite(StartElementStartTypeTest.class);

        return suite;
    }

    /**
     * Rec - Correlation="no (and ("yes" or "join"))"
     */
    public void testCorrelateTypeForReceive() {
        Utility.logEnter(getClass().getSimpleName(), "testCorrelateTypeForReceive");
        
        RBPELProcess process = loadBPELModel("correlation/startelementstarttype/TestReceiveparent.bpel");
        
        RActivity act = HelperFunc.getActivity(process, "Corr_No");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Corr_Yes");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Corr_No_Yes");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Corr_No_Join");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Corr_Yes_Join");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Corr_Join_Join");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Corr_Join");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Corr_Yes_Join_No");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "Random_Routing");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY);
        
        Utility.logExit(getClass().getSimpleName(), "testCorrelateTypeForReceive");
    }
    
    /**
     * Rec - CreateInstane = "yes" And Correlation="yes" or "join" or "yes and join"
     * Rec - CreateInstane = "yes" (No correlation)
     */
    public void testCorrelateTypeForReceiveWithinFlow() {
        Utility.logEnter(getClass().getSimpleName(), "testCorrelateTypeForReceiveWithinFlow");
        
        RBPELProcess process = loadBPELModel("correlation/flowmodel/correlatiingReceiveWithinFlow.bpel");
        
        RActivity act = HelperFunc.getActivity(process, "correlatiingReceiveWithinFlowOperation1");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "ThirdReceive");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "FourthReceive");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
        
        act = HelperFunc.getActivity(process, "FifthReceive_Random_Routing");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY);
        
        Utility.logExit(getClass().getSimpleName(), "testCorrelateTypeForReceiveWithinFlow");
    }
    
    /**
     * Rec - CreateInstane = "yes" (No correlation)
     * Rec - CreateInstane = "yes" And Correlation="yes" or "join" or "yes and join"
     */
    public void testCreateTypeForReceive() {
        Utility.logEnter(getClass().getSimpleName(), "testCreateTypeForReceive");
        
        RBPELProcess process = loadBPELModel("correlation/startelementstarttype/TestReceivechild.bpel");
        
        RActivity act = HelperFunc.getActivity(process, "MyRole.TestReceivechildOperation.Receive");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        process = loadBPELModel("correlation/startelementstarttype/TestReceivechild2.bpel");
        act = HelperFunc.getActivity(process, "MyRole.TestReceivechild2Operation.Receive");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        process = loadBPELModel("correlation/startelementstarttype/TestReceivechild3.bpel");
        act = HelperFunc.getActivity(process, "MyRole.TestReceivechild3Operation.Receive");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        process = loadBPELModel("correlation/startelementstarttype/TestReceivechild4.bpel");
        act = HelperFunc.getActivity(process, "MyRole.TestReceivechild4Operation.Receive");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        Utility.logExit(getClass().getSimpleName(), "testCreateTypeForReceive");
    }


    /**
     * Flow  
     *     - Rec - CreateInstane = "yes" And Correlation="yes" or "join" or "yes and join"
     *     
     * (Or)
     *     
     * Flow 
     *     - Rec - CreateInstane = "yes" (No correlation)
     */
    public void testCreateTypeForReceiveWithinFlow() {
        Utility.logEnter(getClass().getSimpleName(), "testCreateTypeForReceiveWithinFlow");
        
        RBPELProcess process = loadBPELModel("correlation/flowmodel/creatingReceiveWithinFlow.bpel");
        
        RActivity act = HelperFunc.getActivity(process, "creatingReceiveWithinFlowOperation");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        process = loadBPELModel("correlation/flowmodel/creatingReceiveWithinFlow2.bpel");
        act = HelperFunc.getActivity(process, "creatingReceiveWithinFlow2Operation");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        process = loadBPELModel("correlation/flowmodel/creatingReceiveWithinFlow3.bpel");
        act = HelperFunc.getActivity(process, "creatingReceiveWithinFlow3Operation");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        process = loadBPELModel("correlation/flowmodel/creatingReceiveWithinFlow4.bpel");
        act = HelperFunc.getActivity(process, "creatingReceiveWithinFlow4Operation");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
        
        Utility.logExit(getClass().getSimpleName(), "testCreateTypeForReceiveWithinFlow");
    }


    /**
     * Flow  
     *     - Rec - CreateInstane = "yes" And Correlation="join"
     *     - Rec - CreateInstane = "yes" And Correlation="join"
     *     
     */
    public void testCreateOrCorrelateTypeForReceive() {
        Utility.logEnter(getClass().getSimpleName(), "testCreateOrCorrelateTypeForReceive");
        
        RBPELProcess process = loadBPELModel("correlation/flowmodel/createOrCorrelateReceive.bpel");
        
        RActivity act = HelperFunc.getActivity(process, "createOrCorrelateReceiveOperation");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_OR_CORRELATE);

        act = HelperFunc.getActivity(process, "createOrCorrelateReceiveOperation1");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_OR_CORRELATE);
        
        act = HelperFunc.getActivity(process, "ThirdReceive");
        assertTrue(act instanceof RStartElement);
        assertTrue(((RStartElement) act).getStartType() == Engine.RECEIVE_TYPE_CREATE_OR_CORRELATE);
        
        Utility.logExit(getClass().getSimpleName(), "testCreateOrCorrelateTypeForReceive");
    }
    
}
