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
 * @(#)CorrelationModelTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.modelparsing;


import java.util.Iterator;
import java.util.Set;
import java.util.logging.Logger;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELProcessOrScope;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RInvoke;
import com.sun.bpel.model.meta.ROnMessage;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.impl.REventHandlersOnEventImpl;
import com.sun.bpel.model.meta.impl.RPickImpl;
import com.sun.bpel.model.parser.impl.ParseCorrelationHelper.CorrelationDefn;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.test.common.HelperFunc;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 * tests while, scope, sequence. Simulates the execution of the bpel.
 */
public class CorrelationModelTest extends AbstractTestCase {
    private static Logger mLogger = Logger.getLogger(CorrelationModelTest.class.getName());

    public CorrelationModelTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CorrelationModelTest.class);

        return suite;
    }

    public void testInvokeModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInvokeModel");

        RBPELProcess process = loadBPELModel("correlation/Invoke1parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "Activity");

        assertNotNull(act);
        assertTrue(act instanceof RInvoke);
        
        RInvoke invoke = (RInvoke) act;
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = invoke.getRequestCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() == null || requestDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(requestDefns.getCreateCorrDefns() != null && !requestDefns.getCreateCorrDefns().isEmpty());
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper responseDefns  = invoke.getResponseCorrelationDefnWrapper();
            
            assertNotNull(responseDefns);
            assertTrue(responseDefns.getCorrelateCorrDefns() != null && !responseDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(responseDefns.getCreateCorrDefns() == null || responseDefns.getCreateCorrDefns().isEmpty());
            assertTrue(responseDefns.getJoinCorrDefns() == null || responseDefns.getJoinCorrDefns().isEmpty());
        }
        
        Utility.logExit(getClass().getSimpleName(), "testInvokeModel");
    }
    
    public void testInvokeModel_Invoke2parent() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInvokeModel_Invoke2parent");

        RBPELProcess process = loadBPELModel("correlation/invoke2/Invoke2parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "Activity");

        assertNotNull(act);
        assertTrue(act instanceof RInvoke);
        
        RInvoke invoke = (RInvoke) act;
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = invoke.getRequestCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() == null || requestDefns.getCorrelateCorrDefns().isEmpty());
            //assertTrue(requestDefns.getCreateCorrDefns() != null && (requestDefns.getCreateCorrDefns().size() == 1));
            assertTrue(requestDefns.getCreateCorrDefns() == null || requestDefns.getCreateCorrDefns().isEmpty());
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper responseDefns  = invoke.getResponseCorrelationDefnWrapper();
            
            assertNotNull(responseDefns);
            assertTrue(responseDefns.getCorrelateCorrDefns() != null && responseDefns.getCorrelateCorrDefns().size() == 1);
            assertTrue(responseDefns.getCreateCorrDefns() == null || responseDefns.getCreateCorrDefns().isEmpty());
            assertTrue(responseDefns.getJoinCorrDefns() == null || responseDefns.getJoinCorrDefns().isEmpty());
        }
        
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForRequest = invoke.getReqRespCorrelationDefnWrapperForRequest();
            assertNotNull(reqRespCorrDefnsForRequest);
            assertTrue(reqRespCorrDefnsForRequest.getCorrelateCorrDefns() == null || reqRespCorrDefnsForRequest.getCorrelateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForRequest.getCreateCorrDefns() != null && reqRespCorrDefnsForRequest.getCreateCorrDefns().size() == 1);
            assertTrue(reqRespCorrDefnsForRequest.getJoinCorrDefns() == null || reqRespCorrDefnsForRequest.getJoinCorrDefns().isEmpty());
        	
        }
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForResponse = invoke.getReqRespCorrelationDefnWrapperForResponse();
            assertNotNull(reqRespCorrDefnsForResponse);
            assertTrue(reqRespCorrDefnsForResponse.getCorrelateCorrDefns() == null || reqRespCorrDefnsForResponse.getCorrelateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForResponse.getCreateCorrDefns() != null && reqRespCorrDefnsForResponse.getCreateCorrDefns().size() == 1);
            assertTrue(reqRespCorrDefnsForResponse.getJoinCorrDefns() == null || reqRespCorrDefnsForResponse.getJoinCorrDefns().isEmpty());
        	
        }
        
        Utility.logExit(getClass().getSimpleName(), "testInvokeModel_Invoke2parent");
    }
    
    public void testInvokeModel_invoke_response_initiated_parent() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInvokeModel_invoke_response_initiated_parent");

        RBPELProcess process = loadBPELModel("correlation/invoke2/invoke_response_initiated/invoke_response_initiated_parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "Activity");

        assertNotNull(act);
        assertTrue(act instanceof RInvoke);
        
        RInvoke invoke = (RInvoke) act;
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = invoke.getRequestCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() == null || requestDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(requestDefns.getCreateCorrDefns() == null || requestDefns.getCreateCorrDefns().isEmpty());
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper responseDefns  = invoke.getResponseCorrelationDefnWrapper();
            
            assertNotNull(responseDefns);
            assertTrue(responseDefns.getCorrelateCorrDefns() == null || responseDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(responseDefns.getCreateCorrDefns() != null && responseDefns.getCreateCorrDefns().size() == 1);
            assertTrue(responseDefns.getJoinCorrDefns() == null || responseDefns.getJoinCorrDefns().isEmpty());
        }
        
        Utility.logExit(getClass().getSimpleName(), "testInvokeModel_invoke_response_initiated_parent");
    }
    
    public void testInvokeModel_invoke_response_join_parent() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInvokeModel_invoke_response_join_parent");

        RBPELProcess process = loadBPELModel("correlation/invoke2/invoke_response_join/invoke_response_join_parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "Activity");

        assertNotNull(act);
        assertTrue(act instanceof RInvoke);
        
        RInvoke invoke = (RInvoke) act;
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = invoke.getRequestCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() == null || requestDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(requestDefns.getCreateCorrDefns() == null || requestDefns.getCreateCorrDefns().isEmpty());
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper responseDefns  = invoke.getResponseCorrelationDefnWrapper();
            
            assertNotNull(responseDefns);
            assertTrue(responseDefns.getCorrelateCorrDefns() == null || responseDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(responseDefns.getCreateCorrDefns() == null || responseDefns.getCreateCorrDefns().isEmpty());
            assertTrue(responseDefns.getJoinCorrDefns() != null && responseDefns.getJoinCorrDefns().size() == 1);
        }
        
        Utility.logExit(getClass().getSimpleName(), "testInvokeModel_invoke_response_join_parent");
    }
    
    public void testInvokeModel_initiatejoin_parent() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInvokeModel_initiatejoin_parent");

        RBPELProcess process = loadBPELModel("correlation/invoke2/request-response-initiatejoin/request-response-initiatejoin_parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "Activity");

        assertNotNull(act);
        assertTrue(act instanceof RInvoke);
        
        RInvoke invoke = (RInvoke) act;
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = invoke.getRequestCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() == null || requestDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(requestDefns.getCreateCorrDefns() == null || requestDefns.getCreateCorrDefns().isEmpty());
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper responseDefns  = invoke.getResponseCorrelationDefnWrapper();
            
            assertNotNull(responseDefns);
            // IMPORTANT. Although it is initiate=join since it is request-response, the correlation 
            // on response is considered as though it was defined as initiate=no.
            assertTrue(responseDefns.getCorrelateCorrDefns() == null || responseDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(responseDefns.getCreateCorrDefns() == null || responseDefns.getCreateCorrDefns().isEmpty());
            assertTrue(responseDefns.getJoinCorrDefns() == null || responseDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForRequest = invoke.getReqRespCorrelationDefnWrapperForRequest();
            assertNotNull(reqRespCorrDefnsForRequest);
            assertTrue(reqRespCorrDefnsForRequest.getCorrelateCorrDefns() == null || reqRespCorrDefnsForRequest.getCorrelateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForRequest.getCreateCorrDefns() == null || reqRespCorrDefnsForRequest.getCreateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForRequest.getJoinCorrDefns() != null && reqRespCorrDefnsForRequest.getJoinCorrDefns().size() == 1);
        	
        }
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForResponse = invoke.getReqRespCorrelationDefnWrapperForResponse();
            assertNotNull(reqRespCorrDefnsForResponse);
            assertTrue(reqRespCorrDefnsForResponse.getCorrelateCorrDefns() == null || reqRespCorrDefnsForResponse.getCorrelateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForResponse.getCreateCorrDefns() == null || reqRespCorrDefnsForResponse.getCreateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForResponse.getJoinCorrDefns() != null && reqRespCorrDefnsForResponse.getJoinCorrDefns().size() == 1);
        	
        }
        
        
        Utility.logExit(getClass().getSimpleName(), "testInvokeModel_initiatejoin_parent");
    }
    
    public void testInvokeModel_initiateno_parent() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInvokeModel_initiateno_parent");

        RBPELProcess process = loadBPELModel("correlation/invoke2/request-response-initiateno/request-response-initiateno-parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "Activity");

        assertNotNull(act);
        assertTrue(act instanceof RInvoke);
        
        RInvoke invoke = (RInvoke) act;
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = invoke.getRequestCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() == null || requestDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(requestDefns.getCreateCorrDefns() != null && requestDefns.getCreateCorrDefns().size() == 1);
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper responseDefns  = invoke.getResponseCorrelationDefnWrapper();
            
            assertNotNull(responseDefns);
            assertTrue(responseDefns.getCorrelateCorrDefns() == null || responseDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(responseDefns.getCreateCorrDefns() == null || responseDefns.getCreateCorrDefns().isEmpty());
            assertTrue(responseDefns.getJoinCorrDefns() == null || responseDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForRequest = invoke.getReqRespCorrelationDefnWrapperForRequest();
            assertNotNull(reqRespCorrDefnsForRequest);
            assertTrue(reqRespCorrDefnsForRequest.getCorrelateCorrDefns() != null && reqRespCorrDefnsForRequest.getCorrelateCorrDefns().size() == 1);
            assertTrue(reqRespCorrDefnsForRequest.getCreateCorrDefns() == null || reqRespCorrDefnsForRequest.getCreateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForRequest.getJoinCorrDefns() == null || reqRespCorrDefnsForRequest.getJoinCorrDefns().isEmpty());
        	
        }
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForResponse = invoke.getReqRespCorrelationDefnWrapperForResponse();
            assertNotNull(reqRespCorrDefnsForResponse);
            assertTrue(reqRespCorrDefnsForResponse.getCorrelateCorrDefns() != null && reqRespCorrDefnsForResponse.getCorrelateCorrDefns().size() == 1);
            assertTrue(reqRespCorrDefnsForResponse.getCreateCorrDefns() == null || reqRespCorrDefnsForResponse.getCreateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForResponse.getJoinCorrDefns() == null || reqRespCorrDefnsForResponse.getJoinCorrDefns().isEmpty());
        	
        }
        
        
        Utility.logExit(getClass().getSimpleName(), "testInvokeModel_initiateno_parent");
    }
    
    public void testInvokeModel_initiateyes_parent() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInvokeModel_initiateyes_parent");

        RBPELProcess process = loadBPELModel("correlation/invoke2/request-response-initiateyes/request-response-initiateyes_parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "Activity");

        assertNotNull(act);
        assertTrue(act instanceof RInvoke);
        
        RInvoke invoke = (RInvoke) act;
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = invoke.getRequestCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() == null || requestDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(requestDefns.getCreateCorrDefns() == null || requestDefns.getCreateCorrDefns().isEmpty());
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper responseDefns  = invoke.getResponseCorrelationDefnWrapper();
            
            assertNotNull(responseDefns);
            // IMPORTANT. Although it is initiate=yes since it is request-response, the correlation 
            // on response is considered as though it was defined as initiate=no.
            assertTrue(responseDefns.getCorrelateCorrDefns() == null || responseDefns.getCorrelateCorrDefns().isEmpty());
            assertTrue(responseDefns.getCreateCorrDefns() == null || responseDefns.getCreateCorrDefns().isEmpty());
            assertTrue(responseDefns.getJoinCorrDefns() == null || responseDefns.getJoinCorrDefns().isEmpty());
        }
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForRequest = invoke.getReqRespCorrelationDefnWrapperForRequest();
            assertNotNull(reqRespCorrDefnsForRequest);
            assertTrue(reqRespCorrDefnsForRequest.getCorrelateCorrDefns() == null || reqRespCorrDefnsForRequest.getCorrelateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForRequest.getCreateCorrDefns() != null && reqRespCorrDefnsForRequest.getCreateCorrDefns().size() == 1);
            assertTrue(reqRespCorrDefnsForRequest.getJoinCorrDefns() == null || reqRespCorrDefnsForRequest.getJoinCorrDefns().isEmpty());
        	
        }
        if (true) {
        	CorrelationDefnWrapper reqRespCorrDefnsForResponse = invoke.getReqRespCorrelationDefnWrapperForResponse();
            assertNotNull(reqRespCorrDefnsForResponse);
            assertTrue(reqRespCorrDefnsForResponse.getCorrelateCorrDefns() == null || reqRespCorrDefnsForResponse.getCorrelateCorrDefns().isEmpty());
            assertTrue(reqRespCorrDefnsForResponse.getCreateCorrDefns() != null && reqRespCorrDefnsForResponse.getCreateCorrDefns().size() == 1);
            assertTrue(reqRespCorrDefnsForResponse.getJoinCorrDefns() == null || reqRespCorrDefnsForResponse.getJoinCorrDefns().isEmpty());
        	
        }
        
        
        Utility.logExit(getClass().getSimpleName(), "testInvokeModel_initiateyes_parent");
    }

    public void testReceiveModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testReceiveModel");

        RBPELProcess process = loadBPELModel("correlation/Invoke1parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "MyRole.Invoke1parentOperation.Receive");

        assertNotNull(act);
        assertTrue(act instanceof RStartElement);
        
        if (true) {
            RStartElement se = (RStartElement) act;
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper defns = se.getCorrelationDefnWrapper();
            assertNotNull(defns);
            assertTrue(defns.getCorrelateCorrDefns() == null || defns.getCorrelateCorrDefns().isEmpty());
            assertTrue(defns.getCreateCorrDefns() != null && !defns.getCreateCorrDefns().isEmpty());
            assertTrue(defns.getJoinCorrDefns() == null || defns.getJoinCorrDefns().isEmpty());
          }
        
        act = HelperFunc.getActivity(process, "Invoke1parentOperation1");

        assertNotNull(act);
        assertTrue(act instanceof RStartElement);
        
        if (true) {
            RStartElement se = (RStartElement) act;
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper defns = se.getCorrelationDefnWrapper();
            assertNotNull(defns);
            assertTrue(defns.getCorrelateCorrDefns() != null && !defns.getCorrelateCorrDefns().isEmpty());
            assertTrue(defns.getCreateCorrDefns() == null || defns.getCreateCorrDefns().isEmpty());
            assertTrue(defns.getJoinCorrDefns() == null || defns.getJoinCorrDefns().isEmpty());
          }
        Utility.logExit(getClass().getSimpleName(), "testReceiveModel");
        
    }
    
    public void testReplyModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testReplyModel");

        RBPELProcess process = loadBPELModel("correlation/Invoke1parent.bpel");
        RActivity act = HelperFunc.getActivity(process, "MyRole.Invoke1parentOperation.Reply");

        assertNotNull(act);
        assertTrue(act instanceof RReply);
        
        if (true) {
            RReply se = (RReply) act;
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper defns = se.getCorrelationDefnWrapper();
            assertNull(defns);
          }
        

        Utility.logExit(getClass().getSimpleName(), "testReplyModel");        
    }
    
    public void testOnMesgModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testOnMesgModel");

        RBPELProcess process = loadBPELModel("correlation/onmessage/correlationOnMesg.bpel");
        RActivity act = HelperFunc.getActivity(process, "Event_Based_Decision");

        assertNotNull(act);
        assertTrue(act instanceof RPickImpl);
        assertTrue(act instanceof Pick);
        
        if (true) {
            // just to avoid variable clashes by mistake.
            Pick pick = (Pick) act;
            assertTrue(pick.getOnMessageSize() == 2);
            assertNotNull(pick.getOnMessage(0));
            ROnMessage onMsg = (ROnMessage) pick.getOnMessage(0);
            assertNotNull(onMsg);
            assertTrue(onMsg.getRCreateInstance());

            assertTrue(onMsg.getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
            CorrelationDefnWrapper defns = onMsg.getCorrelationDefnWrapper();
            assertNotNull(defns);
            assertTrue(defns.getCorrelateCorrDefns() == null || defns.getCorrelateCorrDefns().isEmpty());
            assertTrue(defns.getCreateCorrDefns() != null && !defns.getCreateCorrDefns().isEmpty());
            assertTrue(defns.getJoinCorrDefns() == null || defns.getJoinCorrDefns().isEmpty());
        }

        if (true) {
            // just to avoid variable clashes by mistake.
            Pick pick = (Pick) act;
            assertNotNull(pick.getOnMessage(1));
            ROnMessage onMsg = (ROnMessage) pick.getOnMessage(1);
            assertNotNull(onMsg);
            assertTrue(onMsg.getRCreateInstance());
            assertTrue(onMsg.getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
            
            CorrelationDefnWrapper defns = onMsg.getCorrelationDefnWrapper();
            assertNotNull(defns);
            assertTrue(defns.getCorrelateCorrDefns() == null || defns.getCorrelateCorrDefns().isEmpty());
            assertTrue(defns.getCreateCorrDefns() != null && !defns.getCreateCorrDefns().isEmpty());
            assertTrue(defns.getJoinCorrDefns() == null || defns.getJoinCorrDefns().isEmpty());
        }
        
        act = HelperFunc.getActivity(process, "Event_Based_Decision1");

        assertNotNull(act);
        assertTrue(act instanceof RPickImpl);
        assertTrue(act instanceof Pick);

        if (true) {
            // just to avoid variable clashes by mistake.
            Pick pick = (Pick) act;
            assertTrue(pick.getOnMessageSize() == 1);
            assertNotNull(pick.getOnMessage(0));
            ROnMessage onMsg = (ROnMessage) pick.getOnMessage(0);
            assertNotNull(onMsg);
            assertTrue(!onMsg.getRCreateInstance());

            assertTrue(onMsg.getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
            CorrelationDefnWrapper defns = onMsg.getCorrelationDefnWrapper();
            assertNotNull(defns);
            assertTrue(defns.getCorrelateCorrDefns() != null && !defns.getCorrelateCorrDefns().isEmpty());
            assertTrue(defns.getCreateCorrDefns() == null || defns.getCreateCorrDefns().isEmpty());
            assertTrue(defns.getJoinCorrDefns() == null || defns.getJoinCorrDefns().isEmpty());
        }
        Utility.logExit(getClass().getSimpleName(), "testOnMesgModel");        
    }

    public void testFlowCreateOrCorrelateModel() {
        Utility.logEnter(getClass().getSimpleName(), "testFlowCreateOrCorrelateModel");
    
        RBPELProcess process = loadBPELModel("correlation/flowmodel/w7/R4.bpel");
        Set startElems = process.getStartElements(true);
        assertTrue(startElems.size() == 2);
        
        RStartElement rSElem = (RStartElement) startElems.iterator().next();
        assertTrue(rSElem.getCorrelationDefnWrapper().getCommonJoinCorrDefs() != null &&
                rSElem.getCorrelationDefnWrapper().getCommonJoinCorrDefs().size() == 1);
               
        CorrelationDefn defn = (CorrelationDefn) rSElem.getCorrelationDefnWrapper().getCommonJoinCorrDefs().get(0);
        
        Iterator itr = startElems.iterator();
        itr.next();
        RStartElement rSElem2 = (RStartElement) itr.next();
        assertTrue(rSElem2.getCorrelationDefnWrapper().getCommonJoinCorrDefs() != null&&
                rSElem.getCorrelationDefnWrapper().getCommonJoinCorrDefs().size() == 1);
        CorrelationDefn defn2 = (CorrelationDefn) rSElem.getCorrelationDefnWrapper().getCommonJoinCorrDefs().get(0);
        
        assertTrue(defn.getCorrelationSetID() == defn2.getCorrelationSetID());
        
        RActivity rootAct = (RActivity) process.getActivity(); //sequenceActivity
        RActivityHolder root = (RActivityHolder) rootAct; // sequenceActivity
        RActivity flow1Act = root.getChildActivity(); // FlowActivity
        RActivity flow1SeqAct = ((RActivityHolder) flow1Act).getChildActivity();
        RActivity rec1 = ((RActivityHolder) flow1SeqAct).getChildActivity();
        
        assertTrue(((Activity) rec1).getName().equals("R4Operation"));
        RStartElement _2_CorrStartElement = (RStartElement) rec1;
        assertTrue(_2_CorrStartElement.getCorrelationDefnWrapper().getJoinCorrDefns() != null &&
                _2_CorrStartElement.getCorrelationDefnWrapper().getJoinCorrDefns().size() == 1);
        
        RActivity flow1Seq2Act = flow1SeqAct.getNextActivity();
        RActivity rec2 = ((RActivityHolder) flow1Seq2Act).getChildActivity();
        
        assertTrue(((Activity) rec2).getName().equals("R4Operation1"));
        RStartElement _1_CorrStartElement = (RStartElement) rec2;
        assertTrue(_1_CorrStartElement.getCorrelationDefnWrapper().getJoinCorrDefns() == null ||
                _1_CorrStartElement.getCorrelationDefnWrapper().getJoinCorrDefns().size() == 0);
               
        Utility.logExit(getClass().getSimpleName(), "testFlowCreateOrCorrelateModel");
    }

    public void testFlowCreateOrCorrelateModel2() {
        Utility.logEnter(getClass().getSimpleName(), "testFlowCreateOrCorrelateModel2");
    
        RBPELProcess process = loadBPELModel("correlation/flowmodel/correlationWithFlow3.bpel");
        Set startElemsWhichCreateInstance = process.getStartElements(true);
        assertTrue(startElemsWhichCreateInstance.size() == 1);
        
        Set allStartElems = process.getStartElements();
        assertTrue(allStartElems.size() == 2);
        
        Set startElemsWhichCorrelate = process.getStartElements(false);
        assertTrue(startElemsWhichCorrelate.size() == 1);
        
        if (true) {
            RStartElement rSElem = (RStartElement) startElemsWhichCreateInstance.iterator().next();
            assertTrue(rSElem.getStartType() == Engine.RECEIVE_TYPE_CREATE_ONLY);
            assertTrue(rSElem.getCorrelationDefnWrapper().getCommonJoinCorrDefs() == null ||
                    rSElem.getCorrelationDefnWrapper().getCommonJoinCorrDefs().size() == 0);
            assertTrue(rSElem.getCorrelationDefnWrapper().getCreateCorrDefns() != null ||
                    rSElem.getCorrelationDefnWrapper().getCreateCorrDefns().size() == 1);
        }
        
        if (true) {
            RStartElement rSElem2 = (RStartElement) startElemsWhichCorrelate.iterator().next();
            assertTrue(rSElem2.getStartType() == Engine.RECEIVE_TYPE_CORRELATE_ONLY);
            assertTrue(rSElem2.getCorrelationDefnWrapper().getCommonJoinCorrDefs() == null ||
                    rSElem2.getCorrelationDefnWrapper().getCommonJoinCorrDefs().size() == 0);
            assertTrue(rSElem2.getCorrelationDefnWrapper().getCreateCorrDefns() == null ||
                    rSElem2.getCorrelationDefnWrapper().getCreateCorrDefns().size() == 0);
            assertTrue(rSElem2.getCorrelationDefnWrapper().getCorrelateCorrDefns() == null ||
                    rSElem2.getCorrelationDefnWrapper().getCorrelateCorrDefns().size() == 0);
            assertTrue(rSElem2.getCorrelationDefnWrapper().getJoinCorrDefns() != null ||
                    rSElem2.getCorrelationDefnWrapper().getJoinCorrDefns().size() == 1);
        }
                      
        Utility.logExit(getClass().getSimpleName(), "testFlowCreateOrCorrelateModel2");
    }

    public void testonEventModel() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testonEventModel");
    
        RBPELProcess process = loadBPELModel("correlation/onevent/onEventOnProcess1.bpel");
        assertNotNull(process);
        assertTrue(process instanceof BPELProcessOrScope);
        EventHandlers handlers = ((BPELProcessOrScope) process).getEventHandlers();           
        EventHandlersOnEvent[] onEvents = handlers.getOnEvents();
        REventHandlersOnEventImpl event = (REventHandlersOnEventImpl) onEvents[0];
        
        assertTrue(event instanceof RStartElement);
        RStartElement onEvent = (RStartElement) event;
        
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = onEvent.getCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() != null && requestDefns.getCorrelateCorrDefns().size() == 1);
            assertTrue(requestDefns.getCreateCorrDefns() == null || requestDefns.getCreateCorrDefns().isEmpty());
            assertTrue(requestDefns.getJoinCorrDefns() == null || requestDefns.getJoinCorrDefns().isEmpty());
        }
        Utility.logExit(getClass().getSimpleName(), "testonEventModel");
    }

    public void testonEventModel_init_no_yes_join() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testonEventModel_init_no_yes_join");
    
        RBPELProcess process = loadBPELModel("correlation/onevent/onEventOnProcess1_init_no_yes_join.bpel");
        assertNotNull(process);
        assertTrue(process instanceof BPELProcessOrScope);
        EventHandlers handlers = ((BPELProcessOrScope) process).getEventHandlers();           
        EventHandlersOnEvent[] onEvents = handlers.getOnEvents();
        REventHandlersOnEventImpl event = (REventHandlersOnEventImpl) onEvents[0];
        
        assertTrue(event instanceof RStartElement);       
        RStartElement onEvent = (RStartElement) event;
        
        if (true) {
            // just to avoid variable clashes by mistake.
            CorrelationDefnWrapper requestDefns  = onEvent.getCorrelationDefnWrapper();
            assertNotNull(requestDefns);
            assertTrue(requestDefns.getCorrelateCorrDefns() != null && requestDefns.getCorrelateCorrDefns().size() == 1);
            assertTrue(requestDefns.getCreateCorrDefns() != null && requestDefns.getCreateCorrDefns().size() == 1);
            assertTrue(requestDefns.getJoinCorrDefns() != null && requestDefns.getJoinCorrDefns().size() == 1);
        }
        Utility.logExit(getClass().getSimpleName(), "testonEventModel_init_no_yes_join");
    }
}
