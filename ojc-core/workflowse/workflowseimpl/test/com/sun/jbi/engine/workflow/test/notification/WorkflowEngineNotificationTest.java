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
 * @(#)WorkflowEngineEscalationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.test.notification;

import java.security.Principal;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.security.auth.Subject;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import sun.security.acl.PrincipalImpl;

import com.sun.jbi.engine.workflow.BPELWorkflowRequest;
import com.sun.jbi.engine.workflow.ClientWorkflowRequest;
import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;
import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.test.DefaultTestInOutCallBack;
import com.sun.jbi.workflow.model.Task;


/**
 *
 * 
 */
public class WorkflowEngineNotificationTest extends TestCase {

    private PortType mPortType;
    
    
    private Map<String, Element> mMsgIdToResultElementMap = new HashMap<String, Element>();
    
    public WorkflowEngineNotificationTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        Util.initTaskManager();
        init();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(WorkflowEngineNotificationTest.class);

        return suite;
    } 
    
    public void testWorkflowCreateToCompleteTask() throws Exception {
        WorkflowEngine engine = new WorkflowEngine ();
        engine.init();
        engine.setContext(TaskManagerFactory.getInstance().getTaskManager().getContext());
        
        
        DefaultTestInOutCallBack callBack = new MyDefaultTestInOutCallBack();
        engine.setInOutCallBack(callBack);
        engine.setConsumerCallBack(callBack);
        
        engine.start();
        
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseValidNotification.wf";
        Element root = Util.loadElement(wfFileName);
        

        Task task = Util.loadTask(root);
        assertNotNull("task should not be null "+ task);

        EngineContext engineContext = TaskManagerFactory.getInstance().getTaskManager().getContext();
        if (engineContext != null) {
            WorkflowMapEntryTable workflowMapEntryTable = engineContext.getWorkflowMapEntryTable();
            if(workflowMapEntryTable != null) {
                QName serviceQName = new QName("http://jbi.com.sun/wfse/wsdl/workflow_po_3/ApprovePurchase", "notifyManagerPartner");
                QName interfaceQName = new QName("http://j2ee.netbeans.org/wsdl/EmailNotificationHandler", "NotificationHandlerPortType");
                //endpoint name not important use any
                String endPointName = "NotificationHandlerOperation";
//              serviceUnitName name not important use any
                String serviceUnitName = "MYServiceUnit";
                
                WorkflowMapEntry entry = new WorkflowMapEntry (serviceQName, interfaceQName, endPointName, task, WorkflowMapEntry.EntryType.ENTRY_CONSUME);            
                workflowMapEntryTable.addEntry(entry, serviceUnitName);
                
                Util.setModelInContext(task);
                }                
            }
        
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        org.w3c.dom.Node node = ((DOMSource) taskInput).getNode();
        Element taskEl = null;
        if (node instanceof Document) {
            taskEl = ((Document) node ).getDocumentElement();
        } else {
            taskEl = (Element) node;
        }
 
        
        BPELWorkflowRequest bpelReq = new BPELWorkflowRequest ("TestCreateTask",  "TestCreateTask", taskEl, task);
        engine.acceptRequest(bpelReq);
        Thread.sleep(1500);
        
        //Get task list
        Operation getTaskListOpt =  mPortType.getOperation("GetTaskList", null, null);
        
        Element getTaskListInput = Util.loadGetTaskListInputElement();
        Principal principal = new PrincipalImpl ("radval");
        Set<Principal> principals = new HashSet<Principal> ();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet (), new HashSet ());       
      
        ClientWorkflowRequest getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList1", getTaskListInput, getTaskListOpt, subject);
        engine.acceptRequest(getTaskListReq);
        Thread.sleep(1500);
        
//        //Claim Task
//        
//        //Get Task Input
//        Operation getTaskInputOpt =  mPortType.getOperation("GetOptInput", null, null);
//        
//        Element getTaskInputOptInput = Util.loadGetTaskInputElement(1);
//        ClientWorkflowRequest getTaskInputOptReq = new ClientWorkflowRequest ("TestGetOptInput", getTaskInputOptInput, getTaskInputOpt, subject);
//        engine.acceptRequest(getTaskInputOptReq);
//        Thread.sleep(500);        
//        
//        //Get Output
//
//        Operation getTaskOutputOpt =  mPortType.getOperation("GetOptOutput", null, null);
//        String fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptOutputInput.xml";
//        Element getTaskOutputOptInput = Util.loadInputMessageElement(fileName);
//        ClientWorkflowRequest getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetOptOutput", getTaskOutputOptInput, getTaskOutputOpt, subject);
//        engine.acceptRequest(getTaskOutputOptReq);
//        Thread.sleep(500); 
//        
//        
//        //Set Output
//        Operation setOutputOpt =  mPortType.getOperation("SetOptOutput", null, null);
//        fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/SetTaskOutput_Input.xml";
//        Element setOutputInput = Util.loadInputMessageElement(fileName);
//        ClientWorkflowRequest setOutputReq = new ClientWorkflowRequest ("TestSetOutput1", setOutputInput, setOutputOpt, subject);
//        engine.acceptRequest(setOutputReq);
//        Thread.sleep(500);
//        
//        
//        //Get Output
//        getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetOptOutput", getTaskOutputOptInput, getTaskOutputOpt, subject);
//        engine.acceptRequest(getTaskOutputOptReq);
//        Thread.sleep(500);         
//       
//        //set Output 2
//        fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/SetTaskOutput_Input2.xml";
//        setOutputInput = Util.loadInputMessageElement(fileName);
//        setOutputReq = new ClientWorkflowRequest ("TestSetOutput2", setOutputInput, setOutputOpt, subject);
//        engine.acceptRequest(setOutputReq);
//        Thread.sleep(500);        
//        
//        //Get Output 2
//        getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetOptOutput2", getTaskOutputOptInput, getTaskOutputOpt, subject);
//        engine.acceptRequest(getTaskOutputOptReq);
//        Thread.sleep(500);            
          
        //Complete task
        Operation completeTaskOpt =  mPortType.getOperation("CompleteTask", null, null);
        Element completeTaskInput = Util.loadCompleteTaskInputElement(1);
        
        ClientWorkflowRequest completeTask = new ClientWorkflowRequest ("TestCompleteTask", completeTaskInput, completeTaskOpt, subject);
        engine.acceptRequest(completeTask);        
        Thread.sleep(1500);
        
        //Get task list again
        getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList4", getTaskListInput, getTaskListOpt, subject);
        engine.acceptRequest(getTaskListReq);
        
        Thread.sleep(5000);
        
        assertFalse( "Did not expect fault but got one, see output for more details", callBack.isFaulted());
        
        assertFalse( "Expected notification, but did not get one, see output for more details", callBack.isNotified());
    }    
    
   private void init() throws Exception {
      if(mPortType != null) {
          return;
      }
      
        this.mPortType = Util.loadTaskCommonPortType();
        
        Element el1 = Util.loadElement("/com/sun/jbi/engine/workflow/test/escalation/GetTaskListOutput1.xml");
        mMsgIdToResultElementMap.put("TestGetTaskList1", el1);
        
        Element el2 = Util.loadElement("/com/sun/jbi/engine/workflow/test/escalation/GetTaskListOutput2.xml");
        mMsgIdToResultElementMap.put("TestGetTaskList2", el2);
        
        Element el3 = Util.loadElement("/com/sun/jbi/engine/workflow/test/escalation/GetTaskListOutput3.xml");
        mMsgIdToResultElementMap.put("TestGetTaskList3", el3);
        
        Element el4 = Util.loadElement("/com/sun/jbi/engine/workflow/test/escalation/GetTaskListOutput4.xml");
        mMsgIdToResultElementMap.put("TestGetTaskList4", el4);
        
   }
   
   class MyDefaultTestInOutCallBack extends DefaultTestInOutCallBack {
	   
	   public void  onReply(String meId, DOMSource reply) {
		   super.onReply(meId, reply);
		   Element expected = mMsgIdToResultElementMap.get(meId);
		   if(expected != null) {
			   try {
				   Util.compareElements(expected, (Element) reply.getNode());
			   } catch(Exception ex) {
				   fail(ex.getMessage());
			   }
		   }
	   }
   }
}
