/*
 * WorkflowEngineTest.java
 * JUnit based test
 *
 * Created on April 14, 2005, 11:49 PM
 */

package com.sun.jbi.engine.workflow.test;

import java.security.Principal;
import java.security.acl.Group;
import java.util.HashSet;
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
import com.sun.jbi.engine.workflow.clientapi.GroupImpl;
import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory;
import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.workflow.model.Task;

/**
 *
 * 
 */
public class WorkflowEngineTest extends TestCase {

    private PortType mPortType;
    
    
    public WorkflowEngineTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        Util.initTaskManager();
        init();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(WorkflowEngineTest.class);

        return suite;
    } 
    
    public void testWorkflowCreateToCompleteTask() throws Exception {
        WorkflowEngine engine = new WorkflowEngine ();
        engine.init();
        DefaultTestInOutCallBack callBack = new DefaultTestInOutCallBack();
        engine.setInOutCallBack(callBack);
        
        engine.start();
        
        //Create task
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        org.w3c.dom.Node node = ((DOMSource) taskInput).getNode();
        Element taskEl = null;
        if (node instanceof Document) {
            taskEl = ((Document) node ).getDocumentElement();
        } else {
            taskEl = (Element) node;
        }
 
        
        
        
        
        BPELWorkflowRequest bpelReq = new BPELWorkflowRequest ("TestCreateTask",  "TestCreateTask", taskEl,  task);
        engine.acceptRequest(bpelReq);
        Thread.sleep(2500);
        
        //Get task list
        Operation getTaskListOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskList, null, null);
        
        Element getTaskListInput = Util.loadGetTaskListInputElement();
        Principal principal = new PrincipalImpl ("radval");
        Set<Principal> principals = new HashSet<Principal> ();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet (), new HashSet ());       
      
        ClientWorkflowRequest getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList1", getTaskListInput, getTaskListOpt, subject);
        engine.acceptRequest(getTaskListReq);
        Thread.sleep(2500);
        
        //Claim Task
        
        //Get TaskInputXform
        Operation getTaskInputXformOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskXform, null, null);
        
        Element getTaskInputXformOptInput = Util.loadGetTaskXformElement(1);
        ClientWorkflowRequest getTaskInputXformOptReq = new ClientWorkflowRequest ("TestGetTaskXForm", getTaskInputXformOptInput, getTaskInputXformOpt, subject);
        engine.acceptRequest(getTaskInputXformOptReq);
        Thread.sleep(2500);    
        
        //Get Task Input
        Operation getTaskInputOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskInput, null, null);
        
        Element getTaskInputOptInput = Util.loadGetTaskInputElement(1);
        ClientWorkflowRequest getTaskInputOptReq = new ClientWorkflowRequest ("TestGetTaskInput", getTaskInputOptInput, getTaskInputOpt, subject);
        engine.acceptRequest(getTaskInputOptReq);
        Thread.sleep(2500);        
        
        //Get Output

        Operation getTaskOutputOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskOutput, null, null);
        String fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptOutputInput.xml";
        Element getTaskOutputOptInput = Util.loadInputMessageElement(fileName);
        ClientWorkflowRequest getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetTaskOutput1", getTaskOutputOptInput, getTaskOutputOpt, subject);
        engine.acceptRequest(getTaskOutputOptReq);
        Thread.sleep(2500); 
        
        
        //Set Output
        Operation setOutputOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_setTaskOutput, null, null);
        fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/SetTaskOutput_Input.xml";
        Element setOutputInput = Util.loadInputMessageElement(fileName);
        ClientWorkflowRequest setOutputReq = new ClientWorkflowRequest ("TestSetTaskOutput1", setOutputInput, setOutputOpt, subject);
        engine.acceptRequest(setOutputReq);
        Thread.sleep(2500);
        
        
        //Get Output
        getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetTaskOutput2", getTaskOutputOptInput, getTaskOutputOpt, subject);
        engine.acceptRequest(getTaskOutputOptReq);
        Thread.sleep(2500);         
       
        //set Output 2
        fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/SetTaskOutput_Input2.xml";
        setOutputInput = Util.loadInputMessageElement(fileName);
        setOutputReq = new ClientWorkflowRequest ("TestSetTaskOutput2", setOutputInput, setOutputOpt, subject);
        engine.acceptRequest(setOutputReq);
        Thread.sleep(2500);        
        
        //Get Output 2
        getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetTaskOutput2", getTaskOutputOptInput, getTaskOutputOpt, subject);
        engine.acceptRequest(getTaskOutputOptReq);
        Thread.sleep(2500);            
        
        //Complete task
        Operation completeTaskOpt =  mPortType.getOperation("CompleteTask", null, null);
        Element completeTaskInput = Util.loadCompleteTaskInputElement(1);
        
        ClientWorkflowRequest completeTask = new ClientWorkflowRequest ("TestCompleteTask", completeTaskInput, completeTaskOpt, subject);
        engine.acceptRequest(completeTask);        
        Thread.sleep(2500);
        
        //Get task list again
        getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList2", getTaskListInput, getTaskListOpt, subject);
        engine.acceptRequest(getTaskListReq);
        
        Thread.sleep(5000);
        
        assertFalse( "Did not expect fault but got one, see output for more details", callBack.isFaulted());
        
        
    }    
    
    public void testComplexTypeCreateToComplete() throws Exception {
        WorkflowEngine engine = new WorkflowEngine ();
        engine.init();
        DefaultTestInOutCallBack callBack = new DefaultTestInOutCallBack();
        engine.setInOutCallBack(callBack);
        
        engine.start();
        
        //Create task
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ManualApprove.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        EngineContext engineContext = TaskManagerFactory.getInstance().getTaskManager().getContext();
        if (engineContext != null) {
            WorkflowMapEntryTable workflowMapEntryTable = engineContext.getWorkflowMapEntryTable();
            QName qName = task.getQName();
            WorkflowMapEntry entry = new WorkflowMapEntry (null, null, null, task, WorkflowMapEntry.EntryType.ENTRY_PROVIDE);            
//            Map<QName, Tasks> tasksMap = workflowMapEntryTable.getTaskModelMap();
            workflowMapEntryTable.addEntry(entry, "TEST");
        }
         
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/ManualApproveInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        org.w3c.dom.Node node = ((DOMSource) taskInput).getNode();
        Element taskEl = null;
        if (node instanceof Document) {
            taskEl = ((Document) node ).getDocumentElement();
        } else {
            taskEl = (Element) node;
        }
       
        BPELWorkflowRequest bpelReq = new BPELWorkflowRequest ("TestCreateTask",  "TestCreateTask", taskEl,  task);
        engine.acceptRequest(bpelReq);
        Thread.sleep(2500);
        
        //Get task list
        Operation getTaskListOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskList, null, null);
        
        Element getTaskListInput = Util.loadGetTaskListInputElement();
        Principal principal = new PrincipalImpl ("support1");
        Set<Principal> principals = new HashSet<Principal> ();
        principals.add(principal);
        Group group = new GroupImpl("CustomerServiceRep");
        principals.add(group);
        Subject subject = new Subject(true, principals, new HashSet (), new HashSet ());       
      
        ClientWorkflowRequest getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList1", getTaskListInput, getTaskListOpt, subject);
        engine.acceptRequest(getTaskListReq);
        Thread.sleep(2500);
        
//        //Claim Task
//        
//        //Get TaskInputXform
//        Operation getTaskInputXformOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskXform, null, null);
//        
//        Element getTaskInputXformOptInput = Util.loadGetTaskXformElement(1);
//        ClientWorkflowRequest getTaskInputXformOptReq = new ClientWorkflowRequest ("TestGetTaskXForm", getTaskInputXformOptInput, getTaskInputXformOpt, subject);
//        engine.acceptRequest(getTaskInputXformOptReq);
//        Thread.sleep(2500);    
//        
//        //Get Task Input
//        Operation getTaskInputOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskInput, null, null);
//        
//        Element getTaskInputOptInput = Util.loadGetTaskInputElement(1);
//        ClientWorkflowRequest getTaskInputOptReq = new ClientWorkflowRequest ("TestGetTaskInput", getTaskInputOptInput, getTaskInputOpt, subject);
//        engine.acceptRequest(getTaskInputOptReq);
//        Thread.sleep(2500);        
//        
//        //Get Output
//
//        Operation getTaskOutputOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskOutput, null, null);
//        String fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/GetOptOutputInput.xml";
//        Element getTaskOutputOptInput = Util.loadInputMessageElement(fileName);
//        ClientWorkflowRequest getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetTaskOutput1", getTaskOutputOptInput, getTaskOutputOpt, subject);
//        engine.acceptRequest(getTaskOutputOptReq);
//        Thread.sleep(2500); 
//        
//        
//        //Set Output
//        Operation setOutputOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_setTaskOutput, null, null);
//        fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/SetTaskOutput_Input.xml";
//        Element setOutputInput = Util.loadInputMessageElement(fileName);
//        ClientWorkflowRequest setOutputReq = new ClientWorkflowRequest ("TestSetTaskOutput1", setOutputInput, setOutputOpt, subject);
//        engine.acceptRequest(setOutputReq);
//        Thread.sleep(2500);
//        
//        
//        //Get Output
//        getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetTaskOutput2", getTaskOutputOptInput, getTaskOutputOpt, subject);
//        engine.acceptRequest(getTaskOutputOptReq);
//        Thread.sleep(2500);         
//       
//        //set Output 2
//        fileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/SetTaskOutput_Input2.xml";
//        setOutputInput = Util.loadInputMessageElement(fileName);
//        setOutputReq = new ClientWorkflowRequest ("TestSetTaskOutput2", setOutputInput, setOutputOpt, subject);
//        engine.acceptRequest(setOutputReq);
//        Thread.sleep(2500);        
//        
//        //Get Output 2
//        getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetTaskOutput2", getTaskOutputOptInput, getTaskOutputOpt, subject);
//        engine.acceptRequest(getTaskOutputOptReq);
//        Thread.sleep(2500);            
//        
//        //Complete task
//        Operation completeTaskOpt =  mPortType.getOperation("CompleteTask", null, null);
//        Element completeTaskInput = Util.loadCompleteTaskInputElement(1);
//        
//        ClientWorkflowRequest completeTask = new ClientWorkflowRequest ("TestCompleteTask", completeTaskInput, completeTaskOpt, subject);
//        engine.acceptRequest(completeTask);        
//        Thread.sleep(2500);
//        
//        //Get task list again
//        getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList2", getTaskListInput, getTaskListOpt, subject);
//        engine.acceptRequest(getTaskListReq);
//        
//        Thread.sleep(5000);
//        
//        assertFalse( "Did not expect fault but got one, see output for more details", callBack.isFaulted());
        
        
    }        
    
   private void init() throws Exception {
      if(mPortType != null) {
          return;
      }
      
        this.mPortType = Util.loadTaskCommonPortType();
   }
   
   
}
