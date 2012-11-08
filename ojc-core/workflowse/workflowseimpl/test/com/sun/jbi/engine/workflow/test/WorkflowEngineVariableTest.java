/*
 * WorkflowEngineTest.java
 * JUnit based test
 *
 * Created on April 14, 2005, 11:49 PM
 */

package com.sun.jbi.engine.workflow.test;

import com.sun.jbi.engine.workflow.clientapi.Util;
import java.net.URI;
import java.net.URL;
import java.security.Principal;
import java.util.HashSet;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;

import junit.framework.*;

import com.sun.corba.se.impl.orbutil.graph.Node;
import com.sun.jbi.engine.workflow.BPELWorkflowRequest;
import com.sun.jbi.engine.workflow.ClientWorkflowRequest;
import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.EnginePropertyConstants;
import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;
import com.sun.jbi.engine.workflow.WorkflowModelManager;
import com.sun.jbi.engine.workflow.clientapi.StaticOperation;
import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory;
import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactoryTest;
import com.sun.jbi.engine.workflow.clientapi.TestContext;
import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.process.InOutCallBack;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactoryTest;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.Task;

import javax.naming.InitialContext;
import javax.security.auth.Subject;
import javax.sql.DataSource;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import sun.security.acl.PrincipalImpl;

/**
 *
 * 
 */
public class WorkflowEngineVariableTest extends TestCase {

    private PortType mPortType;
    private static ResourceBundle rb = ResourceBundle.getBundle("com.sun.jbi.engine.workflow.clientapi.config");
    
    public WorkflowEngineVariableTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        Util.initTaskManager();
        init();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(WorkflowEngineVariableTest.class);

        return suite;
    } 
    
    public void testWorkflowCreateToCompleteTask() throws Exception {
        WorkflowEngine engine = new WorkflowEngine ();
        engine.init();
        DefaultTestInOutCallBack callBack = new DefaultTestInOutCallBack();
        engine.setInOutCallBack(callBack);
        
        engine.start();
        
        //Create task
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseInVariable.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        setModelInContext(task);
        
        WorkflowModelManager modelManager = new WorkflowModelManager();
        
        String taskXFormName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseXform.xhtml";
        Util.setXform(task, modelManager, taskXFormName);
       
        String taskInputXFormInstanceName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseInputInstance.xml";
        Util.setXformInstance(task, modelManager, taskInputXFormInstanceName);
        String taskOutputXFormInstanceName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseOutputInstance.xml";
        Util.setXformInstance(task, modelManager, taskOutputXFormInstanceName);                
        
        
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderVariableInput.xml";
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
        Thread.sleep(2500);

        
        //Get task list
        Operation getTaskListOpt =  mPortType.getOperation(StaticOperationFactory.OPERATION_getTaskList, null, null);
        
        Element getTaskListInput = Util.loadGetTaskListInputElement();
        Principal principal = new PrincipalImpl ("Mary");
        Set<Principal> principals = new HashSet<Principal> ();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet (), new HashSet ());       
      
        ClientWorkflowRequest getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList-Mary", getTaskListInput, getTaskListOpt, subject);
        engine.acceptRequest(getTaskListReq);
        Thread.sleep(2500);
        
        getTaskListInput = Util.loadGetTaskListInputElement();
        principal = new PrincipalImpl ("John");
        principals = new HashSet<Principal> ();
        principals.add(principal);
        subject = new Subject(true, principals, new HashSet (), new HashSet ());       
      
        getTaskListReq = new ClientWorkflowRequest ("TestGetTaskList-John", getTaskListInput, getTaskListOpt, subject);
        engine.acceptRequest(getTaskListReq);
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
        getTaskOutputOptReq = new ClientWorkflowRequest ("TestGetTaskOutput3", getTaskOutputOptInput, getTaskOutputOpt, subject);
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
    
   private void init() throws Exception {
      if(mPortType != null) {
          return;
      }
      
        this.mPortType = Util.loadTaskCommonPortType();
   }
   
   public static void initTaskManager() {
       String key = TaskManagerFactory.class.getName();
       String factoryImpl = rb.getString(key);
       if(factoryImpl == null) {
           //default is Memory for now
           factoryImpl = "com.sun.jbi.engine.workflow.runtime.model.impl.MemoryTaskManagerFactoryImpl";
       }
       
       System.setProperty(key, factoryImpl);
       try {
           initContext ();
       } catch (Exception e) {
           // TODO Auto-generated catch block
           e.printStackTrace();
       }
       
   }
   public static void initContext() throws Exception {
       String key = TaskManagerFactory.class.getName();
       if (System.getProperty(key).equals ("com.sun.jbi.engine.workflow.runtime.model.impl.MemoryTaskManagerFactoryImpl")) {
           return;
       }
       // TODO Auto-generated method stub
       EngineContext engineContext = new EngineContext();
       InitialContext initialContext = null;

       initialContext = new TestContext();
       DataSource ds = (DataSource) initialContext.lookup("jdbc/__workflow");
       assert (ds != null);

       engineContext.setInitialContext(initialContext);

       WorkflowMapEntryTable workflowMapEntry = new WorkflowMapEntryTable();
       engineContext.setWorkflowMapEntryTable(workflowMapEntry);
       String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseInVariable.wf";
       
       Task task = Util.loadTask(wfFileName);

//       Tasks tasks = (Tasks) task.getParent();
//       QName qName = new QName(tasks.getTargetNamespace(), task.getName());
//       Map<QName, Tasks> tasksMap = new HashMap<QName, Tasks>();
//       tasksMap.put(qName, tasks);
//       workflowMapEntry.setTaskModelMap(tasksMap);

       

       Properties props = new Properties();
       props.put(EnginePropertyConstants.DATASOURCE_JNDI, "jdbc/__workflow");
       props.put(EnginePropertyConstants.DATASOURCE_TYPE, DAO.DERBY);
       props.put(EnginePropertyConstants.TEST_MODE, "true");

       engineContext.setConfig(props);
       TaskManagerFactory.getInstance().getTaskManager().setContext(engineContext);

   }   
   
   public static void setModelInContext (Task task ) throws Exception {
       EngineContext engineContext = TaskManagerFactory.getInstance().getTaskManager().getContext();
       if (engineContext != null) {
           WorkflowMapEntryTable workflowMapEntryTable = engineContext.getWorkflowMapEntryTable();
           QName qName = task.getQName();
           WorkflowMapEntry entry = new WorkflowMapEntry (null, null, null, task, WorkflowMapEntry.EntryType.ENTRY_PROVIDE);            
//           Map<QName, Tasks> tasksMap = workflowMapEntryTable.getTaskModelMap();
           workflowMapEntryTable.addEntry(entry, "TEST");
       }
   }   
}
