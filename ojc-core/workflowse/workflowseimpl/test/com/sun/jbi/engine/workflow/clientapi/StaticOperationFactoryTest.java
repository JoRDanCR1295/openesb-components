/*
 * StaticOperationFactoryTest.java
 * JUnit based test
 *
 * Created on October 25, 2006, 11:48 PM
 */

package com.sun.jbi.engine.workflow.clientapi;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import javax.security.auth.Subject;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.NamespaceContext;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import junit.framework.TestCase;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.xmlbeans.XmlDateTime;
import org.w3c.dom.Element;

import sun.security.acl.PrincipalImpl;

import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.engine.workflow.WorkflowModelManager;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.db.hibernate.TaskAssignee;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.runtime.model.impl.PersistenceTaskManagerImpl;
import com.sun.jbi.engine.workflow.util.XPathUtil;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;

/**
 * 
 * @author radval
 */
public class StaticOperationFactoryTest extends TestCase {

    private PortType mPortTypeTaskStatic;

    private PortType mPortTypeTaskDynamic;

    public StaticOperationFactoryTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        Util.initTaskManager();
        init();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getInstance method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testGetInstance() throws Exception {
        System.out.println("getInstance");

        StaticOperationFactory expResult = null;
        StaticOperationFactory result = StaticOperationFactory.getInstance();
        assertNotNull(result);

    }

    /**
     * Test of newGetTaskListOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewGetTaskListOperation() throws Exception {

        System.out.println("newGetTaskListOperation");

        // Test when the tasklist is null
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        Element input = Util.loadGetTaskListInputElement();
        Principal principal = new PrincipalImpl("workflow");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());
        StaticOperationFactory instance = new StaticOperationFactory();
        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();
        Element taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("No task xml:"
                + Util.createXmlString(taskOutputResult));

        Element expected = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListOutput_NoTasks.xml");
        assertTrue(Util.compareElements(expected, taskOutputResult));

        Element inputQuery1 = Util.loadGetTaskListQuery1InputElement();
        taskOp = instance.newGetTaskListOperation(operation, inputQuery1,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("No task xml:"
                + Util.createXmlString(taskOutputResult));

        assertTrue(Util.compareElements(expected, taskOutputResult));
        // Test when tasklist is not null;
        // Create task:
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = result
                .createTask(exchangeId, createTaskInput, task);

        // Create task 2: simulate recovery, invoker sends a request with the
        // same exchange id:
        RuntimeTask task2 = result
                .createTask(exchangeId, createTaskInput, task);

        // Compare the task1 and task2
        assertTrue(task2.equals(task1));
        assertEquals(new Long(1), task2.getId());
        assertFalse(task2.isNew());
        
        RuntimeTask task3 = result.getTask(task1.getId());
        JXPathContext jxcontext2 = task3.getJXpathContext();
        String taskId =  XPathUtil.getStringValue(XPathUtil.evaluateExpression("get-task-id()", jxcontext2), "");
        assertEquals(taskId, task3.getId().toString());

        // Assign task:
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("workflow", TaskPrincipal.PrincipalType.User);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal);
        result.assignTask(task1, assignees, null);

        // Assign task to another
        Collection<TaskPrincipal> assignees2 = new ArrayList<TaskPrincipal>();
        TaskPrincipal taskPrincipalGroup1 = TaskModelFactory.getInstance()
                .createPrincipal("group1", TaskPrincipal.PrincipalType.Group);
        assignees2.add(taskPrincipalGroup1);

        task3= result.createTask(Util.getRandomME(),
                createTaskInput, task);
        result.assignTask(task3, assignees2, null);

        // GetTaskList
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("Having task xml:"
                + Util.createXmlString(taskOutputResult));

        Map prefixToNSMap = new HashMap();
        prefixToNSMap.put("jbi",
                "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper");
        prefixToNSMap.put("tc", "http://jbi.com.sun/wfse/wsdl/TaskCommon");

        String creationDate = Util
                .extractTextContent(taskOutputResult,
                        "/jbi:message/jbi:part/tc:task/tc:submittedDate",
                        prefixToNSMap);
        assertNotNull("task creation date should not be null", creationDate);

        Element expectedTasks = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListOutput_HaveTasks.xml");

        Util.replaceTextContent(expectedTasks,
                "/jbi:message/jbi:part/tc:task/tc:submittedDate", creationDate,
                prefixToNSMap);
        String deadlineDate = Util
        .extractTextContent(taskOutputResult,
                "/jbi:message/jbi:part/tc:task/tc:deadline",
                prefixToNSMap);
        assertNotNull("task deadline date should not be null", deadlineDate);        
        Util.replaceTextContent(expectedTasks,
                "/jbi:message/jbi:part/tc:task/tc:deadline", deadlineDate,
                prefixToNSMap);        
        System.out.println("expected " + Util.createXmlString(expectedTasks));
        System.out.println("result " + Util.createXmlString(taskOutputResult));
        
        assertTrue(Util.compareElements(expectedTasks, taskOutputResult));

        taskOp = instance.newGetTaskListOperation(operation, inputQuery1,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        creationDate = Util
                .extractTextContent(taskOutputResult,
                        "/jbi:message/jbi:part/tc:task/tc:submittedDate",
                        prefixToNSMap);
        Element expectedTasksQuery = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListOutput_HaveTasksQuery.xml");
        Util.replaceTextContent(expectedTasksQuery,
                "/jbi:message/jbi:part/tc:task/tc:submittedDate", creationDate,
                prefixToNSMap);
        System.out.println("expected "
                + Util.createXmlString(expectedTasksQuery));
         deadlineDate = Util
        .extractTextContent(taskOutputResult,
                "/jbi:message/jbi:part/tc:task/tc:deadline",
                prefixToNSMap);
        assertNotNull("task deadline date should not be null", deadlineDate);        
        Util.replaceTextContent(expectedTasksQuery,
                "/jbi:message/jbi:part/tc:task/tc:deadline", deadlineDate,
                prefixToNSMap);        
        
        System.out.println("result " + Util.createXmlString(taskOutputResult));
        assertTrue(Util.compareElements(expectedTasksQuery, taskOutputResult));

    }

    /**
     * Test of newGetTaskListOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testTextSearchOperation() throws Exception {

        System.out.println("testTextSearchOperation");
        StaticOperationFactory instance = new StaticOperationFactory();
        
        // Create task:
        TaskManager result = instance.getService().getManager();
        TaskManagerFactory.getInstance().getTaskManager().createFullTextIndex();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseKeywordSearch.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = result
                .createTask(exchangeId, createTaskInput, task);
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("workflow", TaskPrincipal.PrincipalType.User);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal);
        result.assignTask(task1, assignees, null);
      
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        Element input = Util.loadGetTaskListInputElement();
        Principal principal = new PrincipalImpl("workflow");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());

        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();

        Element taskOutputResult = taskOp.getOutput().getReply();

        System.out.println("Having task xml:"
                + Util.createXmlString(taskOutputResult));

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(new NamespaceContextImpl());
        javax.xml.xpath.XPathExpression expr = xpath
                .compile("count(//tm:task)");
        javax.xml.xpath.XPathExpression expr2 = xpath
                .compile("//tm:totalRecords");
        javax.xml.xpath.XPathExpression expr3 = xpath
                .compile("//tm:returnedRecords");
        javax.xml.xpath.XPathExpression expr4 = xpath.compile("//tm:keywords");

        Object resultObject = expr.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object totalRecords = expr2.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object returnRecords = expr3.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object keywords = expr4.evaluate(taskOutputResult,
                XPathConstants.STRING);

        assertEquals(1, ((Double) resultObject).intValue());
        assertEquals(1, ((Double) returnRecords).intValue());
        assertEquals(1, ((Double) totalRecords).intValue());
        assertEquals("[12345][MyPurchase][productABC]", ((String) keywords));

        QueryType query = QueryType.Factory.newInstance();
        query.setType(QueryType.Type.TEXTSEARCH);
        query.setSearchString("12345");

        String queryString = query.toString();

        // System.out.println("QueryString:" + queryString);

        Element textSearchQuery = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput1.xml");

        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml1:"
                + Util.createXmlString(taskOutputResult));

        resultObject = expr.evaluate(taskOutputResult, XPathConstants.NUMBER);
        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);
        returnRecords = expr3.evaluate(taskOutputResult, XPathConstants.NUMBER);
        keywords = expr4.evaluate(taskOutputResult, XPathConstants.STRING);

        assertEquals(1, ((Double) totalRecords).intValue());
        assertEquals("[12345][MyPurchase][productABC]", ((String) keywords));

        textSearchQuery = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput2.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) totalRecords).intValue());
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput3.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());        
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput4.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());                
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput5.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml5:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());              
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput6.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml6:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());               
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput7.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml7:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());         

        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput8.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml8:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());            
        
        // claim task
        Operation claimOperation = mPortTypeTaskStatic.getOperation("ClaimTask",
                null, null);
        Element claimInput = Util.loadClaimTaskInputElement(task1.getId());

        StaticOperation expResult = null;
        StaticOperation op = instance.newClaimTaskOperation(claimOperation, claimInput,
                subject);
        op.execute();        

   
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput7.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml7-2:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) totalRecords).intValue());   
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput8.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml8-2:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());                  
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput9.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml9:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) totalRecords).intValue());           
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput10.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml10:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) totalRecords).intValue());               
        
        textSearchQuery = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetSearchInput11.xml");
        taskOp = instance.newGetTaskListOperation(operation, textSearchQuery,
                subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        
        System.out.println("search result xml11:"
                + Util.createXmlString(taskOutputResult));

        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(1, ((Double) totalRecords).intValue());              

    }

    public void testNewFourEyeExcludedUsersOperation() throws Exception {
        System.out.println("testNewFourEyeExcludedUsersOperation");
        StaticOperationFactory instance = new StaticOperationFactory();
        // Create task:
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseExludedUsers.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = result
                .createTask(exchangeId, createTaskInput, task);

        // Assign task:
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("Support", TaskPrincipal.PrincipalType.Group);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal);

        TaskPrincipal taskExcludedUser = TaskModelFactory.getInstance()
                .createPrincipal("Jay", TaskPrincipal.PrincipalType.User);
        Collection<TaskPrincipal> excludedAssignees = new ArrayList<TaskPrincipal>();
        excludedAssignees.add(taskExcludedUser);

        TaskPrincipal taskExcludedUser2 = TaskModelFactory.getInstance()
                .createPrincipal("Ling", TaskPrincipal.PrincipalType.User);
        excludedAssignees.add(taskExcludedUser2);

        result.assignTask(task1, assignees, excludedAssignees);

        // Test getTask list for non-excluded;

        Principal principal = new PrincipalImpl("Mark");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Principal principal2 = new GroupImpl("Support");
        principals.add(principal2);

        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());
        instance = new StaticOperationFactory();
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        Element input = Util.loadGetTaskListInputElement();

        Map prefixToNSMap = new HashMap();
        prefixToNSMap.put("jbi",
                "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper");
        prefixToNSMap.put("tc", "http://jbi.com.sun/wfse/wsdl/TaskCommon");

        Util.replaceTextContent(input, "/jbi:message/jbi:part[2]", "Mark",
                prefixToNSMap);

        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();
        Element taskOutputResult = taskOp.getOutput().getReply();

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(new NamespaceContextImpl());
        javax.xml.xpath.XPathExpression expr = xpath
                .compile("count(//tm:task)");
        javax.xml.xpath.XPathExpression expr2 = xpath
                .compile("//tm:totalRecords");
        javax.xml.xpath.XPathExpression expr3 = xpath
                .compile("//tm:returnedRecords");

        Object resultObject = expr.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object totalRecords = expr2.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object returnRecords = expr3.evaluate(taskOutputResult,
                XPathConstants.NUMBER);

        assertEquals(1, ((Double) resultObject).intValue());
        assertEquals(1, ((Double) returnRecords).intValue());
        assertEquals(1, ((Double) totalRecords).intValue());
        
        //Test the Filtered
        input = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskInput2.xml");

//         Util.replaceTextContent(input, "/jbi:message/jbi:part[2]", "Mark",
//               prefixToNSMap);    
           
         taskOp = instance.newGetTaskListOperation(operation,
                 input, subject);
         
         taskOp.execute();
         taskOutputResult = taskOp.getOutput().getReply();


          resultObject = expr.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          totalRecords = expr2.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          returnRecords = expr3.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);

         assertEquals(1, ((Double) resultObject).intValue());
         assertEquals(1, ((Double) returnRecords).intValue());
         assertEquals(1, ((Double) totalRecords).intValue());             
        
        input = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskInput2.xml");

         Util.replaceTextContent(input, "/jbi:message/jbi:part[1]/tc:users/tc:user", "Jay",
               prefixToNSMap);    
           
         taskOp = instance.newGetTaskListOperation(operation,
                 input, subject);
         
         taskOp.execute();
         taskOutputResult = taskOp.getOutput().getReply();


          resultObject = expr.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          totalRecords = expr2.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          returnRecords = expr3.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);

         assertEquals(0, ((Double) resultObject).intValue());
         assertEquals(0, ((Double) returnRecords).intValue());
         assertEquals(0, ((Double) totalRecords).intValue());           

        // Test getTask list for non-excluded;

        Principal principal_jay = new PrincipalImpl("Jay");
        principals = new HashSet<Principal>();
        principals.add(principal_jay);
        principals.add(principal2);

        Util.replaceTextContent(input, "/jbi:message/jbi:part[2]", "Jay",
                prefixToNSMap);

        subject = new Subject(true, principals, new HashSet(), new HashSet());
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();

        resultObject = expr.evaluate(taskOutputResult, XPathConstants.NUMBER);
        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);
        returnRecords = expr3.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) resultObject).intValue());
        assertEquals(0, ((Double) returnRecords).intValue());
        assertEquals(0, ((Double) totalRecords).intValue());

        Principal principal_ling = new PrincipalImpl("Ling");
        principals = new HashSet<Principal>();
        principals.add(principal_ling);
        principals.add(principal2);
        Util.replaceTextContent(input, "/jbi:message/jbi:part[2]", "Ling",
                prefixToNSMap);

        subject = new Subject(true, principals, new HashSet(), new HashSet());
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();

        resultObject = expr.evaluate(taskOutputResult, XPathConstants.NUMBER);
        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);
        returnRecords = expr3.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) resultObject).intValue());
        assertEquals(0, ((Double) returnRecords).intValue());
        assertEquals(0, ((Double) totalRecords).intValue());

    }

    public void testNewFourEyeExcludedGroupsOperation() throws Exception {
        System.out.println("testNewFourEyeExcludedUsersOperation");
        StaticOperationFactory instance = new StaticOperationFactory();
        // Create task:
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseExludedUsers.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = result
                .createTask(exchangeId, createTaskInput, task);

        // Assign task:
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("Support", TaskPrincipal.PrincipalType.Group);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal);

        TaskPrincipal taskExcludedUser = TaskModelFactory.getInstance()
                .createPrincipal("Opt1", TaskPrincipal.PrincipalType.Group);
        Collection<TaskPrincipal> excludedAssignees = new ArrayList<TaskPrincipal>();
        excludedAssignees.add(taskExcludedUser);

        TaskPrincipal taskExcludedUser2 = TaskModelFactory.getInstance()
                .createPrincipal("Opt2", TaskPrincipal.PrincipalType.Group);
        excludedAssignees.add(taskExcludedUser2);

        result.assignTask(task1, assignees, excludedAssignees);
        
        //
 

        // Test getTask list for non-excluded;

        Principal principal = new PrincipalImpl("Mark");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Principal principal2 = new GroupImpl("Support");
        principals.add(principal2);

        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());
        instance = new StaticOperationFactory();
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        Element input = Util.loadGetTaskListInputElement();

        Map prefixToNSMap = new HashMap();
        prefixToNSMap.put("jbi",
                "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper");
        prefixToNSMap.put("tc", "http://jbi.com.sun/wfse/wsdl/TaskCommon");

        Util.replaceTextContent(input, "/jbi:message/jbi:part[2]", "Mark",
                prefixToNSMap);

        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();
        Element taskOutputResult = taskOp.getOutput().getReply();

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(new NamespaceContextImpl());
        javax.xml.xpath.XPathExpression expr = xpath
                .compile("count(//tm:task)");
        javax.xml.xpath.XPathExpression expr2 = xpath
                .compile("//tm:totalRecords");
        javax.xml.xpath.XPathExpression expr3 = xpath
                .compile("//tm:returnedRecords");

        Object resultObject = expr.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object totalRecords = expr2.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object returnRecords = expr3.evaluate(taskOutputResult,
                XPathConstants.NUMBER);

        assertEquals(1, ((Double) resultObject).intValue());
        assertEquals(1, ((Double) returnRecords).intValue());
        assertEquals(1, ((Double) totalRecords).intValue());

        // Test getTask list for excluded;

        Principal principal_jay = new PrincipalImpl("Mark");
        principals = new HashSet<Principal>();
        principals.add(principal_jay);
        principals.add(principal2);

        Principal principal_Opt1 = new GroupImpl("Opt1");
        principals.add(principal_Opt1);

        subject = new Subject(true, principals, new HashSet(), new HashSet());
        Util.replaceTextContent(input, "/jbi:message/jbi:part[2]", "Mark",
                prefixToNSMap);
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();

        resultObject = expr.evaluate(taskOutputResult, XPathConstants.NUMBER);
        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);
        returnRecords = expr3.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) resultObject).intValue());
        assertEquals(0, ((Double) returnRecords).intValue());
        assertEquals(0, ((Double) totalRecords).intValue());

        Principal principal_ling = new PrincipalImpl("Mark");
        principals = new HashSet<Principal>();
        principals.add(principal_ling);
        principals.add(principal2);

        Principal principal_Opt2 = new GroupImpl("Opt2");
        principals.add(principal_Opt2);

        subject = new Subject(true, principals, new HashSet(), new HashSet());
        Util.replaceTextContent(input, "/jbi:message/jbi:part[2]", "Mark",
                prefixToNSMap);
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();

        resultObject = expr.evaluate(taskOutputResult, XPathConstants.NUMBER);
        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);
        returnRecords = expr3.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(0, ((Double) resultObject).intValue());
        assertEquals(0, ((Double) returnRecords).intValue());
        assertEquals(0, ((Double) totalRecords).intValue());
        
        //Test the Filtered
        principal_ling = new PrincipalImpl("Mark");
        principals = new HashSet<Principal>();
        principals.add(principal_ling);
        principal_Opt2 = new GroupImpl("Support");
        principals.add(principal_Opt2);

        subject = new Subject(true, principals, new HashSet(), new HashSet());        
         input = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskInput3.xml");

//        Util.replaceTextContent(input, "/jbi:message/jbi:part[1]/tc:groups/tc:group[2]", "InternalSupport",
//                prefixToNSMap);  
           
         taskOp = instance.newGetTaskListOperation(operation,
                 input, subject);
         
         taskOp.execute();
         taskOutputResult = taskOp.getOutput().getReply();


          resultObject = expr.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          totalRecords = expr2.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          returnRecords = expr3.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);

         assertEquals(1, ((Double) resultObject).intValue());
         assertEquals(1, ((Double) returnRecords).intValue());
         assertEquals(1, ((Double) totalRecords).intValue());             
        
        input = Util
        .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskInput3.xml");

      Util.replaceTextContent(input, "/jbi:message/jbi:part[1]/tc:groups/tc:group[1]", "InternalSupport",
      prefixToNSMap);     
           
         taskOp = instance.newGetTaskListOperation(operation,
                 input, subject);
         
         taskOp.execute();
         taskOutputResult = taskOp.getOutput().getReply();


          resultObject = expr.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          totalRecords = expr2.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);
          returnRecords = expr3.evaluate(taskOutputResult,
                 XPathConstants.NUMBER);

         assertEquals(0, ((Double) resultObject).intValue());
         assertEquals(0, ((Double) returnRecords).intValue());
         assertEquals(0, ((Double) totalRecords).intValue());          

    }

    /**
     * Test of newClaimTaskOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewClaimTaskOperation() throws Exception {
        System.out.println("newClaimTaskOperation");
        StaticOperationFactory instance = new StaticOperationFactory();

        // Create task
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer(new Random()
                .nextInt()).toString(), createTaskInput, task);

        // claim task
        Operation operation = mPortTypeTaskStatic.getOperation("ClaimTask",
                null, null);
        Element input = Util.loadClaimTaskInputElement(task1.getId());

        Principal principal = new PrincipalImpl("radval");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());

        StaticOperation expResult = null;
        StaticOperation op = instance.newClaimTaskOperation(operation, input,
                subject);
        op.execute();
        Element domResult1 = op.getOutput().getReply();
        assertNotNull(domResult1);

        String outputFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ClaimTaskOutput.xml";
        Element expected = Util.loadInputMessageElement(outputFileName);

        assertTrue(Util.compareElements(expected, domResult1));

    }

    public void testNewClaimReAssignTaskOperation() throws Exception {
        System.out.println("newClaimTaskOperation");
        StaticOperationFactory instance = new StaticOperationFactory();

        // Create task
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer(new Random()
                .nextInt()).toString(), createTaskInput, task);

        // Assign task:
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("radval", TaskPrincipal.PrincipalType.User);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal);
        result.assignTask(task1, assignees, null);

        // claim task
        Operation operation = mPortTypeTaskStatic.getOperation("ClaimTask",
                null, null);
        Element input = Util.loadClaimTaskInputElement(task1.getId());

        Principal principal = new PrincipalImpl("radval");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());

        StaticOperation expResult = null;
        StaticOperation op = instance.newClaimTaskOperation(operation, input,
                subject);
        op.execute();
        Element domResult1 = op.getOutput().getReply();
        assertNotNull(domResult1);

        String outputFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ClaimTaskOutput.xml";
        Element expected = Util.loadInputMessageElement(outputFileName);

        assertTrue(Util.compareElements(expected, domResult1));

        // ReAssign task
        Principal principal2 = new PrincipalImpl("workflow2");
        Set<Principal> principal2s = new HashSet<Principal>();
        principal2s.add(principal2);
        Subject subject2 = new Subject(true, principal2s, new HashSet(),
                new HashSet());
        TaskPrincipal taskPrincipa2 = TaskModelFactory.getInstance()
                .createPrincipal("workflow2", TaskPrincipal.PrincipalType.User);
        List<TaskPrincipal> newPrincipals = new ArrayList<TaskPrincipal>();
        newPrincipals.add(taskPrincipa2);
        result.reassignTask(task1.getId(), newPrincipals, null);

        // Get task list for old principal
        operation = mPortTypeTaskStatic.getOperation("GetTaskList", null, null);
        input = Util.loadGetTaskListInputElement();
        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();
        Element taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("No task xml:"
                + Util.createXmlString(taskOutputResult));

        expected = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListOutput_NoTasks.xml");
        assertTrue(Util.compareElements(expected, taskOutputResult));

    }

    public void testAssignToGroupOperation() throws Exception {
        System.out.println("assignToGroupOperation");
        StaticOperationFactory instance = new StaticOperationFactory();

        // Create task
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseGroup.wf";

        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer(new Random()
                .nextInt()).toString(), createTaskInput, task);

        // Assign task
        task1.execute();

        // Claim task
        Operation operation = mPortTypeTaskStatic.getOperation("ClaimTask",
                null, null);
        Element input = Util.loadClaimTaskInputElement(task1.getId());

        Principal principal = new PrincipalImpl("meiwu");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        principal = new GroupImpl("workflow1");
        principals.add(principal);

        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());

        StaticOperation expResult = null;
        StaticOperation op = instance.newClaimTaskOperation(operation, input,
                subject);
        op.execute();
        Element domResult1 = op.getOutput().getReply();
        assertNotNull(domResult1);

        String outputFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ClaimTaskOutput.xml";
        Element expected = Util.loadInputMessageElement(outputFileName);
        assertTrue(Util.compareElements(expected, domResult1));

        // Get task list
        operation = mPortTypeTaskStatic.getOperation("GetTaskList", null, null);
        input = Util.loadGetTaskListInputElement();
        op = instance.newGetTaskListOperation(operation, input, subject);
        op.execute();
        domResult1 = op.getOutput().getReply();
        assertNotNull(domResult1);

        Map prefixToNSMap = new HashMap();
        prefixToNSMap.put("jbi",
                "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper");
        prefixToNSMap.put("tc", "http://jbi.com.sun/wfse/wsdl/TaskCommon");

        String creationDate = Util
                .extractTextContent(domResult1,
                        "/jbi:message/jbi:part/tc:task/tc:submittedDate",
                        prefixToNSMap);
        assertNotNull("task creation date should not be null", creationDate);

        String actId = Util.extractTextContent(domResult1,
                "/jbi:message/jbi:part/tc:task/tc:taskId", prefixToNSMap);

        Element expectedTasks = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListOutputGroup.xml");
        Util.replaceTextContent(expectedTasks,
                "/jbi:message/jbi:part/tc:task/tc:submittedDate", creationDate,
                prefixToNSMap);
        Util
                .replaceTextContent(expectedTasks,
                        "/jbi:message/jbi:part/tc:task/tc:taskId", actId,
                        prefixToNSMap);
        System.out.println("expected2 " + Util.createXmlString(expectedTasks));
        System.out.println("result2 " + Util.createXmlString(domResult1));
        // assertTrue(Util.compareElements(expectedTasks, domResult1));

    }

    /**
     * Test of newRevokeTaskOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewRevokeTaskOperation() throws Exception {
        System.out.println("newRevokeTaskOperation");

        StaticOperationFactory instance = new StaticOperationFactory();

        // Create task
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer(new Random()
                .nextInt()).toString(), createTaskInput, task);
        // Assign task
        task1.execute();
        assertEquals(new Long(1), task1.getId());
        task1 = result.getTask(new Long(1));
        Collection<TaskPrincipal> newPrincipals = task1.getAssignedTo();
        assertEquals(2, newPrincipals.size());

        // claim task
        Operation operation = mPortTypeTaskStatic.getOperation("ClaimTask",
                null, null);
        Element input = Util.loadClaimTaskInputElement(task1.getId());

        Principal principal = new PrincipalImpl("radval");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());

        StaticOperation expResult = null;
        StaticOperation op = instance.newClaimTaskOperation(operation, input,
                subject);
        op.execute();
        Element domResult1 = op.getOutput().getReply();
        assertNotNull(domResult1);

        task1 = result.getTask(new Long(1));
        newPrincipals = task1.getAssignedTo();
        assertEquals(2, newPrincipals.size());

        List expectedPrincipalNames = new ArrayList();
        expectedPrincipalNames.add("radval");
        expectedPrincipalNames.add("rwaldorf");

        Iterator<TaskPrincipal> it = newPrincipals.iterator();
        TaskPrincipal tp1 = it.next();
        assertTrue("expected new user", expectedPrincipalNames.contains(tp1
                .getName()));

        TaskPrincipal tp2 = it.next();
        assertTrue("expected new role", expectedPrincipalNames.contains(tp2
                .getName()));

        String outputFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ClaimTaskOutput.xml";
        Element expected = Util.loadInputMessageElement(outputFileName);

        assertTrue(Util.compareElements(expected, domResult1));

        // revoke task
        Operation revokeOperation = mPortTypeTaskStatic.getOperation(
                "RevokeTask", null, null);
        Element revokeInputElement = Util.loadRevokeTaskInputElement(task1
                .getId());

        StaticOperation revokeOp = instance.newRevokeTaskOperation(
                revokeOperation, revokeInputElement, subject);
        revokeOp.execute();
        Element domResult2 = revokeOp.getOutput().getReply();
        assertNotNull(domResult2);

        String revokeOutput = "/com/sun/jbi/engine/workflow/clientapi/testMessages/RevokeTaskOutput.xml";
        Element revokeOutputElement = Util
                .loadInputMessageElement(revokeOutput);
        assertTrue(Util.compareElements(revokeOutputElement, domResult2));

        RuntimeTask task2 = result.getTask(new Long(1));
        assertTrue(task2.getState() == RuntimeTask.TaskState.ASSIGNED);
        assertTrue(task2.getClaimedBy() == null);

        newPrincipals = task2.getAssignedTo();
        assertEquals(2, newPrincipals.size());

        expectedPrincipalNames = new ArrayList();
        expectedPrincipalNames.add("radval");
        expectedPrincipalNames.add("rwaldorf");

        it = newPrincipals.iterator();
        tp1 = it.next();
        assertTrue("expected new user", expectedPrincipalNames.contains(tp1
                .getName()));

        tp2 = it.next();
        assertTrue("expected new role", expectedPrincipalNames.contains(tp2
                .getName()));

    }

    /**
     * Test of newGetTaskListOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testGetTaskListPaginationOperation() throws Exception {
        System.out.println("GetTaskListPaginationOperation");

        // Test when the tasklist is null
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        Element input = Util.loadGetTaskListInputElement(0, 50);
        Principal principal = new PrincipalImpl("workflow");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());
        StaticOperationFactory instance = new StaticOperationFactory();
        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();
        Element taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("No task xml:"
                + Util.createXmlString(taskOutputResult));

        Element expected = Util
                .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/GetTaskListOutput_NoTasks.xml");
        assertTrue(Util.compareElements(expected, taskOutputResult));

        // Test when tasklist is not null;
        // Create 500 task:

        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = null;

        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("workflow", TaskPrincipal.PrincipalType.User);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal); // GetTaskList
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("Having task xml:"
                + Util.createXmlString(taskOutputResult));

        System.out.println("Now creating 45 tasks.");
        for (int i = 0; i < 45; i++) {
            task1 = result.createTask(exchangeId, createTaskInput, task);
            result.assignTask(task1, assignees, null);
            exchangeId = new Integer(new Random().nextInt()).toString();
        }

        // GetTaskList
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("Having task xml 1 page:"
                + Util.createXmlString(taskOutputResult));

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(new NamespaceContextImpl());
        javax.xml.xpath.XPathExpression expr = xpath
                .compile("count(//tm:task)");
        javax.xml.xpath.XPathExpression expr2 = xpath
                .compile("//tm:totalRecords");
        javax.xml.xpath.XPathExpression expr3 = xpath
                .compile("//tm:returnedRecords");

        Object resultObject = expr.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object totalRecords = expr2.evaluate(taskOutputResult,
                XPathConstants.NUMBER);
        Object returnRecords = expr3.evaluate(taskOutputResult,
                XPathConstants.NUMBER);

        assertEquals(45, ((Double) resultObject).intValue());
        assertEquals(45, ((Double) returnRecords).intValue());
        assertEquals(45, ((Double) totalRecords).intValue());

        System.out.println("Now creating 45 tasks.");
        for (int i = 0; i < 45; i++) {
            task1 = result.createTask(exchangeId, createTaskInput, task);
            result.assignTask(task1, assignees, null);
            exchangeId = new Integer(new Random().nextInt()).toString();
        }

        // GetTaskList
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("Having task xml 2 pages page 1:"
                + Util.createXmlString(taskOutputResult));

        resultObject = expr.evaluate(taskOutputResult, XPathConstants.NUMBER);
        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);
        returnRecords = expr3.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(50, ((Double) resultObject).intValue());
        assertEquals(50, ((Double) returnRecords).intValue());
        assertEquals(90, ((Double) totalRecords).intValue());

        input = Util.loadGetTaskListInputElement(50, 50);
        // GetTaskList
        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("Having task xml 2 pages page 2:"
                + Util.createXmlString(taskOutputResult));

        resultObject = expr.evaluate(taskOutputResult, XPathConstants.NUMBER);
        totalRecords = expr2.evaluate(taskOutputResult, XPathConstants.NUMBER);
        returnRecords = expr3.evaluate(taskOutputResult, XPathConstants.NUMBER);

        assertEquals(40, ((Double) resultObject).intValue());
        assertEquals(40, ((Double) returnRecords).intValue());
        assertEquals(90, ((Double) totalRecords).intValue());
    }

    /**
     * Test of newCompleteTaskOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewCompleteTaskOperation() throws Exception {
        System.out.println("newCompleteTaskOperation");

        // Create task:
        TaskManagerFactory modelFactoryInstance = TaskManagerFactory
                .getInstance();
        TaskManager result = modelFactoryInstance.getTaskManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer(new Random()
                .nextInt()).toString(), createTaskInput, task);
        Calendar initialDate = Calendar.getInstance();
        Thread.sleep(1000);
        // Assign task:
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("workflow", TaskPrincipal.PrincipalType.User);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal);
        result.assignTask(task1, assignees, null);

        // GetTaskList
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);

        Element input = Util.loadGetTaskListInputElement();
        Principal principal = new PrincipalImpl("workflow");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());
        StaticOperationFactory instance = new StaticOperationFactory();
        instance.getService().setManager(result);
        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();
        Element taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("Having task xml:"
                + Util.createXmlString(taskOutputResult));

        // Complete task
        Calendar oldDate = Calendar.getInstance();
        Thread.sleep(1000);
        Operation completeTaskOpt = mPortTypeTaskStatic.getOperation(
                "CompleteTask", null, null);
        Element completeTaskInput = Util.loadCompleteTaskInputElement(task1
                .getId());
        StaticOperation completeOp = instance.newCompleteTaskOperation(
                completeTaskOpt, completeTaskInput, subject);
        completeOp.execute();
        // Check the task status
        RuntimeTask checkTask = result.getTask(task1.getId());
        assertNotNull(checkTask.getEndDate());
        assertTrue(checkTask.getEndDate().after(oldDate));

        Set<TaskAssignee> curAssigness = ((PersistenceTaskManagerImpl) result)
                .getDBOperation().getCurrentAssignees(task1.getId());
        for (TaskAssignee assignee : curAssigness) {
            Date startDate = assignee.getStartDate();
            assertNotNull(startDate);
            Calendar tocompare = Calendar.getInstance();
            tocompare.setTime(startDate);
            assertTrue(initialDate.before(tocompare));
            assertTrue(checkTask.getEndDate().after(tocompare));
        }
        Element compleTaskOutput = completeOp.getOutput().getReply();
        System.out.println("Complete task output:"
                + Util.createXmlString(compleTaskOutput));

        String outputFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/CompleteTaskOutput.xml";
        Element expected = Util.loadInputMessageElement(outputFileName);

        // System.out.println(createXmlString(domResult1));
        // System.out.println(createXmlString(expected));

        assertTrue(Util.compareElements(expected, compleTaskOutput));

        // Now check the tasklist again:

        taskOp = instance.newGetTaskListOperation(operation, input, subject);
        taskOp.execute();
        taskOutputResult = taskOp.getOutput().getReply();
        System.out.println("After complete task:"
                + Util.createXmlString(taskOutputResult));
        // assertEquals(expResult, result);
        //        
        // // TODO review the generated test code and remove the default call to
        // fail.
        // fail("The test case is a prototype.");
    }

    /**
     * Test of newReassignTaskOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewReassignTaskOperation() throws Exception {
        System.out.println("newReassignTaskOperation");

        StaticOperationFactory instance = new StaticOperationFactory();

        // Create task
        TaskManager result = instance.getService().getManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer(new Random()
                .nextInt()).toString(), createTaskInput, task);

        // claim task
        Operation operation = mPortTypeTaskStatic.getOperation("ClaimTask",
                null, null);
        Element input = Util.loadClaimTaskInputElement(task1.getId());

        Principal principal = new PrincipalImpl("radval");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());

        StaticOperation expResult = null;
        StaticOperation op = instance.newClaimTaskOperation(operation, input,
                subject);
        op.execute();
        Element domResult1 = op.getOutput().getReply();
        assertNotNull(domResult1);

        String outputFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ClaimTaskOutput.xml";
        Element expected = Util.loadInputMessageElement(outputFileName);

        assertTrue(Util.compareElements(expected, domResult1));

        // reassign task
        Operation reassignOperation = mPortTypeTaskStatic.getOperation(
                "ReassignTask", null, null);
        String reassignInput = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ReassignTaskInput.xml";
        Element reassignInputElement = Util
                .loadInputMessageElement(reassignInput);

        StaticOperation reassignOp = instance.newReassignTaskOperation(
                reassignOperation, reassignInputElement, subject);
        reassignOp.execute();
        Element domResult2 = reassignOp.getOutput().getReply();
        assertNotNull(domResult2);

        String reassignOutput = "/com/sun/jbi/engine/workflow/clientapi/testMessages/ReassignTaskOutput.xml";
        Element reassignOutputElement = Util
                .loadInputMessageElement(reassignOutput);
        assertTrue(Util.compareElements(reassignOutputElement, domResult2));

        RuntimeTask task2 = result.getTask(new Long(1));
        Collection<TaskPrincipal> newPrincipals = task2.getAssignedTo();
        assert (newPrincipals.size() == 2);

        ArrayList expectedPrincipalNames = new ArrayList();
        expectedPrincipalNames.add("john");
        expectedPrincipalNames.add("CustomerRep");

        Iterator<TaskPrincipal> it = newPrincipals.iterator();
        TaskPrincipal tp1 = it.next();
        assertTrue("expected new user", expectedPrincipalNames.contains(tp1
                .getName()));

        TaskPrincipal tp2 = it.next();
        assertTrue("expected new role", expectedPrincipalNames.contains(tp2
                .getName()));

    }

    /**
     * Test of newGetTaskInputOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewGetTaskInputOperation() {
        System.out.println("newGetTaskInputOperation");

        Operation operation = null;
        Element input = null;
        Subject subject = null;
        StaticOperationFactory instance = new StaticOperationFactory();

        StaticOperation expResult = null;
        StaticOperation result = instance.newGetTaskInputOperation(operation,
                input, subject);
        // assertEquals(expResult, result);
        //        
        // // TODO review the generated test code and remove the default call to
        // fail.
        // fail("The test case is a prototype.");
    }

    /**
     * Test of newGetTaskOutputOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewGetTaskOutputOperation() {
        System.out.println("newGetTaskOutputOperation");

        Operation operation = null;
        Element input = null;
        Subject subject = null;
        StaticOperationFactory instance = new StaticOperationFactory();

        StaticOperation expResult = null;
        StaticOperation result = instance.newGetTaskOutputOperation(operation,
                input, subject);
        // assertEquals(expResult, result);
        //        
        // // TODO review the generated test code and remove the default call to
        // fail.
        // fail("The test case is a prototype.");
    }

    /**
     * Test of newSetTaskOutputOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testNewSetTaskOutputOperation() {
        System.out.println("newSetTaskOutputOperation");

        Operation operation = null;
        Element input = null;
        Subject subject = null;
        StaticOperationFactory instance = new StaticOperationFactory();

        StaticOperation expResult = null;
        StaticOperation result = instance.newSetTaskOutputOperation(operation,
                input, subject);
        // assertEquals(expResult, result);
        //        
        // // TODO review the generated test code and remove the default call to
        // fail.
        // fail("The test case is a prototype.");
    }

    public void testRecovery() throws Exception {
        System.out.println("testRecovery");
        String key = TaskManagerFactory.class.getName();
        String factoryImpl = System.getProperty(key);
        if (factoryImpl
                .equals("com.sun.jbi.engine.workflow.runtime.model.impl.MemoryTaskManagerFactoryImpl")) {
            return;
        }

        TaskManager taskManager = TaskManagerFactory.getInstance()
                .getTaskManager();
        // load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseEscalationDeadlineTest.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);

        Element root = Util.loadElement(wfFileName);

        // define 3 second deadline from current date
        Calendar c1 = Calendar.getInstance();
        c1.add(Calendar.SECOND, 3);

        XmlDateTime d1 = XmlDateTime.Factory.newInstance();
        d1.setCalendarValue(c1);

        Map<String, String> prefixToNSMap = new HashMap<String, String>();
        prefixToNSMap.put("wf", "http://jbi.com.sun/wfse");
        Util.replaceTextContent(root, "/wf:task/wf:escalation/wf:deadline", "'"
                + d1.getStringValue() + "'", prefixToNSMap);

        task = Util.loadTask(root);
        assertNotNull("task should not be null " + task);

        // load task input
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);

        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(
                taskInput);

        // create task
        RuntimeTask runtimeTask1 = taskManager.createTask(new Integer(
                new Random().nextInt()).toString(), tInput, task);
        taskManager.unregisterTimers(runtimeTask1);

        Thread.sleep(5000);
        runtimeTask1 = taskManager.getTask(runtimeTask1.getId());
        assertEquals(RuntimeTask.TaskState.UNASSIGNED, runtimeTask1.getState());

        taskManager.recover(runtimeTask1.getTaskMeta());

        Thread.sleep(5000);
        runtimeTask1 = taskManager.getTask(runtimeTask1.getId());
        assertEquals(RuntimeTask.TaskState.ESCALATED, runtimeTask1.getState());

    }

    /**
     * Test of newGetTaskListOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testGetTaskXForm() throws Exception {
        System.out.println("GetTaskXForm");

        // Create task:
        StaticOperationFactory instance = new StaticOperationFactory();
        TaskManager result = instance.getService().getManager();
        WorkflowModelManager modelManager = new WorkflowModelManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = result
                .createTask(exchangeId, createTaskInput, task);

        String taskXFormName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseXform.xhtml";
        Util.setXform(task, modelManager, taskXFormName);
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("workflow", TaskPrincipal.PrincipalType.User);

        Element getInput = result.getTaskXform(new Long(1));

        Element expectedInput = Util.loadInputMessageElement(taskXFormName);
        assertTrue(Util.compareElements(expectedInput, getInput));

    }

    public void testGetTaskXFormInstance() throws Exception {
        System.out.println("GetTaskXForm");

        // Create task:
        StaticOperationFactory instance = new StaticOperationFactory();
        TaskManager result = instance.getService().getManager();
        WorkflowModelManager modelManager = new WorkflowModelManager();

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = result
                .createTask(exchangeId, createTaskInput, task);

        String taskInputXFormInstanceName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseInputInstance.xml";
        Util.setXformInstance(task, modelManager, taskInputXFormInstanceName);
        String taskOutputXFormInstanceName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseOutputInstance.xml";
        Util.setXformInstance(task, modelManager, taskOutputXFormInstanceName);
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("workflow", TaskPrincipal.PrincipalType.User);

        Element getInput = result.getTaskInputXformInstance(new Long(1));
        Element getOutput = result.getTaskOutputXformInstance(new Long(1));

        Element expectedInput = Util
                .loadInputMessageElement(taskInputXFormInstanceName);
        Element expectedOutput = Util
                .loadInputMessageElement(taskOutputXFormInstanceName);

        assertTrue(Util.compareElements(expectedInput, getInput));

        assertTrue(Util.compareElements(expectedOutput, getOutput));

    }

    public void testChangeVariables() throws Exception {
        System.out.println("changeVariables");

        StaticOperationFactory instance = new StaticOperationFactory();
        Element input = Util.loadGetTaskListInputElement(0, 50);
        Principal principal = new PrincipalImpl("Mary");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());

        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        TaskManager result = instance.getService().getManager();

        Operation getTaskOutputOpt = mPortTypeTaskStatic.getOperation(
                "GetTaskOutput", null, null);

        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseChangeVariable.wf";
        Task task = Util.loadTask(wfFileName);
        Util.setModelInContext(task);
        assertNotNull("task should not be null " + task);

        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderVariableInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);

        TaskInput createTaskInput = TaskModelFactory.getInstance()
                .createTaskInput(taskInput);
        String exchangeId = new Integer(new Random().nextInt()).toString();

        RuntimeTask task1 = null;

        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                .createPrincipal("Mary", TaskPrincipal.PrincipalType.User);
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
        assignees.add(taskPrincipal);

        WorkflowModelManager modelManager = new WorkflowModelManager();
        String taskOutputXFormInstanceName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseOutputInstance.xml";
        Util.setXformInstance(task, modelManager, taskOutputXFormInstanceName);

        // Assign Task
        task1 = result.createTask(exchangeId, createTaskInput, task);
        result.assignTask(task1, assignees, null);
        // Check task output
        Element taskOutputReq = Util.loadGetTaskOutputElement(task1.getId()
                .longValue());
        StaticOperation taskOutOp = instance.newGetTaskOutputOperation(
                getTaskOutputOpt, taskOutputReq, subject);
        taskOutOp.execute();
        Element taskOutputResult = taskOutOp.getOutput().getReply();
        System.out.println(XmlUtil.print(new DOMSource(taskOutputResult)));

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();
        xpath.setNamespaceContext(new NamespaceContextImpl());
        javax.xml.xpath.XPathExpression expr = xpath
                .compile("//po:approveResult");
        javax.xml.xpath.XPathExpression expr2 = xpath
                .compile("//po:approveDate");
        javax.xml.xpath.XPathExpression expr3 = xpath.compile("//po:orderId");

        Object resultObject = expr.evaluate(taskOutputResult,
                XPathConstants.STRING);
        assertEquals("Approved", (String) resultObject);

        // Claim Task
        Operation claimOperation = mPortTypeTaskStatic.getOperation(
                "ClaimTask", null, null);
        Element claimReq = Util.loadClaimTaskInputElement(task1.getId());
        StaticOperation claimOp = instance.newClaimTaskOperation(
                claimOperation, claimReq, subject);
        claimOp.execute();

        // Check output again

        taskOutOp.execute();
        taskOutputResult = taskOutOp.getOutput().getReply();
        resultObject = expr.evaluate(taskOutputResult, XPathConstants.STRING);
        assertEquals("Rejected", (String) resultObject);

        // Complete task

        Operation completeTaskOpt = mPortTypeTaskStatic.getOperation(
                "CompleteTask", null, null);
        Element completeTaskInput = Util.loadCompleteTaskInputElement(task1
                .getId());
        StaticOperation completeOp = instance.newCompleteTaskOperation(
                completeTaskOpt, completeTaskInput, subject);
        completeOp.execute();

        // Check output again

        taskOutOp.execute();
        taskOutputResult = taskOutOp.getOutput().getReply();
        resultObject = expr2.evaluate(taskOutputResult, XPathConstants.STRING);
        assertNotNull(resultObject);
        System.out.println("ApproveDate:" + resultObject);

        resultObject = expr3.evaluate(taskOutputResult, XPathConstants.STRING);
        assertEquals("12345", (String) resultObject);
    }

    private void init() throws Exception {
        // System.setProperty("javax.xml.transform.TransformerFactory",
        // "com.sun.org.apache.xalan.internal.xsltc.trax.TransformerFactoryImpl");

        if (mPortTypeTaskStatic != null && mPortTypeTaskDynamic != null) {
            return;
        }

        this.mPortTypeTaskStatic = Util.loadTaskCommonPortType();
        this.mPortTypeTaskDynamic = Util.loadTaskDynamicPortType();

    }

    static class NamespaceContextImpl implements NamespaceContext {

        private final Map<String, String> taskListNSMap = new HashMap<String, String>();

        private final Map<String, String> taskListNSReverseMap = new HashMap<String, String>();

        public NamespaceContextImpl() {
            // TODO Auto-generated constructor stub
            taskListNSMap.put("jbi",
                    "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper");
            taskListNSMap.put("tm", "http://jbi.com.sun/wfse/wsdl/TaskCommon");
            taskListNSMap.put("po", "http://wlmse.sample/po");
            taskListNSReverseMap.put(
                    "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper", "jbi");
            taskListNSReverseMap.put("http://jbi.com.sun/wfse/wsdl/TaskCommon",
                    "tm");
            taskListNSReverseMap.put("http://wlmse.sample/po", "po");
        }

        public String getNamespaceURI(String prefix) {
            // TODO Auto-generated method stub
            return taskListNSMap.get(prefix);
        }

        public String getPrefix(String namespaceURI) {
            // TODO Auto-generated method stub
            return taskListNSReverseMap.get(namespaceURI);
        }

        public Iterator getPrefixes(String namespaceURI) {
            // TODO Auto-generated method stub
            return taskListNSReverseMap.keySet().iterator();
        }

    }
}