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
 * @(#)$Id: TestPerformance.java,v 1.3 2010/02/15 19:24:50 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi;

import java.security.Principal;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.security.auth.Subject;
import javax.sql.DataSource;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.transform.Source;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;

import sun.security.acl.PrincipalImpl;

import com.sun.jbi.engine.workflow.EnginePropertyConstants;
import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactoryTest.NamespaceContextImpl;
import com.sun.jbi.engine.workflow.clientapi.operations.TasklistResult;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.db.opt.DBOperation;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.runtime.model.impl.PersistenceTaskManagerImpl;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.model.Task;

import junit.framework.TestCase;

public class TestPerformance extends TestCase {

    private PortType mPortTypeTaskStatic;

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public TestPerformance(String name) {
        super(name);
        // TODO Auto-generated constructor stub
    }

    private void init() throws Exception {
        // System.setProperty("javax.xml.transform.TransformerFactory",
        // "com.sun.org.apache.xalan.internal.xsltc.trax.TransformerFactoryImpl");

        if (mPortTypeTaskStatic != null) {
            return;
        }

        this.mPortTypeTaskStatic = Util.loadTaskCommonPortType();
    }

    /**
     * Test of newGetTaskListOperation method, of class
     * com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory.
     */
    public void testGetTaskListPaginationOperation() throws Exception {
        Util.initTaskManager(true);
        init();
        System.out.println("GetTaskListPaginationOperation");

        // Test when the tasklist is null
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        Element input = Util.loadGetTaskListInputElement(0, 20);
        Principal principal = new PrincipalImpl("workflow0");
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

        for (int i = 0; i < 25000; i++) {
            int index = i % 50;
            TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
                    .createPrincipal("workflow" + index,
                            TaskPrincipal.PrincipalType.User);
//          TaskPrincipal taskPrincipal = TaskModelFactory.getInstance()
//          .createPrincipal("workflow1",
//                  TaskPrincipal.PrincipalType.User);            
            Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal>();
            assignees.add(taskPrincipal);
            task1 = result.createTask(exchangeId, createTaskInput, task);
            result.assignTask(task1, assignees, null);
            exchangeId = new Integer(new Random().nextInt()).toString();
        }

    }
    
    public void testCreateIndex () throws Exception {
        Util.initTaskManager(false);
        init();
        System.out.println("testCreateIndex");
        long before = System.currentTimeMillis();
        TaskManagerFactory.getInstance().getTaskManager().createFullTextIndex();
        long after = System.currentTimeMillis();
        long duration = after - before;
        System.out.println("Index total:" + duration) ;
        System.out.println("Index each:" + duration/20008) ;
    }
    
    public void testFullTextSearch () throws Exception {
        Util.initTaskManager(false);
        init();
        System.out.println("testFullTextSearch");
        long before = System.currentTimeMillis();
//        TaskManagerFactory.getInstance().getTaskManager().testSearch();
        long after = System.currentTimeMillis();
        long duration = after - before;
        System.out.println("search total:" + duration) ;
    }

    public void testPerf() throws Exception {
        Util.initTaskManager(false);
        init();
        System.out.println("TestPerf");

        // GetTaskList
        StaticOperationFactory instance = new StaticOperationFactory();
        Operation operation = mPortTypeTaskStatic.getOperation("GetTaskList",
                null, null);
        Element input = Util.loadGetTaskListInputElement(0, 20);
        Principal principal = new PrincipalImpl("workflow0");
//        Principal principal = new PrincipalImpl("john");
        Set<Principal> principals = new HashSet<Principal>();
        principals.add(principal);
        Subject subject = new Subject(true, principals, new HashSet(),
                new HashSet());
        StaticOperation taskOp = instance.newGetTaskListOperation(operation,
                input, subject);
        taskOp.execute();
        Element taskOutputResult = taskOp.getOutput().getReply();
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

        assertEquals(20, ((Double) resultObject).intValue());
        assertEquals(20, ((Double) returnRecords).intValue());
//        assertEquals(500, ((Double) totalRecords).intValue());

        long total_getTask = 0;
        long total_claimTask = 0;
        long total_getTaskInput = 0;
        long total_setTaskOutput = 0;
        long total_completeTask = 0;

        for (int i = 0; i < 100; i++) {
            int index = i % 50;
            principal = new PrincipalImpl("workflow" + index);
//            principal = new PrincipalImpl("john");
            principals = new HashSet<Principal>();
            principals.add(principal);

            
            Principal group1 = new GroupImpl("group1");
            principals.add(group1);
            
            Principal group2 = new GroupImpl("group2");
            principals.add(group2);       
            
            Principal group3 = new GroupImpl("group3");
            principals.add(group3);            
              
            Principal group4 = new GroupImpl("group4");
            principals.add(group4);         
            
            Principal group5 = new GroupImpl("group5");
            principals.add(group5);
            
            Principal group6 = new GroupImpl("group6");
            principals.add(group6);       
            
            Principal group7 = new GroupImpl("group7");
            principals.add(group7);            
              
            Principal group8 = new GroupImpl("group8");
            principals.add(group8);               
            subject = new Subject(true, principals, new HashSet(),
                    new HashSet());

            operation = mPortTypeTaskStatic.getOperation("GetTaskList", null,
                    null);
            input = Util.loadGetTaskListInputElement(0, 20);

            long before = System.currentTimeMillis();
            taskOp = instance
                    .newGetTaskListOperation(operation, input, subject);
            taskOp.execute();
            taskOutputResult = taskOp.getOutput().getReply();
            long after = System.currentTimeMillis();

            total_getTask = total_getTask + (after - before);

            javax.xml.xpath.XPathExpression idExpr = xpath
                    .compile("//tm:taskId[1]");
            int idObj = ((Double) idExpr.evaluate(taskOutputResult,
                    XPathConstants.NUMBER)).intValue();

            // claim task
            operation = mPortTypeTaskStatic.getOperation("ClaimTask", null,
                    null);
            input = Util.loadClaimTaskInputElement(idObj);

            before = System.currentTimeMillis();
            taskOp = instance.newClaimTaskOperation(operation, input, subject);
            taskOp.execute();
            taskOutputResult = taskOp.getOutput().getReply();
            after = System.currentTimeMillis();

            total_claimTask = total_claimTask + (after - before);

            assertNotNull(taskOutputResult);

            javax.xml.xpath.XPathExpression resultExp = xpath
                    .compile("//jbi:part[1]");
            String result = (String) resultExp.evaluate(taskOutputResult,
                    XPathConstants.STRING);
            assertEquals("SUCCESS", result);

            // get Input
            operation = mPortTypeTaskStatic.getOperation("GetTaskInput", null,
                    null);
            input = Util.loadGetTaskInputElement(idObj);

            before = System.currentTimeMillis();
            taskOp = instance.newGetTaskInputOperation(operation, input,
                    subject);
            taskOp.execute();
            taskOutputResult = taskOp.getOutput().getReply();
            after = System.currentTimeMillis();

            total_getTaskInput = total_getTaskInput + (after - before);

            assertNotNull(taskOutputResult);

            // set Output
            operation = mPortTypeTaskStatic.getOperation("SetTaskOutput", null,
                    null);
            input = Util
                    .loadInputMessageElement("/com/sun/jbi/engine/workflow/clientapi/testMessages/SetTaskOutput_Input.xml");

            before = System.currentTimeMillis();
            taskOp = instance.newSetTaskOutputOperation(operation, input,
                    subject);
            taskOp.execute();
            taskOutputResult = taskOp.getOutput().getReply();
            after = System.currentTimeMillis();

            total_setTaskOutput = total_setTaskOutput + (after - before);

            assertNotNull(taskOutputResult);
            result = (String) resultExp.evaluate(taskOutputResult,
                    XPathConstants.STRING);
            assertEquals("SUCCESS", result);

            // complete Task
            operation = mPortTypeTaskStatic.getOperation("CompleteTask", null,
                    null);
            input = Util.loadCompleteTaskInputElement(idObj);
            before = System.currentTimeMillis();
            taskOp = instance.newCompleteTaskOperation(operation, input,
                    subject);
            taskOp.execute();
            taskOutputResult = taskOp.getOutput().getReply();
            after = System.currentTimeMillis();

            total_completeTask = total_completeTask + (after - before);

            assertNotNull(taskOutputResult);
            result = (String) resultExp.evaluate(taskOutputResult,
                    XPathConstants.STRING);
            assertEquals("SUCCESS", result);
        }
        double getTaskAver = 0;
        double claimTaskAver = 0;
        double getTaskInputAver = 0;
        double setTaskOutputAver = 0;
        double completeTaskAver = 0;

        getTaskAver = ((double) total_getTask) / 100.0;
        claimTaskAver = ((double) total_claimTask) / 100.0;
        getTaskInputAver = ((double) total_getTaskInput) / 100.0;
        setTaskOutputAver = ((double) total_setTaskOutput) / 100.0;
        completeTaskAver = ((double) total_completeTask) / 100.0;
        
        System.out.println("Get Task Average: " + getTaskAver);
        System.out.println("Claim Task Average: " + claimTaskAver);
        System.out.println("Get Task Input: " + getTaskInputAver);
        System.out.println("Set Task Output: " + setTaskOutputAver);
        System.out.println("Complete Task: " + completeTaskAver);
    }
}
