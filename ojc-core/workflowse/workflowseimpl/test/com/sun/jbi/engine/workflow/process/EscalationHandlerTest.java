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
 * @(#)EscalationHandlerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.transform.Source;

import junit.framework.TestCase;

import org.apache.xmlbeans.XmlDateTime;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.runtime.model.DefaultRuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.Task;

/**
 *
 * 
 */
public class EscalationHandlerTest extends TestCase {
    
    public EscalationHandlerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    	Util.initTaskManager();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of execute method, of class com.sun.jbi.engine.workflow.process.EscalationHandler.
     */
    public void testDeadline() throws Exception {
        System.out.println("testDeadline");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseEscalationDeadlineTest.wf";
        Element root = Util.loadElement(wfFileName);
        
        //define 1 second deadline from current date
        Calendar c1 = Calendar.getInstance();
        c1.add(Calendar.SECOND, 4);
        
        XmlDateTime d1 =  XmlDateTime.Factory.newInstance();
        d1.setCalendarValue(c1);
        
        Map<String, String> prefixToNSMap = new HashMap<String, String>();
        prefixToNSMap.put("wf", "http://jbi.com.sun/wfse");
        Util.replaceTextContent(root, "/wf:task/wf:escalation/wf:deadline",
       "'" +  d1.getStringValue() + "'", prefixToNSMap);

        Task task = Util.loadTask(root);
        assertNotNull("task should not be null "+ task);
        
                

        //get first escalation
        List<Escalation> escalations = task.getTaskEscalations();
        assertEquals("should have 1 escalation", 1, escalations.size());
        
        
        
        //load task input
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange1", tInput, task);
        
//        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt1 = new DefaultRuntimeTaskTimer(Long.valueOf("1"), escalation1, rTask);
//		rTask.addTaskTimer(rtt1);
//		
//        
//        //escalate task
//        EscalationHandler instance = new EscalationHandler(rtt1, taskManager);
//        
//        instance.execute();
        Thread.sleep(500);
        rTask = taskManager.getTask(rTask.getId());
        assertTrue("task state should not be escalated" ,rTask.getState() != RuntimeTask.TaskState.ESCALATED);
        Collection<TaskPrincipal> assignedTo = rTask.getAssignedTo();
        assertTrue("should have zero task principal ", assignedTo.size() == 0);
        
        List<RuntimeTaskTimer> rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have one active escalation timer" , rtts.size() == 1);
        
        
        
        Thread.sleep(5000);
        rTask = taskManager.getTask(rTask.getId());
        assertTrue("task state should be escalated" ,rTask.getState() == RuntimeTask.TaskState.ESCALATED);
        assignedTo = rTask.getAssignedTo();
        TaskPrincipal expectedPrincipal = TaskModelFactory.getInstance().createPrincipal("rwaldorf", TaskPrincipal.PrincipalType.User);
        
        assertTrue("should have one task principal ", assignedTo.size() == 1);
        assertEquals("task expected principal" ,expectedPrincipal, assignedTo.iterator().next());
        
        rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have zero active escalation timer" , rtts.size() == 0);
        
    }
    
    
    
    public void testTaskCompletedBeforeDeadline() throws Exception {
        System.out.println("testTaskCompletedBeforeDeadline");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseEscalationDeadlineTest.wf";
        Element root = Util.loadElement(wfFileName);
        
        //define 1 second deadline from current date
        Calendar c1 = Calendar.getInstance();
        c1.add(Calendar.SECOND, 2);
        
        XmlDateTime d1 =  XmlDateTime.Factory.newInstance();
        d1.setCalendarValue(c1);
        
        Map<String, String> prefixToNSMap = new HashMap<String, String>();
        prefixToNSMap.put("wf", "http://jbi.com.sun/wfse");
        Util.replaceTextContent(root, "/wf:task/wf:escalation/wf:deadline",
        "'" + d1.getStringValue() + "'", prefixToNSMap);

        Task task = Util.loadTask(root);
        assertNotNull("task should not be null "+ task);
        
                

        //get first escalation
        List<Escalation> escalations = task.getTaskEscalations();
        assertTrue("should have 1 escalation", escalations.size() == 1);
        
        
        
        //load task input
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange2", tInput, task);
        
//        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt1 = new DefaultRuntimeTaskTimer(Long.valueOf("1"), escalation1, rTask);
//		rTask.addTaskTimer(rtt1);
//		
//        
//        //escalate task
//        EscalationHandler instance = new EscalationHandler(rtt1, taskManager);
//        
//        instance.execute();
        Thread.sleep(500);
        rTask = taskManager.getTask(rTask.getId());
        assertTrue("task state should not be escalated" ,rTask.getState() != RuntimeTask.TaskState.ESCALATED);
        Collection<TaskPrincipal> assignedTo = rTask.getAssignedTo();
        assertTrue("should have zero task principal ", assignedTo.size() == 0);
        
        List<RuntimeTaskTimer> rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have one active escalation timer" , rtts.size() == 1);
        
        rTask.execute();
        Thread.sleep(200);        
        
        TaskPrincipal claimPrincipal = TaskModelFactory.getInstance().createPrincipal("radval", TaskPrincipal.PrincipalType.User);
        taskManager.claimTask(rTask.getId(), claimPrincipal);
        Thread.sleep(200);        
        taskManager.completeTask(rTask.getId(), claimPrincipal);
        Thread.sleep(1500);        
        
        rTask = taskManager.getTask(rTask.getId());
        assertTrue("task state should be completed" ,rTask.getState() == RuntimeTask.TaskState.COMPLETED);
//        assignedTo = rTask.getAssignedTo();
//        TaskPrincipal expectedPrincipal = TaskModelFactory.getInstance().createPrincipal("rwaldorf", TaskPrincipal.PrincipalType.User);
//        
//        assertTrue("should have one task principal ", assignedTo.size() == 1);
//        assertEquals("task expected principal" ,expectedPrincipal, assignedTo.iterator().next());
//      
        Thread.sleep(4000);        
        rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have zero active escalation timer" , rtts.size() == 0);
        
    }
    
    public void testDuration() throws Exception {
        System.out.println("duration");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseEscalationDurationTest.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        //get second escalation
        List<Escalation> escalations = task.getTaskEscalations();
        assertTrue("should have 2 escalation", escalations.size() == 1);
        
       
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange3", tInput, task);
//        
//        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt2 = new DefaultRuntimeTaskTimer(Long.valueOf("2"), escalation2, rTask);
//		rTask.addTaskTimer(rtt2);
//		
//        //escalate task
//        EscalationHandler instance = new EscalationHandler(rtt2, taskManager);
//        
//        instance.execute();
        
        Thread.sleep(4000);
        rTask = taskManager.getTask(rTask.getId());
        assertTrue("task state should be escalated" ,rTask.getState() == RuntimeTask.TaskState.ESCALATED);
        Collection<TaskPrincipal> assignedTo = rTask.getAssignedTo();
        assertTrue("should have one task principal ", assignedTo.size() == 1);
        TaskPrincipal expectedPrincipal = TaskModelFactory.getInstance().createPrincipal("rwaldorf", TaskPrincipal.PrincipalType.User);
        
        assertEquals("task expected principal" ,expectedPrincipal, assignedTo.iterator().next());
    }
    
    public void testDeadlineNegative() throws Exception {
        System.out.println("testDeadline");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseEscalationDeadlineNegativeTest.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        //get first escalation
        List<Escalation> escalations = task.getTaskEscalations();
        assertTrue("should have 1 escalation", escalations.size() == 1);
        
        //load task input
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        try {
        //create task
        taskManager.createTask("exchange4", tInput, task);
        } catch(Exception ex) {
        	assertTrue("should fail because deadline has elapsed", true);
        }

    }
    
    
    public void testDurationNegative() throws Exception {
        System.out.println("duration");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseEscalationDurationNegativeTest.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        //get second escalation
        List<Escalation> escalations = task.getTaskEscalations();
        assertTrue("should have 1 escalation", escalations.size() == 1);
        
       
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        try {
          taskManager.createTask("exchange5", tInput, task);
        } catch(Exception ex) {
        	assertTrue("should fail because duration has elapsed", true);
        }
    }

}
