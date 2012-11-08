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
 * @(#)TaskHandlerManagerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.transform.Source;

import junit.framework.TestCase;

import org.apache.xmlbeans.XmlDateTime;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.Task;

/**
 *
 * @author radval
 */
public class TaskHandlerManagerTest extends TestCase {
    
    public TaskHandlerManagerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    	Util.initTaskManager();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getInstance method, of class com.sun.jbi.engine.workflow.process.TaskHandlerManager.
     */
    public void testGetInstance() throws Exception {
        System.out.println("getInstance");
        
        TaskHandlerManager result = TaskHandlerManager.getInstance();
        assertNotNull(result);
        
    }

    /**
     * Test of processHandler method, of class com.sun.jbi.engine.workflow.process.TaskHandlerManager.
     */
    public void testProcessHandler() throws Exception {
        System.out.println("processHandler");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseTimeoutEscalationTest.wf";
        Element root = Util.loadElement(wfFileName);
        
        //define 1 second deadline from current date
        Calendar c1 = Calendar.getInstance();
        c1.add(Calendar.SECOND, 3);
        
        XmlDateTime d1 =  XmlDateTime.Factory.newInstance();
        d1.setCalendarValue(c1);
        
        Map<String, String> prefixToNSMap = new HashMap<String, String>();
        prefixToNSMap.put("wf", "http://jbi.com.sun/wfse");
        Util.replaceTextContent(root, "/wf:task/wf:escalation/wf:deadline",
       "'" + d1.getStringValue() + "'", prefixToNSMap);

        Task task = Util.loadTask(root);
        assertNotNull("task should not be null "+ task);

//      get first escalation
        List<Escalation> escalations = task.getTaskEscalations();
        assertTrue("should have 2 escalation", escalations.size() == 2);
        
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange1", tInput, task);
//        TaskHandlerManager instance = TaskHandlerManager.getInstance();
        
        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt1 = new DefaultRuntimeTaskTimer(Long.valueOf("1"), escalation1, rTask);
//		rTask.addTaskTimer(rtt1);
//		
//		RuntimeTaskTimer rtt2 = new DefaultRuntimeTaskTimer(Long.valueOf("2"), escalation2, rTask);
//		rTask.addTaskTimer(rtt2);
//		
//		
//		
//        instance.processHandler(rTask.getTaskTimers(), taskManager);
//        
        Thread.sleep(3500);
        rTask = taskManager.getTask(rTask.getId());
        assertTrue("task state should be escalated" ,rTask.getState() == RuntimeTask.TaskState.ESCALATED);
        Collection<TaskPrincipal> assignedTo = rTask.getAssignedTo();
        assertTrue("should have one task principal ", assignedTo.size() == 1);
        
        TaskPrincipal expectedPrincipal1 = TaskModelFactory.getInstance().createPrincipal("rwaldorf", TaskPrincipal.PrincipalType.User);
        
        assertEquals("task expected principal" ,expectedPrincipal1, assignedTo.iterator().next());
        
        Thread.sleep(2500);
        rTask = taskManager.getTask(rTask.getId());
        assertTrue("task state should be escalated" ,rTask.getState() == RuntimeTask.TaskState.ESCALATED);
        Collection<TaskPrincipal> assignedTo2 = rTask.getAssignedTo();
        assertTrue("should have two task principal ", assignedTo2.size() == 2);
        
        TaskPrincipal expectedPrincipal2 = TaskModelFactory.getInstance().createPrincipal("dale", TaskPrincipal.PrincipalType.User);
        
        Collection<TaskPrincipal> expectedAssignees = new ArrayList<TaskPrincipal> (2);
        expectedAssignees.add (expectedPrincipal1);
        expectedAssignees.add (expectedPrincipal2);
        
         for (Iterator<TaskPrincipal> newAssigneeIt = assignedTo2.iterator(); newAssigneeIt.hasNext();) {
             TaskPrincipal newAssignee = newAssigneeIt.next();
             for (TaskPrincipal expectedAssignee : expectedAssignees) {
                 if (newAssignee.equals(expectedAssignee)) {
                     expectedAssignees.remove(expectedAssignee);
                     newAssigneeIt.remove();
                     break;
                 }
             }
         }
        
         assertTrue(assignedTo2.size() == 0);
    }

//    /**
//     * Test of addTaskListener method, of class com.sun.jbi.engine.workflow.process.TaskHandlerManager.
//     */
//    public void testAddTaskListener() {
//        System.out.println("addTaskListener");
//        
//        TaskHandlerListener l = null;
//        TaskHandlerManager instance = null;
//        
//        instance.addTaskListener(l);
//        
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of removeTaskListener method, of class com.sun.jbi.engine.workflow.process.TaskHandlerManager.
//     */
//    public void testRemoveTaskListener() {
//        System.out.println("removeTaskListener");
//        
//        TaskHandlerListener l = null;
//        TaskHandlerManager instance = null;
//        
//        instance.removeTaskListener(l);
//        
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//
//    /**
//     * Test of notifyOnTimeout method, of class com.sun.jbi.engine.workflow.process.TaskHandlerManager.
//     */
//    public void testNotifyOnTimeout() {
//        System.out.println("notifyOnTimeout");
//        
//        Long taskId = null;
//        Element output = null;
//        TaskHandlerManager instance = null;
//        
//        instance.notifyOnTimeout(taskId, output);
//        
//        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
//    }
//    
}
