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
 * @(#)TimeOutHandlerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
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
import com.sun.jbi.engine.workflow.runtime.model.DefaultRuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.Timeout;

/**
 *
 * 
 */
public class TimeOutHandlerTest extends TestCase {
    
    public TimeOutHandlerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    	Util.initTaskManager();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of execute method, of class com.sun.jbi.engine.workflow.process.TimeOutHandler.
     */
    public void testDeadline() throws Exception {
        System.out.println("testDeadline");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseTimeoutDeadlineTest.wf";
        Element root = Util.loadElement(wfFileName);
        
        //define 1 second deadline from current date
        Calendar c1 = Calendar.getInstance();
        c1.add(Calendar.SECOND, 4);
        
        XmlDateTime d1 =  XmlDateTime.Factory.newInstance();
        d1.setCalendarValue(c1);
        
        Map<String, String> prefixToNSMap = new HashMap<String, String>();
        prefixToNSMap.put("wf", "http://jbi.com.sun/wfse");
        Util.replaceTextContent(root, "/wf:task/wf:timeout/wf:deadline",
        "'" + d1.getStringValue() + "'", prefixToNSMap);

        Task task = Util.loadTask(root);
        assertNotNull("task should not be null "+ task);

        //get first timeout
        List<Timeout> timeouts = task.getTaskTimeouts();
        assertTrue("should have 1 timeouts", timeouts.size() == 1);
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange1", tInput, task);
        
        TaskHandlerManager handlerManager = TaskHandlerManager.getInstance();
        DefaultTaskHandlerListener listener = new DefaultTaskHandlerListener();
        handlerManager.addTaskListener(listener);
        
//        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt1 = new DefaultRuntimeTaskTimer(Long.valueOf("1"), timeout1, rTask);
//		rTask.addTaskTimer(rtt1);
//		
//        //execute timeout
//        TimeOutHandler instance = new TimeOutHandler(rtt1, handlerManager, taskManager);
//        
//        instance.execute();
        
        Thread.sleep(500);        
        rTask = taskManager.getTask(rTask.getId());
        List<RuntimeTaskTimer> rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have one active timeout timer" , rtts.size() == 1);
        assertFalse("task should not timeout" ,listener.isTimedOut());
        assertTrue("task state should not timeout" ,rTask.getState() != RuntimeTask.TaskState.EXPIRED);
        
        Thread.sleep(4000);        
        
        rTask = taskManager.getTask(rTask.getId());
        rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have zero active timeout timer" , rtts.size() == 0);
        assertTrue("task should have timedout" ,listener.isTimedOut());
        assertTrue("task state should have timedout" ,rTask.getState() == RuntimeTask.TaskState.EXPIRED);
    }
    
    public void testTaskCompletedBeforeDeadline() throws Exception {
        System.out.println("testDeadline");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseTimeoutDeadlineTest.wf";
        Element root = Util.loadElement(wfFileName);
        
        //define 2 second deadline from current date
        Calendar c1 = Calendar.getInstance();
        c1.add(Calendar.SECOND, 5);
        
        XmlDateTime d1 =  XmlDateTime.Factory.newInstance();
        d1.setCalendarValue(c1);
        
        Map<String, String> prefixToNSMap = new HashMap<String, String>();
        prefixToNSMap.put("wf", "http://jbi.com.sun/wfse");
        Util.replaceTextContent(root, "/wf:task/wf:timeout/wf:deadline",
        "'" + d1.getStringValue() + "'", prefixToNSMap);

        Task task = Util.loadTask(root);
        assertNotNull("task should not be null "+ task);

        //get first timeout
        List<Timeout> timeouts = task.getTaskTimeouts();
        assertTrue("should have 1 timeouts", timeouts.size() == 1);
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange2", tInput, task);
        
        TaskHandlerManager handlerManager = TaskHandlerManager.getInstance();
        DefaultTaskHandlerListener listener = new DefaultTaskHandlerListener();
        handlerManager.addTaskListener(listener);
        
//        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt1 = new DefaultRuntimeTaskTimer(Long.valueOf("1"), timeout1, rTask);
//		rTask.addTaskTimer(rtt1);
//		
//        //execute timeout
//        TimeOutHandler instance = new TimeOutHandler(rtt1, handlerManager, taskManager);
//        
//        instance.execute();
        Thread.sleep(500);        
        rTask = taskManager.getTask(rTask.getId());
        List<RuntimeTaskTimer> rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have one active timeout timer" , rtts.size() == 1);
        assertFalse("task should not timeout" ,listener.isTimedOut());
        assertTrue("task state should not timeout" ,rTask.getState() != RuntimeTask.TaskState.EXPIRED);
        
        rTask.execute();
        Thread.sleep(500);        
        
        TaskPrincipal claimPrincipal = TaskModelFactory.getInstance().createPrincipal("radval", TaskPrincipal.PrincipalType.User);
        taskManager.claimTask(rTask.getId(), claimPrincipal);
        Thread.sleep(500);        
        taskManager.completeTask(rTask.getId(), claimPrincipal);
        Thread.sleep(500);        
        
        rTask = taskManager.getTask(rTask.getId());
        rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have zero active timeout timer" , rtts.size() == 0);
        //wait till timer deadline and make sure task is not timed out
        Thread.sleep(5000);        
        assertFalse("task should not timedout" ,listener.isTimedOut());
        assertTrue("task state should have completed" ,rTask.getState() == RuntimeTask.TaskState.COMPLETED);
        
    }
    
    public void testDuration() throws Exception {
        System.out.println("duration");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseTimeoutDurationTest.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        //get second timeout
        List<Timeout> timeouts = task.getTaskTimeouts();
        assertTrue("should have 1 timeouts", timeouts.size() == 1);
        
       
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange3", tInput, task);
        
        TaskHandlerManager handlerManager = TaskHandlerManager.getInstance();
        DefaultTaskHandlerListener listener = new DefaultTaskHandlerListener();
        handlerManager.addTaskListener(listener);
        
//        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt2 = new DefaultRuntimeTaskTimer(Long.valueOf("2"), timeout2, rTask);
//		rTask.addTaskTimer(rtt2);
//		
//        //execute timeout
//        TimeOutHandler instance = new TimeOutHandler(rtt2, handlerManager, taskManager);
//        
//        instance.execute();
        
        Thread.sleep(500);        
        rTask = taskManager.getTask(rTask.getId());
        List<RuntimeTaskTimer> rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have one active timeout timer" , rtts.size() == 1);
        assertFalse("task should not timeout" ,listener.isTimedOut());
        assertTrue("task state should not timeout" ,rTask.getState() != RuntimeTask.TaskState.EXPIRED);
        
        Thread.sleep(2000);        
        
        rTask = taskManager.getTask(rTask.getId());
        rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have zero active timeout timer" , rtts.size() == 0);
        assertTrue("task should have timedout" ,listener.isTimedOut());
        assertTrue("task state should have timedout" ,rTask.getState() == RuntimeTask.TaskState.EXPIRED);
    }
    
    
    public void testTaskCompletedBeforeDuration() throws Exception {
        System.out.println("duration");
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseTimeoutDurationTest.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        //get second timeout
        List<Timeout> timeouts = task.getTaskTimeouts();
        assertTrue("should have 1 timeouts", timeouts.size() == 1);
        
       
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange4", tInput, task);
        
        TaskHandlerManager handlerManager = TaskHandlerManager.getInstance();
        DefaultTaskHandlerListener listener = new DefaultTaskHandlerListener();
        handlerManager.addTaskListener(listener);
        
//        //add RuntimeTaskTimer
//        RuntimeTaskTimer rtt2 = new DefaultRuntimeTaskTimer(Long.valueOf("2"), timeout2, rTask);
//		rTask.addTaskTimer(rtt2);
//		
//        //execute timeout
//        TimeOutHandler instance = new TimeOutHandler(rtt2, handlerManager, taskManager);
//        
//        instance.execute();
        Thread.sleep(200);        
        rTask = taskManager.getTask(rTask.getId());
        List<RuntimeTaskTimer> rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have one active timeout timer" , rtts.size() == 1);
        assertFalse("task should not timeout" ,listener.isTimedOut());
        assertTrue("task state should not timeout" ,rTask.getState() != RuntimeTask.TaskState.EXPIRED);
        
        rTask.execute();
        Thread.sleep(200);        
        
        TaskPrincipal claimPrincipal = TaskModelFactory.getInstance().createPrincipal("radval", TaskPrincipal.PrincipalType.User);
        taskManager.claimTask(rTask.getId(), claimPrincipal);
        Thread.sleep(200);        
        taskManager.completeTask(rTask.getId(), claimPrincipal);
        Thread.sleep(200);        
        
        rTask = taskManager.getTask(rTask.getId());
        rtts = taskManager.getActiveTimers(rTask.getId());
        assertTrue("task should have zero active timeout timer" , rtts.size() == 0);
        
        Thread.sleep(1000);        
        //wait till timer duration and make sure task is not timed out
        assertFalse("task should not have timedout" ,listener.isTimedOut());
        assertTrue("task state should have completed" ,rTask.getState() == RuntimeTask.TaskState.COMPLETED);
        
    }
}
