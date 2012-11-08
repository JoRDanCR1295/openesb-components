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
 * @(#)TaskManagerFactoryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;

import javax.xml.transform.Source;

import junit.framework.TestCase;

import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.clientapi.operations.TasklistResult;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.taskcommon.xmlbeans.GroupsType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus;
import com.sun.jbi.workflow.taskcommon.xmlbeans.UsersType;

/**
 *
 * @author radval
 */
public class TaskManagerFactoryTest extends TestCase {
    
    public TaskManagerFactoryTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        Util.initTaskManager();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getInstance method, of class com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory.
     */
    public void testGetInstance() throws Exception {
        System.out.println("getInstance");
        
        TaskManagerFactory expResult = null;
        TaskManagerFactory result = TaskManagerFactory.getInstance();
        assertNotNull("TaskManagerFactory should not be null", result);
        
        
    }

    /**
     * Test of newTaskManager method, of class com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory.
     */
    public void testNewTaskManager() throws Exception {
        System.out.println("newTaskManager");
        
        TaskManagerFactory instance = TaskManagerFactory.getInstance();
        
        
        TaskManager result = instance.getTaskManager();
        assertNotNull("TaskManager should not be null", result);
        
    }
    
    public void testCreateOneTask() throws Exception {
        TaskManagerFactory instance = TaskManagerFactory.getInstance();
        TaskManager result = instance.getTaskManager();
        
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput input = TaskModelFactory.getInstance().createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer (new Random().nextInt()).toString(), input, task);
        
        RuntimeTask task2 = result.getTask(task1.getId());
        
        assertEquals(task1, task2);
        
        
    }
    
    public void testAssignOneTask() throws Exception {
        TaskManagerFactory instance = TaskManagerFactory.getInstance();
        TaskManager result = instance.getTaskManager();
        
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);
        TaskInput input = TaskModelFactory.getInstance().createTaskInput(taskInput);
        RuntimeTask task1 = result.createTask(new Integer (new Random().nextInt()).toString(), input, task);
        
        RuntimeTask task2 = result.getTask(task1.getId());
        
        assertEquals(task1, task2);
        
        TaskPrincipal taskPrincipal = TaskModelFactory.getInstance().createPrincipal("radval", TaskPrincipal.PrincipalType.User);
        
        Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal> ();
        assignees.add(taskPrincipal);
        
        result.assignTask(task1, assignees, null);
        
        QueryType query = QueryType.Factory.newInstance();
        query.setType(QueryType.Type.FILTERED);
        UsersType users = UsersType.Factory.newInstance();
        GroupsType groups = GroupsType.Factory.newInstance();
        
        query.addTaskStatus(TaskStatus.ASSIGNED);
        query.addTaskStatus(TaskStatus.CLAIMED);
        query.addTaskStatus(TaskStatus.ESCALATED);
        
        users.addUser(taskPrincipal.getName());
        
        TasklistResult tasks = result.getAssignedTaskList(query, 0, 0);        

        assertTrue("should have one task assigned to user ", tasks.getTaskList().size() == 1);
        
        assertEquals("radval", tasks.getTaskList().iterator().next().getAssignedTo());
    }
    
  
}
