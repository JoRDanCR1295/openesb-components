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
 * @(#)HandlerFactoryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.List;

import javax.xml.transform.Source;

import junit.framework.TestCase;

import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.runtime.model.DefaultRuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.workflow.model.DeadlineOrDuration;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.Timeout;

/**
 *
 * @author radval
 */
public class HandlerFactoryTest extends TestCase {
    
    public HandlerFactoryTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    	Util.initTaskManager();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getInstance method, of class com.sun.jbi.engine.workflow.process.HandlerFactory.
     */
    public void testGetInstance() throws Exception {
        System.out.println("getInstance");
        
        HandlerFactory result = HandlerFactory.getInstance();
        assertNotNull(result);
        
    }

    /**
     * Test of newHandler method, of class com.sun.jbi.engine.workflow.process.HandlerFactory.
     */
    public void testNewHandler() throws Exception {
        System.out.println("newHandler");
        
        
        ModelElement modelElement = null;
        TaskHandlerManager handerManager = null;
        HandlerFactory instance = new HandlerFactory();
        
        TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
        
        //load task definition
        String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
        Task task = Util.loadTask(wfFileName);
        assertNotNull("task should not be null "+ task);
        
        //get first timeout
        List<Timeout> timeouts = task.getTaskTimeouts();
        assertTrue("should have 2 timeouts", timeouts.size() == 2);
        
        Timeout timeout1 = timeouts.get(0);
       
        //get first escalation
        List<Escalation> escalations = task.getTaskEscalations();
        assertTrue("should have 2 escalation", escalations.size() == 2);
        Escalation escalation1 = escalations.get(0);
        

        
        //load task input
        String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
        Source taskInput = Util.loadTaskInput(taskInputFileName);     
        
        TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
        
        //create task
        RuntimeTask rTask = taskManager.createTask("exchange1", tInput, task);

        //add RuntimeTaskTimer
        RuntimeTaskTimer rtt1 = new DefaultRuntimeTaskTimer(Long.valueOf("1"), timeout1, taskManager, rTask.getId());
//		rTask.addTaskTimer(rtt1);
		
		RuntimeTaskTimer rtt2 = new DefaultRuntimeTaskTimer(Long.valueOf("2"), escalation1, taskManager, rTask.getId());
//		rTask.addTaskTimer(rtt2);
		
        Handler result1 = instance.newHandler(rtt1, handerManager, taskManager);
        assertEquals("should get timeout handler", TimeOutHandler.class.getName(), result1.getClass().getName());
        
        Handler result2 = instance.newHandler(rtt2, handerManager, taskManager);
        assertEquals("should get escalation handler", EscalationHandler.class.getName(), result2.getClass().getName());
       
    }
    
}
