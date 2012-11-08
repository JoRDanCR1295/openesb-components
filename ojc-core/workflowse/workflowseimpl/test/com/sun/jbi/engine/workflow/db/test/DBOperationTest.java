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
 * @(#)DBOperationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.test;

import java.net.URL;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.sql.DataSource;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import junit.framework.TestCase;

import org.apache.xmlbeans.XmlDateTime;
import org.apache.xmlbeans.XmlString;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.TaskItem;
import com.sun.jbi.engine.workflow.clientapi.Util;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.db.connection.ConnectionException;
import com.sun.jbi.engine.workflow.db.dao.DAOFactory;
import com.sun.jbi.engine.workflow.db.hibernate.TaskInstance;
import com.sun.jbi.engine.workflow.db.hibernate.TaskTimer;
import com.sun.jbi.engine.workflow.db.opt.DBOperation;
import com.sun.jbi.engine.workflow.runtime.model.DefaultRuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.taskcommon.xmlbeans.GroupsType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus;
import com.sun.jbi.workflow.taskcommon.xmlbeans.UsersType;


public class DBOperationTest extends TestCase {
    
    private DBOperation dbOperation;
    
    private Properties mProps;
    
    private DataSource mDS;

    public DBOperationTest(String name) throws SQLException {
        super(name);    
        mProps = DBUtil.getDBProperties();
        mDS = DAOFactory.getDataSource(mProps, DAOFactory.getDBType(mProps.getProperty("DB_Type")));
    }
    
    protected void setUp() throws Exception {

        dbOperation = new DBOperation (mDS, mProps.getProperty("DB_Type"), true, "WORKFLOW", null, false);
    }
    

    public void testDbOperation () {
        
        System.out.println("testDbOperation"); 
        try {
            //Create 2 tasks and check the returned id, since MEIds are randomly created, 2 records should be inserted into 
            //TASK_INSTANCE table, and the runtimeTask's id is updated
            String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchase.wf";
            Task task = Util.loadTask(wfFileName);
            assertNotNull("task should not be null "+ task);
            String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
            Element taskInputEl = Util.loadInputMessageElement(taskInputFileName);    

            TaskInput taskInput = TaskModelFactory.getInstance().createTaskInput(taskInputEl);
            RuntimeTask runtimeTask1 = new DefaultRuntimeTask(new Long(0), Util.getRandomME(), taskInput, task);
            RuntimeTask runtimeTask2 = new DefaultRuntimeTask(new Long(0), Util.getRandomME(), taskInput, task);
            
            List<TaskTimer> timers1 = dbOperation.createTask(runtimeTask1);
            List<TaskTimer> timers2 = dbOperation.createTask(runtimeTask2);
            
            //Check the modified runtimeTask Id
            assertEquals(new Long (1), runtimeTask1.getId());
            assertEquals(new Long (2), runtimeTask2.getId());
            
            //Check the timeout/escalation
            assertEquals(4, timers1.size());
            assertEquals(4, timers2.size());
            
            assertEquals(1, timers1.get(0).getId());
            assertEquals(8, timers2.get(3).getId());
            
            
            TaskPrincipal taskPrincipal = TaskModelFactory.getInstance().createPrincipal("radval", TaskPrincipal.PrincipalType.User);
            TaskPrincipal taskPrincipal2 = TaskModelFactory.getInstance().createPrincipal("workflow", TaskPrincipal.PrincipalType.User);
            TaskPrincipal taskPrincipal3 = TaskModelFactory.getInstance().createPrincipal("rwaldorf", TaskPrincipal.PrincipalType.User);
            TaskPrincipal taskPrincipal4 = TaskModelFactory.getInstance().createPrincipal("meiwu", TaskPrincipal.PrincipalType.User);
            
            TaskPrincipal taskPrincipal5 = TaskModelFactory.getInstance().createPrincipal("user5", TaskPrincipal.PrincipalType.User);
            
            TaskPrincipal taskPrincipalGroup1 = TaskModelFactory.getInstance().createPrincipal("group1", TaskPrincipal.PrincipalType.Group);
            
            
            //Assign the tasks
            Collection<TaskPrincipal> assignees = new ArrayList<TaskPrincipal> ();
            assignees.add(taskPrincipal);
            assignees.add(taskPrincipal5);
 
            dbOperation.assignTask(runtimeTask1, assignees, null);
            dbOperation.assignTask(runtimeTask2, assignees, null);
            
//            //Get TaskList 
//            Set<TaskInstance> tasks = dbOperation.getAssignedTaskList(taskPrincipal);
//            assertEquals(2, tasks.size());
            
            
            //Get TaskList using Query
            QueryType query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            UsersType users = query.addNewUsers();
            XmlString user = users.addNewUser();
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
            setText (user, taskPrincipal.getName());
            List<TaskItem> tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
            assertEquals(2, tasks2.size());
            

            query = QueryType.Factory.newInstance();
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
            users = query.addNewUsers();
            user = users.addNewUser();
            setText (user, taskPrincipal5.getName());
             tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
             assertEquals(2, tasks2.size());
             
             query = QueryType.Factory.newInstance();
             query.setType(QueryType.Type.FILTERED);
             query.addTaskStatus(TaskStatus.ASSIGNED);
             query.addTaskStatus(TaskStatus.CLAIMED);
             query.addTaskStatus(TaskStatus.ESCALATED);
             users = query.addNewUsers();
             user = users.addNewUser();
             setText (user, taskPrincipal2.getName());
              tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
              assertEquals(0, tasks2.size());
              
            //2. Query with users (=2)
              query = QueryType.Factory.newInstance();
              query.setType(QueryType.Type.FILTERED);
              query.addTaskStatus(TaskStatus.ASSIGNED);
              query.addTaskStatus(TaskStatus.CLAIMED);
              query.addTaskStatus(TaskStatus.ESCALATED);
              users = query.addNewUsers();
              user = users.addNewUser();
              setText (user, taskPrincipal.getName());
              user = users.addNewUser();
              setText (user, taskPrincipal5.getName());
               tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
               assertEquals(2, tasks2.size());
               
              RuntimeTask runtimeTask3 = new DefaultRuntimeTask(new Long(0), Util.getRandomME(), taskInput, task);
              dbOperation.createTask(runtimeTask3);
              Collection<TaskPrincipal> assignees2 = new ArrayList<TaskPrincipal> ();
              assignees2.add(taskPrincipal5);
              dbOperation.assignTask(runtimeTask3, assignees2, null);
              tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
              assertEquals(3, tasks2.size());
             
              
              //3. Query with user and group
              RuntimeTask runtimeTask4 = new DefaultRuntimeTask(new Long(0), Util.getRandomME(), taskInput, task);
              dbOperation.createTask(runtimeTask4);
              Collection<TaskPrincipal> assignees3 = new ArrayList<TaskPrincipal> ();
              assignees3.add(taskPrincipalGroup1);
              dbOperation.assignTask(runtimeTask4, assignees3, null);
              GroupsType groups = query.addNewGroups();
              XmlString group = groups.addNewGroup();
              setText (group, taskPrincipalGroup1.getName());
              tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
              assertEquals(4, tasks2.size());              
              
              //4. Query status
              query = QueryType.Factory.newInstance();
              query.setType(QueryType.Type.FILTERED);
              query.addTaskStatus(TaskStatus.ASSIGNED);
              tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
              assertEquals(4, tasks2.size());
              
              query = QueryType.Factory.newInstance();
              query.setType(QueryType.Type.FILTERED);
              query.addTaskStatus(TaskStatus.CLAIMED);
              tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
              assertEquals(0, tasks2.size());
              
              query = QueryType.Factory.newInstance();
              query.setType(QueryType.Type.FILTERED);
              query.addTaskStatus(TaskStatus.ASSIGNED);
              users = query.addNewUsers();
              user = users.addNewUser();
              setText (user, taskPrincipal2.getName());              
              tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
              assertEquals(0, tasks2.size());              
              
            
              query = QueryType.Factory.newInstance();
              query.setType(QueryType.Type.FILTERED);
              query.addTaskStatus(TaskStatus.ASSIGNED);
              users = query.addNewUsers();
              user = users.addNewUser();
              setText (user, taskPrincipal2.getName()); 
              groups = query.addNewGroups();
               group = groups.addNewGroup();
              setText (group, taskPrincipalGroup1.getName());
              
              tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
              assertEquals(1, tasks2.size());              
                            
            //Claim Task
            dbOperation.claimTask(new Long(1), taskPrincipal);
            
            query = QueryType.Factory.newInstance();
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.CLAIMED);

            users = query.addNewUsers();
            user = users.addNewUser();
            setText (user, taskPrincipal.getName()); 
            groups = query.addNewGroups();
             group = groups.addNewGroup();
            setText (group, taskPrincipalGroup1.getName());
            
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
            assertEquals(1, tasks2.size());    
            
            query = QueryType.Factory.newInstance();
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);

            users = query.addNewUsers();
            user = users.addNewUser();
            setText (user, taskPrincipal.getName()); 
            groups = query.addNewGroups();
             group = groups.addNewGroup();
            setText (group, taskPrincipalGroup1.getName());
            
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
            assertEquals(2, tasks2.size());                
            
            
            query.addTaskStatus(TaskStatus.CLAIMED);            
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();
            assertEquals(3, tasks2.size());    
            
            //Test the task status after claimed
            //Get TaskList using Query
             query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
             users = query.addNewUsers();
             user = users.addNewUser();
            setText (user, taskPrincipal.getName());
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
            assertEquals(2, tasks2.size());
            for (TaskItem taskInst : tasks2) {
                if (taskInst.getId() == 1) {
                    assertEquals(RuntimeTask.TaskState.CLAIMED, taskInst.getState());
                }
            }            
            
            //Test reAssign
            List<TaskPrincipal> newPrincipals = new ArrayList<TaskPrincipal>();
            newPrincipals.add(taskPrincipal2);
            
            dbOperation.reAssignTask(new Long(1),  newPrincipals, null);
            
            //Get task list
            //Get TaskList using Query
            query = QueryType.Factory.newInstance();
           //1. Query with users
           query.setType(QueryType.Type.FILTERED);
           query.addTaskStatus(TaskStatus.ASSIGNED);
           query.addTaskStatus(TaskStatus.CLAIMED);
           query.addTaskStatus(TaskStatus.ESCALATED);
            users = query.addNewUsers();
            user = users.addNewUser();
           setText (user, taskPrincipal.getName());
           tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
           assertEquals(1, tasks2.size());
//           for (TaskItem taskInst : tasks2) {
//               if (taskInst.getId() == 1) {
//                   assertEquals(new Integer (RuntimeTask.TaskState.CLAIMED.getState()), taskInst.getState());
//               }
//           }      
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal);
//            assertEquals(1, tasks.size());       
            
           query = QueryType.Factory.newInstance();
           //1. Query with users
           query.setType(QueryType.Type.FILTERED);
           query.addTaskStatus(TaskStatus.ASSIGNED);
           query.addTaskStatus(TaskStatus.CLAIMED);
           query.addTaskStatus(TaskStatus.ESCALATED);
            users = query.addNewUsers();
            user = users.addNewUser();
           setText (user, taskPrincipal2.getName());
           tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
           assertEquals(1, tasks2.size());           
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal2);
//            assertEquals(1, tasks.size());               
            
            //TimeoutTask
            List<TaskTimer>  cancelTimers = dbOperation.timeoutTask(new Long(1), new Long (1));
            assertEquals(3, cancelTimers.size());
            
            
            //Get Task input
            String taskInputStr = dbOperation.getTaskInput(new Long (1));
             
            assertTrue(Util.compareElements(
                    taskInputEl, 
                    Util.loadString(taskInputStr)));
            
            //Set Task ouput
            URL url = Util.class.getResource(taskInputFileName);            

            dbOperation.setTaskOutput(new Long (1), Util.readFile(url.getFile()));
            
            //Get Task output
            String taskOutputStr = dbOperation.getTaskOutput(new Long (1));
             
            assertTrue(Util.compareElements(
            		 Util.loadString(Util.readFile(url.getFile())), 
                    Util.loadString(taskOutputStr)));            
            
            //Get task list after timeout
            query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
             users = query.addNewUsers();
             user = users.addNewUser();
            setText (user, taskPrincipal.getName());
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
            assertEquals(1, tasks2.size());  
            
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal);
//            assertEquals(1, tasks.size());       
            
            query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
             users = query.addNewUsers();
             user = users.addNewUser();
            setText (user, taskPrincipal2.getName());
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
            assertEquals(0, tasks2.size());              
            
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal2);
//            assertEquals(0, tasks.size());                   
            
            //Escalate task
            List <TaskPrincipal> newAssignees = new ArrayList<TaskPrincipal>  ();
            newAssignees.add(taskPrincipal3);
            newAssignees.add(taskPrincipal4);
            dbOperation.escalateTask(new Long (2), new Long (5), newAssignees, null);
                        
            //Get task list after escalation
            query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
             users = query.addNewUsers();
             user = users.addNewUser();
            setText (user, taskPrincipal.getName());
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
            assertEquals(1, tasks2.size());    
            
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal);
//            assertEquals(1, tasks.size());       
            
            query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);            
             users = query.addNewUsers();
             user = users.addNewUser();
            setText (user, taskPrincipal2.getName());
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
            assertEquals(0, tasks2.size());    
            
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal2);
//            assertEquals(0, tasks.size());                  

            query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
             users = query.addNewUsers();
             user = users.addNewUser();
            setText (user, taskPrincipal3.getName());
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
            assertEquals(1, tasks2.size());                
            
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal3);
//            assertEquals(1, tasks.size()); 
            
            //Claim Task
            dbOperation.claimTask(new Long(2), taskPrincipal3);            
            
            
            //Complete Task
            List<TaskTimer> timers = dbOperation.getActiveTimers(new Long (2));
            assertEquals(3, timers.size());
            List<TaskTimer> toCancel = dbOperation.completeTask(new Long (2), taskPrincipal3);
            assertEquals(3, toCancel.size());
            
            TaskInstance taskInstance = dbOperation.getTaskInstance(new Long (2));
             timers = dbOperation.getActiveTimers(taskInstance.getId());
            assertEquals(0, timers.size());
            for (TaskTimer timer : timers) {
                if (timer.getId() == 5) {
                    assertEquals(new Integer(2), timer.getStatus());
                } else {
                    assertEquals(new Integer(3), timer.getStatus());
                }
            }
            
            //Get task list
            query = QueryType.Factory.newInstance();
            //1. Query with users
            query.setType(QueryType.Type.FILTERED);
            query.addTaskStatus(TaskStatus.ASSIGNED);
            query.addTaskStatus(TaskStatus.CLAIMED);
            query.addTaskStatus(TaskStatus.ESCALATED);
             users = query.addNewUsers();
             user = users.addNewUser();
            setText (user, taskPrincipal3.getName());
            tasks2 =  dbOperation.getAssignedTaskList (query, -1, -1).getTaskList();            
            assertEquals(0, tasks2.size()); 
            
//            tasks = dbOperation.getAssignedTaskList(taskPrincipal3);
//            assertEquals(0, tasks.size());            

        } catch (ConnectionException e) {
            // TODO Auto-generated catch block
            fail(e.getMessage());
            e.printStackTrace();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            fail(e.getMessage());
            e.printStackTrace();
        }

    }
    
        private void setText(XmlString node, String text) {
        // TODO Auto-generated method stub
         node.setStringValue(text);
        
    }

        public void testRecovery () {
            System.out.println("testRecovery");            
          
            try {
                TaskManager taskManager = TaskManagerFactory.getInstance().getTaskManager();
            //load task definition
            String wfFileName = "/com/sun/jbi/engine/workflow/clientapi/testMessages/purchaseOrder/ApprovePurchaseEscalationDeadlineTest.wf";
            Element root = Util.loadElement(wfFileName);
            
            //define 1 second deadline from current date
            Calendar c1 = Calendar.getInstance();
            c1.add(Calendar.SECOND, 10);
            
            XmlDateTime d1 =  XmlDateTime.Factory.newInstance();
            d1.setCalendarValue(c1);
            
            Map<String, String> prefixToNSMap = new HashMap<String, String>();
            prefixToNSMap.put("wf", "http://jbi.com.sun/wfse");
            Util.replaceTextContent(root, "/wf:task/wf:escalation/wf:deadline",
            "'" + d1.getStringValue() + "'", prefixToNSMap);

            Task task = Util.loadTask(root);
            assertNotNull("task should not be null "+ task);
            
            //load task input
            String taskInputFileName = "/com/sun/jbi/engine/workflow/runtime/model/testData/purchaseOrderInput.xml";
            Source taskInput = Util.loadTaskInput(taskInputFileName);     
            
            TaskInput tInput = TaskModelFactory.getInstance().createTaskInput(taskInput);
            
            //create task
            RuntimeTask runtimeTask1 = new DefaultRuntimeTask(new Long(0), Util.getRandomME(), tInput, task);
            
            dbOperation.createTask(runtimeTask1);
            String targetNs =  task.getTargetNamespace();
            QName qname = new QName(targetNs, task.getName());
            Map<Long, List<TaskTimer>>  timers = dbOperation.recoverTimers(qname);
            assertEquals(1 , timers.size());
           Map.Entry<Long, List<TaskTimer>> entry =  timers.entrySet().iterator().next();
           
           assertEquals(1 , entry.getValue().size());
           Long taskId = entry.getValue().get(0).getTaskInstance().getId();
            assertEquals(runtimeTask1.getId().intValue(), taskId.intValue());
            } catch (ConnectionException e) {
                // TODO Auto-generated catch block
                fail(e.getMessage());
                e.printStackTrace();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                fail(e.getMessage());
                e.printStackTrace();
            }            
        }

}
