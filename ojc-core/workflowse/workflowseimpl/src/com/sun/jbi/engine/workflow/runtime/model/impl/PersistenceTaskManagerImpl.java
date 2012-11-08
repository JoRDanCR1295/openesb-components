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
 * @(#)PersistenceTaskManagerImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model.impl;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.EnginePropertyConstants;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;
import com.sun.jbi.engine.workflow.clientapi.operations.TasklistResult;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.common.model.impl.TaskOutputImpl;
import com.sun.jbi.engine.workflow.db.connection.ConnectionException;
import com.sun.jbi.engine.workflow.db.hibernate.TaskAssignee;
import com.sun.jbi.engine.workflow.db.hibernate.TaskInstance;
import com.sun.jbi.engine.workflow.db.hibernate.TaskTimer;
import com.sun.jbi.engine.workflow.db.opt.DBOperation;
import com.sun.jbi.engine.workflow.process.TaskHandlerManager;
import com.sun.jbi.engine.workflow.runtime.model.DefaultRuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.DefaultRuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerHelper;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.DeadlineOrDuration;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;

public class PersistenceTaskManagerImpl extends AbstractTaskManagerImpl {

    private static final Logger LOGGER = Logger
            .getLogger(PersistenceTaskManagerImpl.class.getName());

    private DBOperation mDBOperation;

    private WorkflowMapEntryTable mWorkflowMapEntryTable;

    private Map<Long, List<RuntimeTaskTimer>> mtaskIdToRuntimeTaskTimerss = new Hashtable<Long, List<RuntimeTaskTimer>>();

    public void assignTask(RuntimeTask task, Collection<TaskPrincipal> users, Collection<TaskPrincipal> excludedUsers)
            throws TaskException {
        // TODO Auto-generated method stub
        try {
            RuntimeTask.TaskState oldState = task.getState();
            mDBOperation.assignTask(task, users, excludedUsers);
            task = getTask(task.getId());
            RuntimeTask.TaskState newState = task.getState();
            fireTaskStateChange(oldState, newState, task);
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        }
    }

    public void claimTask(Long taskId, TaskPrincipal user) throws TaskException {
        // TODO Auto-generated method stub
        try {
            RuntimeTask task = getTask(taskId);
            RuntimeTask.TaskState oldState = task.getState();
            mDBOperation.claimTask(taskId, user);
            task = getTask(taskId);
            RuntimeTask.TaskState newState = task.getState();
            fireTaskStateChange(oldState, newState, task);
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        }

    }

    public void completeTask(Long taskId, TaskPrincipal user)
            throws TaskException {
        // TODO Auto-generated method stub
        try {
            RuntimeTask task = getTask(taskId);
            RuntimeTask.TaskState oldState = task.getState();
            unregisterTimers(task);
            List<TaskTimer> timers = mDBOperation.completeTask(taskId, user);
            task = getTask(taskId);
            if (task.getOutput() == null) {
                task.setOutput(new TaskOutputImpl());
            }           
            RuntimeTask.TaskState newState = task.getState();
            fireTaskStateChange(oldState, newState, task);          
            Element el = Util.makeJBIReply(task);
            notifyOnComplete(task.getExchangeId(), taskId, el);
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        } catch (Exception ex) {
            throw new TaskException(ex);
        }
    }
    
    public void unregisterTimers(RuntimeTask task) {
       List<RuntimeTaskTimer> taskTimers = mtaskIdToRuntimeTaskTimerss.get(task.getId());
       if (taskTimers != null && taskTimers.size() > 0) {
           Iterator<RuntimeTaskTimer> it = taskTimers.iterator();
           while (it.hasNext()) {
               RuntimeTaskTimer tt = it.next();
               tt.cancel();
               it.remove();
               removeActiveTimer(task.getId(), tt.getId());
           }
       }
    }
    
    public RuntimeTask createTask(String exchangeId, TaskInput input,
            Task taskMetaData) throws TaskException {
        // TODO Auto-generated method stub
        RuntimeTask task = null;
        try {
            Element taskXmlInput = TaskManagerHelper
                    .extractTaskInputFromJBIWrapper(taskMetaData, input
                            .getInput());
            TaskInput taskInput = TaskModelFactory.getInstance()
                    .createTaskInput(taskXmlInput);

            task = new DefaultRuntimeTask(new Long(0), exchangeId, taskInput,
                    taskMetaData);
            
            task.init(getContext());
            // Register timers in memory
            List<TaskTimer> taskTimers = mDBOperation.createTask(task);
            
            List<RuntimeTaskTimer> runtimeTimers = createAndAddRuntimeTaskTimer(taskMetaData, taskTimers, task.getId(), task);
            TaskHandlerManager hManager = TaskHandlerManager.getInstance();
            hManager.processTaskTimerHandler(runtimeTimers, this);
            hManager.processNotificationHandler(taskMetaData, this);

        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        } catch (Exception ex) {
            throw new TaskException(ex);
        }
        return task;
    }

    public TasklistResult getAssignedTaskList (QueryType query,  int startIndex, int pageSize)   throws TaskException {
        TasklistResult taskListResult = null;
        try {
            taskListResult = mDBOperation.getAssignedTaskList(query, startIndex, pageSize);
//            if (taskInstances != null) {
//                for (TaskInstance taskInstance : taskInstances) {
//                    TaskItem item = new TaskItem (taskInstance);
//                    taskList.add(item);
//                }
//            }
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        } catch (Exception ex) {
            throw new TaskException(ex);
        }
        return taskListResult;
    }
    
    public RuntimeTask getTask(Long taskId) throws TaskException {
        // TODO Auto-generated method stub
        RuntimeTask runtimeTask = null;
        try {
            TaskInstance taskInstance = mDBOperation.getTaskInstance(taskId);
            runtimeTask = convertToRuntimeTask(taskInstance);
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        } catch (Exception ex) {
            throw new TaskException(ex);
        }
        return runtimeTask;
    }

    public Element getTaskInput(Long taskId) throws TaskException {
        // TODO Auto-generated method stub
        Element taskInput = null;
        try {
            String taskInputStr = mDBOperation.getTaskInput(taskId);
            taskInput = XmlUtil.createDocumentFromXML(true, taskInputStr)
                    .getDocumentElement();
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        } catch (Exception ex) {
            // TODO Auto-generated catch block
            throw new TaskException(ex);
        }
        return taskInput;
    }

    public Element getTaskOutput(Long taskId) throws TaskException {
        // TODO Auto-generated method stub
        Element taskOutput = null;
        try {
            String taskOutputStr = mDBOperation.getTaskOutput(taskId);
            if (taskOutputStr != null && taskOutputStr.length() > 0) {
                taskOutput = XmlUtil.createDocumentFromXML(true, taskOutputStr)
                        .getDocumentElement();
            }
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        } catch (Exception ex) {
            // TODO Auto-generated catch block
            throw new TaskException(ex);
        }
        return taskOutput;
    }

    public void reassignTask(Long taskId, Collection<TaskPrincipal> targets, Collection<TaskPrincipal> excluded)
            throws TaskException {
        try {
            RuntimeTask task = getTask(taskId);
            RuntimeTask.TaskState oldState = task.getState();
            mDBOperation.reAssignTask(taskId, targets, excluded);
            task = getTask(taskId);
            RuntimeTask.TaskState newState = task.getState();
            fireTaskStateChange(oldState, newState, task);

        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        }
    }

    public void escalateTask(Escalation  escalation , Long taskId, Long timerId,
            List<TaskPrincipal> newPrincipals,  Collection<TaskPrincipal> excludedUsers) throws TaskException {
        try {
            RuntimeTask task = getTask(taskId);
            RuntimeTask.TaskState oldState = task.getState();
            removeActiveTimer(taskId, timerId);
            mDBOperation.escalateTask(taskId, timerId, newPrincipals, excludedUsers);
            task = getTask(taskId);
            RuntimeTask.TaskState newState = task.getState();
            fireTaskStateChange(oldState, newState, task, escalation);
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        }
    }

    public void timeoutTask(Long taskId, Long timerId) throws TaskException {
        try {
            RuntimeTask task = getTask(taskId);
            RuntimeTask.TaskState oldState = task.getState();
            removeActiveTimer(taskId, timerId);
            mDBOperation.timeoutTask(taskId, timerId);
            task = getTask(taskId);
            unregisterTimers(task);
            RuntimeTask.TaskState newState = task.getState();
            fireTaskStateChange(oldState, newState, task);
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        }

    }

    public void revokeTask(Long taskId) throws TaskException {
        try {
            RuntimeTask task = getTask(taskId);
            if (task != null) {
                Collection<TaskPrincipal> newPrincipals = TaskManagerHelper
                        .getTaskPrincipalsFromWorkflowDefinition(task);
                mDBOperation.revokeTask(taskId, newPrincipals);
            }
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        }
    }

    public void setContext(EngineContext context) throws TaskException, NamingException {
        // TODO Auto-generated method stub
        super.setContext(context);
        InitialContext initContext = context.getInitialContext();
        Properties engineProps = context.getConfig();
        String dataSourceJNDI = engineProps
                .getProperty(EnginePropertyConstants.DATASOURCE_JNDI);
        String dataSourceType = engineProps
                .getProperty(EnginePropertyConstants.DATASOURCE_TYPE);
        String schemaName = engineProps
                .getProperty(EnginePropertyConstants.DATABASE_SCHEMA);
        String indexDirProp = engineProps.getProperty(EnginePropertyConstants.INDEX_DIR_PROP);
        Boolean updateIndexOnStart = Boolean.valueOf(engineProps.getProperty(EnginePropertyConstants.UPDATE_INDEX_ON_START));
        
        mWorkflowMapEntryTable = context.getWorkflowMapEntryTable();
        DataSource ds = null;
        try {
            ds = (DataSource) initContext.lookup(dataSourceJNDI);
        } catch (NamingException e) {
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7011: Cannot get datasource");
            LOGGER.log(Level.SEVERE, msg, e);
            throw e;
        }
        String testMode = engineProps
                .getProperty(EnginePropertyConstants.TEST_MODE);
        boolean recreate = false;
        if (testMode.equalsIgnoreCase("true")) {
            recreate = true;
        }
        mDBOperation = new DBOperation(ds, dataSourceType, recreate, schemaName, indexDirProp, updateIndexOnStart);
        //Recreate index if the flag is true
//        if (updateIndexOnStart.booleanValue() && !recreate) {
//            createFullTextIndex();
//        }
    }

    public void setTaskOutput(Long taskId, Element output, TaskPrincipal setter)
            throws TaskException {
        // TODO Auto-generated method stub
        try {
            String taskOutputStr = XmlUtil.toXml(output, "UTF-8", false);
            mDBOperation.setTaskOutput(taskId, taskOutputStr);
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        } catch (Exception ex) {
            // TODO Auto-generated catch block
            throw new TaskException(ex);
        }
    }

    public void recover(Task taskMetaData) throws TaskException, ConnectionException {
        try {
            QName qname =taskMetaData.getQName();
            TaskHandlerManager hManager = TaskHandlerManager.getInstance();
            Map<Long, List <TaskTimer>> recoveredTimers = mDBOperation.recoverTimers(qname);
            if (recoveredTimers != null && recoveredTimers.size() > 0) {
                for (Map.Entry<Long, List <TaskTimer>> entry  : recoveredTimers.entrySet()) {
                     Long taskId = entry.getKey();
                     List<TaskTimer> timers = entry.getValue();
                     List<RuntimeTaskTimer> runtimeTimers = createAndAddRuntimeTaskTimer(taskMetaData, timers, taskId, null);
                    hManager.processTaskTimerHandler(runtimeTimers, this);
                }
            }
            hManager.processNotificationHandler(taskMetaData, this);
        } catch (ConnectionException e) {
            //throw e;
        } catch (Exception ex) {
            throw new TaskException(ex);
        }
    }

    private RuntimeTask convertToRuntimeTask(TaskInstance taskInstance)
            throws Exception {
        String taskInputStr = taskInstance.getInputData();
        Element taskInputEl = XmlUtil.createDocumentFromXML(true, taskInputStr)
                .getDocumentElement();
        String taskOutputStr = taskInstance.getOutputData();
        TaskOutput taskOutput = null;
        if (taskOutputStr != null && taskOutputStr.length() > 0) {
            Element taskOutputEl = XmlUtil.createDocumentFromXML(true,
                    taskOutputStr).getDocumentElement();
            taskOutput = TaskModelFactory.getInstance().createTaskOutput(
                    taskOutputEl);
        }
        TaskInput taskInput = TaskModelFactory.getInstance().createTaskInput(
                taskInputEl);
        QName taskQname = Util.parseQNameString(taskInstance.getTaskDefId());

        Task taskMeta = mWorkflowMapEntryTable.getTasksModel(taskQname);
        RuntimeTask task = new DefaultRuntimeTask(taskInstance.getId(),
                taskInstance.getMessageExchangeId(), taskInput, taskMeta);
        Calendar cal = Calendar.getInstance();
        cal.setTime(taskInstance.getCreateDate());
        task.setCreateDate(cal);
        
        if (taskInstance.getDeadline() != null) {
            Calendar deadlinecal = Calendar.getInstance();
            deadlinecal = Calendar.getInstance();
            deadlinecal.setTime(taskInstance.getDeadline());
            task.setDeadline(deadlinecal);
        }

        Calendar endCal = Calendar.getInstance();
        if (taskInstance.getEndDate() !=null) {
            endCal.setTime(taskInstance.getEndDate());
            task.setEndDate(endCal);  
        }
        
        Set<TaskAssignee> assignees = mDBOperation.getCurrentAssignees(task
                .getId());
        List<TaskPrincipal> principals = new ArrayList<TaskPrincipal>();
        for (TaskAssignee taskAssignee : assignees) {
            TaskPrincipal principal = convertToTaskPrincipal(taskAssignee);
            if (taskAssignee.getAssignedStatus() == RuntimeTask.TaskState.CLAIMED
                    .getState()) {
                task.setClaimedBy(principal);
                List<TaskAssignee> oldAssigned = mDBOperation
                        .getOldAssignees(task.getId());
                for (TaskAssignee oldAssignee : oldAssigned) {
                    principal = convertToTaskPrincipal(oldAssignee);
                    principals.add(principal);
                }
                break;
            } else {
                principals.add(principal);
            }
        }
        task.setAssignedTo(principals);

        // RuntimeVariables runtimeVar = new RuntimeVariablesImpl ();
        //
        // Set<TaskVariable> variables = taskInstance.getVariables();
        // for (Iterator<TaskVariable> it = variables.iterator(); it.hasNext();)
        // {
        // TaskVariable taskVar = it.next();
        // RuntimeVariables.VariableType varType =
        // RuntimeVariables.VariableType.getType(taskVar.getVarType());
        // if (varType == RuntimeVariables.VariableType.Element) {
        // Element el = XmlUtil.createDocumentFromXML(true, taskVar.getValue())
        // .getDocumentElement();
        // runtimeVar.declareVariable(taskVar.getVarName(), el,
        // RuntimeVariables.VariableType.Element );
        // } else {
        // runtimeVar.declareVariable(taskVar.getVarName(), taskVar.getValue(),
        // RuntimeVariables.VariableType.Text );
        // }
        // }
        // task.setRuntimeVariables(runtimeVar);
        // initialize variables in the init section
        if (taskOutput != null) {
            task.setOutput(taskOutput);
        }
        if (taskInstance.getCompletedBy() != null) {
            TaskPrincipal completedBy = TaskModelFactory.getInstance()
                    .createPrincipal(taskInstance.getCompletedBy(),
                            TaskPrincipal.PrincipalType.User);
            task.setCompletedBy(completedBy);
        }

        task.setPriority(taskInstance
                .getPriority());
        task.setState(RuntimeTask.TaskState.getTaskState(taskInstance
                .getStatus()));
        task.setDescription(taskInstance.getTitle());     
        String keyword = taskInstance.getKeyword();
        if (keyword != null && keyword.length() > 0) {
            task.setKeywords(taskInstance.getKeyword());
        }
        task.init(getContext());
        return task;

    }

    private TaskPrincipal convertToTaskPrincipal(TaskAssignee taskAssignee)
            throws Exception {
        TaskPrincipal tp = DBOperation.convertToTaskPrincipal(taskAssignee.getAssignee());
        return tp;

    }

    // private TaskPrincipal convertToTaskPrincipal(TaskAssigneeOld
    // taskAssignee) throws Exception {
    // String type = taskAssignee.getAssigneeType();
    // type = type.trim();
    // TaskPrincipal.PrincipalType principalType = TaskPrincipal.PrincipalType
    // .getPrincipalType(type);
    // return
    // TaskModelFactory.getInstance().createPrincipal(taskAssignee.getAssignee(),
    // principalType);
    //
    // }

    private List<RuntimeTaskTimer> createAndAddRuntimeTaskTimer(Task taskMetaData,
            Collection<TaskTimer> taskTimers, Long taskId, RuntimeTask task) throws TaskException {
        Iterator<TaskTimer> it = taskTimers.iterator();
        List<RuntimeTaskTimer> runtimeTimers = new ArrayList<RuntimeTaskTimer>  ();
       if (task == null) {
           task = getTask(taskId);
       }
        while (it.hasNext()) {
            TaskTimer tt = it.next();
            if (tt.getStatus() == 1) {
                String xpath = tt.getXpath();
                DeadlineOrDuration deadlineOrDuration = taskMetaData
                        .findDeadlineOrDuration(xpath);
                if (validateDeadlineOrDuration(deadlineOrDuration, task)) {
                    RuntimeTaskTimer rtt = new DefaultRuntimeTaskTimer(tt.getId(),
                            deadlineOrDuration, this,  taskId);
                    rtt.setDueDate(tt.getDueDate());
                    runtimeTimers.add(rtt);
                }
            }
        }
        mtaskIdToRuntimeTaskTimerss.put(task.getId(), runtimeTimers);
        return runtimeTimers;
    }


    protected void removeActiveTimer(Long taskId, Long timerId) {
        List<RuntimeTaskTimer> rtts = mtaskIdToRuntimeTaskTimerss.get(taskId);
        if (rtts != null) {
            Iterator<RuntimeTaskTimer> rIt = rtts.iterator();
            while (rIt.hasNext()) {
                RuntimeTaskTimer rtt = rIt.next();
                if (timerId.equals(rtt.getId())) {
                    rIt.remove();
                }
            }
        }
        mtaskIdToRuntimeTaskTimerss.remove(taskId);
    }

    public void shutDown() {
        // TODO Auto-generated method stub
        if (mDBOperation != null) {
            mDBOperation.close ();
        }
    }
    
    public DBOperation getDBOperation () {
        return mDBOperation;
    }

    public List<RuntimeTaskTimer> getActiveTimers(Long taskId) {
        // TODO Auto-generated method stub
        List<RuntimeTaskTimer>  timers = mtaskIdToRuntimeTaskTimerss.get(taskId);
        if (timers == null) 
            return new ArrayList<RuntimeTaskTimer> ();
        return timers;
    }

    public void createFullTextIndex() throws TaskException {
        // TODO Auto-generated method stub
        try {
            mDBOperation.createIndex();
        } catch (ConnectionException e) {
            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
        }
    }

//    public void testSearch() throws TaskException {
//        // TODO Auto-generated method stub
////        TasklistResult result = null;
//        try {
//            mDBOperation.testSearch();
//        } catch (ConnectionException e) {
//            throw new TaskException(e.getFaultCode(), e.getMessage(), e);
//        }
//    }

}
