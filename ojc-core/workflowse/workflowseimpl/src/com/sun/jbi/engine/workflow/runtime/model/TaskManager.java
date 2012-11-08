/*
 * TaskManager.java
 *
 * Created on October 25, 2006, 11:45 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.engine.workflow.runtime.model;

import java.util.Collection;
import java.util.List;

import javax.naming.NamingException;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.clientapi.TaskItem;
import com.sun.jbi.engine.workflow.clientapi.operations.TasklistResult;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;

/**
 *  TaskManager is the entry point in workflow runtime.
 *  TaskManager can be implemented either as  in memory
 *  or using database.
 *  
 *  
 */
public interface TaskManager {
    
    /**
     * create a new task.
     *
     * @param input is task input in denormalized form.
     * @param taskMetaData is task model element.
     */
    RuntimeTask createTask(String exchangeId, TaskInput input, Task taskMetaData) throws TaskException;    
  
    TasklistResult  getAssignedTaskList (QueryType query,  int startIndex, int pageSize)   throws TaskException;    
    
    RuntimeTask getTask(Long taskId) throws TaskException;
    
    void claimTask(Long taskId, TaskPrincipal user) throws TaskException;
    
    void revokeTask(Long taskId) throws TaskException;
    
    void completeTask(Long taskId, TaskPrincipal user) throws TaskException;
    
    void reassignTask(Long taskId,  Collection<TaskPrincipal> targets, Collection<TaskPrincipal> excluded) throws TaskException;
    
    void setTaskOutput(Long taskId, Element output, TaskPrincipal setter)throws TaskException;
    
    Element getTaskInput (Long taskId) throws TaskException;
    
    Element getTaskOutput (Long taskId) throws TaskException;
    
    void assignTask(RuntimeTask task,  Collection<TaskPrincipal> user, Collection<TaskPrincipal> excludedUsers) throws TaskException;
    
    void escalateTask(Escalation escalation, Long taskId, Long timerId, List<TaskPrincipal> newPrincipals,  Collection<TaskPrincipal> excludedUsers) throws TaskException;
    
    void timeoutTask(Long taskId, Long timerId) throws TaskException;
    
    
    void addTaskListener(TaskManagerListener l); 

    void removeTaskListener(TaskManagerListener l); 

    void addTaskStateListener(TaskStateListener l); 

    void removeTaskStateListener(TaskStateListener l); 

    void setContext (EngineContext context) throws TaskException, NamingException;
    
    EngineContext getContext () throws TaskException;
 
    void recover(Task taskMetaData) throws TaskException;
    
    void unregisterTimers(RuntimeTask task) throws TaskException;
    
    Element getTaskXform (Long taskId) throws TaskException;    
   
    Element getTaskInputXformInstance (Long taskId) throws TaskException;
    
    Element getTaskOutputXformInstance (Long taskId) throws TaskException;

    void removeTaskStateListeners(QName name);
    
    boolean hasTaskStateListeners(QName name);
    
    void shutDown ();
    
    List<RuntimeTaskTimer> getActiveTimers (Long taskId);
    
    void createFullTextIndex () throws TaskException;
//    
//     void  testSearch () throws TaskException;

}
