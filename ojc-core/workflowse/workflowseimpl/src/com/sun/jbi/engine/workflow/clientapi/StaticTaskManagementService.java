/*
 * DefaultTaskManagementOperations.java
 *
 * Created on October 25, 2006, 12:19 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.engine.workflow.clientapi;

import java.security.Principal;
import java.security.acl.Group;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.Subject;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.operations.TasklistResult;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.taskcommon.xmlbeans.Direction;
import com.sun.jbi.workflow.taskcommon.xmlbeans.GroupsType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.SortField;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus;
import com.sun.jbi.workflow.taskcommon.xmlbeans.UsersType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType.Excluded;
import com.sun.jbi.workflow.taskcommon.xmlbeans.impl.UsersTypeImpl;

/**
 * Static Client operation logic is defined here. Uses TaskManager to deal with
 * how to do task specific operations.
 * 
 */
public class StaticTaskManagementService {

    private static final Logger LOGGER = Logger
            .getLogger(StaticTaskManagementService.class.getName());

    private TaskManager mManager;

    /** Creates a new instance of DefaultTaskManagementOperations */
    public StaticTaskManagementService() {
        try {
            mManager = TaskManagerFactory.getInstance().getTaskManager();
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, I18n
                    .loc("WLM-6013: Failed to get task manager"), ex);
        }
    }

    public TaskManager getManager() {
        return mManager;
    }

    public void setManager(TaskManager tManager) {
        mManager = tManager;
    }

    public TasklistResult getTaskFilteredList(QueryType query,  TaskSubject source, int startIndex,
            int pageSize) throws ClientApiException {
        TasklistResult resultTaskList = null;
        try {

            Iterator<Principal> it = null;
            //needs to set the excluded for the current user and query user 
            if (source != null) {
                Subject subject = source.getJAASSubject();
                if (subject != null) {
                    Set<Principal> principals = subject.getPrincipals();
                    it = principals.iterator();
                }
            }
             List<String> addExcludedUsers = null;
             List<String> addExcludedGroups = null;
              if (query != null) {
                 UsersType users =  query.getUsers();
                  if (users != null && users.getUserList().size() > 0) {
                      addExcludedUsers = users.getUserList();
                  }
                  
                  GroupsType groups = query.getGroups();
                  if (groups != null && groups.getGroupList().size() > 0) {
                      addExcludedGroups = groups.getGroupList();
                  }                  
              }
              
              Excluded excluded = null;
              UsersType excludedUsers = UsersType.Factory.newInstance();
              GroupsType excludedGroups = GroupsType.Factory.newInstance();
              
              if (query.getExcluded() == null) {
                  excluded = query.addNewExcluded();
              } else {
                  excluded = query.getExcluded();
                  if (excluded.getUsers() != null) {
                      excludedUsers = excluded.getUsers() ;
                  }
                  if (excluded.getGroups() != null) {
                      excludedGroups = excluded.getGroups();
                  }
              }
              
              while (it.hasNext()) {
                  Principal p = it.next();
                  if (!(p instanceof Group) && ! excludedUsers.getUserList().contains(p.getName())) {
                      excludedUsers.addUser(p.getName());
                  } else if ( ! excludedGroups.getGroupList().contains(p.getName()))
                      excludedGroups.addGroup(p.getName());
                  }
               
               if (addExcludedUsers != null && addExcludedUsers.size() > 0) {
                   for (String user : addExcludedUsers) {
                       if (!excludedUsers.getUserList().contains(user)) {
                           excludedUsers.addUser(user);
                       }
                   }              
              }              
               
               if (addExcludedGroups != null && addExcludedGroups.size() > 0) {
                   for (String group : addExcludedGroups) {
                       if (!excludedGroups.getGroupList().contains(group)) {
                           excludedGroups.addGroup(group);
                       }
                   }                      
               }
               excluded.setGroups(excludedGroups);
               excluded.setUsers(excludedUsers);
               
               resultTaskList = mManager.getAssignedTaskList(query, startIndex,
                       pageSize);
        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }
        return resultTaskList;
    }
    
    public TasklistResult getTaskList(QueryType query,  int startIndex,
            int pageSize) throws ClientApiException {
        TasklistResult resultTaskList = null;
        try {
            resultTaskList = mManager.getAssignedTaskList(query, startIndex,
                    pageSize);
        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }
        return resultTaskList;
    }    

    public TaskItem getTask(Long taskId) throws ClientApiException {
        TaskItem item = null;
        try {
            RuntimeTask task = mManager.getTask(taskId);
            if (task != null) {
                item = new TaskItem(task);
            }
        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }
        return item;
    }

    public TasklistResult getTaskList(QueryType oldQuery, TaskSubject source,
            int startIndex, int pageSize) throws ClientApiException {
        TasklistResult resultTaskList = null;
        // List <TaskPrincipal> taskPrincipals = new ArrayList <TaskPrincipal>
        // ();

        try {
            if (source != null) {
                Subject subject = source.getJAASSubject();
                if (subject != null) {
                    Set<Principal> principals = subject.getPrincipals();
                    Iterator<Principal> it = principals.iterator();
                    TaskPrincipal tp = null;

                    QueryType query = null;
                    if (oldQuery.getType() == null
                            || oldQuery.getType() == QueryType.Type.DEFAULT) {
                        query = QueryType.Factory.newInstance();
                        query.setType(QueryType.Type.FILTERED);

                        // If the old Query specifies statuses and sort, use it
                        // in the query
                        if (oldQuery != null) {
                            List<com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.Enum> statusList = oldQuery
                                    .getTaskStatusList();
                            if (statusList != null && statusList.size() > 0) {
                                for (TaskStatus.Enum taskStatus : statusList) {
                                    query.addTaskStatus(taskStatus);
                                }
                            } else {
                                query.addTaskStatus(TaskStatus.ASSIGNED);
                                query.addTaskStatus(TaskStatus.CLAIMED);
                                query.addTaskStatus(TaskStatus.ESCALATED);
                            }
                            SortField.Enum sort = oldQuery.getSort();
                            Direction.Enum dir = oldQuery.getDir();
                            if (sort != null) {
                                query.setSort(sort);
                                if (dir == null) {
                                    dir = Direction.ASC;
                                }
                                query.setDir(dir);
                            }
                        }

                        UsersType users = UsersType.Factory.newInstance();
                        GroupsType groups = GroupsType.Factory.newInstance();

                        Excluded excluded = query.addNewExcluded();

                        while (it.hasNext()) {
                            Principal p = it.next();
                            if (!(p instanceof Group)) {
                                users.addUser(p.getName());
                            } else {
                                groups.addGroup(p.getName());
                            }
                        }

                        if (users.sizeOfUserArray() > 0) {
                            query.setUsers(users);
                            excluded.setUsers(users);
                        }
                        if (groups.sizeOfGroupArray() > 0) {
                            query.setGroups(groups);
                            excluded.setGroups(groups);
                        }
                    } else {
                        query = oldQuery;
                    }
                    resultTaskList = getTaskList(query, startIndex, pageSize);
                } else {
                    // hardcode to one specific principal
                }

            } else {
                // hardcode to one specific principal
            }
        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }
        return resultTaskList;
    }

    public OperationStatus claimTask(Long taskId, TaskSubject source)
            throws ClientApiException {
        OperationStatus status = OperationStatus.FAILURE;

        try {
            if (source != null) {
                Subject subject = source.getJAASSubject();
                if (subject != null) {
                    Set<Principal> principals = subject.getPrincipals();
                    if (principals.size() != 0) {
                        TaskPrincipal tp = null;
                        Iterator<Principal> it = principals.iterator();
                        while (it.hasNext()) {
                            Principal p = it.next();
                            if (p instanceof Group) {
                                continue;
                            }
                            tp = TaskModelFactory.getInstance()
                                    .createPrincipal(p.getName(),
                                            TaskPrincipal.PrincipalType.User);

                            try {
                                mManager.claimTask(taskId, tp);
                                status = OperationStatus.SUCCESS;
                            } catch (TaskException ex) {
                                LOGGER
                                        .log(
                                                Level.WARNING,
                                                I18n
                                                        .loc("WLM-6014: Failed to claim the task"),
                                                ex);
                                status = OperationStatus.FAILURE;
                            }
                            break;
                        }
                    }
                }
            }

        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }

        return status;
    }

    public OperationStatus revokeTask(Long taskId) throws ClientApiException {
        OperationStatus status = OperationStatus.SUCCESS;

        try {

            mManager.revokeTask(taskId);
        } catch (TaskException ex) {
            LOGGER.log(Level.WARNING,
                    I18n.loc(
                            "WLM-6015: Failed to revoke task with task id {0}",
                            taskId), ex);
            status = OperationStatus.FAILURE;
            return status;
        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }

        return status;
    }

    public OperationStatus completeTask(Long taskId, TaskSubject source)
            throws ClientApiException {

        OperationStatus status = OperationStatus.SUCCESS;

        try {
            if (source != null) {
                Subject subject = source.getJAASSubject();
                if (subject != null) {
                    Set<Principal> principals = subject.getPrincipals();
                    if (principals.size() != 0) {
                        TaskPrincipal tp = null;
                        Iterator<Principal> it = principals.iterator();
                        while (it.hasNext()) {
                            Principal p = it.next();
                            if (p instanceof Group) {
                                continue;
                            }
                            tp = TaskModelFactory.getInstance()
                                    .createPrincipal(p.getName(),
                                            TaskPrincipal.PrincipalType.User);
                            try {
                                mManager.completeTask(taskId, tp);
                            } catch (TaskException ex) {
                                LOGGER
                                        .log(
                                                Level.WARNING,
                                                I18n
                                                        .loc(
                                                                "WLM-6016: Failed to complete the task with task id {0}",
                                                                taskId), ex);
                                status = OperationStatus.FAILURE;
                            }
                            break;
                        }
                    }
                }
            }

        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }

        return status;
    }

    public OperationStatus reassignTask(Long taskId, TaskSubject source,
            Collection<TaskPrincipal> principals,
            Collection<TaskPrincipal> excludedUsers) throws ClientApiException {

        OperationStatus status = OperationStatus.SUCCESS;

        try {
            if (source != null) {
                Subject subject = source.getJAASSubject();
                if (subject != null) {

                    try {
                        mManager
                                .reassignTask(taskId, principals, excludedUsers);
                    } catch (TaskException ex) {
                        LOGGER.log(Level.WARNING, I18n.loc(
                                "WLM-6017: Failed to reassign task to {0}",
                                principals), ex);
                        status = OperationStatus.FAILURE;
                    }

                }
            }

        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }
        return status;
    }

    public Element getTaskInput(Long taskId) throws ClientApiException {

        Element element = null;
        try {
            element = mManager.getTaskInput(taskId);
        } catch (TaskException e) {
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-6018: Error in getting input data");
            throw new ClientApiException("Error_In_Getting_Input", msg + " "
                    + e.getMessage(), e);

        }
        return element;
    }

    public Element getTaskOutput(Long taskId) throws ClientApiException {

        Element element = null;
        try {
            element = mManager.getTaskOutput(taskId);
            if (element == null) {
                RuntimeTask task = mManager.getTask(taskId);
                if (task.getOutput() != null) {
                    element = task.getOutput().getOutput();
                }
            }
        } catch (TaskException e) {
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-6019: Error in getting output data");
            throw new ClientApiException("Error_In_Getting_Output", msg + " "
                    + e.getMessage(), e);

        }

        return element;
    }

    public OperationStatus setTaskOutput(Long taskId, Element output,
            TaskSubject source) throws ClientApiException {
        OperationStatus status = OperationStatus.SUCCESS;

        try {
            if (source != null) {
                Subject subject = source.getJAASSubject();
                if (subject != null) {
                    Set<Principal> principals = subject.getPrincipals();
                    if (principals.size() != 0) {
                        TaskPrincipal tp = null;
                        Iterator<Principal> it = principals.iterator();
                        while (it.hasNext()) {
                            Principal p = it.next();
                            if (p instanceof Group) {
                                continue;
                            }
                            tp = TaskModelFactory.getInstance()
                                    .createPrincipal(p.getName(),
                                            TaskPrincipal.PrincipalType.User);
                            try {
                                mManager.setTaskOutput(taskId, output, tp);
                            } catch (TaskException ex) {
                                LOGGER
                                        .log(
                                                Level.SEVERE,
                                                I18n
                                                        .loc("WLM-6020: Failed to set task output data"),
                                                ex);
                                status = OperationStatus.FAILURE;
                            }
                            break;
                        }
                    }
                }
            }

        } catch (Exception ex) {
            throw new ClientApiException(ex);
        }
        return status;
    }

    public Element getTaskXform(Long taskId) throws ClientApiException {
        Element element = null;
        try {
            element = mManager.getTaskXform(taskId);
        } catch (TaskException e) {
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-6021: Error in getting  XForm");
            throw new ClientApiException("Error_In_Getting_XForm", msg + " "
                    + e.getMessage(), e);

        }
        return element;
    }

    public Element getTaskInputXformInstance(Long taskId)
            throws ClientApiException {
        Element element = null;
        try {
            element = mManager.getTaskInputXformInstance(taskId);
        } catch (TaskException e) {
            // TODO Auto-generated catch block
            String msg = I18n
                    .loc("WLM-6022: Error in getting input XForm Instance");
            throw new ClientApiException(
                    "Error_In_Getting_Input_XForm_Instance", msg + " "
                            + e.getMessage(), e);

        }
        return element;
    }

    public Element getTaskOutputXformInstance(Long taskId)
            throws ClientApiException {
        Element element = null;
        try {
            element = mManager.getTaskOutputXformInstance(taskId);
        } catch (TaskException e) {
            // TODO Auto-generated catch block
            String msg = I18n
                    .loc("WLM-6023: Error in getting output XForm Instance");
            throw new ClientApiException(
                    "Error_In_Getting_Output_XForm_Instance", msg + " "
                            + e.getMessage(), e);

        }
        return element;
    }

    public enum OperationStatus {
        SUCCESS, FAILURE;
    }
}
