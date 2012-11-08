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
 * @(#)HibernateTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.test;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.sql.DataSource;
import javax.xml.namespace.QName;

import org.apache.commons.jxpath.JXPathContext;
import org.hibernate.FetchMode;
import org.hibernate.Hibernate;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.criterion.Restrictions;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.TaskItem;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.db.connection.ConnectionException;
import com.sun.jbi.engine.workflow.db.connection.ConnectionManager;
import com.sun.jbi.engine.workflow.db.connection.ConnectionProperties;
import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.db.dao.DAOFactory;
import com.sun.jbi.engine.workflow.db.hibernate.TaskAssignee;
import com.sun.jbi.engine.workflow.db.hibernate.TaskInstance;
import com.sun.jbi.engine.workflow.db.hibernate.TaskTimer;
import com.sun.jbi.engine.workflow.db.opt.DBOperation;
import com.sun.jbi.engine.workflow.runtime.model.DefaultRuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask.TaskState;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XPathUtil;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskListDocument;

import junit.framework.TestCase;

public class HibernateTest extends TestCase {

    private SessionFactory sessionFactory;

    private DataSource dataSource;

    private static final String PRINCIPAL_DELIMETER = ",";
    
    private static final String TASK_LIST_PART = "taskList";
    
    private static final String HBM_CACHE_EHCACHE = "ehcache.xml";    

    public HibernateTest(String name) throws SQLException {
        super(name);
        sessionFactory = getSessionFactory();
        Properties props = new Properties();
        props.setProperty(ConnectionProperties.DB_TYPE, DAO.DERBY);
        props.setProperty(ConnectionProperties.DB_DATABASE_NAME, "WORKFLOWDB");
        props.setProperty(ConnectionProperties.DB_SERVER_NAME, "localhost");
        props.setProperty(ConnectionProperties.DB_PORT, "1527");
        props.setProperty(ConnectionProperties.DB_USERNAME, "WORKFLOW");
        props.setProperty(ConnectionProperties.DB_PASSWORD, "WORKFLOW");

        dataSource = DAOFactory.getDataSource(props, DAOFactory
                .getDBType(DAO.DERBY));

        // TODO Auto-generated constructor stub
    }

    private SessionFactory getSessionFactory() {
        String tempDir =System.getProperty("java.io.tmpdir");
        String oldPath = "com/sun/jbi/engine/workflow/db/hibernate/";
        String path = "com/sun/jbi/engine/workflow/db/hibernate/identity/";
        Configuration cfg = new Configuration().setProperty(
                "hibernate.dialect", "org.hibernate.dialect.DerbyDialect")
 .setProperty(
                "hibernate.current_session_context_class", "thread")
                .setProperty("hibernate.show_sql", "false")
                .setProperty(
                        "hibernate.cache.use_second_level_cache", "true")
                .setProperty("hibernate.cache.provider_class",
                        "net.sf.ehcache.hibernate.SingletonEhCacheProvider")
                .setProperty("hibernate.cache.use_query_cache", "true")
                .setProperty("net.sf.ehcache.configurationResourceName",  oldPath + HBM_CACHE_EHCACHE)
                .setProperty("hibernate.search.default.directory_provider", "org.hibernate.search.store.FSDirectoryProvider")
                .setProperty("hibernate.search.default.indexBase", tempDir )                 
                .addResource(path + "TaskInstance.hbm.xml").addResource(
                        path + "TaskAssignee.hbm.xml").
                 addResource(path + "TaskExcludedAssignee.hbm.xml").
                addResource(path + "TaskTimer.hbm.xml");
        // addResource(path + "TaskAssigneeOld.hbm.xml");
        return cfg.buildSessionFactory();

    }

    protected void setUp() throws Exception {
//        CreateDropVerifyDBSchemaTest dropSchema = new CreateDropVerifyDBSchemaTest(
//                "drop");
//        dropSchema.testCheckSchema();

    }

//    public void testHibernate() {
//
//        Connection conn = null;
//        try {
//
//            conn = dataSource.getConnection();
//        } catch (ConnectionException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        } catch (SQLException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        }
//        assertNotNull(conn);
//        Session session = sessionFactory.openSession(conn);
//        session.beginTransaction();
//        Query query = session
//                .createQuery("from TaskInstance as taskInst where taskInst.messageExchangeId = 'abc'");
//
//        List taskInstances = query.list();
//
//        assertEquals(0, taskInstances.size());
//
//        TaskInstance taskInstance = new TaskInstance();
//        taskInstance.setCreateDate(new Date());
//        taskInstance.setInputData("InputData");
//        taskInstance.setOutputData("OutputData");
//        taskInstance.setMessageExchangeId("abc");
//        taskInstance.setStatus(RuntimeTask.TaskState.ASSIGNED.getState());
//        taskInstance.setTaskDefId("taskDefId1");
//        taskInstance.setTitle("task1");
//        session.save(taskInstance);
//
//        taskInstances = query.list();
//        assertEquals(1, taskInstances.size());
//        assertEquals(1, taskInstance.getId());
//
//        TaskTimer timer = new TaskTimer();
//        timer.setTaskInstance(taskInstance);
//        timer.setXpath("xpath1");
//        Calendar cal = Calendar.getInstance();
//        cal.roll(Calendar.DATE, 3);
//        timer.setDueDate(cal.getTime());
//        timer.setStatus(1);
//
//        session.save(timer);
//        assertEquals(1, timer.getId());
//
//        TaskAssignee assignee = new TaskAssignee();
//        assignee.setTaskInstance(taskInstance);
//        assignee.setAssignedStatus(RuntimeTask.TaskState.ASSIGNED.getState());
//        assignee.setAssignee("assignee1");
//        assignee.setAssigneeType("USER");
//        assignee.setUpdateDate(new Date());
//
//        session.save(assignee);
//
//        query = session
//                .createQuery("from TaskTimer as taskTimer where taskTimer.id = 1");
//        List timers = query.list();
//        assertEquals(1, timers.size());
//
//        TaskTimer taskTimer = (TaskTimer) timers.get(0);
//
//        assertEquals(1, assignee.getId());
//
//        session.getTransaction().commit();
//        TaskInstance taskInst = taskTimer.getTaskInstance();
//        assertEquals(1, taskInst.getId());
//
//        session = sessionFactory.openSession(conn);
//        session.beginTransaction();
//        TaskInstance instance2 = (TaskInstance) session.load(
//                TaskInstance.class, new Long(1));
//        assertEquals(1, instance2.getAssignees().size());
//        assertEquals(1, instance2.getTimers().size());
//
//        final String taskListQueryStr1 = "select taskInstance from TaskAssignee as taskAssignee, TaskInstance as taskInstance "
//                + "where taskAssignee.taskInstance = taskInstance and taskInstance.status not in (3, 6, 7) "
//                + "and taskAssignee.assignee = ? and taskAssignee.assigneeType = 'USER' and taskAssignee.assignedStatus in (1, 2)";
//
//        Query taskListQuery = session.createQuery(taskListQueryStr1).setString(
//                0, "assignee1");
//        List<TaskInstance> taskInstList = taskListQuery.list();
//        assertEquals(1, taskInstList.size());
//
//        for (TaskInstance task : taskInstList) {
//            System.out.println("TaskId: " + task.getId());
//        }
//
//        session.getTransaction().commit();
//        assertEquals(1, taskInstList.get(0).getAssignees().size());
//        assertEquals(1, taskInstList.get(0).getTimers().size());
//
//        try {
//            conn.close();
//        } catch (SQLException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        }
//
//    }

    public void testLoadGetTaskList() {
        // final String taskListQueryStr1 = "select taskInstance from
        // TaskAssignee as taskAssignee, TaskInstance as taskInstance "
        // + "where taskAssignee.taskInstance = taskInstance and
        // taskInstance.status not in (3, 4, 6, 7) "
        // + "and taskAssignee.assignee = ? and taskAssignee.assigneeType =
        // 'USER' and taskAssignee.assignedStatus in (1, 2) "
        // + "and taskAssignee.activeAssignee = ?";
        // // Group
        // final String taskListQueryStr2 = "select taskInstance from
        // TaskAssignee as taskAssignee, TaskInstance as taskInstance "
        // + "where taskAssignee.taskInstance = taskInstance and
        // taskInstance.status not in (3, 4, 6, 7) "
        // + "and taskAssignee.assignee = ? and taskAssignee.assigneeType =
        // 'GROUP' and taskAssignee.assignedStatus = 1 "
        // + "and taskAssignee.activeAssignee = ?";
//        Connection conn = null;
//        try {
//
//            conn = dataSource.getConnection();
//        } catch (ConnectionException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        } catch (SQLException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        }
//        assertNotNull(conn);
//        Session session = sessionFactory.openSession(conn);
//        session.beginTransaction();
//
//        List<TaskInstance> taskInstancesForUser = session.createCriteria(
//                TaskInstance.class).add(
//                Restrictions.not(Restrictions.in("status", new Integer[] {
//                        RuntimeTask.TaskState.COMPLETED.getState(),
//                        RuntimeTask.TaskState.EXPIRED.getState(),
//                        RuntimeTask.TaskState.ABORTED.getState(),
//                        RuntimeTask.TaskState.FAILED.getState() })))
//                .setFetchMode("assignees", FetchMode.JOIN).createCriteria(
//                        "assignees").add(Restrictions.and( Restrictions.in(
//                                "assignedStatus", new Integer[] {
//                                        RuntimeTask.TaskState.ASSIGNED
//                                                .getState(),
//                                        RuntimeTask.TaskState.CLAIMED
//                                                .getState() }), Restrictions
//                                .and(Restrictions.eq("assignee", "[USER]john"),
//                                        Restrictions.eq("activeAssignee",
//                                                Boolean.TRUE)))).list();
//
//        List<TaskInstance> taskInstancesForGroup = session.createCriteria(
//                TaskInstance.class).add(
//                Restrictions.not(Restrictions.in("status", new Integer[] {
//                        RuntimeTask.TaskState.COMPLETED.getState(),
//                        RuntimeTask.TaskState.EXPIRED.getState(),
//                        RuntimeTask.TaskState.ABORTED.getState(),
//                        RuntimeTask.TaskState.FAILED.getState() })))
//                .setFetchMode("assignees", FetchMode.JOIN).createCriteria(
//                        "assignees").add(
//                        Restrictions.and(Restrictions.eq(
//                                "assignedStatus",
//                                RuntimeTask.TaskState.ASSIGNED.getState()),
//                                Restrictions.and(Restrictions.eq("assignee",
//                                        "[GROUP]CustomerServiceRep"), Restrictions.eq(
//                                        "activeAssignee", Boolean.TRUE))))
//                .list();
//
//        session.getTransaction().commit();
//        Set<TaskInstance> taskListSet = new HashSet<TaskInstance>();
//        taskListSet.addAll(taskInstancesForUser);
//        taskListSet.addAll(taskInstancesForGroup);
//        System.out.println("Total tasks: " + taskListSet.size());
//        List<TaskItem> taskItems = new ArrayList<TaskItem>();
//        if (taskListSet != null) {
//            for (TaskInstance taskInstance : taskListSet) {
//                TaskItem taskItem = new TaskItem (taskInstance);
//                taskItems.add(taskItem);
//            }
//        }
//
////
//        Element result = null;
//        try {
//            result = createResult(taskItems);
//        } catch (Exception e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        }
//        
//        String xmlString = XmlUtil.toXmlPretty(result, "UTF-8", true);
//        System.out.println("Result: \n" + xmlString);

    }

    private Element createResult(Collection<TaskItem> taskItems) throws Exception {
        TaskListDocument taskdoc = TaskListDocument.Factory.newInstance();
        taskdoc.addNewTaskList();
        if (taskItems != null && taskItems.size() > 0) {
            for (TaskItem taskItem : taskItems) {
                com.sun.jbi.workflow.taskcommon.xmlbeans.TaskType taskType = taskdoc.getTaskList()
                        .addNewTask();
                taskType.setTaskId(taskItem.getId());
                taskType.setAssignedTo(taskItem.getAssignedTo());
                taskType.setClaimedBy(taskItem.getClaimedBy());
                taskType.setPriority(taskItem.getPriority());
                taskType.setStatus(makeStatus(taskItem.getState()));
                taskType.setSubmittedDate(taskItem.getCreatedDate());
                taskType.setTitle(taskItem.getDescription());
                taskType.setKeywords(taskItem.getKeywords());
            }
        }
        
        Element  element = (Element) taskdoc.getTaskList().getDomNode();
        return element;
    }

    private String makeString(Collection<TaskPrincipal> assignedTo) {
        // TODO Auto-generated method stub
        String returnStr = "";
        if (assignedTo != null && assignedTo.size() > 0) {
            StringBuffer buffer = new StringBuffer();
            int i = 0;
            for (TaskPrincipal principal : assignedTo) {
                buffer.append(principal.getName());
                if (i < assignedTo.size() - 1) {
                    buffer.append(PRINCIPAL_DELIMETER);
                    i++;
                }

            }
            returnStr = buffer.toString();
        }
        return returnStr;
    }

    private String makeString(TaskPrincipal claimedBy) {
        // TODO Auto-generated method stub
        return claimedBy == null ? "": claimedBy.getName();
       }
    
//    private Enum makePriority(
//            com.sun.jbi.engine.workflow.runtime.model.RuntimeTask.TaskPriority priority) {
//        // TODO Auto-generated method stub
//        if (priority == RuntimeTask.TaskPriority.HIGH) {
//            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskPriority.HIGH;
//        } else if (priority == RuntimeTask.TaskPriority.MEDIUM) {
//            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskPriority.MEDIUM;
//        } else if (priority == RuntimeTask.TaskPriority.LOW) {
//            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskPriority.LOW;
//        }
//        return null;
//    }
    
    private com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.Enum makeStatus(TaskState state) {
        
        if (state == RuntimeTask.TaskState.UNASSIGNED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.UNASSIGNED;
        } else if (state == RuntimeTask.TaskState.ASSIGNED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.ASSIGNED;
        } else if (state == RuntimeTask.TaskState.CLAIMED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.CLAIMED;
        } else if (state == RuntimeTask.TaskState.COMPLETED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.COMPLETED;
        }  else if(state == RuntimeTask.TaskState.EXPIRED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.EXPIRED;
        } else if(state == RuntimeTask.TaskState.ESCALATED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.ESCALATED;
        } else if(state == RuntimeTask.TaskState.ABORTED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.ABORTED;
        } else if(state == RuntimeTask.TaskState.FAILED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.FAILED;
        }
        return null;
    }    

    private TaskPrincipal convertToTaskPrincipal(TaskAssignee taskAssignee)
            throws Exception {
        TaskPrincipal tp = DBOperation.convertToTaskPrincipal(taskAssignee.getAssignee());
        return tp;

    }

    private RuntimeTask lightWeightConvertToRuntimeTask(
            TaskInstance taskInstance) throws Exception {

        Task taskMeta = null;
        RuntimeTask task = new DefaultRuntimeTask(taskInstance.getId(),
                taskInstance.getMessageExchangeId(), null, taskMeta);
        Calendar cal = Calendar.getInstance();
        cal.setTime(taskInstance.getCreateDate());
        task.setCreateDate(cal);

        Set<TaskAssignee> assignees = taskInstance.getAssignees();
        Set<TaskAssignee> currentAssignees = new HashSet<TaskAssignee>();
        Set<TaskAssignee> oldAssignees = new HashSet<TaskAssignee>();
        Date __updated = null;
        for (TaskAssignee taskAssignee : assignees) {
            if (taskAssignee.getActiveAssignee().booleanValue()) {
                currentAssignees.add(taskAssignee);
            } else {
                if (__updated == null) {
                    __updated = taskAssignee.getUpdateDate();
                    oldAssignees.add(taskAssignee);
                } else if (__updated.equals(taskAssignee.getUpdateDate())) {
                    oldAssignees.add(taskAssignee);
                } else {
                    break;
                }
            }
        }
        List<TaskPrincipal> principals = new ArrayList<TaskPrincipal>();
        for (TaskAssignee taskAssignee : currentAssignees) {
            TaskPrincipal principal = convertToTaskPrincipal(taskAssignee);
            if (taskAssignee.getAssignedStatus() == RuntimeTask.TaskState.CLAIMED
                    .getState()) {
                task.setClaimedBy(principal);
                for (TaskAssignee oldAssignee : oldAssignees) {
                    principal = convertToTaskPrincipal(oldAssignee);
                    principals.add(principal);
                }
                break;
            } else {
                principals.add(principal);
            }
        }
        task.setAssignedTo(principals);

        if (taskInstance.getCompletedBy() != null) {
            TaskPrincipal completedBy = TaskModelFactory.getInstance()
                    .createPrincipal(taskInstance.getCompletedBy(),
                            TaskPrincipal.PrincipalType.User);
            task.setCompletedBy(completedBy);
        }

        return task;
    }
}
