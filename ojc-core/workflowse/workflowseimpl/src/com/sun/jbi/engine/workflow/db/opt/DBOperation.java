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
 * @(#)DBOperation.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.opt;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.DataSource;
import javax.xml.namespace.QName;

import org.apache.commons.logging.LogFactory;
import org.apache.lucene.queryParser.MultiFieldQueryParser;
import org.apache.lucene.queryParser.QueryParser;
import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.Hibernate;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.search.FullTextQuery;
import org.hibernate.search.FullTextSession;
import org.hibernate.search.Search;
import org.hibernate.search.event.FullTextIndexEventListener;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.TaskItem;
import com.sun.jbi.engine.workflow.clientapi.operations.TasklistResult;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.common.model.impl.TaskGroupPrincipalImpl;
import com.sun.jbi.engine.workflow.common.model.impl.TaskUserPrincipalImpl;
import com.sun.jbi.engine.workflow.db.connection.ConnectionException;
import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.db.dao.DAOFactory;
import com.sun.jbi.engine.workflow.db.dao.DBType;
import com.sun.jbi.engine.workflow.db.hibernate.TaskAssignee;
import com.sun.jbi.engine.workflow.db.hibernate.TaskExcludedAssignee;
import com.sun.jbi.engine.workflow.db.hibernate.TaskInstance;
import com.sun.jbi.engine.workflow.db.hibernate.TaskTimer;
import com.sun.jbi.engine.workflow.db.schema.DBSchemaCreator;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerHelper;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.DeadlineOrDuration;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.Timeout;
import com.sun.jbi.workflow.taskcommon.xmlbeans.DeadlineQueryType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.Direction;
import com.sun.jbi.workflow.taskcommon.xmlbeans.GroupsType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.SortField;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus;
import com.sun.jbi.workflow.taskcommon.xmlbeans.UsersType;

/**
 * Encapsulates all db related operations
 * 
 * @author Sun Microsystems
 * 
 */
public class DBOperation {
    private static final Logger LOGGER = Logger.getLogger(DBOperation.class
            .getName());

    private static final String DEFAULT_SCHEMA = "WORKFLOW";

    private static final String CREATE_SCHEMA_SCRIP = "workflow_schema.sql";

    private static final String DROP_SCHEMA_SCRIP = "workflow_drop.sql";

    private static final String HBM_TASK_ASSIGNEE = "TaskAssignee.hbm.xml";

    private static final String HBM_TASK_EXCLUDED_ASSIGNEE = "TaskExcludedAssignee.hbm.xml";

    private static final String HBM_TASK_INSTANCE = "TaskInstance.hbm.xml";

    private static final String HBM_TASK_TIMER = "TaskTimer.hbm.xml";

    private static final String HBM_TASK_VARIABLE = "TaskVariable.hbm.xml";

    private static final String HBM_IDENTITY_PATH = "identity";

    private static final String HBM_CACHE_EHCACHE = "ehcache.xml";

    private static final String HBM_SEQUENCE_PATH = "sequence";

    public static final String ALIAS_PREFIX = "alias_";

    public static final String GROUP_PREFIX = "[GROUP]";

    public static final String USER_PREFIX = "[USER]";

    private DataSource mDataSource;

    private SessionFactory mSessionFactory;

    private DBType dbType;

    private String mSchemaName;

    private String mIndexDirProp;

    public DBOperation(DataSource dataSource, String dbTypeStr,
            boolean recreate, String schemaName, String indexDirProp,
            boolean updateIndexOnStart) {
        mDataSource = dataSource;
        dbType = DAOFactory.getDBType(dbTypeStr);
        this.mSchemaName = (schemaName != null) ? schemaName : DEFAULT_SCHEMA;
        boolean recreated = checkAndCreateSchema(recreate);
        mIndexDirProp = (indexDirProp == null ? "java.io.tmpdir" : indexDirProp);
        if (recreated || updateIndexOnStart) {
            // schema recreated means
            createIndex();
        } else {
            mSessionFactory = getSessionFactory(dbType);
        }
    }

    private boolean checkAndCreateSchema(boolean recreate) {
        // Right now, we recreate the schema everytime we start
        // TODO, check the shema and only create the schema when it does not
        // exist
        boolean recreated = true;
        DBSchemaCreator dbSchemaCreator = new DBSchemaCreator(dbType
                .getDBType(), mDataSource, mSchemaName);
        boolean result = dbSchemaCreator.checkSchemaAndTablesIntegrity();
        LOGGER
                .log(Level.INFO, I18n.loc("WLM-5025: Schema exist : {0}",
                        result));
        if (!result || recreate) {
            recreated = true;
            try {
                dbSchemaCreator.executeScript(DROP_SCHEMA_SCRIP);
            }  catch (Exception ex) {
                // Ignore...
            }
            result = dbSchemaCreator.checkSchemaAndTablesIntegrity();
            assert !result;
            try {
                String err =      dbSchemaCreator.executeScript(CREATE_SCHEMA_SCRIP);
                if (err != null) {
                    LOGGER.log(Level.WARNING, 
                            I18n.loc("WLM-6044: SQL Exception in executing the script"), err);                    
                }
            } catch (Exception ex) {
                LOGGER.log(Level.WARNING, 
                        I18n.loc("WLM-6044: SQL Exception in executing the script"), ex.getMessage());
            }
//            dbSchemaCreator.executeScript(CREATE_SCHEMA_SCRIP);
            result = dbSchemaCreator.checkSchemaAndTablesIntegrity();

        }
        assert result;
        LOGGER.log(Level.INFO, I18n.loc("WLM-5026: Schema is created"));
        return recreated;
    }

    private SessionFactory getSessionFactory(DBType dbType) {
        String tempDir = System.getProperty(mIndexDirProp);
        String oldPath = "com/sun/jbi/engine/workflow/db/hibernate/";
        Configuration cfg = new Configuration().setProperty(
                "hibernate.dialect", getHibernateDialect(dbType)).setProperty(
                "hibernate.current_session_context_class", "thread")
                .setProperty("hibernate.show_sql", "false").setProperty(
                        "hibernate.cache.use_second_level_cache", "true")
                .setProperty("hibernate.cache.provider_class",
                        "net.sf.ehcache.hibernate.SingletonEhCacheProvider")
                .setProperty("hibernate.cache.use_query_cache", "true")
                .setProperty("net.sf.ehcache.configurationResourceName",
                        oldPath + HBM_CACHE_EHCACHE).setProperty(
                        "hibernate.search.default.directory_provider",
                        "org.hibernate.search.store.FSDirectoryProvider")
                .setProperty("hibernate.search.default.indexBase", tempDir)
                .setProperty(
                        "hibernate.connection.isolation",
                        new Integer(Connection.TRANSACTION_REPEATABLE_READ)
                                .toString());
        // cfg.setListener("post-update", new FullTextIndexEventListener ());
        cfg.setListener("post-insert", new FullTextIndexEventListener());
        // cfg.setListener("post-delete", new FullTextIndexEventListener ());
        addHibernateResources(cfg, dbType);
        return cfg.buildSessionFactory();

    }

    private void addHibernateResources(Configuration cfg, DBType dbType) {
        String oldPath = "com/sun/jbi/engine/workflow/db/hibernate/";
        String path = oldPath
                + ((dbType == DAO.ORACLE_TYPE) ? HBM_SEQUENCE_PATH
                        : HBM_IDENTITY_PATH);
        cfg.addResource(path + "/" + HBM_TASK_ASSIGNEE);
        cfg.addResource(path + "/" + HBM_TASK_INSTANCE);
        cfg.addResource(path + "/" + HBM_TASK_EXCLUDED_ASSIGNEE);
        cfg.addResource(path + "/" + HBM_TASK_TIMER);
        // cfg.addResource( oldPath + HBM_CACHE_EHCACHE);
        // cfg.addResource(path + "/" + HBM_TASK_VARIABLE);
        // cfg.addResource(path + "/" + HBM_TASK_ASSIGNEE_OLD);
    }

    private String getHibernateDialect(DBType dbType)
            throws ConnectionException {
        if (dbType == DAO.DERBY_TYPE) {
            return "org.hibernate.dialect.DerbyDialect";
        } else if (dbType == DAO.ORACLE_TYPE) {
            return "org.hibernate.dialect.Oracle9Dialect";
        } else if (dbType == DAO.SYBASE_TYPE) {
            return "org.hibernate.dialect.SybaseDialect";
        } else if (dbType == DAO.SQLSERVER_TYPE) {
            return "org.hibernate.dialect.SQLServerDialect";
        } else if (dbType == DAO.DB2_TYPE) {
            return "org.hibernate.dialect.DB2Dialect";
        } else if (dbType == DAO.MYSQL_TYPE) {
            return "org.hibernate.dialect.MySQL5Dialect";
        } else {
            String msg = I18n.loc(
                    "WLM-7004: Database type: {0} is not supported", dbType
                            .getDBType());
            LOGGER.log(Level.SEVERE, msg);
            throw new ConnectionException(msg);
        }
    }

    /**
     * Creates the task in the database if the task is a new task. If Task
     * already exists in the database (its exchangeId can be found), no db
     * record will be created, and the taskId will be the id from the existing
     * record, calling code should check the task's isNew() and only assign the
     * task when task is new.
     * 
     * @param task
     *            the RuntineTask to be created in db
     * @return the list of TaskTimer this task is associated with
     * @throws ConnectionException
     */
    public List<TaskTimer> createTask(RuntimeTask task)
            throws ConnectionException {
        List<TaskTimer> timers = new ArrayList<TaskTimer>();

        Connection conn = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            String exchangeId = task.getExchangeId();
            Task taskModel = task.getTaskMeta();
            String targetNs = taskModel.getTargetNamespace();
            QName qname = new QName(targetNs, taskModel.getName());
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            Query query = session
                    .createQuery(
                            "from TaskInstance as taskInst where taskInst.messageExchangeId = ?")
                    .setString(0, exchangeId);
            List taskInstances = query.list();
            if (taskInstances.size() > 0) {
                // don't proceed with any db operation, the task is already in
                // db
                TaskInstance taskInst = (TaskInstance) taskInstances.get(0);
                task.setId(taskInst.getId());
                task.setNew(false);

                task.setState(RuntimeTask.TaskState.getTaskState(taskInst.getStatus()));
                if (RuntimeTask.TaskState.isTerminalStatus(taskInst.getStatus())) {
                    TaskOutput taskOutput = null;
                    String taskOutputStr = taskInst.getOutputData();
                    if (taskOutputStr != null && taskOutputStr.length() > 0) {
                        Element taskOutputEl = XmlUtil.createDocumentFromXML(true,
                                taskOutputStr).getDocumentElement();
                        taskOutput = TaskModelFactory.getInstance().createTaskOutput(
                                taskOutputEl);
                        task.setOutput(taskOutput);
                    }
                }     
                session.getTransaction().commit();
                return timers;
            }

            TaskInstance taskInstance = new TaskInstance();
            taskInstance.setCreateDate(task.getCreateDate().getTime());

            taskInstance.setInputData(XmlUtil.toXml(task.getInput().getInput(),
                    "UTF-8", false));
            if (task.getOutput() != null) {
                taskInstance.setOutputData(XmlUtil.toXml(task.getOutput()
                        .getOutput(), "UTF-8", false));
            }
            taskInstance.setMessageExchangeId(exchangeId);
            taskInstance.setStatus(RuntimeTask.TaskState.UNASSIGNED.getState());
            taskInstance.setTaskDefId(qname.toString());
            if (task.getDescription() == null) {
                String title = task.getTaskMeta().getName();
                task.setDescription("This is a task for " + title);
            }
            taskInstance.setTitle(task.getDescription());

            taskInstance.setPriority(new Integer(task.getPriority()));
            if (task.getOutput() != null) {
                taskInstance.setOutputData(XmlUtil.toXml(task.getOutput()
                        .getOutput(), "UTF-8", false));
            }
            if (task.getKeywords() != null) {
                taskInstance.setKeyword(task.getKeywords());
            }
            session.save(taskInstance);

            task.setId(taskInstance.getId());

            // When timer can be found from taskModel, do the following:
            List<Escalation> escalations = taskModel.getTaskEscalations();
            if (escalations != null && escalations.size() > 0) {
                for (Escalation escalation : escalations) {
                    // Create a TasskTimer

                    TaskTimer timer = makeTimer(escalation, task);
                    timer.setTaskInstance(taskInstance);
                    session.save(timer);
                    timers.add(timer);
                }
            }
            List<Timeout> timeouts = taskModel.getTaskTimeouts();
            if (timeouts != null && timeouts.size() > 0) {
                for (Timeout timeout : timeouts) {
                    // Create a TasskTimer
                    TaskTimer timer = makeTimer(timeout, task);
                    timer.setTaskInstance(taskInstance);
                    session.save(timer);

                    timers.add(timer);
                }
            }
            // Sort the timers, and update the deadline of the task
            Date deadline = null;
            for (TaskTimer t : timers) {
                Date toCompare = t.getDueDate();
                if (deadline == null) {
                    deadline = toCompare;
                } else if (deadline.after(toCompare)) {
                    deadline = toCompare;
                }
            }
            if (deadline != null) {
                taskInstance.setDeadline(deadline);
            } else {
                // To make sort work, set Deadline to an initial date
                Date deadline_initial = Util.getInitialDate();
                taskInstance.setDeadline(deadline_initial);
            }
            session.update(taskInstance);
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                session.close();
                conn.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return timers;
    }

    private TaskTimer makeTimer(DeadlineOrDuration deadlineOrDuration,
            RuntimeTask task) throws ModelException {
        // TODO Auto-generated method stub
        TaskTimer timer = new TaskTimer();
        timer.setXpath(((ModelElement) deadlineOrDuration).getXPathInfo()
                .getXPath());
        if (deadlineOrDuration.getDeadlineObject(task.getJXpathContext()) != null) {
            timer.setDueDate(deadlineOrDuration.getDeadlineObject(task
                    .getJXpathContext()));
        } else {
            timer.setDueDate(deadlineOrDuration.getDurationDate(task
                    .getJXpathContext()));
        }

        timer.setStatus(1);
        return timer;

    }

    /**
     * Reassign the task to the specific principal, remove all assignees and
     * replaced with the new principals
     * 
     * @param taskId
     * @param oldPrincipal
     *            can be null
     * @param newPrincipal
     *            new principal to assign the task to
     * @throws ConnectionException
     */
    public void reAssignTask(Long taskId,
            Collection<TaskPrincipal> newPrincipals,
            Collection<TaskPrincipal> excludedUsers) throws ConnectionException {
        Connection conn = null;
        Session session = null;
        String newAssignees = TaskManagerHelper.makeString(newPrincipals);
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            Integer taskStatus = taskInstance.getStatus();
            if (taskStatus == RuntimeTask.TaskState.COMPLETED.getState()
                    || taskStatus == RuntimeTask.TaskState.EXPIRED.getState()
                    || taskStatus == RuntimeTask.TaskState.ABORTED.getState()
                    || taskStatus == RuntimeTask.TaskState.FAILED.getState()) {

                String msg = I18n.loc(
                        "WLM-6041: Task id: {0} is in status :{1}", taskId,
                        RuntimeTask.TaskState.getTaskState(taskStatus)
                                .getDescription());
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException(
                        "DBOperation.NOT_IN_CONTINUE_STATE", msg);
            }

            Query query2 = session
                    .createQuery(
                            "from TaskAssignee as taskAssignee where taskAssignee.taskInstance.id = ? and taskAssignee.activeAssignee = ?")
                    .setLong(0, taskId).setBoolean(1, true);

            Date __now = new Date();
            List<TaskAssignee> oldAssignees = query2.list();
            for (TaskAssignee oldTaskAssineeAssignee : oldAssignees) {
                // session.delete(oldTaskAssineeAssignee);
                oldTaskAssineeAssignee.setActiveAssignee(new Boolean(false));
                oldTaskAssineeAssignee.setUpdateDate(__now);
                session.update(oldTaskAssineeAssignee);
            }

            // Do it for excluded if any
            query2 = session
                    .createQuery(
                            "from TaskExcludedAssignee as taskAssignee where taskAssignee.taskInstance.id = ? and taskAssignee.activeAssignee = ?")
                    .setLong(0, taskId).setBoolean(1, true);
            List<TaskExcludedAssignee> oldAssigneeExcludeds = query2.list();
            for (TaskExcludedAssignee  oldTaskAssineeAssignee : oldAssigneeExcludeds) {
                // session.delete(oldTaskAssineeAssignee);
                oldTaskAssineeAssignee.setActiveAssignee(new Boolean(false));
                oldTaskAssineeAssignee.setUpdateDate(__now);
                session.update(oldTaskAssineeAssignee);
            }

            Iterator<TaskPrincipal> it = newPrincipals.iterator();
            while (it.hasNext()) {
                TaskPrincipal newPrincipal = it.next();
                String assigneeType = newPrincipal.getType().getTypeString();
                TaskAssignee assignee = new TaskAssignee();
                assignee.setTaskInstance(taskInstance);
                assignee.setAssignedStatus(RuntimeTask.TaskState.ASSIGNED
                        .getState());
                assignee.setAssignee(convertToAssigneeInTable(newPrincipal));
                assignee.setActiveAssignee(new Boolean(true));
                assignee.setUpdateDate(__now);
                assignee.setStartDate(__now);
                session.save(assignee);
            }
            // if <excluded> present
            if (excludedUsers != null && excludedUsers.size() > 0) {
                for (TaskPrincipal principal : excludedUsers) {
                    String assigneeType = principal.getType().getTypeString();
                    TaskExcludedAssignee assignee = new TaskExcludedAssignee();
                    assignee.setTaskInstance(taskInstance);
                    assignee.setAssignee(convertToAssigneeInTable(principal));
                    assignee.setActiveAssignee(new Boolean(true));
                    assignee.setUpdateDate(__now);
                    session.save(assignee);
                }
            }

            taskInstance.setOwner(null);
            if (taskInstance.getStatus() == RuntimeTask.TaskState.CLAIMED
                    .getState()) {
                taskInstance.setStatus(RuntimeTask.TaskState.ASSIGNED
                        .getState());
            }
            taskInstance.setAssignedTo(newAssignees);
            session.update(taskInstance);
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

    }

    /**
     * revoke the task. remove all old assignees and set new assignees which
     * were original specified in wf file
     * 
     * @param taskId
     * @param oldPrincipal
     *            can be null
     * @param newPrincipal
     *            new principal to assign the task to
     * @throws ConnectionException
     */
    public void revokeTask(Long taskId, Collection<TaskPrincipal> newPrincipals)
            throws ConnectionException {

        Connection conn = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            Integer taskStatus = taskInstance.getStatus();
            if (taskStatus != RuntimeTask.TaskState.CLAIMED.getState()) {
                String msg = I18n.loc(
                        "WLM-6041: Task id: {0} is in status :{1}", taskId,
                        RuntimeTask.TaskState.getTaskState(taskStatus)
                                .getDescription());
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException(
                        "DBOperation.NOT_IN_CONTINUE_STATE", msg);

            }
            // Get current Assignee
            Query query2 = session
                    .createQuery(
                            "from TaskAssignee as taskAssignee where taskAssignee.taskInstance.id = ? and taskAssignee.activeAssignee = ?")
                    .setLong(0, taskId).setBoolean(1, true);
            List<TaskAssignee> tobe_oldAssignees = query2.list();
            // get Previous Assignees
            Query query3 = session
                    .createQuery(
                            "from TaskAssignee as taskAssignee where taskAssignee.taskInstance.id = ? and taskAssignee.activeAssignee = ? order by taskAssignee.updateDate desc")
                    .setLong(0, taskId).setBoolean(1, false);
            List<TaskAssignee> previousAssignees = query3.list();
            List<TaskAssignee> tobe_newAssignees = new ArrayList<TaskAssignee>();
            Date __updated = null;
            // Get Latest previous assignees
            for (TaskAssignee preAssignee : previousAssignees) {
                if (__updated == null) {
                    __updated = preAssignee.getUpdateDate();
                    tobe_newAssignees.add(preAssignee);
                } else if (__updated.equals(preAssignee.getUpdateDate())) {
                    tobe_newAssignees.add(preAssignee);
                } else {
                    break;
                }
            }
            // Update Assignee status
            Date __now = new Date();
            for (TaskAssignee oldAssignee : tobe_oldAssignees) {
                oldAssignee.setActiveAssignee(new Boolean(false));
                oldAssignee.setUpdateDate(__now);
                session.update(oldAssignee);
            }
            for (TaskAssignee newAssignee : tobe_newAssignees) {
                newAssignee.setActiveAssignee(new Boolean(true));
                newAssignee.setUpdateDate(__now);
                newAssignee.setStartDate(__now);
                session.update(newAssignee);
            }
            String newAssignees = TaskManagerHelper
                    .makeStringFromTaskAssignee(tobe_newAssignees);
            taskInstance.setOwner(null);
            if (taskInstance.getStatus() == RuntimeTask.TaskState.CLAIMED
                    .getState()) {
                taskInstance.setStatus(RuntimeTask.TaskState.ASSIGNED
                        .getState());
            }

            session.update(taskInstance);
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

    }

    public void assignTask(RuntimeTask task,
            Collection<TaskPrincipal> principals,
            Collection<TaskPrincipal> excludedUsers) throws ConnectionException {
        Connection conn = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            Long taskId = task.getId();

            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            Date curDate = new Date();
            for (TaskPrincipal principal : principals) {
                String assigneeType = principal.getType().getTypeString();

                // Query query = session
                // .createQuery(
                // "from TaskAssignee as taskAssignee where
                // taskAssignee.assignee = ? and taskAssignee.assigneeType = ?
                // and taskAssignee.taskInstance.id = ? and
                // taskAssignee.activeAssignee = ?")
                // .setString(0, principal.getName()).setString(1,
                // assigneeType).setLong(2, taskId).setBoolean(3,
                // true).setBoolean(4, false);
                //
                // if (query.list().size() > 0) {
                // // Already assigned, do nothing
                // session.getTransaction().commit();
                // continue;
                // }
                TaskAssignee assignee = new TaskAssignee();
                assignee.setTaskInstance(taskInstance);
                assignee.setAssignedStatus(RuntimeTask.TaskState.ASSIGNED
                        .getState());
                assignee.setAssignee(convertToAssigneeInTable(principal));
                assignee.setActiveAssignee(new Boolean(true));
                assignee.setUpdateDate(curDate);
                assignee.setStartDate(curDate);
                session.save(assignee);
            }

            if (excludedUsers != null && excludedUsers.size() > 0) {
                for (TaskPrincipal principal : excludedUsers) {
                    String assigneeType = principal.getType().getTypeString();

                    // Query query = session
                    // .createQuery(
                    // "from TaskAssignee as taskAssignee where
                    // taskAssignee.assignee = ? and taskAssignee.assigneeType =
                    // ? and taskAssignee.taskInstance.id = ? and
                    // taskAssignee.activeAssignee = ? and
                    // taskAssignee.excludedAssignee = ?")
                    // .setString(0, principal.getName()).setString(1,
                    // assigneeType).setLong(2, taskId).setBoolean(3,
                    // true).setBoolean(4, true);
                    //
                    // if (query.list().size() > 0) {
                    // // Already assigned, do nothing
                    // session.getTransaction().commit();
                    // continue;
                    // }
                    TaskExcludedAssignee assignee = new TaskExcludedAssignee();
                    assignee.setTaskInstance(taskInstance);
                    assignee.setAssignee(convertToAssigneeInTable(principal));
                    assignee.setActiveAssignee(new Boolean(true));
                    assignee.setUpdateDate(curDate);
                    session.save(assignee);
                }
            }
            String assignees = TaskManagerHelper.makeString(principals);
            taskInstance.setAssignedTo(assignees);
            taskInstance.setStatus(RuntimeTask.TaskState.ASSIGNED.getState());
            session.update(taskInstance);
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public List<TaskAssignee> getOldAssignees(Long taskId)
            throws ConnectionException {
        Connection conn = null;
        Session session = null;
        List<TaskAssignee> toReturn = new ArrayList<TaskAssignee>();
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            Query query3 = session
                    .createQuery(
                            "from TaskAssignee as taskAssignee where taskAssignee.taskInstance.id = ? and taskAssignee.activeAssignee = ?  order by taskAssignee.updateDate desc")
                    .setLong(0, taskId).setBoolean(1, false);
            List<TaskAssignee> previousAssignees = query3.list();
            Date __updated = null;
            session.getTransaction().commit();
            // Get Latest previous assignees
            for (TaskAssignee preAssignee : previousAssignees) {
                if (__updated == null) {
                    __updated = preAssignee.getUpdateDate();
                    toReturn.add(preAssignee);
                } else if (__updated.equals(preAssignee.getUpdateDate())) {
                    toReturn.add(preAssignee);
                } else {
                    break;
                }
            }

        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return toReturn;
    }

    public void claimTask(Long taskId, TaskPrincipal principal)
            throws ConnectionException {
        Connection conn = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            String assigneeType = principal.getType().getTypeString();

            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            if (taskInstance.getOwner() != null) {
                String msg = I18n.loc(
                        "WLM-6042: Task id: {0} is already claimed by {1}",
                        taskId, taskInstance.getOwner());
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.ALREADY_CLAIMED",
                        msg);
            }
            Query query = session
                    .createQuery(
                            "from TaskAssignee as taskAssignee where taskAssignee.taskInstance = ? and taskAssignee.activeAssignee = ?")
                    .setCacheable(true).setEntity(0, taskInstance).setBoolean(
                            1, true);
            List<TaskAssignee> taskAssigneeList = query.list();
            Date startDate = null;
            Date __now = new Date();
            // Set all old assignees to inactive
            for (TaskAssignee assignee : taskAssigneeList) {
                // TaskAssigneeOld oldAssignee = new TaskAssigneeOld ();
                // oldAssignee.setAssignedStatus(assignee.getAssignedStatus());
                // oldAssignee.setAssignee(assignee.getAssignee());
                // oldAssignee.setAssigneeType(assignee.getAssigneeType());
                // oldAssignee.setCorId(newAssignee.getId());
                // oldAssignee.setTaskInstance(newAssignee.getTaskInstance());
                // oldAssignee.setUpdateDate(new Date ());
                assignee.setActiveAssignee(new Boolean(false));
                startDate = assignee.getStartDate();
                assignee.setUpdateDate(__now);
                // session.save(oldAssignee);
                // session.delete(assignee);
                session.update(assignee);
            }
            if (startDate == null) {
                startDate = __now;
            }

            TaskAssignee newAssignee = new TaskAssignee();
            newAssignee.setTaskInstance(taskInstance);
            newAssignee.setAssignedStatus(RuntimeTask.TaskState.CLAIMED
                    .getState());
            newAssignee.setAssignee(convertToAssigneeInTable(principal));
            newAssignee.setActiveAssignee(new Boolean(true));
            newAssignee.setUpdateDate(__now);
            newAssignee.setStartDate(startDate);
            session.save(newAssignee);

            session.update(newAssignee);

            taskInstance.setStatus(RuntimeTask.TaskState.CLAIMED.getState());
            taskInstance.setOwner(principal.getName());
            session.update(taskInstance);
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

    }

    public String getTaskInput(Long taskId) throws ConnectionException {
        Connection conn = null;
        String inputStr = "";
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            inputStr = taskInstance.getInputData();
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return inputStr;
    }

    public String getTaskOutput(Long taskId) throws ConnectionException {
        Connection conn = null;
        String outputStr = "";
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            outputStr = taskInstance.getOutputData();
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return outputStr;
    }

    public void setTaskOutput(Long taskId, String outputStr)
            throws ConnectionException {
        Connection conn = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            taskInstance.setOutputData(outputStr);
            session.update(taskInstance);
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public List<TaskTimer> completeTask(Long taskId, TaskPrincipal user)
            throws ConnectionException {
        Connection conn = null;
        List<TaskTimer> taskTimerList = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            Date _curDate = new Date();
            taskInstance.setStatus(RuntimeTask.TaskState.COMPLETED.getState());
            taskInstance.setEndDate(_curDate);
            taskInstance.setCompletedBy(user.getName());
            session.update(taskInstance);
            Query query = session
                    .createQuery(
                            "from TaskTimer as taskTimer where taskTimer.taskInstance = ? and taskTimer.status = ?")
                    .setCacheable(true).setLong(0, taskId).setInteger(1, 1);
            taskTimerList = query.list();
            for (TaskTimer timer : taskTimerList) {
                timer.setStatus(3);
                session.update(timer);
            }
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return taskTimerList;
    }

    /**
     * Time out a task, returns the other timers associated with the task which
     * are still active
     * 
     * @param taskId
     * @param timerId
     *            The timer that is expiring
     * @return a list of timers (other than the timer with the timerId passed
     *         in) which are still active
     * @throws ConnectionException
     */
    public List<TaskTimer> timeoutTask(Long taskId, Long timerId)
            throws ConnectionException {
        Connection conn = null;
        List<TaskTimer> taskTimerList = null;
        List<TaskTimer> cancalTimers = new ArrayList<TaskTimer>();
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            taskInstance.setStatus(RuntimeTask.TaskState.EXPIRED.getState());
            taskInstance.setEndDate(new Date());
            session.update(taskInstance);
            Query query = session
                    .createQuery(
                            "from TaskTimer as taskTimer where taskTimer.taskInstance = ? and taskTimer.status = ?")
                    .setLong(0, taskId).setInteger(1, 1);
            taskTimerList = query.list();
            for (TaskTimer timer : taskTimerList) {
                if (timer.getId() == timerId) {
                    timer.setStatus(2);
                } else {
                    timer.setStatus(3);
                    cancalTimers.add(timer);
                }
                session.update(timer);
            }
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return cancalTimers;
    }

    /**
     * Escalate a task, returns the other timers associated with the task which
     * are still active
     * 
     * @param taskId
     * @param timerId
     *            The timer that is expiring
     * @param newOwner
     *            The new owner of the task
     * @throws ConnectionException
     */
    public void escalateTask(Long taskId, Long timerId,
            List<TaskPrincipal> newOwners,
            Collection<TaskPrincipal> excludedUsers) throws ConnectionException {
        Connection conn = null;
        Session session = null;
        String newAssignees = TaskManagerHelper.makeString(newOwners);
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            TaskInstance taskInstance = (TaskInstance) session.load(
                    TaskInstance.class, taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            taskInstance.setStatus(RuntimeTask.TaskState.ESCALATED.getState());
            String assignedTo = taskInstance.getAssignedTo();
            assignedTo = newAssignees + "," + assignedTo;
            taskInstance.setAssignedTo(assignedTo);

            Set<TaskTimer> timers = taskInstance.getTimers();
            Hibernate.initialize(timers);

            // upate Tasktimer and update task deadline
            Date newDeadline = null;
            for (TaskTimer timer : timers) {
                Date toCompare = null;
                if (timer.getId() == timerId) {
                    timer.setStatus(2);
                    session.update(timer);
                } else if (timer.getStatus() != 2) {
                    toCompare = timer.getDueDate();
                    if (newDeadline == null) {
                        newDeadline = timer.getDueDate();
                    } else {
                        if (newDeadline.after(toCompare)) {
                            newDeadline = toCompare;
                        }
                    }
                }
            }
            if (newDeadline != null) {
                taskInstance.setDeadline(newDeadline);
            }
            session.update(taskInstance);
            // TaskTimer timer = (TaskTimer) session
            // .load(TaskTimer.class, timerId);
            // if (taskInstance == null) {
            // String msg = I18n.loc("WLM-6039: Timer id: {0} is not found",
            // timerId);
            // LOGGER.log(Level.WARNING, msg);
            // throw new ConnectionException("DBOperation.CAN_NOT_FIND_TIMER",
            // msg);
            // }
            // timer.setStatus(2);
            // session.update(timer);
            // Set current excluded assignees to inactive
            Query query = session
                    .createQuery(
                            "from TaskExcludedAssignee as taskAssignee where taskAssignee.taskInstance = ? and taskAssignee.activeAssignee = ?")
                    .setEntity(0, taskInstance).setBoolean(1, true);
            List<TaskExcludedAssignee> taskAssigneeList = query.list();
            Date __now = new Date();
            for (TaskExcludedAssignee assignee : taskAssigneeList) {
                // session.delete(assignee);
                assignee.setActiveAssignee(new Boolean(false));
                assignee.setUpdateDate(__now);
                session.update(assignee);
            }
            // Escalate to new active Assignee
            for (TaskPrincipal newOwner : newOwners) {
                String assigneeType = newOwner.getType().getTypeString();

                TaskAssignee assignee = new TaskAssignee();
                assignee.setTaskInstance(taskInstance);
                assignee.setAssignedStatus(RuntimeTask.TaskState.ASSIGNED
                        .getState());
                assignee.setAssignee(convertToAssigneeInTable(newOwner));
                assignee.setActiveAssignee(new Boolean(true));
                assignee.setUpdateDate(__now);
                assignee.setStartDate(__now);
                session.save(assignee);
            }
            if (excludedUsers != null && excludedUsers.size() > 0) {
                for (TaskPrincipal principal : excludedUsers) {
                    String assigneeType = principal.getType().getTypeString();

                    TaskExcludedAssignee assignee = new TaskExcludedAssignee();
                    assignee.setTaskInstance(taskInstance);
                    assignee.setAssignee(convertToAssigneeInTable(principal));
                    assignee.setActiveAssignee(new Boolean(true));
                    assignee.setUpdateDate(__now);
                    session.save(assignee);
                }
            }
            taskInstance.setOwner(null);

            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public TasklistResult getAssignedTaskList(QueryType query, int startIndex,
            int pageSize) throws ConnectionException {
        long before = System.currentTimeMillis();
        // long before1 =0 ;
        // long before0 =0;
        // long before2 =0;
        //        
        // long after0 =0;
        // long after1 =0;
        // long after2 =0;

        TasklistResult taskListResult = new TasklistResult();
        List<Object> taskList = null;
        List<Object> taskExcludedList = null;
        Connection conn = null;
        Session session = null;
        List<Long> textSearchResult = new ArrayList<Long>();
        // Text search, just return the text search result
        boolean textSearchIncluded = false;
        if (query.getType() == QueryType.Type.TEXTSEARCH) {
            taskListResult = getTextSearchResult(query.getSearchString(),
                    startIndex, pageSize);
            return taskListResult;
        }

        if (query.getSearchString() != null
                && query.getSearchString().trim().length() > 0) {
            textSearchIncluded = true;
            textSearchResult = getTextSearchResult(query.getSearchString());
        }

        List<String> queryUsers = new ArrayList<String>();
        Collection<Integer> queryStates = null;

        List<String> queryExcluded = new ArrayList<String>();
        List<Long> excludedIds = null;
        int excludedNum = 0;

        DeadlineQueryType deadlineQuey = query.getDeadlineQuery();
        Calendar deadlineCal = null;

        if (deadlineQuey != null && deadlineQuey.getPeriod() > 0) {
            deadlineCal = Calendar.getInstance();
            int amount = deadlineQuey.getPeriod();
            if (deadlineQuey.getPeriodType().intValue() == DeadlineQueryType.PeriodType.INT_HOUR) {
                deadlineCal.add(Calendar.HOUR_OF_DAY, amount);
            } else if (deadlineQuey.getPeriodType().intValue() == DeadlineQueryType.PeriodType.INT_DAY) {
                deadlineCal.add(Calendar.DATE, amount);
            } else if (deadlineQuey.getPeriodType().intValue() == DeadlineQueryType.PeriodType.INT_WEEK) {
                deadlineCal.add(Calendar.WEEK_OF_MONTH, amount);
            } else if (deadlineQuey.getPeriodType().intValue() == DeadlineQueryType.PeriodType.INT_MONTH) {
                deadlineCal.add(Calendar.MONTH, amount);
            }
        }

        SortField.Enum sort = query.getSort();
        Direction.Enum direction = query.getDir();

        GroupsType groups = query.getGroups();
        UsersType users = query.getUsers();
        List<TaskStatus.Enum> taskStatuses = query.getTaskStatusList();

        UsersType excludedUsers = query.getExcluded() == null ? null : query
                .getExcluded().getUsers();
        GroupsType excludedGroups = query.getExcluded() == null ? null : query
                .getExcluded().getGroups();

        if (groups != null && groups.getGroupList() != null
                && groups.getGroupList().size() > 0) {
            List<String> groupList = groups.getGroupList();
            for (String obj : groupList) {
                if (obj.trim().length() > 0) {
                    queryUsers
                            .add(convertToAssigneeInTable(new TaskGroupPrincipalImpl(
                                    obj.trim())));
                }
            }
        }

        if (users != null && users.getUserList() != null
                && users.getUserList().size() > 0) {
            List<String> userList = users.getUserList();
            for (String obj : userList) {
                if (obj.trim().length() > 0) {
                    queryUsers
                            .add(convertToAssigneeInTable(new TaskUserPrincipalImpl(
                                    obj.trim())));
                }
            }
        }

        if (excludedGroups != null && excludedGroups.getGroupList() != null
                && excludedGroups.getGroupList().size() > 0) {
            List<String> excludedGroupList = excludedGroups.getGroupList();
            for (String obj : excludedGroupList) {
                if (obj.trim().length() > 0) {
                    queryExcluded
                            .add(convertToAssigneeInTable(new TaskGroupPrincipalImpl(
                                    obj.trim())));
                }
            }
        }

        if (excludedUsers != null && excludedUsers.getUserList() != null
                && excludedUsers.getUserList().size() > 0) {
            List<String> excludedUserList = excludedUsers.getUserList();
            for (String obj : excludedUserList) {
                if (obj.trim().length() > 0) {
                    queryExcluded
                            .add(convertToAssigneeInTable(new TaskUserPrincipalImpl(
                                    obj.trim())));
                }
            }
        }

        if (taskStatuses != null && taskStatuses.size() > 0) {
            queryStates = new ArrayList<Integer>(taskStatuses.size());
            for (TaskStatus.Enum taskStatus : taskStatuses) {
                Integer status = RuntimeTask.TaskState.getTaskState(
                        taskStatus.toString()).getState();
                queryStates.add(status);
            }
        }

        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            // session.beginTransaction();

            Criterion statusRestriction = null;
            Criterion deadlineRestriction = null;
            // For row count
            Criteria criteriaA = session.createCriteria(TaskInstance.class)
                    .setCacheable(true);
            // .setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY) ;
            // For details
            Criteria criteriaB = session.createCriteria(TaskInstance.class)
                    .setCacheable(true);
            // .setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY) ;
            // Add sorting, pagination to criteriaB
            if (direction == null) {
                direction = Direction.ASC;
            }
            if (sort != null) {
                Order order = null;
                if (direction == Direction.ASC) {
                    order = Order.asc(sort.toString());
                } else {
                    order = Order.desc(sort.toString());
                }
                if (order != null) {
                    criteriaB.addOrder(order);
                }
            }

            if (queryStates != null) {
                statusRestriction = Restrictions.in("status", queryStates);
                criteriaA.add(statusRestriction);
                criteriaB.add(statusRestriction);
            }

            // Adding deadline criteria
            if (deadlineCal != null) {
                deadlineRestriction = Restrictions.le("deadline", deadlineCal
                        .getTime());
                criteriaA.add(deadlineRestriction);
                criteriaB.add(deadlineRestriction);
            }
            // Adding text search
            if (textSearchIncluded) {

                if (textSearchResult.size() > 0) {
                    Criterion textSearchRestriction = Restrictions.in("id",
                            textSearchResult);
                    criteriaA.add(textSearchRestriction);
                    criteriaB.add(textSearchRestriction);
                } else {
                    // finding no match on textsearch, meaning the rest of the
                    // search is aborted
                    return taskListResult;
                }
            }

            // Add count (*) to criteriaA
            criteriaA = criteriaA
                    .setProjection(Projections.countDistinct("id"));
            // Optimize the query to not load input, output LOBS
            ProjectionList projectionList = Projections.projectionList();
            projectionList = projectionList.add(Projections.property("id"))
                    .add(Projections.property("title")).add(
                            Projections.property("status")).add(
                            Projections.property("priority")).add(
                            Projections.property("owner")).add(
                            Projections.property("assignedTo")).add(
                            Projections.property("completedBy")).add(
                            Projections.property("failedCode")).add(
                            Projections.property("failedReason")).add(
                            Projections.property("createDate")).add(
                            Projections.property("keyword")).add(
                            Projections.property("deadline")).add(
                            Projections.property("completedBy")).add(
                            Projections.property("taskDefId"));
            criteriaB.setProjection(projectionList);

            // Exclucde:
            // Process excluded users/groups
            // The QuertyType is created by passing all of the user's identities
            // to query Task_Assignee table
            // and finds tasks assignment specifically excluding these
            // identities. these are the tasks that will be
            // removed.
            if (queryExcluded != null && queryExcluded.size() > 0) {
                Criteria criteriaExcluded = session.createCriteria(
                        TaskInstance.class).setCacheable(true);
                ProjectionList excludedProjectList = Projections
                        .projectionList();
                excludedProjectList.add(Projections.distinct(Projections
                        .property("id")));
                criteriaExcluded.setProjection(excludedProjectList);

                Criterion userExcludedRestriction = null;
                userExcludedRestriction = Restrictions.and(Restrictions.eq(
                        "activeAssignee", Boolean.TRUE), Restrictions.in(
                        "assignee", queryExcluded));
                criteriaExcluded = criteriaExcluded.setFetchMode(
                        "excludedAssignees", FetchMode.JOIN).createCriteria(
                        "excludedAssignees");
                criteriaExcluded.add(userExcludedRestriction);
                // before0 = System.currentTimeMillis();
                taskExcludedList = criteriaExcluded.list();
                // after0 = System.currentTimeMillis();
                if (taskExcludedList != null && taskExcludedList.size() > 0) {
                    if (pageSize != -1 && startIndex != -1 && pageSize != 0) {
                        pageSize = pageSize + taskExcludedList.size();
                    }
                    excludedIds = new ArrayList<Long>(taskExcludedList.size());
                    for (Object row : taskExcludedList) {
                        excludedIds.add((Long) row);
                    }
                    excludedNum = taskExcludedList.size();
                }
            }

            if (pageSize != -1 && startIndex != -1 && pageSize != 0) {
                criteriaB.setFirstResult(startIndex);
                criteriaB.setMaxResults(pageSize);
            }
            // For criteriaA: the count (distinct Id) has better performance
            // than subquery
            // For criteriaB: the subquery has bette performance than disctinct
            // (...)

            if (queryUsers != null && queryUsers.size() > 0) {
                DetachedCriteria subquery = null;
                subquery = DetachedCriteria.forClass(TaskAssignee.class, "a");
                subquery.setProjection(Projections.property("a.taskInstance"));
                Criterion userRestriction = null;
                userRestriction = Restrictions.and(Restrictions.eq(
                        "a.activeAssignee", Boolean.TRUE), Restrictions.in(
                        "a.assignee", queryUsers));
                subquery.add(userRestriction);

                criteriaB.add(Subqueries.propertyIn("id", subquery));
                criteriaA = criteriaA.setFetchMode("assignees", FetchMode.JOIN)
                        .createCriteria("assignees", "a").add(userRestriction);
            }
            // before1 = System.currentTimeMillis();
            Long count = (Long) criteriaA.uniqueResult();
            // after1 = System.currentTimeMillis();

            int total = count == null ? 0 : count.intValue();

            total = total - excludedNum;

            // before2 = System.currentTimeMillis();
            taskList = criteriaB.list();
            // after2 = System.currentTimeMillis();
            // session.getTransaction().commit();

            List<TaskItem> taskItems = new ArrayList<TaskItem>();

            if (taskList != null) {
                int num = 0;
                for (Object taskInstance : taskList) {
                    Long taskId = (Long) ((Object[]) taskInstance)[0];
                    if (excludedIds != null && excludedIds.contains(taskId)) {
                        continue;
                    }
                    num++;
                    if (pageSize != -1 && startIndex != -1 && pageSize != 0
                            && num > pageSize) {
                        break;
                    }
                    TaskItem item = new TaskItem((Object[]) taskInstance);
                    taskItems.add(item);
                }
            }

            taskListResult.setTaskList(taskItems);
            taskListResult.setTotalRecords(total);

        } catch (SQLException sqlException) {
            // session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
            // session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            // session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        long after = System.currentTimeMillis();
        long duration = after - before;
        // long duration0 = after0 - before0;
        // long duration1 = after1 - before1;
        // long duration2 = after2 - before2;
        // LOGGER.log(Level.INFO, "Query Duration0 " + duration0);
        // LOGGER.log(Level.INFO, "Query Duration1 " + duration1);
        // LOGGER.log(Level.INFO, "Query Duration2 " + duration2);
        LOGGER.log(Level.INFO, "Query getTaskList " + duration);
        return taskListResult;
    }

    public TasklistResult getJDBCAssignedTaskList(QueryType query,
            int startIndex, int pageSize) throws ConnectionException {
        long before = System.currentTimeMillis();
        // long before1 =0 ;
        // long before0 =0;
        // long before2 =0;
        //        
        // long after0 =0;
        // long after1 =0;
        // long after2 =0;
        final String COUNT_TOTAL_QUERY = "select count(distinct instance.ID) from Task_Instance instance inner join Task_Assignee assignee on instance.ID=assignee.TASK_ID where instance.STATUS in (1, 2, 5) and (assignee.ACTIVE_ASSIGNEE=1 and assignee.ASSIGNEE in TOKEN_USER)"; // ('[GROUP]CustomerServiceRep',
                                                                                                                                                                                                                                                                                        // '[USER]john'))"

        final String TASKLIST_QUERY = "select instance.ID, instance.TITLE, instance.STATUS, instance.PRIORITY, instance.OWNER, instance.ASSIGNEDTO, instance.CompletedBY, instance.FAILED_CODE, instance.FAILED_REASON, instance.CREATEDATE from Task_Instance instance where instance.STATUS in (1, 2, 5) and instance.ID in (select a_.TASK_ID as y0_ from Task_Assignee a_ where (a_.ACTIVE_ASSIGNEE=1 and a_.ASSIGNEE in TOKEN_USER)) order by instance.ID asc limit TOKEN_START, TOKEN_LIMIT";

        final String EXCLUDED_QUERY = "select instance.ID  from Task_Instance instance inner join Task_Excluded_Assignee excluded on instance.ID=excluded.TASK_ID where instance.STATUS in (1, 2, 5) and (excluded.ACTIVE_ASSIGNEE=1 and excluded.ASSIGNEE in TOKEN_USER)"; // ('[GROUP]CustomerServiceRep',
                                                                                                                                                                                                                                                                            // '[USER]john'))"

        TasklistResult taskListResult = new TasklistResult();
        // List<Object> taskList = null;
        // List<Object> taskExcludedList = null;
        Connection conn = null;
        // Session session = null;

        List<String> queryUsers = new ArrayList<String>();
        Collection<Integer> queryStates = null;

        List<String> queryExcluded = new ArrayList<String>();
        List<Long> excludedIds = new ArrayList<Long>();
        int excludedNum = 0;

        // SortField.Enum sort = query.getSort();
        // Direction.Enum direction = query.getDir();

        GroupsType groups = query.getGroups();
        UsersType users = query.getUsers();
        List<TaskStatus.Enum> taskStatuses = query.getTaskStatusList();

        UsersType excludedUsers = query.getExcluded() == null ? null : query
                .getExcluded().getUsers();
        GroupsType excludedGroups = query.getExcluded() == null ? null : query
                .getExcluded().getGroups();

        if (groups != null && groups.getGroupList() != null
                && groups.getGroupList().size() > 0) {
            List<String> groupList = groups.getGroupList();
            for (String obj : groupList) {
                queryUsers
                        .add(convertToAssigneeInTable(new TaskGroupPrincipalImpl(
                                obj)));
            }
        }

        if (users != null && users.getUserList() != null
                && users.getUserList().size() > 0) {
            List<String> userList = users.getUserList();
            for (String obj : userList) {
                queryUsers
                        .add(convertToAssigneeInTable(new TaskUserPrincipalImpl(
                                obj)));
            }
        }

        if (excludedGroups != null && excludedGroups.getGroupList() != null
                && excludedGroups.getGroupList().size() > 0) {
            List<String> excludedGroupList = excludedGroups.getGroupList();
            for (String obj : excludedGroupList) {
                queryExcluded
                        .add(convertToAssigneeInTable(new TaskGroupPrincipalImpl(
                                obj)));
            }
        }

        if (excludedUsers != null && excludedUsers.getUserList() != null
                && excludedUsers.getUserList().size() > 0) {
            List<String> excludedUserList = excludedUsers.getUserList();
            for (String obj : excludedUserList) {
                queryExcluded
                        .add(convertToAssigneeInTable(new TaskUserPrincipalImpl(
                                obj)));
            }
        }

        if (taskStatuses != null && taskStatuses.size() > 0) {
            queryStates = new ArrayList<Integer>(taskStatuses.size());
            for (TaskStatus.Enum taskStatus : taskStatuses) {
                Integer status = RuntimeTask.TaskState.getTaskState(
                        taskStatus.toString()).getState();
                queryStates.add(status);
            }
        }
        Statement countst = null;
        Statement instst = null;
        Statement excludest = null;
        try {
            conn = mDataSource.getConnection();
            DatabaseMetaData metaData = conn.getMetaData();
            ResultSet res = metaData.getTypeInfo();
            while (res.next()) {
                String type = res.getString(1);
                int typeInt = res.getInt(2);
                System.out.println("Type:" + type);
                // System.out.println("TypeInt:" + type);

            }
            // session = mSessionFactory.openSession(conn);
            // session.beginTransaction();

            // Criterion statusRestriction = null;
            // // For row count
            // Criteria criteriaA = session.createCriteria(TaskInstance.class)
            // .setCacheable(true);
            // // .setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY) ;
            // // For details
            // Criteria criteriaB = session.createCriteria(TaskInstance.class)
            // .setCacheable(true);
            // // .setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY) ;
            // // Add sorting, pagination to criteriaB
            // if (direction == null) {
            // direction = Direction.ASC;
            // }
            // if (sort != null) {
            // Order order = null;
            // if (direction == Direction.ASC) {
            // order = Order.asc(sort.toString());
            // } else {
            // order = Order.desc(sort.toString());
            // }
            // if (order != null) {
            // criteriaB.addOrder(order);
            // }
            // }

            String userCriteria = "(";
            if (queryUsers != null && queryUsers.size() > 0) {
                boolean begin = true;
                for (String user : queryUsers) {
                    if (!begin) {
                        userCriteria = userCriteria + "," + "'" + user + "'";
                    } else {
                        userCriteria = userCriteria + "'" + user + "'";
                        begin = false;
                    }
                }
            }
            userCriteria = userCriteria + ")";

            String countQuery = COUNT_TOTAL_QUERY.replaceAll("TOKEN_USER",
                    userCriteria);
            String instanceQuery = TASKLIST_QUERY.replaceAll("TOKEN_USER",
                    userCriteria);
            String excludedQuery = EXCLUDED_QUERY.replaceAll("TOKEN_USER",
                    userCriteria);

            excludest = conn.createStatement();
            ResultSet excludeRs = excludest.executeQuery(excludedQuery);
            while (excludeRs.next()) {
                long id = excludeRs.getLong(1);
                excludedIds.add(id);
            }
            if (excludedIds.size() > 0) {
                if (pageSize != -1 && startIndex != -1 && pageSize != 0) {
                    pageSize = pageSize + excludedIds.size();
                }
            }
            countst = conn.createStatement();
            ResultSet countRs = countst.executeQuery(countQuery);

            int total = 0;

            while (countRs.next()) {
                total = countRs.getInt(1);
                break;
            }

            // if (queryStates != null) {
            // statusRestriction = Restrictions.in("status", queryStates);
            // criteriaA.add(statusRestriction);
            // criteriaB.add(statusRestriction);
            // }
            // // Add count (*) to criteriaA
            // criteriaA =
            // criteriaA.setProjection(Projections.countDistinct("id"));
            // // Optimize the query to not load input, output LOBS
            // ProjectionList projectionList = Projections.projectionList();
            // projectionList = projectionList.add(Projections.property("id"))
            // .add(Projections.property("title")).add(
            // Projections.property("status")).add(
            // Projections.property("priority")).add(
            // Projections.property("owner")).add(
            // Projections.property("assignedTo")).add(
            // Projections.property("completedBy")).add(
            // Projections.property("failedCode")).add(
            // Projections.property("failedReason")).add(
            // Projections.property("createDate"));
            // criteriaB.setProjection(projectionList);

            // Exclucde:
            // Process excluded users/groups
            // The QuertyType is created by passing all of the user's identities
            // to query Task_Assignee table
            // and finds tasks assignment specifically excluding these
            // identities. these are the tasks that will be
            // removed.
            // if (queryExcluded != null && queryExcluded.size() > 0) {
            // Criteria criteriaExcluded = session.createCriteria(
            // TaskInstance.class).setCacheable(true);
            // ProjectionList excludedProjectList = Projections
            // .projectionList();
            // excludedProjectList.add(Projections.distinct(Projections
            // .property("id")));
            // criteriaExcluded.setProjection(excludedProjectList);
            //
            // Criterion userExcludedRestriction = null;
            // userExcludedRestriction = Restrictions.and(Restrictions.eq(
            // "activeAssignee", Boolean.TRUE), Restrictions.in(
            // "assignee", queryExcluded));
            // criteriaExcluded = criteriaExcluded.setFetchMode(
            // "excludedAssignees", FetchMode.JOIN).createCriteria(
            // "excludedAssignees");
            // criteriaExcluded.add(userExcludedRestriction);
            // // before0 = System.currentTimeMillis();
            // taskExcludedList = criteriaExcluded.list();
            // // after0 = System.currentTimeMillis();
            // if (taskExcludedList != null && taskExcludedList.size() > 0) {
            // if (pageSize != -1 && startIndex != -1 && pageSize != 0) {
            // pageSize = pageSize + taskExcludedList.size();
            // }
            // excludedIds = new ArrayList<Long>(taskExcludedList.size());
            // for (Object row : taskExcludedList) {
            // excludedIds.add((Long) row);
            // }
            // excludedNum = taskExcludedList.size();
            // }
            // }

            if (pageSize != -1 && startIndex != -1 && pageSize != 0) {
                // criteriaB.setFirstResult(startIndex);
                // criteriaB.setMaxResults(pageSize);
                instanceQuery = instanceQuery.replaceAll("TOKEN_START",
                        new Integer(startIndex).toString());
                instanceQuery = instanceQuery.replaceAll("TOKEN_LIMIT",
                        new Integer(pageSize).toString());

            }

            instst = conn.createStatement();
            ResultSet instRs = countst.executeQuery(instanceQuery);
            int num = 0;
            List<TaskItem> taskItems = new ArrayList<TaskItem>();
            while (instRs.next()) {
                Long taskId = (Long) (instRs.getLong(1));
                if (excludedIds != null && excludedIds.contains(taskId)) {
                    continue;
                }
                num++;
                if (pageSize != -1 && startIndex != -1 && pageSize != 0
                        && num > pageSize) {
                    break;
                }
                TaskItem item = new TaskItem(instRs);
                taskItems.add(item);
            }
            // For criteriaA: the count (distinct Id) has better performance
            // than subquery
            // For criteriaB: the subquery has bette performance than disctinct
            // (...)

            // if (queryUsers != null && queryUsers.size() > 0) {
            // DetachedCriteria subquery = null;
            // subquery = DetachedCriteria.forClass(TaskAssignee.class, "a");
            // subquery.setProjection(Projections
            // .property("a.taskInstance"));
            // Criterion userRestriction = null;
            // userRestriction = Restrictions.and(Restrictions.eq(
            // "a.activeAssignee", Boolean.TRUE), Restrictions.in(
            // "a.assignee", queryUsers));
            // subquery.add(userRestriction);
            // criteriaB.add(Subqueries.propertyIn("id", subquery));
            //                
            // criteriaA = criteriaA.setFetchMode("assignees", FetchMode.JOIN)
            // .createCriteria("assignees", "a").add(userRestriction);
            // }
            // // before1 = System.currentTimeMillis();
            // Integer count = (Integer) criteriaA.uniqueResult();
            // // after1 = System.currentTimeMillis();
            //
            // int total = count == null ? 0 : count.intValue();
            //
            // total = total - excludedNum;
            //
            // // before2 = System.currentTimeMillis();
            // taskList = criteriaB.list();
            // // after2 = System.currentTimeMillis();
            // // session.getTransaction().commit();
            //
            // List<TaskItem> taskItems = new ArrayList<TaskItem>();
            //
            // if (taskList != null) {
            // int num = 0;
            // for (Object taskInstance : taskList) {
            // Long taskId = (Long) ((Object[]) taskInstance)[0];
            // if (excludedIds != null && excludedIds.contains(taskId)) {
            // continue;
            // }
            // num++;
            // if (pageSize != -1 && startIndex != -1 && pageSize != 0
            // && num > pageSize) {
            // break;
            // }
            // TaskItem item = new TaskItem((Object[]) taskInstance);
            // taskItems.add(item);
            // }
            // }

            taskListResult.setTaskList(taskItems);
            taskListResult.setTotalRecords(total);

        } catch (SQLException sqlException) {
            // session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
            // session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            // session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                // session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        long after = System.currentTimeMillis();
        long duration = after - before;
        // long duration0 = after0 - before0;
        // long duration1 = after1 - before1;
        // long duration2 = after2 - before2;
        // LOGGER.log(Level.INFO, "Query Duration0 " + duration0);
        // LOGGER.log(Level.INFO, "Query Duration1 " + duration1);
        // LOGGER.log(Level.INFO, "Query Duration2 " + duration2);
        LOGGER.log(Level.INFO, "Query getTaskList " + duration);
        return taskListResult;
    }

    // /**
    // * Returns all the TaskInstances assigned to the user
    // *
    // * @param user
    // * The user whose TaskInstances are queried
    // * @param users
    // * All the users that the user is associated with
    // * @return
    // * @throws ConnectionException
    // */
    // public Set<TaskInstance> getAssignedTaskList(TaskPrincipal user)
    // throws ConnectionException {
    // Set<TaskInstance> taskListSet = new HashSet<TaskInstance>();
    // Connection conn = null;
    // Session session = null;
    // // Individual user
    // final String taskListQueryStr1 = "select taskInstance from TaskAssignee
    // as taskAssignee, TaskInstance as taskInstance "
    // // + "where taskAssignee.taskInstance = taskInstance and
    // // taskInstance.status not in (3, 4, 6, 7) "
    // + "where taskAssignee.taskInstance = taskInstance and taskInstance.status
    // not in (4, 6, 7) "
    // + "and taskAssignee.assignee = ? and taskAssignee.assigneeType = 'USER'
    // and taskAssignee.assignedStatus in (1, 2) "
    // + "and taskAssignee.activeAssignee = ?";
    // // Group
    // final String taskListQueryStr2 = "select taskInstance from TaskAssignee
    // as taskAssignee, TaskInstance as taskInstance "
    // // + "where taskAssignee.taskInstance = taskInstance and
    // // taskInstance.status not in (3, 4, 6, 7) "
    // + "where taskAssignee.taskInstance = taskInstance and taskInstance.status
    // not in (4, 6, 7) "
    // + "and taskAssignee.assignee = ? and taskAssignee.assigneeType = 'GROUP'
    // and taskAssignee.assignedStatus = 1 "
    // + "and taskAssignee.activeAssignee = ?";
    //
    // try {
    // conn = mDataSource.getConnection();
    // session = mSessionFactory.openSession(conn);
    // session.beginTransaction();
    //
    // if (user.getType() == TaskPrincipal.PrincipalType.User) {
    // // user query
    // List<TaskInstance> taskInstancesForUser = session
    // .createCriteria(TaskInstance.class)
    // .add(
    // Restrictions.not(Restrictions.in("status",
    // new Integer[] {
    // RuntimeTask.TaskState.COMPLETED
    // .getState(),
    // RuntimeTask.TaskState.EXPIRED
    // .getState(),
    // RuntimeTask.TaskState.ABORTED
    // .getState(),
    // RuntimeTask.TaskState.FAILED
    // .getState() })))
    // .setFetchMode("assignees", FetchMode.JOIN)
    // .createCriteria("assignees")
    // .add(
    // Restrictions
    // .and(
    // Restrictions
    // .and(
    // Restrictions
    // .and(
    // Restrictions
    // .eq(
    // "assigneeType",
    // "USER"),
    // Restrictions
    // .in(
    // "assignedStatus",
    // new Integer[] {
    // RuntimeTask.TaskState.ASSIGNED
    // .getState(),
    // RuntimeTask.TaskState.CLAIMED
    // .getState() })),
    // Restrictions
    // .and(
    // Restrictions
    // .eq(
    // "assignee",
    // user
    // .getName()),
    // Restrictions
    // .eq(
    // "activeAssignee",
    // Boolean.TRUE))),
    // Restrictions
    // .eq(
    // "excludedAssignee",
    // Boolean.FALSE)))
    //                                                                                
    // .list();
    // taskListSet.addAll(taskInstancesForUser);
    //
    // } else {
    // // user query
    // List<TaskInstance> taskInstancesForGroup = session
    // .createCriteria(TaskInstance.class).add(
    // Restrictions.not(Restrictions.in("status",
    // new Integer[] {
    // RuntimeTask.TaskState.COMPLETED
    // .getState(),
    // RuntimeTask.TaskState.EXPIRED
    // .getState(),
    // RuntimeTask.TaskState.ABORTED
    // .getState(),
    // RuntimeTask.TaskState.FAILED
    // .getState() })))
    // .setFetchMode("assignees", FetchMode.JOIN)
    // .createCriteria("assignees").add(
    // Restrictions
    // .and(
    // Restrictions.and(Restrictions.and(Restrictions
    // .eq("assigneeType", "GROUP"),
    // Restrictions.eq("assignedStatus",
    // RuntimeTask.TaskState.ASSIGNED
    // .getState())),
    // Restrictions.and(Restrictions.eq(
    // "assignee", user.getName()),
    // Restrictions.eq(
    // "activeAssignee",
    // Boolean.TRUE))),
    // Restrictions
    // .eq(
    // "excludedAssignee",
    // Boolean.FALSE))).list();
    // taskListSet.addAll(taskInstancesForGroup);
    // }
    // // for (TaskInstance taskInstance : taskListSet) {
    // // Set<TaskTimer> timers = taskInstance.getTimers();
    // // Hibernate.initialize(timers);
    // // }
    // session.getTransaction().commit();
    // } catch (SQLException sqlException) {
    // session.getTransaction().rollback();
    // // TODO Auto-generated catc catch (SQLException sqlException) {
    // session.getTransaction().rollback();
    // // TODO Auto-generated catch block
    // String msg = I18n.loc("WLM-7005: Can not get a connection");
    // LOGGER.log(Level.SEVERE, msg, sqlException);
    // throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
    // msg, sqlException);
    // } catch (Exception ex) {
    // session.getTransaction().rollback();
    // String msg = I18n.loc("WLM-6043: Database operation failed");
    // LOGGER.log(Level.WARNING, msg, ex);
    // throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);
    //
    // } finally {
    // try {
    // //conn.close();
    // } catch (SQLException e) {
    // // TODO Auto-generated catch block
    // e.printStackTrace();
    // }
    // }
    // return taskListSet;
    //
    // }

    public TaskInstance getTaskInstance(Long taskId) throws ConnectionException {
        Connection conn = null;
        Session session = null;
        TaskInstance taskInstance;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            taskInstance = (TaskInstance) session.load(TaskInstance.class,
                    taskId);
            if (taskInstance == null) {
                String msg = I18n.loc("WLM-6038: Task id: {0} is not found",
                        taskId);
                LOGGER.log(Level.WARNING, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_FIND_TASK",
                        msg);
            }
            Hibernate.initialize(taskInstance.getAssignees());
            // Hibernate.initialize(taskInstance.getTimers());
            // Hibernate.initialize(taskInstance.getVariables());
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return taskInstance;
    }

    public List<TaskTimer> getActiveTimers(Long taskId)
            throws ConnectionException {

        List<TaskTimer> timers = new ArrayList<TaskTimer>();
        Connection conn = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();

            String qry = "from TaskTimer as taskTimer where taskTimer.status = 1 and taskTimer.taskInstance.id = ?  ";
            Query query = session.createQuery(qry).setLong(0, taskId);
            timers = query.list();
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                if (conn != null) {
                    conn.close();
                    session.close();
                }
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return timers;
    }

    public Map<Long, List<TaskTimer>> recoverTimers(QName taskQName)
            throws ConnectionException {
        List<TaskTimer> timers = new ArrayList<TaskTimer>();
        Map<Long, List<TaskTimer>> timerMap = new HashMap<Long, List<TaskTimer>>();
        Connection conn = null;
        Session session = null;
        try {
            conn = mDataSource.getConnection();
          
            if (conn == null) {
                String msg = I18n.loc("WLM-7005: Can not get a connection");
                LOGGER.log(Level.SEVERE, msg);
                throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                        msg);  
            }
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();

            String qry = "from TaskTimer as  taskTimer  where taskTimer.status = 1 and  taskTimer.taskInstance.taskDefId = ? order by taskTimer.taskInstance.id";
            Query query = session.createQuery(qry).setString(0,
                    taskQName.toString());
            timers = query.list();
            Long oldTaskId = null;
            for (TaskTimer taskTimer : timers) {
                if (oldTaskId == null) {
                    oldTaskId = taskTimer.getTaskInstance().getId();
                }
                if (taskTimer.getTaskInstance().getId() == oldTaskId) {
                    List<TaskTimer> sameTaskTimers = timerMap.get(oldTaskId);
                    if (sameTaskTimers == null) {
                        sameTaskTimers = new ArrayList<TaskTimer>();
                        timerMap.put(taskTimer.getTaskInstance().getId(),
                                sameTaskTimers);
                    }
                    sameTaskTimers.add(taskTimer);
                } else {
                    List<TaskTimer> sameTaskTimers = new ArrayList<TaskTimer>();
                    sameTaskTimers.add(taskTimer);
                    timerMap.put(taskTimer.getTaskInstance().getId(),
                            sameTaskTimers);
                }
                oldTaskId = taskTimer.getTaskInstance().getId();
            }
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            if (session != null) {
                session.getTransaction().rollback();            
            }
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            if (session != null) {
                session.getTransaction().rollback();
            }
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                if (conn != null) {
                    conn.close();
                    session.close();
                }
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return timerMap;

    }

    public Set<TaskAssignee> getCurrentAssignees(Long taskId)
            throws ConnectionException {
        // TODO Auto-generated method stub
        // Get current Assignee Connection conn = null;
        Session session = null;
        TaskInstance taskInstance;
        Connection conn = null;
        Set<TaskAssignee> currentAssignees = new HashSet<TaskAssignee>();

        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            session.beginTransaction();
            taskInstance = (TaskInstance) session.load(TaskInstance.class,
                    taskId);

            Query query2 = session
                    .createQuery(
                            "from TaskAssignee as taskAssignee where taskAssignee.taskInstance.id = ? and taskAssignee.activeAssignee = ? ")
                    .setLong(0, taskId).setBoolean(1, true);

            List<TaskAssignee> assignees = query2.list();
            session.getTransaction().commit();
            currentAssignees.addAll(assignees);
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return currentAssignees;
    }

    public void createIndex() throws ConnectionException {
        Session session = null;
        Connection conn = null;
        String tempDir = System.getProperty(mIndexDirProp);
        String oldPath = "com/sun/jbi/engine/workflow/db/hibernate/";

        try {
            Util.deleteDir(new File(tempDir, "indexes"));
            conn = mDataSource.getConnection();
            if (conn != null) {
                mSessionFactory = getSessionFactory(dbType);
                session = mSessionFactory.openSession(conn);
                FullTextSession fullTextSession = Search
                        .getFullTextSession(session);
                fullTextSession.beginTransaction();
                fullTextSession.purgeAll(TaskInstance.class);
                List<TaskInstance> allInstances = session.createQuery(
                        "from TaskInstance as instance").list();
                for (TaskInstance instance : allInstances) {
                    fullTextSession.index(instance);
                }
                fullTextSession.getTransaction().commit();
            } else {
                String msg = I18n.loc("WLM-7005: Can not get a connection");
                LOGGER.log(Level.SEVERE, msg);

            }
        } catch (SQLException sqlException) {
            if (session != null) {
                session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
                session.getTransaction().rollback();
            }
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
//            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
//                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
//            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                if (conn != null) 
                    conn.close();
                if (session != null)
                    session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public TasklistResult getTextSearchResult(String searchText,
            int startIndex, int pageSize) throws ConnectionException {
        TasklistResult taskListResult = new TasklistResult();
        List<Object> taskList = null;
        Session session = null;
        Connection conn = null;
        int total = 0;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            FullTextSession fullTextSession = Search
                    .getFullTextSession(session);
            fullTextSession.beginTransaction();
            // 2 searching fields:
            String[] fields = new String[] { "title", "keyword" };
            MultiFieldQueryParser parser = new MultiFieldQueryParser(fields,
                    fullTextSession.getSearchFactory().getAnalyzer(
                            TaskInstance.class));
            parser.setDefaultOperator(QueryParser.OR_OPERATOR);
            org.apache.lucene.search.Query parseQuery = parser
                    .parse(searchText);
            org.hibernate.search.FullTextQuery query = fullTextSession
                    .createFullTextQuery(parseQuery, TaskInstance.class);
            // query.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

            total = query.getResultSize();

            if (pageSize != -1 && startIndex != -1 && pageSize != 0) {
                query.setFirstResult(startIndex);
                query.setMaxResults(pageSize);
            }

            List<TaskInstance> instances = query.list();
            if (instances != null && instances.size() > 0) {
                List<TaskItem> taskItems = new ArrayList<TaskItem>(instances
                        .size());
                for (TaskInstance taskInstance : instances) {
                    TaskItem item = new TaskItem(taskInstance);
                    taskItems.add(item);
                }
                taskListResult.setTaskList(taskItems);
            }

            taskListResult.setTotalRecords(total);
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return taskListResult;
    }

    private List<Long> getTextSearchResult(String searchText)
            throws ConnectionException {
        // TODO Auto-generated method stub
        List<Long> taskList = new ArrayList<Long>();
        Session session = null;
        Connection conn = null;
        int total = 0;
        try {
            conn = mDataSource.getConnection();
            session = mSessionFactory.openSession(conn);
            FullTextSession fullTextSession = Search
                    .getFullTextSession(session);
            fullTextSession.beginTransaction();
            // 2 searching fields:
            String[] fields = new String[] { "title", "keyword" };
            MultiFieldQueryParser parser = new MultiFieldQueryParser(fields,
                    fullTextSession.getSearchFactory().getAnalyzer(
                            TaskInstance.class));
            parser.setDefaultOperator(QueryParser.OR_OPERATOR);
            org.apache.lucene.search.Query parseQuery = parser
                    .parse(searchText);
            org.hibernate.search.FullTextQuery query = fullTextSession
                    .createFullTextQuery(parseQuery, TaskInstance.class);
            query.setProjection(FullTextQuery.ID);

            List<Object> instances = query.list();
            if (instances != null && instances.size() > 0) {
                for (Object row : instances) {
                    Object[] result = (Object[]) row;
                    taskList.add((Long) result[0]);
                }
            }
            session.getTransaction().commit();
        } catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catc catch (SQLException sqlException) {
            session.getTransaction().rollback();
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-7005: Can not get a connection");
            LOGGER.log(Level.SEVERE, msg, sqlException);
            throw new ConnectionException("DBOperation.CAN_NOT_GET_CONNECTION",
                    msg, sqlException);
        } catch (Exception ex) {
            session.getTransaction().rollback();
            String msg = I18n.loc("WLM-6043: Database operation failed");
            LOGGER.log(Level.WARNING, msg, ex);
            throw new ConnectionException("DBOperation.DB_OPT_FAILED", msg, ex);

        } finally {
            try {
                conn.close();
                session.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return taskList;
    }

    public void close() {
        if (mSessionFactory != null) {
            mSessionFactory.close();
        }
        // Clean Up LogFactory, the code is taken from
        // org.apache.commons.logging.impl.ServletContextCleaner
        Class[] RELEASE_SIGNATURE = { ClassLoader.class };

        ClassLoader tccl = Thread.currentThread().getContextClassLoader();

        Object[] params = new Object[1];
        params[0] = tccl;

        // Walk up the tree of classloaders, finding all the available
        // LogFactory classes and releasing any objects associated with
        // the tccl (ie the webapp).
        //
        // When there is only one LogFactory in the classpath, and it
        // is within the webapp being undeployed then there is no problem;
        // garbage collection works fine.
        //
        // When there are multiple LogFactory classes in the classpath but
        // parent-first classloading is used everywhere, this loop is really
        // short. The first instance of LogFactory found will
        // be the highest in the classpath, and then no more will be found.
        // This is ok, as with this setup this will be the only LogFactory
        // holding any data associated with the tccl being released.
        //
        // When there are multiple LogFactory classes in the classpath and
        // child-first classloading is used in any classloader, then multiple
        // LogFactory instances may hold info about this TCCL; whenever the
        // webapp makes a call into a class loaded via an ancestor classloader
        // and that class calls LogFactory the tccl gets registered in
        // the LogFactory instance that is visible from the ancestor
        // classloader. However the concrete logging library it points
        // to is expected to have been loaded via the TCCL, so the
        // underlying logging lib is only initialised/configured once.
        // These references from ancestor LogFactory classes down to
        // TCCL classloaders are held via weak references and so should
        // be released but there are circumstances where they may not.
        // Walking up the classloader ancestry ladder releasing
        // the current tccl at each level tree, though, will definitely
        // clear any problem references.
        ClassLoader loader = tccl;

        while (loader != null) {
            // Load via the current loader. Note that if the class is not
            // accessable
            // via this loader, but is accessable via some ancestor then that
            // class
            // will be returned.
            try {
                Class logFactoryClass = loader
                        .loadClass("org.apache.commons.logging.LogFactory");
                Method releaseMethod = logFactoryClass.getMethod("release",
                        RELEASE_SIGNATURE);
                releaseMethod.invoke(null, params);
                loader = logFactoryClass.getClassLoader().getParent();

            } catch (ClassNotFoundException ex) {
                // Neither the current classloader nor any of its ancestors
                // could find
                // the LogFactory class, so we can stop now.
                loader = null;
            } catch (NoSuchMethodException ex) {
                // This is not expected; every version of JCL has this method
                System.err
                        .println("LogFactory instance found which does not support release method!");
                loader = null;
            } catch (IllegalAccessException ex) {
                // This is not expected; every ancestor class should be
                // accessable
                System.err
                        .println("LogFactory instance found which is not accessable!");
                loader = null;
            } catch (InvocationTargetException ex) {
                // This is not expected
                System.err
                        .println("LogFactory instance release method failed!");
                loader = null;
            }
        }

        // Just to be sure, invoke release on the LogFactory that is visible
        // from
        // this ServletContextCleaner class too. This should already have been
        // caught
        // by the above loop but just in case...
        LogFactory.release(tccl);

    }

    public static String convertToAssigneeInTable(TaskPrincipal tp) {
        if (tp.getType() == TaskPrincipal.PrincipalType.Group) {
            return GROUP_PREFIX + tp.getName();
        } else if (tp.getType() == TaskPrincipal.PrincipalType.User) {
            return USER_PREFIX + tp.getName();
        }
        return null;
    }

    public static TaskPrincipal convertToTaskPrincipal(String assigneeInTable) {
        TaskPrincipal tp = null;
        if (assigneeInTable.startsWith(GROUP_PREFIX)) {
            int index = assigneeInTable.indexOf("]");
            String name = assigneeInTable.substring(index + 1);
            tp = new TaskGroupPrincipalImpl(name);

        } else if (assigneeInTable.startsWith(USER_PREFIX)) {
            int index = assigneeInTable.indexOf("]");
            String name = assigneeInTable.substring(index + 1);
            tp = new TaskUserPrincipalImpl(name);
        }
        return tp;
    }
}
