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
 * @(#)$Id: BPELEventPersister.java,v 1.28 2010/02/04 02:51:27 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.sql.Clob;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.rowset.serial.SerialClob;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.While;
import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.event.ActivityEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELInstanceEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.management.BPELSEManagement;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

public class BPELEventPersister {

    private static final Logger LOGGER = Logger
            .getLogger(BPELEventPersister.class.getName());

    private static final String INSERT_INSTANCE = "INSERT INTO  MONITORBPELINSTANCE  (engineid, instanceid, bpelid, " +
    		"status, starttime, updatedtime ) VALUES(?, ?, ?, ?, ?, ?)";

    private static final String COMPLETE_INSTANCE = "UPDATE MONITORBPELINSTANCE SET status = ?, endtime = ?, " +
    		"updatedtime = ? WHERE instanceid=? and  status <> 'FAULTED' ";

    private static final String FAULT_INSTANCE = "UPDATE MONITORBPELINSTANCE SET status = 'FAULTED', endtime = ?, " +
    		"updatedtime = ? WHERE instanceid=?";

    private static final String UPDATE_INSTANCE_STATUS = "UPDATE MONITORBPELINSTANCE SET status = ?, updatedtime = ? " +
    		"WHERE instanceid=?";

    private static final String UPDATE_INSTANCE_DATA = "UPDATE MONITORBPELINSTANCE SET engineid=?, updatedtime = ? " +
    		"WHERE instanceid=?";

    private static final String QUERY_ACT = "select iteration, status from MONITORBPELACTIVITY " +
    		"WHERE instanceid = ? AND activityid = ?  ORDER BY iteration DESC ";

    private static final String INSERT_ACT = "INSERT INTO MONITORBPELACTIVITY (engineid, instanceid, activityid, " +
    		"activityxpath, status, hasfaulted, starttime, iteration) VALUES (?, ?, ?, ?, ?, ?, ?, ?)";

    private static final String UPDATE_ACT_STATUS = "UPDATE MONITORBPELACTIVITY SET status = ?, hasfaulted = ?, " +
    		"endtime = ?  WHERE instanceid = ? AND activityid = ? AND iteration = ?";

    private static final String UPDATE_ACT_CRM_RECEIVE = "UPDATE MONITORBPELACTIVITY SET crmpreceiveid = ?  "
            + "WHERE  instanceid = ? AND activityid = ? AND iteration = ?";

    private static final String UPDATE_ACT_CRM_INVOKE = "UPDATE MONITORBPELACTIVITY SET crmpinvokeid = ?  "
            + "WHERE instanceid = ? AND activityid = ? AND iteration = ?";

    private static final String QUERY_BPEL_VARIABLE = "SELECT varname FROM MONITORBPELVARIABLE " +
    		"WHERE instanceid= ? AND scopeid = ? AND varname = ?";

    private static final String INSERT_BPEL_VARIABLE = "INSERT INTO MONITORBPELVARIABLE (instanceid, scopeid, varid, " +
    		"varname, isfault, varvalue)  VALUES (?, ?, ?, ?, ?, ?)";

    private static final String UPDATE_BPEL_VARIABLE = "UPDATE MONITORBPELVARIABLE SET isfault = ?, varvalue = ? " +
    		"WHERE  instanceid= ? AND scopeid = ? AND varname = ?";
    
    private static final String QUERY_SIMPLE_VARIABLE = "SELECT varname FROM MONITORSIMPLEVARIABLE WHERE " 
    	+ "instanceid = ? AND scopeid = ? AND varname = ?";
    
    private static final String INSERT_SIMPLE_VARIABLE = "INSERT INTO MONITORSIMPLEVARIABLE (instanceid, scopeid, " 
    	+ "varid, vartype, varname, strvalue, numvalue, datevalue) VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
    
    private static final String UPDATE_SIMPLE_VARIABLE = "UPDATE MONITORSIMPLEVARIABLE SET strvalue = ?, numvalue = ?, " 
    	+ "datevalue = ? WHERE instanceid = ? AND scopeid = ? AND varname = ?";

    private static String QUERY_SUSPENDED_STATE = "SELECT stateid FROM STATE WHERE status = 'SUSPENDED'";

    private static String QUERY_SUSPENDED_INSTANCE = "SELECT instanceid FROM MONITORBPELINSTANCE " +
    		"WHERE status = 'SUSPENDED'";

    private static String SYNCH_UPDATE_STATUS = "UPDATE MONITORBPELINSTANCE SET status = 'TERMINATED', " +
    		"endtime = ?, updatedtime = ? WHERE instanceid = ?";

    private static String QUERY_RUNNING_STATE = "SELECT stateid FROM STATE WHERE status = 'RUNNING'";

    private static String QUERY_RUNNING_INSTANCE = "SELECT instanceid FROM MONITORBPELINSTANCE " +
    		"WHERE status = 'RUNNING'";

    private static String UPDATE_ACT_STATUS_FOR_INSTANCE = "UPDATE MONITORBPELACTIVITY SET status = ?, endtime = ?  "
            + "WHERE instanceid = ? AND status = ?";

    public static final String RUNNING = "RUNNING";

    public static final String COMPLETED = "COMPLETED";

    public static final String SUSPENDED = "SUSPENDED";

    public static final String TERMINATED = "TERMINATED";

    private static final String STARTED = "STARTED";

    public static final String FAULTED = "FAULTED";
    
    private DBConnectionFactory mDBFactory;
    
    public static final String MONITOR_INSTANCE_COUNT_SYNC_QUERY = " select " + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status, count(*) from " 
    + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " where " + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".bpelid = " + "? group by " + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status";


    /**
     * 
     * @param event
     */
    public BPELEventPersister(DBConnectionFactory dbFactory) throws Exception {
        mDBFactory = dbFactory;
    }

    /**
     * 
     * @param event
     */
    public void persistEvent(BPELEvent event) {
    	if (mDBFactory != null) {
    		if (event.getEventType() == BPELEvent.EventType.BP_START) {
    			startBp((BPELInstanceEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.BP_COMPLETE) {
    			completeBp((BPELInstanceEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.BP_SUSPEND) {
    			suspendBp((BPELInstanceEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.BP_RESUME) {
    			resumeBp((BPELInstanceEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.BP_TERMINATE) {
    			terminateBp((BPELInstanceEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.BP_FAULT) {
    			faultBp((BPELInstanceEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.BP_SYNCHRONIZE) {
    			synchBp((BPELEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.ACTIVITY_START) {
    			startAct((ActivityEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.ACTIVITY_COMPLETED) {
    			completeAct((ActivityEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.ACTIVITY_FAULTED) {
    			faultedAct((ActivityEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.VARIABLE_CHANGED) {
    			variableChange((VariableEvent) event);
    		} else if (event.getEventType() == BPELEvent.EventType.ACTIVITY_TERMINATE) {
    			terminateAct((ActivityEvent) event);
    		}
    	}
    }

    /*
     * 
     */
    private void startBp(BPELInstanceEvent event) {
        PreparedStatement insert = null;
        Connection conn = null;
        AbstractDBConnection abstractConn = null;
        boolean retry;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
            	conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                insert = conn.prepareStatement(INSERT_INSTANCE);
                insert.setString(1, event.getEngineId());
                insert.setString(2, event.getInstanceId());
                insert.setString(3, event.getBPELName().toString());
                insert.setString(4, RUNNING);
                insert.setTimestamp(5, event.getTimeStamp());
                insert.setTimestamp(6, event.getTimeStamp());
                insert.execute();
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6031: Error in persisting BP instance start event : {0}",
                		event.toString()), e);
                }
            } finally {
            	closeStatement(insert);
            	closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void completeBp(BPELInstanceEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement update = null;
        boolean retry;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                update = conn.prepareStatement(COMPLETE_INSTANCE);
                update.setString(4, event.getInstanceId());
                update.setString(1, COMPLETED);
                update.setTimestamp(2, event.getTimeStamp());
                update.setTimestamp(3, event.getTimeStamp());
                update.execute();
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6032: Error in persisting BP instance " +
                			"complete event : {0}", event.toString()), e);
                }
            } finally {
            	closeStatement(update);
            	closeConnection(conn);
            }
        } while (retry);
        
    }

    /*
     * 
     */
    private void terminateBp(BPELInstanceEvent event) {
        PreparedStatement update = null;
        PreparedStatement updateStatus = null;
        Connection conn = null;
        AbstractDBConnection abstractConn = null;
        boolean retry = false;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                update = conn.prepareStatement(COMPLETE_INSTANCE);
                update.setString(4, event.getInstanceId());
                update.setString(1, TERMINATED);
                update.setTimestamp(2, event.getTimeStamp());
                update.setTimestamp(3, event.getTimeStamp());
                update.execute();
                
                updateStatus = conn.prepareStatement(UPDATE_ACT_STATUS_FOR_INSTANCE);
                updateActivityStatus(updateStatus, event.getInstanceId(), event.getTimeStamp(), STARTED, TERMINATED);
                updateActivityStatus(updateStatus, event.getInstanceId(), event.getTimeStamp(), SUSPENDED, TERMINATED);
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6033: Error in persisting BP instance " +
                			"terminate event : {0}", event.toString()), e);
                }
            }  finally {
            	closeStatement(updateStatus);
            	closeStatement(update);
            	closeConnection(conn);
            }
        } while (retry);     
    }

    /*
     * 
     */
    private void faultBp(BPELInstanceEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement update = null;
        boolean retry = false;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                update = conn.prepareStatement(FAULT_INSTANCE);
                update.setString(3, event.getInstanceId());
                update.setTimestamp(1, event.getTimeStamp());
                update.setTimestamp(2, event.getTimeStamp());
                update.execute();
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6034: Error in persisting BP instance fault event : {0}",
                		event.toString()), e);
                }
            } finally {
            	closeStatement(update);
            	closeConnection(conn);
            }
        } while (retry);
        
    }

    /*
     * 
     */
    private void synchBp(BPELEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement update = null;
        PreparedStatement updateStatus = null;
        PreparedStatement queryStateSuspended = null;
        PreparedStatement queryInstanceSuspended = null;
        PreparedStatement queryStateRunning = null;
        PreparedStatement queryInstanceRunning = null;
        ResultSet stateResultSuspended = null;
        ResultSet stateResultRunning = null;
        ResultSet instanceResultSuspended = null;
        ResultSet instanceResultRunning = null;
        boolean retry = false;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);

                boolean stateTablecreated = isStateTableCreated(conn, mDBFactory);
                List<String> stateSuspended = new ArrayList<String>();
                if (stateTablecreated) {
                	queryStateSuspended = conn.prepareStatement(QUERY_SUSPENDED_STATE);
                    stateResultSuspended = queryStateSuspended.executeQuery();
                    while (stateResultSuspended.next()) {
                        stateSuspended.add(stateResultSuspended.getString(1));
                    }
                }

                List<String> instanceSuspended = new ArrayList<String>();
                queryInstanceSuspended = conn.prepareStatement(QUERY_SUSPENDED_INSTANCE);
                instanceResultSuspended = queryInstanceSuspended.executeQuery();
                while (instanceResultSuspended.next()) {
                    instanceSuspended.add(instanceResultSuspended.getString(1));
                }

                List<String> unMatched = new ArrayList<String>();
                for (String id : instanceSuspended) {
                    if (!stateSuspended.contains(id)) {
                        unMatched.add(id);
                    }
                }
                
                List<String> stateRunning = new ArrayList<String>();
                if (stateTablecreated) {
                	queryStateRunning = conn.prepareStatement(QUERY_RUNNING_STATE);
                    stateResultRunning = queryStateRunning.executeQuery();
                    while (stateResultRunning.next()) {
                        stateRunning.add(stateResultRunning.getString(1));
                    }
                }
                
                List<String> instanceRunning = new ArrayList<String>();
                queryInstanceRunning = conn.prepareStatement(QUERY_RUNNING_INSTANCE);
                instanceResultRunning = queryInstanceRunning.executeQuery();
                while (instanceResultRunning.next()) {
                    instanceRunning.add(instanceResultRunning.getString(1));
                }

                for (String id : instanceRunning) {
                    if (!stateRunning.contains(id)) {
                        unMatched.add(id);
                    }
                }
                
                if (unMatched.size() > 0) {
                    update = conn.prepareStatement(SYNCH_UPDATE_STATUS);
                    for (String id : unMatched) {
                        Timestamp ts = new Timestamp(System.currentTimeMillis());
                        update.setString(3, id);
                        update.setTimestamp(1, ts);
                        update.setTimestamp(2, ts);
                        update.execute();
                        
                        updateStatus = conn.prepareStatement(UPDATE_ACT_STATUS_FOR_INSTANCE);
                        updateActivityStatus(updateStatus, id, ts, STARTED, TERMINATED);
                        updateActivityStatus(updateStatus, id, ts, SUSPENDED, TERMINATED);
                    }
                }
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6035: Error in synchronizing monitor db with "
                                + "STATE table event : {0}", event.toString()), e);
                }
            } finally {
                closeResultSet(stateResultSuspended);
                closeResultSet(stateResultRunning);
                closeResultSet(instanceResultSuspended);
                closeResultSet(instanceResultRunning);

                closeStatement(update);
                closeStatement(updateStatus);
                closeStatement(queryInstanceSuspended);
                closeStatement(queryStateSuspended);
                closeStatement(queryStateRunning);
                closeStatement(queryInstanceRunning);

                closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void updateActivityStatus(PreparedStatement update, String instanceId, Timestamp endTime, String oldStatus, 
    		String newStatus) throws Exception {

    	update.setString(1, newStatus);
    	update.setTimestamp(2, endTime);
    	update.setString(3, instanceId);
    	update.setString(4, oldStatus);
    	update.execute();

    }

    /*
     * 
     */
    private void suspendBp(BPELInstanceEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement update = null;
        PreparedStatement updateStatus = null;
        boolean retry;
        do {
            retry = false;

            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                update = conn.prepareStatement(UPDATE_INSTANCE_STATUS);
                update.setString(3, event.getInstanceId());
                update.setString(1, SUSPENDED);
                update.setTimestamp(2, event.getTimeStamp());
                update.execute();
                
                updateStatus = conn.prepareStatement(UPDATE_ACT_STATUS_FOR_INSTANCE);
                updateActivityStatus(updateStatus, event.getInstanceId(), null, STARTED, SUSPENDED);
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6036: Error in persisting BP instance " +
                			"suspend event : {0}", event.toString()), e);
                }
            } finally {
            	closeStatement(update);
            	closeStatement(updateStatus);
            	closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void resumeBp(BPELInstanceEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement update = null;
        PreparedStatement updateStatus = null;
        boolean retry;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                update = conn.prepareStatement(UPDATE_INSTANCE_STATUS);
                update.setString(3, event.getInstanceId());
                update.setString(1, RUNNING);
                update.setTimestamp(2, event.getTimeStamp());
                update.execute();
                
                updateStatus = conn.prepareStatement(UPDATE_ACT_STATUS_FOR_INSTANCE);
                updateActivityStatus(updateStatus, event.getInstanceId(), null, SUSPENDED, STARTED);
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6037: Error in persisting BP instance " +
                			"resume event : {0}", event.toString()), e);
                }
            }  finally {
            	closeStatement(update);
            	closeStatement(updateStatus);
            	closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void completeAct(ActivityEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement query = null;
        PreparedStatement update = null;
        PreparedStatement updateInstance = null;
        ResultSet resultSet = null;
        boolean retry;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
            	conn = abstractConn.getUnderlyingConnection();
            	conn.setAutoCommit(false);
            	
                String xpath = event.getActivityXpath();
                int iteration = 0;
                if ((xpath.indexOf(While.TAG) != -1)
                        || (xpath.indexOf(RepeatUntil.TAG) != -1)
                        || (xpath.indexOf(ForEach.TAG) != -1)) {
                    // check if there is such act exist:
                    query = conn.prepareStatement(QUERY_ACT);
                    // WHERE engineid = ? AND instanceid = ? AND activityid = ?
                    // query.setString(1, event.getEngineId());
                    query.setString(1, event.getInstanceId());
                    query.setLong(2, event.getActivityId());

                    resultSet = query.executeQuery();
                    if (resultSet.next()) {
                        // update status, hasfaulted, startTime, endTime
                        iteration = resultSet.getInt(1);
                    }
                }

                update = conn.prepareStatement(UPDATE_ACT_STATUS);
                // "UPDATE MONITORBPELACTIVITY SET status = ?, hasfaulted = ?, endTime = ? " +
                // "WHERE engineid = ? AND instanceid = ? AND activityid = ?";
                update.setString(1, COMPLETED);
                update.setString(2, "N");
                update.setTimestamp(3, event.getTimeStamp());
                // update.setString(4, event.getEngineId());
                update.setString(4, event.getInstanceId());
                update.setLong(5, event.getActivityId());
                update.setInt(6, iteration);
                update.execute();
                
                // update the instance for updatedTime
                updateInstance = conn.prepareStatement(UPDATE_INSTANCE_DATA);
                // "UPDATE MONITORBPELINSTANCE SET updatedTime = ? WHERE engineid = ? AND instanceid=?";
                updateInstance.setString(1, event.getEngineId());
                updateInstance.setTimestamp(2, event.getTimeStamp());
                updateInstance.setString(3, event.getInstanceId());
                updateInstance.execute();
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6038: Error in persisting BP activity " +
                			"complete event : {0}", event.toString()), e);
                }
            } finally {
            	closeResultSet(resultSet);
            	
            	closeStatement(query);
            	closeStatement(update);
            	closeStatement(updateInstance);
            	
            	closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void terminateAct(ActivityEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement query = null;
        PreparedStatement update = null;
        PreparedStatement updateInstance = null;
        ResultSet resultSet = null;
        boolean retry = false;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                String xpath = event.getActivityXpath();
                int iteration = 0;
                if ((xpath.indexOf(While.TAG) != -1)
                        || (xpath.indexOf(RepeatUntil.TAG) != -1)
                        || (xpath.indexOf(ForEach.TAG) != -1)) {
                    // check if there is such act exist:
                    query = conn.prepareStatement(QUERY_ACT);
                    query.setString(1, event.getInstanceId());
                    query.setLong(2, event.getActivityId());
                    resultSet = query.executeQuery();
                    
                    if (resultSet.next()) {
                        // update status, hasfaulted, startTime, endTime
                        iteration = resultSet.getInt(1);
                    }
                }
                
                update = conn.prepareStatement(UPDATE_ACT_STATUS);
                // "UPDATE MONITORBPELACTIVITY SET status = ?, hasfaulted = ?, endTime = ? " +
                // "WHERE engineid = ? AND instanceid = ? AND activityid = ?";
                update.setString(1, TERMINATED);
                update.setString(2, "N");
                update.setTimestamp(3, event.getTimeStamp());
                // update.setString(4, event.getEngineId());
                update.setString(4, event.getInstanceId());
                update.setLong(5, event.getActivityId());
                update.setInt(6, iteration);
                update.execute();
                
                // update the instance for updatedTime
                updateInstance = conn.prepareStatement(UPDATE_INSTANCE_DATA);
                // "UPDATE MONITORBPELINSTANCE SET updatedTime = ? WHERE engineid = ? AND instanceid=?";
                updateInstance.setString(1, event.getEngineId());
                updateInstance.setTimestamp(2, event.getTimeStamp());
                updateInstance.setString(3, event.getInstanceId());
                updateInstance.execute();
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6039: Error in persisting BP activity " +
                			"terminate event : {0}", event.toString()), e);
                }
            } finally {
            	closeResultSet(resultSet);
            	
            	closeStatement(query);
            	closeStatement(update);
            	closeStatement(updateInstance);
            	
            	closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void startAct(ActivityEvent event) {      
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement query = null;
        PreparedStatement insert = null;
        PreparedStatement updateInstance = null;
        ResultSet resultSet = null;
        boolean retry = false;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
                // check if there is such act exist:
                String xpath = event.getActivityXpath();
                int iteration = 0;
                boolean toInsert = true;
                query = conn.prepareStatement(QUERY_ACT);
                query.setString(1, event.getInstanceId());
                query.setLong(2, event.getActivityId());
                resultSet = query.executeQuery();

                if (resultSet.next()) {
                    // update status, hasfaulted, startTime, endTime
                    String actStatus = resultSet.getString(2);
                    iteration = resultSet.getInt(1);
                    boolean looped = false;
                  if ((xpath.indexOf(While.TAG) != -1)
                     || (xpath.indexOf(RepeatUntil.TAG) != -1)
                     || (xpath.indexOf(ForEach.TAG) != -1)) {
                      looped = true;
                  }                 
                    if (!actStatus.equalsIgnoreCase(STARTED) && looped) {
                        ++iteration;
                    } else {                        
                        toInsert = false;
                    }
                }
                
                if (toInsert) {
                	insert = conn.prepareStatement(INSERT_ACT);
                    insert.setString(1, event.getEngineId());
                    insert.setString(2, event.getInstanceId());
                    insert.setLong(3, event.getActivityId());
                    insert.setString(4, event.getActivityXpath());
                    insert.setString(5, STARTED);
                    insert.setString(6, "N");
                    insert.setTimestamp(7, event.getTimeStamp());
                    insert.setInt(8, iteration);
                    insert.execute();
                }

                // update the instance for updatedTime
                updateInstance = conn.prepareStatement(UPDATE_INSTANCE_DATA);
                // "UPDATE MONITORBPELINSTANCE SET updatedTime = ? WHERE engineid = ? AND instanceid=?";
                updateInstance.setTimestamp(2, event.getTimeStamp());
                updateInstance.setString(1, event.getEngineId());
                updateInstance.setString(3, event.getInstanceId());
                updateInstance.execute();
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6040: Error in persisting BP activity start event : {0}",
                                                event.toString()), e);
                }
            } finally {
            	closeResultSet(resultSet);
            	
            	closeStatement(query);
            	closeStatement(insert);
            	closeStatement(updateInstance);
            	
            	closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void closeStatement(Statement stm) {
        if (stm != null) {
            try {
                stm.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    /*
     * 
     */
    private void closeResultSet(ResultSet resultSet) {
        if (resultSet != null) {
            try {
                resultSet.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    /*
     * 
     */
    private void closeConnection(Connection conn) {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    private void rollBack(Connection conn) {
        if (conn != null) {
            try {
                conn.rollback();
            } catch (SQLException e) {
                // TODO Auto-generated catch block
            }
        }
    }

    /*
     * 
     */
    private void faultedAct(ActivityEvent event) {
    	Connection conn = null;
        AbstractDBConnection abstractConn = null;
        PreparedStatement query = null;
        PreparedStatement update = null;
        PreparedStatement insert = null;
        PreparedStatement updateInstance = null;
        ResultSet resultSet = null;
        boolean retry = false;
        do {
            retry = false;
            try {
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);

                // check if there is such act exist:
                query = conn.prepareStatement(QUERY_ACT);
                query.setString(1, event.getInstanceId());
                query.setLong(2, event.getActivityId());
                resultSet = query.executeQuery();
                
                if (resultSet.next()) {
                    // update status, hasfaulted, startTime, endTime
                    int iteration = resultSet.getInt(1);
                    update = conn.prepareStatement(UPDATE_ACT_STATUS);
                    // "UPDATE MONITORBPELACTIVITY SET status = ?, hasfaulted = ?, endTime = ? " +
                    // "WHERE engineid = ? AND instanceid = ? AND activityid = ?";
                    update.setString(1, FAULTED);
                    update.setString(2, "Y");
                    update.setTimestamp(3, event.getTimeStamp());
                    // update.setString(3, event.getEngineId());
                    update.setString(4, event.getInstanceId());
                    update.setLong(5, event.getActivityId());
                    update.setInt(6, iteration);
                    update.execute();
                    
                    // update the instance for updatedTime
                    updateInstance = conn.prepareStatement(UPDATE_INSTANCE_DATA);
                    // "UPDATE MONITORBPELINSTANCE SET updatedTime = ? WHERE engineid = ? AND instanceid=?";
                    updateInstance.setTimestamp(2, event.getTimeStamp());
                    updateInstance.setString(1, event.getEngineId());
                    updateInstance.setString(3, event.getInstanceId());
                    updateInstance.execute();
                } else {
                    // Process Level fault
                    insert = conn.prepareStatement(INSERT_ACT);
                    insert.setString(1, event.getEngineId());
                    insert.setString(2, event.getInstanceId());
                    insert.setLong(3, event.getActivityId());
                    insert.setString(4, event.getActivityXpath());
                    insert.setString(5, FAULTED);
                    insert.setString(6, "Y");
                    insert.setTimestamp(7, event.getTimeStamp());
                    insert.setInt(8, 0);
                    insert.execute();
                }
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6041: Error in persisting BP activity " +
                			"faulted event : {0}", event.toString()), e);
                }
            } finally {
            	closeResultSet(resultSet);
            	
            	closeStatement(query);
            	closeStatement(update);
            	closeStatement(insert);
            	closeStatement(updateInstance);
            	
            	closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private void variableChange(VariableEvent event) {
        PreparedStatement queryBpelVar = null;
        PreparedStatement updateBpelVar = null;
        PreparedStatement insertBpelVar = null;
        PreparedStatement querySimpleVar = null;
        PreparedStatement updateSimpleVar = null;
        PreparedStatement insertSimpleVar = null;
        PreparedStatement crmreceiveupdate = null;
        PreparedStatement crminvokeupdate = null;
        PreparedStatement queryAct = null;
        
        ResultSet bpelVarResultSet = null;
        ResultSet bpelSimpleVarResultSet = null;
        ResultSet actResultSet = null;
        
        Connection conn = null;
        AbstractDBConnection abstractConn = null;
        boolean retry;
        do {
            retry = false;
            try {         
            	abstractConn = mDBFactory.createNonXAConnection();
                conn = abstractConn.getUnderlyingConnection();
                conn.setAutoCommit(false);

                Map<VariableType, List<Variable>> vars = event.getVariables();
                for (Map.Entry<VariableType, List<Variable>> entry : vars.entrySet()) { // Start for
                    List<Variable> varList = entry.getValue();
                    for (Variable var : varList) { // Start for
                    	Variable.DataType varDataType = var.getType();
                    	if (varDataType == null) {
                    		LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6108: Type not specified for Variable {0}. " +
                    				"Not persisting to monitoring database.", var.getVarName()));
                    		continue;
                    	}
                    	
                    	String strValue = var.getValue();
                    	Double numValue = null;
                    	Timestamp dateValue = null;
                    	String varCode = BPELSEManagement.SimpleVarType.String.getCode();
                    	
                    	switch (varDataType) {
                    	// insert/update to the simple variable table
                    	case Boolean:
                    	case Number:
                    	case Date:
                    	case String:
                    		
                    		if (varDataType.equals(Variable.DataType.Boolean)) {
                    			varCode = BPELSEManagement.SimpleVarType.Boolean.getCode();
                    		} else if (varDataType.equals(Variable.DataType.Number)) {
                    			varCode = BPELSEManagement.SimpleVarType.Numeric.getCode();
                    			if (strValue != null) {
                    				numValue = Double.parseDouble(strValue);
                    			}
                    		} else if (varDataType.equals(Variable.DataType.Date)) {
                    			varCode = BPELSEManagement.SimpleVarType.Date.getCode();
                    			if (strValue != null) {
                    				GregorianCalendar gCal;
                    				try {
                    					XMLGregorianCalendar xmlCal = 
                    						DatatypeFactory.newInstance().newXMLGregorianCalendar(strValue);
                    					gCal = xmlCal.toGregorianCalendar();
                    				} catch (DatatypeConfigurationException dce) {
                    					throw new RuntimeException(I18n.loc("BPCOR-6125: Error parsing XSDDate {0}. " +
                    							"Error {1}", strValue, dce.getLocalizedMessage()), dce);
                    				} 
                    				dateValue = new Timestamp(gCal.getTimeInMillis());
                    				// This is the column that we will be doing date comparisons against.
                    				numValue = new Double(gCal.getTimeInMillis());
                    			}                    			
                    		} 
                        	    
                    		// Since we are in a for loop prepare statement only if null to avoid duplicate effort.
                    		if (querySimpleVar == null) {
                    			querySimpleVar = conn.prepareStatement(QUERY_SIMPLE_VARIABLE);
                    		}
                    		querySimpleVar.setString(1, event.getInstanceId());
                    		querySimpleVar.setLong(2, var.getScopeId());
                    		querySimpleVar.setString(3, var.getVarName());
                    		bpelSimpleVarResultSet = querySimpleVar.executeQuery();
                    		
                    		if (bpelSimpleVarResultSet.next()) {
                    			// Since we are in a for loop prepare statement only if null to avoid duplicate effort.
                    			if (updateSimpleVar == null) {
                    				updateSimpleVar = conn.prepareStatement(UPDATE_SIMPLE_VARIABLE);
                    			}
                    			updateSimpleVar.setString(1, strValue);
                    			updateSimpleVar.setObject(2, numValue, Types.DOUBLE);
                    			updateSimpleVar.setTimestamp(3, dateValue);
                    			updateSimpleVar.setString(4, event.getInstanceId());
                    			updateSimpleVar.setLong(5, var.getScopeId());
                    			updateSimpleVar.setString(6, var.getVarName());
                    			updateSimpleVar.execute();
                    		} else {
                    			// Since we are in a for loop prepare statement only if null to avoid duplicate effort.
                    			if (insertSimpleVar == null) {
                    				insertSimpleVar = conn.prepareStatement(INSERT_SIMPLE_VARIABLE);
                    			}
                    			insertSimpleVar.setString(1, event.getInstanceId());
                    			insertSimpleVar.setLong(2, var.getScopeId());
                    			insertSimpleVar.setLong(3, var.getVarId());
                    			insertSimpleVar.setString(4, varCode);
                    			insertSimpleVar.setString(5, var.getVarName());
                    			insertSimpleVar.setString(6, strValue);
                    			insertSimpleVar.setObject(7, numValue, Types.DOUBLE);
                    			insertSimpleVar.setTimestamp(8, dateValue);
                    			insertSimpleVar.execute();
                    		}
                    		// Close the result set here since we are in a for loop
                    		closeResultSet(bpelSimpleVarResultSet);
                    		break;

                    	// Insert/update to the complex variable table.
                    	default:
                    		Clob clob = null;
                    		if (var.getValue() != null) {
                    			clob = createClob(strValue);
                    		} else {
                    			clob = createClob(null);
                    		}
                    		
                    		// Since we are in a for loop prepare statement only if null to avoid duplicate effort.
                    		if (queryBpelVar == null) {
                    			queryBpelVar = conn.prepareStatement(QUERY_BPEL_VARIABLE);
                    		}
                    		queryBpelVar.setString(1, event.getInstanceId());
                    		queryBpelVar.setLong(2, var.getScopeId());
                    		queryBpelVar.setString(3, var.isFault() ? var.getFaultName().toString() : var.getVarName());
                    		bpelVarResultSet = queryBpelVar.executeQuery();

                    		if (bpelVarResultSet.next()) {
                    			// Since we are in a for loop prepare statement only if null to avoid duplicate effort.
                    			if (updateBpelVar == null) {
                    				updateBpelVar = conn.prepareStatement(UPDATE_BPEL_VARIABLE);
                    			}
                    			updateBpelVar.setString(1, var.isFault() ? "Y" : "N");
                    			updateBpelVar.setCharacterStream(2, clob.getCharacterStream(), (int) clob.length());
                    			// updateBpelVar.setString(3, event.getEngineId());
                    			updateBpelVar.setString(3, event.getInstanceId());
                    			updateBpelVar.setLong(4, var.getScopeId());
                    			updateBpelVar.setString(
                    					5, var.isFault() ? var.getFaultName().toString() : var.getVarName());
                    			updateBpelVar.execute();
                    		} else {
                    			// Since we are in a for loop prepare statement only if null to avoid duplicate effort.
                    			if (insertBpelVar == null) {
                    				insertBpelVar = conn.prepareStatement(INSERT_BPEL_VARIABLE);
                    			}
                    			insertBpelVar.setString(1, event.getInstanceId());
                    			insertBpelVar.setLong(2, var.getScopeId());
                    			insertBpelVar.setLong(3, var.getVarId());
                    			insertBpelVar.setString(
                    					4, var.isFault() ? var.getFaultName().toString() : var.getVarName());
                    			insertBpelVar.setString(5, var.isFault() ? "Y" : "N");
                    			insertBpelVar.setCharacterStream(6, clob.getCharacterStream(), (int) clob.length());
                    			insertBpelVar.execute();
                    		}
                    		// Close the result set here since we are in a for loop
                    		closeResultSet(bpelVarResultSet);
                    		break;
                    	}
                    } // end for
                } // end for
                
                if (event.getInvokeCRMPID() != null) {
                	queryAct = conn.prepareStatement(QUERY_ACT);
                    queryAct.setString(1, event.getInstanceId());
                    queryAct.setLong(2, event.getActivityId());
                    actResultSet = queryAct.executeQuery();
                    
                    if (actResultSet.next()) {
                        int iteration = actResultSet.getInt(1);
                        crminvokeupdate = conn.prepareStatement(UPDATE_ACT_CRM_INVOKE);
                        crminvokeupdate.setString(1, event.getInvokeCRMPID());
                        // crminvokeupdate.setString(2, event.getEngineId());
                        crminvokeupdate.setString(2, event.getInstanceId());
                        crminvokeupdate.setLong(3, event.getActivityId());
                        crminvokeupdate.setInt(4, iteration);
                        crminvokeupdate.execute();
                        
                    }

                } else if (event.getReceiveCRMPID() != null) {
                	queryAct = conn.prepareStatement(QUERY_ACT);
                    queryAct.setString(1, event.getInstanceId());
                    queryAct.setLong(2, event.getActivityId());

                    actResultSet = queryAct.executeQuery();
                    if (actResultSet.next()) {
                        int iteration = actResultSet.getInt(1);
                        crmreceiveupdate = conn.prepareStatement(UPDATE_ACT_CRM_RECEIVE);
                        crmreceiveupdate.setString(1, event.getReceiveCRMPID());
                        // crmreceiveupdate.setString(3, event.getEngineId());
                        crmreceiveupdate.setString(2, event.getInstanceId());
                        crmreceiveupdate.setLong(3, event.getActivityId());
                        crmreceiveupdate.setInt(4, iteration);
                        crmreceiveupdate.execute();
                    }
                }
                
                conn.commit();
            } catch (Exception e) {
            	rollBack(conn);
            	if (mDBFactory.doConnectionRetry(abstractConn, e)) {
                    retry = true;
                } else {
                	throw new RuntimeException(I18n.loc("BPCOR-6042: Error in persisting variable change event : {0}", 
                		event.toString()), e);
                }
            } finally {
                closeResultSet(actResultSet);   

                closeStatement(crmreceiveupdate);
                closeStatement(crminvokeupdate);
                closeStatement(queryAct);
                closeStatement(queryBpelVar);
                closeStatement(updateBpelVar);
                closeStatement(insertBpelVar);
                closeStatement(querySimpleVar);
                closeStatement(updateSimpleVar);
                closeStatement(insertSimpleVar);
                
                closeConnection(conn);
            }
        } while (retry);
    }

    /*
     * 
     */
    private Clob createClob(String value) throws SQLException {
        return (value == null) ? new SerialClob(new char[] {}) : new SerialClob(value.toCharArray());
    }

    /*
     * 
     */
    private boolean isStateTableCreated(Connection conn, DBConnectionFactory connFac) throws SQLException {
        ResultSet resultSet = null;
        boolean found = false;
        try {
            String[] objectsToSearchFor = { "TABLE" };
            resultSet = conn.getMetaData().getTables(null, "%", "%", objectsToSearchFor); //$NON-NLS-1$
            String dbUserName = conn.getMetaData().getUserName();
            while (resultSet.next()) {
                String rsTableName = resultSet.getString("TABLE_NAME");

                // Do the check against the schema only if Derby or Oracle. MySql always returns null
                // in this scenario.
                if (connFac.getType() != ConnectionProperties.MYSQL_DB.intValue()) {
                	//if the table is not in a schema same as the user name, go to next
                    String rsTableSchemaName = resultSet.getString("TABLE_SCHEM");
	                if (!(dbUserName.equalsIgnoreCase(rsTableSchemaName))) {
	                    continue;
	                }
                }
                if (rsTableName.equalsIgnoreCase("STATE")) { //$NON-NLS-1$
                    //Found a table, increment the counter
                    found = true;
                    break;
                }
            }
        } catch (SQLException e) {
            throw e;
        } finally {
            closeResultSet(resultSet);
        }
        return found;

    }
}
