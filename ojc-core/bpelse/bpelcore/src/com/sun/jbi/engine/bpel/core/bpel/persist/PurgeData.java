/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel.core.bpel.persist;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.BPELEventPersister;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * @author Sun Inc
 * Nov 12, 2007
 */
public class PurgeData {
    
    private static final Logger LOGGER = Logger.getLogger(PurgeData.class.getName());
    
    private static String STATE_TABLE = PersistenceDBSchemaCreation.STATE.trim();
    private static String EVENT_HDLR_TABLE = PersistenceDBSchemaCreation.EVENTHANDLER.trim();
    private static String[] PERSIST_TABLES_TO_CLEAR;
    private static String[] MONITOR_TABLES_TO_CLEAR;
    private static int PURGE_LOG_COUNT = 50;
    
    static { 
        String[] PERSIST_TABLES = PersistenceDBSchemaCreation.getInstance().getTabels();
        List<String> persistTablesToClear = new ArrayList<String>();
        for (int i = 0; i < PERSIST_TABLES.length; i++) {
            String tableItem = PERSIST_TABLES[i];
            if (tableItem.equalsIgnoreCase(EVENT_HDLR_TABLE)
                    || tableItem.equalsIgnoreCase(STATE_TABLE)
                    || tableItem.equalsIgnoreCase(PersistenceDBSchemaCreation.ENGINE.trim())
                    || tableItem.equalsIgnoreCase(PersistenceDBSchemaCreation.ENGINECORRELATION.trim())) {
                continue;
            }
            persistTablesToClear.add(tableItem);
        }
        PERSIST_TABLES_TO_CLEAR = persistTablesToClear.toArray(new String[] {});
        
        String[] MONITOR_TABLES = MonitorDBSchemaCreation.getInstance().getTabels();
        List<String> monitorTablesToClear = new ArrayList<String>();
        for (int i = 0; i < MONITOR_TABLES.length; i++) {
            String tableItem = MONITOR_TABLES[i];
            if (tableItem.equalsIgnoreCase(MonitorDBSchemaCreation.SERVICE_UNIT)
                    || tableItem.equalsIgnoreCase(MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE)
                    		|| tableItem.equalsIgnoreCase(MonitorDBSchemaCreation.MONITOR_BPEL_PROCESS)) {
                continue;
            }
            monitorTablesToClear.add(tableItem);
        }
        MONITOR_TABLES_TO_CLEAR = monitorTablesToClear.toArray(new String[] {});        
    }
        
    public void purgePersistenceData(DBConnectionFactory connFac) throws Exception {
        
        ResultSet resultSet = null;
        AbstractDBConnection dbConn = null;
        Statement stmt = null;
        try {
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-6152: Purging persistence data"));
            dbConn = connFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            conn.setAutoCommit(false);
            stmt = conn.createStatement();
            // Lock all instances using the most DB-agnostic method
            stmt.execute("UPDATE "+STATE_TABLE+" SET stateId=stateId");
            String stateIn = "SELECT stateId FROM "+STATE_TABLE+" WHERE status='"+StateDBO.COMPLETE_STATUS+"'";
            for (int i = 0; i < PERSIST_TABLES_TO_CLEAR.length; i++) {
                stmt.execute(
                    "DELETE FROM "+PERSIST_TABLES_TO_CLEAR[i]+
                    " WHERE stateId IN (SELECT ehid FROM "+EVENT_HDLR_TABLE+
                    " WHERE stateId IN ("+stateIn+"))"
                );
                stmt.execute("DELETE FROM "+PERSIST_TABLES_TO_CLEAR[i]+" WHERE stateId IN ("+stateIn+")");
            }
            stmt.execute("DELETE FROM "+EVENT_HDLR_TABLE+" WHERE stateId IN ("+stateIn+")");
            int totalCount = stmt.executeUpdate("DELETE FROM "+STATE_TABLE+" WHERE status='"+StateDBO.COMPLETE_STATUS+"'");
            conn.commit();
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-6154: Done purging: Total {0} instances purged", totalCount));
        } catch (Exception e) {
            // This could be due to the fact that the database connection is bad. Check for it and if so
            // mark it as bad. But do not attempt retries here.
            connFac.validateNonXAConnection(dbConn);
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6196: Exception while purging instances: {0}", e));
            throw e;
        } finally {
            if (resultSet != null) {
                try {
                    resultSet.close();
                } catch (SQLException resCloseExcp) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6061: Exception occured while closing a JDBC Resultset"), 
                    		resCloseExcp);
                }
            }
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (SQLException stmtCloseExcp) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6065: Exception while closing a JDBC statement"), 
                    		stmtCloseExcp);
                }
            }
            if (dbConn != null) {
                try {
                    dbConn.close(); // this wrapper takes care of setting the initial value of setAutoCommit
                } catch (SQLException ex) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6062: Exception while closing a JDBC connection"), ex);
                }
            }
        }
    }
    
    public void purgeMonitoringData(DBConnectionFactory connFac) throws Exception {
        
        ResultSet resultSet = null;
        AbstractDBConnection dbConn = null;
        Connection conn = null;
        //This statement is used for getting a list of all process instances. Since we will be 
        //iterating over the result set, we do not want to close the statement. So for executing 
        //other queries that do not return a result set we use stmt2
        Statement stmt = null;
        try {
            dbConn = connFac.createNonXAConnection();
            conn = dbConn.getUnderlyingConnection();
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-6155: Purging monitoring data"));
            conn.setAutoCommit(false);
            stmt = conn.createStatement();
            // Lock all instances using the most DB-agnostic method
            stmt.execute("UPDATE "+MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE+" SET instanceid=instanceid");
            String idIn = "FROM "+MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE+
                " WHERE status='" + BPELEventPersister.COMPLETED + "'" +
                " OR status='" + BPELEventPersister.TERMINATED + "'" +
                " OR status='" + BPELEventPersister.FAULTED + "'";
            for (int i = 0; i < MONITOR_TABLES_TO_CLEAR.length; i++) {
                stmt.execute("DELETE FROM "+MONITOR_TABLES_TO_CLEAR[i]+" WHERE instanceid IN (SELECT instanceid "+idIn+")");
            }
            int totalCount = stmt.executeUpdate("DELETE "+idIn);
            conn.commit();
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-6154: Done purging: Total {0} instances purged", totalCount));
        } catch (Exception e) {
        	// This could be due to the fact that the database connection is bad. Check for it and if so 
        	// mark it as bad. But do not attempt retries here.
        	connFac.validateNonXAConnection(dbConn);
        	if (conn != null) {
        	    try {
        	        conn.rollback();
        	    } catch (Exception se) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
        	    }
        	}
        	throw e;
        } finally {
            if (resultSet != null) {
                try {
                    resultSet.close();
                } catch (SQLException resCloseExcp) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6061: Exception occured while closing a JDBC Resultset"), 
                    		resCloseExcp);
                }
            }
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (SQLException stmtCloseExcp) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6065: Exception while closing a JDBC statement"), 
                    		stmtCloseExcp);
                }
            }
            if (dbConn != null) {
                try {
                    dbConn.close(); // this wrapper takes care of setting the initial value of setAutoCommit
                } catch (SQLException ex) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6062: Exception while closing a JDBC connection"), ex);
                }
            }
        }
    }

}
