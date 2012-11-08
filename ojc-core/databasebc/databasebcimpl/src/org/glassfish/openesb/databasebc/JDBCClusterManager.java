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
 * @(#)JDBCClusterManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import java.sql.Connection;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;
import javax.jbi.component.ComponentContext;
import com.sun.jbi.internationalization.Messages;

public class JDBCClusterManager {

    private static final Messages mMessages = Messages.getMessages(JDBCClusterManager.class);

    private static final Logger mLogger = Messages.getLogger(JDBCClusterManager.class);

    private String mTableName;

    private ComponentContext mContext;
    private String mprodName;

    private String mPKName;

    private String mInstanceName;

    private List<String> mPKList = new ArrayList<String>();

    private String mPKValue;

    private int mPollingInterval;

    private long mHeartbeatUpdateConfigInterval;

    private Connection connection = null;
    
    private String mJNDIName = null;
    
    private String BASE_INSTANCESTATE_INSERT_STMT_STR;
    
    private String BASE_INSTANCESTATE_UPDATE_STMT_STR;
    
    private String BASE_OWNERTABLE_INSERT_STMT_STR;
    
    private String BASE_OWNERTABLE_UPDATE_STMT_STR;
    
    private String BASE_OWNERTABLE_DELETE_STMT_STR;
    
    private String BASE_OWNERTABLE_DELETE_DANGLING_STMT_STR;
    
    private String BASE_OWNERTABLE_UPDATE_DANGLING_STMT_STR;
    
    private String BASE_OWNERTABLE_SELECT_DANGLING_STMT_STR;
    private String BASE_ORACLE_OWNERTABLE_DELETE_DANGLING_STMT_STR;
    
    private Object objectLock = new Object();
    
    private Object mUpdateHeartbeatLock = new Object();
    
    private Object mRecoverDanglingInstanceLock = new Object();
    
    private boolean mRecoverDanglingInstInProgress  = false;

    /*
     * Constructor
     */
    public JDBCClusterManager(ComponentContext context) throws Exception {
        mContext = context;
    }
    
    /*
     * polling table name @param val string
     */
    public void setTableName(String val) {
        this.mTableName = val;
    }
    
    /*
     * get the table name @return mTableName
     */
    public String getTableName() {
        return this.mTableName;
    }
    
    /*
     * primary key name of the polling table @return String mPKName
     */
    public String getPKName() {
        return mPKName;
    }
    
    /*
     * set the primary key name of the polling table @param string val
     */

    public void setPKName(String val) {
        mPKName = val;
    }
    
    /*
     * set the primary key value @param string val
     */
    
    public void setPKValue(String val) {
        mPKValue = val;
    }
    
    /*
     * get the primary key value @return string mPKValue
     */

    public String getPKValue() {
        return mPKValue;
    }
    
    /*
     * set heartbeat Config Interval, i.e 200% of the polling interval @param int val
     */

    public void setHeartbeatConfigInterval(int val) {
        mHeartbeatUpdateConfigInterval = val * 200 / 100;
    }
    
    /*
     * get heartbeat Config Interval, i.e 200% of the polling interval @return long
     * mHeartbeatUpdateConfigInterval
     */
    
    public long getHeartbeatConfigInterval() {
        return mHeartbeatUpdateConfigInterval; 
    }
    
    /*
     * set instance Name @param string val
     */

    public void setInstanceName(String val) {
        mInstanceName = val;
    }
    
    /*
     * get the instance Name @return string mInstance Name
     */
    
    public String getInstanceName() {
        return mInstanceName;
    }
    
    public void setProductName(String prodName){
       mprodName = prodName;   
    }
    
    /*
     * do the clustering task, 1. to update the heartbeat, 2 update the dangling instanes with the
     * current instance for ownership, 3 select dangling instances PK list(this list contains more
     * pk values, while constructing the normalize message in JDBCNormalizer class, based on the
     * numberOfRecords config, process only that many records per poll. If this list contains more
     * than that configured number then process those records in the next poll.)
     */

    public void doClusterTasks() {
        constructSQLQueries();
        // This not requried here againg to set timestamp.
        // boolean heartBeatUpdated = upDateHeartBeat();
        // if(heartBeatUpdated){
        doInstanceFailover();
       // }
    }

    
    /**
     * Check for the failed instance and acquire the dangling instances.
     */
    
    private void doInstanceFailover() {
        synchronized (mRecoverDanglingInstanceLock) {
            if (mRecoverDanglingInstInProgress) {
                return;
            }
            mRecoverDanglingInstInProgress = true;
        }
       /* boolean danglingInstancesUpdated = updateDanglingInstances();
        if(danglingInstancesUpdated){
            this.mPKList = selectDanglingInstancesPKList();
        };*/
        boolean deleted = deleteDanglingInstances();
        mRecoverDanglingInstInProgress = false;
    }
    /*
     * get the under line database connection @return connection
     */
    
    public Connection getDataBaseConnection() throws Exception {
        if(this.connection != null){
            return this.connection;
        }
        try{
            this.connection = getDatabaseConnection(mJNDIName);
            this.connection.setAutoCommit(true);
        }catch (Exception e){
            throw new Exception(mMessages.getString("DBBC_E11101.JCM_CONNECTON_EXCEPTION",
                    new Object[] {this.mJNDIName} ));
        }
        return this.connection;
    }
    

    /*
     * set the under line database connection @return connection
     */
    
    public void setDataBaseConnection(Connection con) throws Exception {
        if(con != null)
            this.connection = con;
    }
    
    /*
     * used to the Datasource @param String val
     */
    
    public void setJNDIName(String val){
        this.mJNDIName = val;
    }
    
    /*
     * used to update the instnace heatbeat per each poll to know instance is alive or not
     */

  /*
     * private boolean upDateHeartBeat() { boolean retry; boolean updated; do { retry = false;
     * updated = false; PreparedStatement ps = null; Connection con = null; try { con =
     * getDataBaseConnection(); ps = con.prepareStatement(BASE_INSTANCESTATE_UPDATE_STMT_STR);
     * ps.setString(1, getInstanceName()); int updateCount = ps.executeUpdate(); if(updateCount ==
     * 0){ // this indicates that no entry exists for this instance id, // insert new one. ps =
     * con.prepareStatement(BASE_INSTANCESTATE_INSERT_STMT_STR); ps.setString(1, getInstanceName());
     * int inserted = ps.executeUpdate(); }else if(updateCount == 1){ System.out.println("updated
     * the time stamp for the instnce "+getInstanceName()); mLogger.log(Level.INFO,
     * mMessages.getString("DBBC_R10901.JCM_UPDATED_TIME_STAMP", new Object[] {getInstanceName()})); }
     * updated = true; }catch(Exception e){ if(con != null){ try{ con.rollback(); }catch(Exception
     * ex){ System.out.println("Exception while roll back the transaction "+ex);
     * mLogger.log(Level.WARNING, mMessages.getString("DBBC_W11001.JCM_EXCEPTION_WHILE_ROLLBACK"),
     * ex); } }else{ // TODO retry for the connection } } finally{ if(ps != null){ try{ ps.close(); }
     * catch(SQLException e){ mLogger.log(Level.SEVERE,
     * mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"), e); } } } } while (retry);
     * return updated; }
     */
    
    /*
     * get the dangling instances PK list.
     */

    public List<String> getDanglingInstancePKList() {
        return mPKList;
    }
    
    /*
     * Query for the dangling instances PK list for instance to process with current instance.
     */
    
    private List selectDanglingInstancesPKList(){
        String selectQuery = BASE_OWNERTABLE_SELECT_DANGLING_STMT_STR;
        PreparedStatement ps = null;
        ResultSet rs = null;
        Connection con = null;
        try {
            con = getDataBaseConnection();
            ps = con.prepareStatement(selectQuery);
            ps.setString(1, getInstanceName());
            rs = ps.executeQuery();
            while(rs.next()){
               String pkValue = rs.getString(getPKName());
               mPKList.add(pkValue);
            }            
        } catch (SQLException ex) {
            mLogger.log(Level.SEVERE, ex.getLocalizedMessage());
        }catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        } finally{
            if (rs != null) {
                try {
                    rs.close();
                } catch (SQLException e) {
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11003.JCM_EXCEPTION_WHILE_CLOSING_RS"), e);
                }
            }
            if (ps != null) {
                try {
                    ps.close();
                } catch (SQLException e) {
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"), e);
                }
            }
            
        }
        return mPKList;        
    }
    
    /*
     * get the instance ownership transfer. @ return boolean update if successed
     */
    private boolean updateDanglingInstances() {
        boolean updated = false;
        String updateQuery = BASE_OWNERTABLE_UPDATE_DANGLING_STMT_STR;
        PreparedStatement ps = null;
        ParameterMetaData paramMetaData = null;
        int parameters = 0;
        Connection con = null;
        try {
            con = getDataBaseConnection();
            ps = con.prepareStatement(updateQuery);
            paramMetaData = ps.getParameterMetaData();
            if (paramMetaData != null) {
                parameters = paramMetaData.getParameterCount();
            }
        } catch (SQLException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        }
        if (parameters != 0) {
            // set default type.
            int columnType = java.sql.Types.VARCHAR;
            try {
                columnType = paramMetaData.getParameterType(1);
                ps.setObject(1, JDBCUtil.convert(getInstanceName(), columnType), columnType);
                ps.setLong(2, getHeartbeatConfigInterval());
                columnType = paramMetaData.getParameterType(3);
                ps.setObject(3, JDBCUtil.convert(getInstanceName(), columnType), columnType);
                columnType = paramMetaData.getParameterType(4);
                ps.setObject(4, JDBCUtil.convert(getTableName(), columnType), columnType);
                int rowsUpdated = ps.executeUpdate();
                if(rowsUpdated > 0 ){
                    mLogger.log(Level.INFO, mMessages.getString("DBBC-R01127.JDBCN_RECORD_LOCKED",
                            new Object[] { getInstanceName() }));
                    updated = true; 
                }
            } catch (SQLException e) {
                mLogger.log(Level.INFO, mMessages.getString("DBBC-R01127.JDBCN_RECORD_LOCKED",
                        new Object[] { getInstanceName() }));
                updated = false;
            }catch (Exception e) {
                mLogger.log(Level.INFO, mMessages.getString("DBBC-R01127.JDBCN_RECORD_LOCKED",
                        new Object[] { getInstanceName() }));
                updated = false;
            } finally{
                if(ps != null){
                    try{
                        ps.close();
                    } catch(SQLException e){
                        mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                                e);
                    }
                }
            }           
        }
        return updated;
    }
    


    private boolean deleteDanglingInstances() {
        boolean deleted = false;
        String deleteQuery = null;
        if(mprodName != null && mprodName.equalsIgnoreCase("ORACLE")){
            deleteQuery = BASE_ORACLE_OWNERTABLE_DELETE_DANGLING_STMT_STR;
        }else{
            deleteQuery = BASE_OWNERTABLE_DELETE_DANGLING_STMT_STR;
        }
        PreparedStatement ps = null;
        ParameterMetaData paramMetaData = null;
        int parameters = 0;
        Connection con = null;
        try {
            con = getDataBaseConnection();
            ps = con.prepareStatement(deleteQuery);
            paramMetaData = ps.getParameterMetaData();
            if (paramMetaData != null) {
                parameters = paramMetaData.getParameterCount();
            }
        } catch (SQLException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        }
        if (parameters != 0) {

            // set default type.
            int columnType = java.sql.Types.VARCHAR;
            try {
                ps.setLong(1, getHeartbeatConfigInterval());
                //columnType = paramMetaData.getParameterType(2);
                //ps.setObject(2, JDBCUtil.convert(getInstanceName(), columnType), columnType);
                //columnType = paramMetaData.getParameterType(3);
                //ps.setObject(3, JDBCUtil.convert(getTableName(), columnType), columnType);
                ps.setString(2,getInstanceName());
                ps.setString(3,getTableName());
                int rowsDeleted = ps.executeUpdate();
                if(rowsDeleted > 0 ){
                    mLogger.log(Level.INFO, mMessages.getString("DBBC-R01127.JDBCN_RECORD_LOCKED",
                            new Object[] { getInstanceName() }));
                    deleted = true; 
                }
            } catch (SQLException e) {
                mLogger.log(Level.INFO, mMessages.getString("DBBC-R01127.JDBCN_RECORD_LOCKED",
                        new Object[] { getInstanceName() }));
                deleted = false;
            }catch (Exception e) {
                mLogger.log(Level.INFO, mMessages.getString("DBBC-R01127.JDBCN_RECORD_LOCKED",
                        new Object[] { getInstanceName() }));
                deleted = false;
            } finally{
                if(ps != null){
                    try{
                        ps.close();
                    } catch(SQLException e){
                        mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                                e);
                    }
                }
            }
        }
        return deleted;
    }
    
    /*
     * Check if the record is already processed by another instance. @return boolean recordInserted
     * if not inserted, insert the record with current instance name and status to "In Progress"
     */
    public boolean isRecordInsertedByCurrentInstance() {
        boolean recordInserted = false;    
        String insertQuery = BASE_OWNERTABLE_INSERT_STMT_STR;
        PreparedStatement ps = null;
        ParameterMetaData paramMetaData = null;
        int parameters = 0;
        Connection con = null;
        try {
            con = getDataBaseConnection();
            ps = con.prepareStatement(insertQuery);
            paramMetaData = ps.getParameterMetaData(); 
            if (paramMetaData != null) {
                parameters = paramMetaData.getParameterCount();
            }
        } catch (SQLException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        }
        if (parameters != 0) { 
            if ((getPKValue() != null) && !getPKValue().trim().equals("")) {
               // set default type.
               int columnType = java.sql.Types.VARCHAR;
                try {
                	try{
                    columnType = paramMetaData.getParameterType(1);
                	}catch(Exception e){		
                	}
                    ps.setObject(1, JDBCUtil.convert(getPKValue(), columnType), columnType);
                    try{
                    columnType = paramMetaData.getParameterType(2);
                    }catch(Exception e){                    	
                    }
                    ps.setObject(2, JDBCUtil.convert(getInstanceName(), columnType), columnType);
                    try{
                    columnType = paramMetaData.getParameterType(3);
                    }catch(Exception e){                    	
                    }
                    ps.setObject(3, JDBCUtil.convert("In Progress", columnType), columnType);
                    int rowsUpdated = ps.executeUpdate();
                    recordInserted = true;
                 } catch (final Exception e) {
                     mLogger.log(Level.INFO,  mMessages.getString("DBBC-R10903.JCM_RECORD_LOCKED", new Object[]{getInstanceName()}));
                     recordInserted = false;
                  }finally{
                      if(ps != null){
                          try{
                              ps.close();
                          } catch(SQLException e){
                              mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                                      e);
                          }
                      }                      
                  }
                }
            }
        return recordInserted;
    }
    
    /*
     * Update the status to "SENT" in the owner table after sending the PKList to NMR. This flag is
     * used to not process the died instance records which are already sent to NMR, and making the staus
     * to "DONE" after getting the done from NMR
     */
    
    public int[] updateStatus(List pkList, String status) throws Exception {
        String updateQuery = BASE_OWNERTABLE_UPDATE_STMT_STR;
        PreparedStatement ps = null;
        ParameterMetaData paramMetaData = null;
        int parameters = 0;
        Connection con = null;
        int[] executedRows = null;
        try {
            con =  getDataBaseConnection();
            ps = con.prepareStatement(updateQuery);
            paramMetaData = ps.getParameterMetaData();
            if (paramMetaData != null) {
                parameters = paramMetaData.getParameterCount();
            }
        } catch (SQLException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        }
        try{
            if (parameters != 0) {
                addBatch(pkList, paramMetaData, ps, status);
            }
            executedRows = ps.executeBatch();    
            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == PreparedStatement.EXECUTE_FAILED) {
                    throw new SQLException(
                            "One of the Queries in the batch didn't update any rows, Should have updated atleast one row");
                }
                ;
            }
        }catch(SQLException e){
            if(ps != null){
                try{
                    ps.clearBatch();
                    ps.close();
                }catch(SQLException se){
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                            e);
                }
            }
            throw e;
        }finally {
            if(ps != null){
                try{
                    ps.clearBatch();
                    ps.close();
                }catch(SQLException se){
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                            se);
                }
            }
        }
        
        return executedRows;
    }
    
    private void addBatch(List pkList, ParameterMetaData parameterMeta, PreparedStatement ps, String status) throws SQLException, Exception  {
        if (!pkList.isEmpty()) {
            for (final Iterator it = pkList.iterator(); it.hasNext();) {
                String pkValue = (String) it.next();
                if ((pkValue != null) && !pkValue.trim().equals("")) {
                    // set default type.
                    int columnType = java.sql.Types.VARCHAR;
                    try {
                    	try{
                        columnType = parameterMeta.getParameterType(1);
                    	}catch(Exception e){                    		
                    	}
                        ps.setObject(1, JDBCUtil.convert(status, columnType), columnType);
                        try{
                        columnType = parameterMeta.getParameterType(2);
                        }catch(Exception e){                        	
                        }
                        ps.setObject(2, JDBCUtil.convert(pkValue, columnType), columnType);
                        ps.addBatch();
                    } catch (SQLException e) {
                        mLogger.log(Level.WARNING, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_ADDING_BATCH_TO_PS"), e);
                        if(ps != null){
                            try{
                                ps.clearBatch();
                                ps.close();
                            }catch(SQLException se){
                                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                                        e);
                            }
                        }
                        throw e;
                    }catch(Exception ex){
                        throw ex;
                    }
                }
            }
        }
    }
    
    public int[] updateStatusToDone(List pkList, String status, Connection conn) throws Exception {
        String updateQuery = BASE_OWNERTABLE_UPDATE_STMT_STR;
        PreparedStatement ps = null;
        ParameterMetaData paramMetaData = null;
        int parameters = 0;
        Connection con = conn;
        int[] executedRows = null;
        try {
            ps = con.prepareStatement(updateQuery);
            paramMetaData = ps.getParameterMetaData();
            if (paramMetaData != null) {
                parameters = paramMetaData.getParameterCount();
            }
        } catch (SQLException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11004.JCM_EXCEPTION_WHILE_GETTING_PS"), ex);
        }
        try{
            if (parameters != 0) {
                addBatch(pkList, paramMetaData, ps, status);
            }
            executedRows = ps.executeBatch();    
            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == PreparedStatement.EXECUTE_FAILED) {
                    throw new SQLException(
                            "One of the Queries in the batch didn't update any rows, Should have updated atleast one row");
                }
                ;
            }
        }catch(SQLException e){
            if(ps != null){
                try{
                    ps.clearBatch();
                    ps.close();
                }catch(SQLException se){
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                            e);
                }
            }
            throw e;
        }finally {
            if(ps != null){
                try{
                    ps.clearBatch();
                    ps.close();
                }catch(SQLException se){
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_W11002.JCM_EXCEPTION_WHILE_CLOSING_PS"),
                            se);
                }
            }
        }
        
        return executedRows;
    }
    
    /*
     * Construct SQL queries for the INSTANCESTATE Table and for OWNER_bussiner_table. to avoid
     * duplication and to support failover
     */    
    private void constructSQLQueries(){
        if(BASE_INSTANCESTATE_INSERT_STMT_STR == null){
            BASE_INSTANCESTATE_INSERT_STMT_STR = "INSERT INTO INSTANCESTATE"+ //$NON-NLS-1$
            " VALUES(?, ?, CURRENT_TIMESTAMP)"; //$NON-NLS-1$
        }
        if(BASE_INSTANCESTATE_UPDATE_STMT_STR == null){
            BASE_INSTANCESTATE_UPDATE_STMT_STR = "UPDATE INSTANCESTATE" +  //$NON-NLS-1$
            " SET lastupdatetime = CURRENT_TIMESTAMP " + "WHERE INSTANCEID = ? and TABLENAME = ?"; //$NON-NLS-1$ 
        }
        if(BASE_OWNERTABLE_INSERT_STMT_STR == null){
            BASE_OWNERTABLE_INSERT_STMT_STR = "INSERT INTO OWNER_" + getTableName() + //$NON-NLS-1$
            " VALUES(?, ?, ?)"; //$NON-NLS-1$
        }
        if(BASE_OWNERTABLE_DELETE_STMT_STR == null){
            BASE_OWNERTABLE_DELETE_STMT_STR = "DELETE FROM OWNER_" + getTableName() + //$NON-NLS-1$
            "WHERE INSTANCEID = ? and status= ?"; //$NON-NLS-1$ 
        }
        if(BASE_OWNERTABLE_UPDATE_STMT_STR == null){
            BASE_OWNERTABLE_UPDATE_STMT_STR = "UPDATE OWNER_" + getTableName() + //$NON-NLS-1$
            " SET status = ? " + " WHERE " + getPKName() + " = ?"; //$NON-NLS-1$ 
        }
        if(BASE_OWNERTABLE_DELETE_DANGLING_STMT_STR == null){
            BASE_OWNERTABLE_DELETE_DANGLING_STMT_STR = "delete from OWNER_"+getTableName()//$NON-NLS-1$ //$NON-NLS-1$ 
            + " where (status ='In Progress' or status ='SENT') and instance_name IN (select instanceid from INSTANCESTATE"
            + " where {fn TIMESTAMPDIFF(SQL_TSI_SECOND, timestamp(lastupdatetime), CURRENT_TIMESTAMP)} > ?/1000 " 
            + " and instanceid != ? and tablename = ?) ";
        }
        if(BASE_ORACLE_OWNERTABLE_DELETE_DANGLING_STMT_STR == null){
            BASE_ORACLE_OWNERTABLE_DELETE_DANGLING_STMT_STR = "delete from OWNER_"+getTableName()//$NON-NLS-1$ //$NON-NLS-1$ 
            + " where (status ='In Progress' or status ='SENT') and instance_name IN (select instanceid from INSTANCESTATE"
            + " where (trim(to_number(substr((sysdate-lastupdatetime)*24*60*60,2,(LENGTH((sysdate-lastupdatetime)*24*60*60)-19)))))  > (? / 1000) " 
            + " and instanceid != ? and tablename = ?) ";
        }
        if(BASE_OWNERTABLE_UPDATE_DANGLING_STMT_STR == null){
            BASE_OWNERTABLE_UPDATE_DANGLING_STMT_STR = "update OWNER_"+getTableName()+" set instance_name = ? " //$NON-NLS-1$ //$NON-NLS-1$ 
            + " where (status ='In Progress') and instance_name IN (select instanceid from INSTANCESTATE"
            + " where {fn TIMESTAMPDIFF(SQL_TSI_SECOND, timestamp(lastupdatetime), CURRENT_TIMESTAMP)} > ?/1000 " 
            + " and instanceid != ? and tablename = ?) ";
        }
        if(BASE_OWNERTABLE_SELECT_DANGLING_STMT_STR == null){
            BASE_OWNERTABLE_SELECT_DANGLING_STMT_STR = "select "+getPKName()+" from OWNER_"+getTableName()+ " where (status ='In Progress')"+
            " and instance_name = ?";
            
        }
        
        
    }
    
    /**
     * @param jndiName
     * @return
     * @throws javax.naming.NamingException
     */
    private Object getDataSourceFromContext(final String jndiName) throws javax.naming.NamingException {
        final Context c = mContext.getNamingContext();

        return c.lookup(jndiName);
    }

    /**
     * @param jndiName
     * @return
     * @throws Exception
     */
    private Connection getDatabaseConnection(final String jndiName) throws SQLException, NamingException {
        return ((DataSource) getDataSourceFromContext(jndiName)).getConnection();
    }

}
