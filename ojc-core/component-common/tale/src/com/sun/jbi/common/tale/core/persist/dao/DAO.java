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
 * @(#)VariableDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.persist.dao;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.tale.core.connection.DBConnection;
import com.sun.jbi.common.tale.core.connection.DBConnectionFactory;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.LoggerChannel;
import com.sun.jbi.common.tale.core.domain.LoggerCode;
import com.sun.jbi.common.tale.core.domain.Payload;
import com.sun.jbi.common.tale.core.domain.RepUser;
import com.sun.jbi.common.tale.core.domain.SourceInfo;
import com.sun.jbi.common.tale.core.util.I18n;

/**
 * Base class contains the implementation for the data access methods.
 */
public abstract class DAO {
    private static Logger LOGGER = Logger.getLogger(DAO.class.getName());
    
    /** Describes the action to take when accessing data. */
    public enum Action { INSERT, UPDATE, DELETE, SELECT };
    
    //insert into ALESE_USER.CSF_CME_LOG (MESG_ID, MESG_DATESTAMP, APP_MESG_ID, PROJECT_NAME, APP_TYPE, APP_NAME, SERVICE_NAME, MODULE_NAME, UNIT_NAME) values ('1001', CURRENT_TIMESTAMP, 'APP_MESG_ID', CURRENT_TIMESTAMP, 'PN', 'AT', 'APP', 'SN', 'MN', 'UN');
    //Mandatory columns - MESG_ID,MESG_DATESTAMP,APP_TYPE, SERVICE_NAME, MODULE_NAME, UNIT_NAME
    //Columns that are not part of the SQL - APP_DATESTAMP
    static final String INSERT_APPLICATION_INFO = "insert into ALESE_USER.CSF_CME_LOG (MESG_ID, MESG_DATESTAMP, APP_MESG_ID, PROJECT_NAME, APP_TYPE, APP_NAME, SERVICE_NAME, MODULE_NAME, UNIT_NAME) values (?, ?, ?, ?, ?, ?, ?, ?, ?)";
    
    //insert into ALESE_USER.CSF_LOGGER_LOG (MESG_ID, LOGGER_CODE, LOG_DETAILS, DISPLAY_MESG) values ('1000', 100, 'log detail', 'display msg')
    //MESG_ID is FK, LOGGER_CODE is FK
    //To populate CSF_LOGGER_CODES, It is required to populate CSF_LOGGER_CHANNELS & CSF_REP_USERS
    static final String INSERT_LOG = "insert into ALESE_USER.CSF_LOGGER_LOG (MESG_ID, LOGGER_CODE, LOG_DETAILS, DISPLAY_MESG) values (?, ?, ?, ?)";

    //insert into ALESE_USER.CSF_PAYLOAD_LOG (MESG_ID, ENCODE_MODE) values ('1003', 'ASCII');
    static final String INSERT_PAYLOAD = "insert into ALESE_USER.CSF_PAYLOAD_LOG (MESG_ID, ENCODE_MODE) values (?, ?)";

    //insert into ALESE_USER.CSF_PAYLOAD_STORE (MESG_ID, VERSION, PAYLOAD_TYPE, PAYLOAD_MESG, CREATE_ID, CREATE_DATE_TIME)
    //values ('1003', 1, 'ORIGINAL_MSG', 'This is the payload', '123', CURRENT_TIMESTAMP)
  //Columns that are not part of the SQL - CREATE_ID
    static final String INSERT_PAYLOAD_STORE = "insert into ALESE_USER.CSF_PAYLOAD_STORE (MESG_ID, VERSION, PAYLOAD_TYPE, PAYLOAD_MESG, CREATE_DATE_TIME) values (?, ?, ?, ?, ?)";
    
    /* Insert Statement for the table CSF_REP_USERS */
    static final String INSERT_REP_USERS = "insert into ALESE_USER.CSF_REP_USERS" +
            " (USER_LOGICAL_ID, USER_DESCRIPTION, USER_NAME, USER_PASSWORD, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
            "(?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    /* Insert Statement for the table CSF_LOGGER_CODES */
    static final String INSERT_LOGGER_CODES = "insert into ALESE_USER.CSF_LOGGER_CODES " +
            "(LOGGER_CODE, LOGGER_LABEL, LOGGER_CATEGORY, LOGGER_LEVEL, LOGGER_DESCRIPTION, LOGGER_CHANNEL_CODE, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
            "(?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    
    /* Insert Statement for the table CSF_LOGGER_CHANNELS */
    static final String INSERT_LOGGER_CHANNELS = "insert into ALESE_USER.CSF_LOGGER_CHANNELS" +
            " (LOGGER_CHANNEL_CODE, CHANNEL_TYPE, FILE_NAME, ACTIVE_FLAG, CREATE_ID, CREATE_DATE_TIME, LAST_MOD_ID, LAST_MOD_DATE_TIME) values " +
            "(?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, CURRENT_TIMESTAMP)";
    protected DBConnection mConn;
    protected String mIdentity;
    
    protected DAO(DBConnectionFactory connFactory) throws DAOException {
        try {
            mConn = connFactory.createDBConnection();
            if (mConn == null) {
                throw new DAOException("Failed to acquire connection!");
            }
            mIdentity = connFactory.getNextIdentityColumnValue();
        } 
        catch (SQLException e) {
            throw error(e, "TALE-6007: Failed to initialize DAO: {0}", e.getMessage());
        }
    }
    
    /**
     * Begins a new transaction boundary.
     * @throws DAOException
     */
    public void beginTransaction() throws DAOException {
        try {
            mConn.setAutoCommit(false);
        }
        catch (SQLException se) {
            throw error(se, "TALE-6008: Failed to begin transaction: {0}", se.getMessage());
        }
    }
    
    /**
     * Commits changes recorded with this DAO and closes transaction boundary. 
     * @throws DAOException
     */
    public void commitTransaction() throws DAOException {
        try {
            mConn.commit();
            mConn.setAutoCommit(true);// TODO do we need this?
        } 
        catch (SQLException se) {
            throw error(se, "TALE-6009: Failed to commit transaction: {0}", se.getMessage());
        }
    }
    
    public void recordLog(TaleRequest request, Action axn) throws DAOException {
        PreparedStatement ps = null;
        String stmt = getStatementForLoggingTable(axn);
        try {
            ps = mConn.prepareStatement(stmt);
            ps.setString(1, getIdentity());

            ps.setInt(2, request.getCode());
            ps.setString(3, request.getDetails());
            ps.setString(4, request.getDisplayMessage());
            
            ps.executeUpdate();
        }
        catch (SQLException se) {
            if (checkConnection(se)) {
                recordLog(request, axn);
            }
            else {
                rollback("recordLog");
                throw error(se, "TALE-6010: Failed to execute {0} query [{1}] on log table: {2}", 
                            axn, stmt, se.getMessage());
            }
        }
        finally {
            closeStatement(ps, "recordLog");
        }
    }
    
    public void recordInfo(SourceInfo info, Action axn) throws DAOException {
        PreparedStatement ps = null;
        String stmt = getStatementForApplicationTable(axn);
        try {
            ps = mConn.prepareStatement(stmt);
            ps.setString(1, getIdentity());

            if (info.getDateTimeStamp() == null) {
                ps.setTimestamp(2, new Timestamp(System.currentTimeMillis()));
            }
            else {
                ps.setTimestamp(2, info.getDateTimeStamp());
            }
            ps.setString(3, info.getAppMessageID());
            ps.setString(4, info.getProjectName());
            ps.setString(5, info.getApplicationType());
            ps.setString(6, info.getApplicationName());
            ps.setString(7, info.getServiceName());
            ps.setString(8, info.getModuleName());
            ps.setString(9, info.getUnitName());
            
            ps.executeUpdate();
        }
        catch (SQLException se) {
            if (checkConnection(se)) {
                recordInfo(info, axn);
            }
            else {
                rollback("recordInfo");
                throw error(se, "TALE-6011: Failed to execute {0} query [{1}] on application table: {2}", 
                            axn, stmt, se.getMessage());
            }
        }
        finally {
            closeStatement(ps, "recordInfo");
        }
    }
    
    public void recordPayload(Payload payload, Action axn) throws DAOException {
        PreparedStatement ps = null;
        String stmt = getStatementForPayloadTable(axn), method = "recordPayload";
        
        try {
            ps = mConn.prepareStatement(stmt);
            ps.setString(1, getIdentity());
            
            ps.setString(2, String.valueOf(payload.getEncodeMode()));
            ps.executeUpdate();
            // explicit cleanup, just being overly safe
            closeStatement(ps, method);
            stmt = null;
            ps = null;
            
            method = "recordPayloadStore";
            stmt = getStatementForPayloadStoreTable(axn);
            ps = mConn.prepareStatement(stmt);
            ps.setString(1, getIdentity());
            ps.setInt(2, Payload.VERSION);
            ps.setString(3, String.valueOf(payload.getPayloadType()));
            ps.setString(4, payload.getPayloadMessage());// TODO getTransformedMessage() ?
            ps.setTimestamp(5, new Timestamp(System.currentTimeMillis()));
            
            ps.executeUpdate();
        }
        catch (SQLException se) {
            if (checkConnection(se)) {
                recordPayload(payload, axn);
            }
            else {
                rollback(method);
                throw error(se, "TALE-6012: Failed to execute {0} query [{1}] on payload table: {2}", 
                            axn, stmt, se.getMessage());
            }
        }
        finally {
            closeStatement(ps, method);
        }
    }

    /**
     * Releases the DAO resource for later use.
     * @throws DAOException if an error occurs closing connection.
     */
    public void release() throws DAOException {
        try {
            mConn.close();
        }
        catch (SQLException se) {
            throw error(se, "TALE-6018: Failed to close connection: {0}", 
                        se.getMessage());
        }
    }

    protected boolean checkConnection(SQLException se) {
        return false;   // TODO Murali to implement
    }
    
    protected void rollback(String method) throws DAOException {
        try {
            mConn.rollback();
        }
        catch (SQLException se) {
            throw error(se, "TALE-6019: Failed to rollback after {0}: {1}", 
                        method, se.getMessage());
        }
    }
    
    /*
     * override these method in base class to change the SQL.
     */
    
    protected String getStatementForApplicationTable(Action axn) throws DAOException {
        switch (axn) {
            case INSERT: return INSERT_APPLICATION_INFO;
            default: {
                throw error(null, "TALE-6013: DAO does not support {0} statements on application table!", axn);
            }
        }
    }

    protected String getStatementForLoggingTable(Action axn) throws DAOException {
        switch (axn) {
            case INSERT: return INSERT_LOG;
            default: {
                throw error(null, "TALE-6014: DAO does not support {0} statements on logging table!", axn);
            }
        }
    }

    protected String getStatementForPayloadTable(Action axn) throws DAOException {
        switch (axn) {
            case INSERT: return INSERT_PAYLOAD;
            default: {  
                throw error(null, "TALE-6015: DAO does not support {0} statements on payload table!", axn);
            }
        }
    }

    protected String getStatementForPayloadStoreTable(Action axn) throws DAOException {
        switch (axn) {
            case INSERT: return INSERT_PAYLOAD_STORE;
            default: {
                throw error(null, "TALE-6016: DAO does not support {0} statements on payload store table!", axn);
            }
        }
    }

    protected String getIdentity() {
        return mIdentity;
    }
    
    protected String getInsertStatementForRepUsers(){
        return INSERT_REP_USERS; 
    }
    
    /**
     * Inserts a record into table CSF_REP_USERS
     * @param dboApp RepUser
     * @throws DAOException
     */
    public void insertRepUserRecord(RepUser dboApp) throws DAOException{
        PreparedStatement ps = null;
        try {
            String statement = getInsertStatementForRepUsers();
            ps = mConn.prepareStatement(statement);
            ps.setString(1, dboApp.getUserId());
            ps.setString(2, dboApp.getUserDesc());
            ps.setString(3, dboApp.getUserName());
            ps.setString(4, dboApp.getUserPassword());
            if (dboApp.isActiveFlag()) {
                ps.setString(5, "Y");
            } else {
                ps.setString(5, "N");   
            }
            ps.setString(6, dboApp.getCreateID());
            ps.setString(7, dboApp.getModifiedID());            
            ps.executeUpdate();
        } catch (SQLException e) {
            rollback("insertRepUserRecord");
            throwException(e, INSERT_REP_USERS, "insertRepUserRecord()");
        } finally {
            closeStatement(ps, "insertRepUserRecord()");
        }
    }
    
    protected String getInsertStatementForLoggerCodes(){
        return INSERT_LOGGER_CODES; 
    }
    
    /**
     * Inserts a record into table CSF_LOGGER_CODES
     * @param dboApp LoggerCode
     * @throws DAOException
     */
    public void insertLoggerCodeRecord(LoggerCode dboApp) throws DAOException{
        PreparedStatement ps = null;
        try {
            String statement = getInsertStatementForLoggerCodes();
            ps = mConn.prepareStatement(statement);
            ps.setInt(1, dboApp.getLoggerCode());
            ps.setString(2, dboApp.getLoggerLabel());
            ps.setString(3, dboApp.getLoggerCategory());
            ps.setString(4, dboApp.getLoggerLevel());
            ps.setString(5, dboApp.getLoggerDescription());
            ps.setInt(6, dboApp.getLoggerChannelCode());
            if (dboApp.isActiveFlag()) {
                ps.setString(7, "Y");
            } else {
                ps.setString(7, "N");	
            }
            ps.setString(8, dboApp.getCreateID());
            ps.setString(9, dboApp.getModifiedID());            
            ps.executeUpdate();
        } catch (SQLException e) {
            rollback("insertLoggerCodeRecord");
            throwException(e, INSERT_LOGGER_CODES, "insertLoggerCodeRecord()");
        } finally {
            closeStatement(ps, "insertLoggerCodeRecord()");
        }
    }
    
    protected String getInsertStatementForLoggerChannels(){
        return INSERT_LOGGER_CHANNELS; 
    }
    
    /**
     * Inserts a record into table CSF_LOGGER_CHANNEL
     * @param dboApp LoggerChannel
     * @throws DAOException
     */
    public void insertLoggerChannelRecord(LoggerChannel dboApp) throws DAOException{
        PreparedStatement ps = null;
        try {
            String statement = getInsertStatementForLoggerChannels();
            ps = mConn.prepareStatement(statement);
            ps.setInt(1, dboApp.getLoggerChannelCode());
            ps.setString(2, dboApp.getChannelType());
            ps.setString(3, dboApp.getFileName());
            if (dboApp.isActiveFlag()) {
                ps.setString(4, "Y");
            } else {
                ps.setString(4, "N");   
            }
            ps.setString(5, dboApp.getCreateID());
            ps.setString(6, dboApp.getModifiedID());            
            ps.executeUpdate();
        } catch (SQLException e) {
            rollback("insertLoggerChannelRecord");
            throwException(e, INSERT_LOGGER_CHANNELS, "insertLoggerChannelRecord()");
        } finally {
            closeStatement(ps, "insertLoggerChannelRecord()");
        }
    }
    /**
     * Utility method to create a {@link DAOException} and log its error message.
     * 
     * @param e The cause.
     * @param msg The raw message.
     * @param params The message parameters.
     * @return a <code>DAOException</code>.
     */
    protected DAOException error(Exception e, String msg, Object... params) {
        String err = I18n.loc(msg, params);
        if (e == null) {
            LOGGER.warning(err);
            return new DAOException(err);
        }
        else {
            LOGGER.log(Level.WARNING, err, e);
            return new DAOException(err, e);
        }
    }
    
    /**
     * Utility method to create a {@link DAOException} without logging its error message
     * (Used for PopulateUtility)
     * @param e Exception
     * @param query The query which failed
     * @param methodName Methodname
     * @return  a <code>DAOException</code>.
     * @throws DAOException
     */
    protected DAOException throwException(SQLException e, String query, String methodName) throws DAOException {
        throw new DAOException("ERROR executing query \n[" + query + "] in " + methodName, e);
    }

    /*
     * Utility method to close SQL Statements.
     */
    private void closeStatement(Statement stmt, String method) throws DAOException {
        try {
            if (stmt != null) stmt.close();
        }
        catch (SQLException se) {
            throw error(se, "TALE-6017: Failed to close statement in {0}!", method);
        }
    }
    
//            
//            commit();
//        } catch (DAOException e) {
//            if (checkConnection((SQLException) e.getCause())) {
//                insertLoggingData(dboApp, dboLogger);
//            } else {
//                rollback();
//                throw e;
//            }
//        } finally {
//            setAutoCommit(true);
//            try {
//                mConn.close();
//            } catch (SQLException e) {
//                throwException(e, "insertLoggingDataAndPayLoad()");
//            }
//        }
//    }
//
//    public void insertLoggingDataAndPayLoad(SourceInfo dboApp,
//            TaleRequest dboLogger, Payload dboPayload) throws DAOException {
//        try {
//            setAutoCommit(false);
//            String identity = mConnFactory.getNextIdentityColumnValue();
//            
//            insertAppRecord(dboApp, identity);
//            insertLoggerRecord(dboLogger, identity);
//            insertPayloadEncode(dboPayload, identity);
//            insertPayload(dboPayload, identity);
//            commit();
//        } catch (DAOException e) {
//            if (checkConnection((SQLException) e.getCause())) {
//                insertLoggingDataAndPayLoad(dboApp, dboLogger, dboPayload);
//            } else {
//                rollback();
//                throw e;
//            }
//        } finally {
//            setAutoCommit(true);
//            try {
//                mConn.close();
//            } catch (SQLException e) {
//                throwException(e, "insertLoggingDataAndPayLoad()");
//            }
//        }
//    }
//
//    private void insertAppRecord(SourceInfo dboApp, String identity) throws DAOException{
//        PreparedStatement ps = null;
//        try {
//            String statement = getInsertStatementForApplicationTable();
//            ps = mConn.prepareStatement(statement);
//            ps.setString(1, identity);
//
//            ps.setTimestamp(2, dboApp.getDateTimeStamp());
//            ps.setString(3, dboApp.getMessageID());
//            ps.setString(4, dboApp.getProjectName());
//            ps.setString(5, dboApp.getApplicationType());
//            ps.setString(6, dboApp.getApplicationName());
//            ps.setString(7, dboApp.getServiceName());
//            ps.setString(8, dboApp.getModuleName());
//            ps.setString(9, dboApp.getUnitName());
//            
//            ps.executeUpdate();
//        } catch (SQLException e) {
//            throwException(e, INSERT_APPLICATION_INFO, "insertAppRecord()");
//        } finally {
//            cleanUpPreparedStatement(ps);
//        }
//    }
//
//    private void insertLoggerRecord(TaleRequest dbo, String identity) throws DAOException {
//        PreparedStatement ps = null;
//        try {
//            String statement = getInsertStatementForLoggingTable();
//            ps = mConn.prepareStatement(statement);
//            ps.setString(1, identity);
//            
//            ps.setInt(2, dbo.getCode());
//            ps.setString(3, dbo.getDetails());
//            ps.setString(4, dbo.getDisplayMessage());
//
//            ps.executeUpdate();
//        } catch (SQLException e) {
//            throwException(e, INSERT_LOG, "insertLoggerRecord()");
//        } finally {
//            cleanUpPreparedStatement(ps);
//        }
//    }
//
//    private void insertPayloadEncode(Payload dboPayload, String identity) throws DAOException {
//        PreparedStatement ps = null;
//        try {
//            String statement = getInsertStatementForPayloadEncode();
//            ps = mConn.prepareStatement(statement);
//            ps.setString(1, identity);
//            
//            ps.setString(2, dboPayload.getEncodeMode().toString());
//            
//            ps.executeUpdate();
//        } catch (SQLException e) {
//            throwException(e, INSERT_PAYLOAD, "insertPayloadEncode()");
//        } finally {
//            cleanUpPreparedStatement(ps);
//        }
//    }
//
//    private void insertPayload(Payload dboPayload, String identity) throws DAOException {
//        PreparedStatement ps = null;
//        try {
//            String statement = getInsertStatementForPayload();
//            ps = mConn.prepareStatement(statement);
//            ps.setString(1, identity);
//
//            ps.setInt(2, Payload.VERSION);
//            ps.setString(3, dboPayload.getPayloadType().toString());
//            ps.setString(4, dboPayload.getOriginalMessage());// TODO getTransformedMessage() ?
//            ps.setTimestamp(5, new Timestamp(System.currentTimeMillis()));
//            
//            ps.executeUpdate();
//        } catch (SQLException e) {
//            throwException(e, INSERT_PAYLOAD_STORE, "insertPayload()");
//        } finally {
//            cleanUpPreparedStatement(ps);
//        }
//    }
//    
//    private void setAutoCommit(boolean flag) throws DAOException {
//        if (mConn == null) {
//            throw new DAOException("Database ERROR in setting auto commit ");
//        }
//        try {
//            mConn.setAutoCommit(flag);
//        } catch (SQLException e) {
//            throw new DAOException("Database ERROR in setting auto commit ", e);
//        }
//    }
//
//    private void commit() throws DAOException {
//        if (mConn == null) {
//            throw new DAOException("Database ERROR in setting auto commit ");
//        }
//        try {
//            mConn.commit();
//        } catch (SQLException e) {
//            throw new DAOException("Database ERROR in commiting the changes", e);
//        }
//    }
//    
//    private void rollback() throws DAOException {
//        if (mConn == null) {
//            throw new DAOException("Database ERROR in setting auto commit ");
//        }
//        try {
//            mConn.rollback();
//        } catch (SQLException e) {
//            throw new DAOException("Database ERROR in rolling back ", e);
//        }
//    }
//    
//    private void cleanUpPreparedStatement(PreparedStatement ps) throws DAOException {
//        try {
//            ps.close();
//        } catch (SQLException e) {
//            throw new DAOException("Database ERROR in rolling back ", e);
//        }
//    }
//
//
//    private boolean checkConnection(SQLException e) throws DAOException {
//        return false;
//    }
//
//    
//    protected DAOException throwException(SQLException e, String query, String methodName) throws DAOException {
//        throw new DAOException("ERROR executing query \n[" + query + "] in " + methodName, e);
//    }
//
//    protected DAOException throwException(SQLException e, String methodName) throws DAOException {
//        throw new DAOException("ERROR while executing " + methodName, e);
//    }
    
} // end of DAO.java
