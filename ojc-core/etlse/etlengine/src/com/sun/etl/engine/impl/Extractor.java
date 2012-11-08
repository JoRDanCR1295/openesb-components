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
 * @(#)Extractor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.etl.engine.impl;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import com.sun.etl.utils.Logger;
import com.sun.etl.engine.ETLEngine;
import com.sun.etl.engine.ETLEngineContext;
import com.sun.etl.engine.ETLEngineExecEvent;
import com.sun.etl.engine.ETLTask;
import com.sun.etl.engine.ETLTaskNode;
import com.sun.etl.engine.utils.ETLException;
import com.sun.etl.engine.utils.MessageManager;
import com.sun.etl.exception.BaseException;
import com.sun.etl.jdbc.DBConnectionFactory;
import com.sun.etl.jdbc.SQLPart;
import com.sun.etl.jdbc.SQLUtils;
import com.sun.etl.utils.AttributeMap;
import com.sun.etl.utils.StringUtil;
import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.sql.Blob;
import java.sql.Clob;
import java.util.concurrent.atomic.AtomicBoolean;
import com.sun.jbi.internationalization.Messages;

/**
 * This class implements ETLTask and does only extraction
 *
 * @author Sudhi Seshachala
 * @author Ahimanikya Satapathy
 * @version
 */
public class Extractor extends SimpleTask {
    
    private static final Messages mMessages = Messages.getMessages(Extractor.class);
    
    /** Attribute key for indicating one-pass operation */
    public static final String KEY_ISONEPASS = "isOnePass";
    
    /** Attribute key for indicating job queue size */
    public static final String KEY_JOBQUEUESIZE = "jobQueueSize";
    
    /** Default value for extractor batch size. */
    private static final int DEFAULT_BATCH_SIZE = 5000;
    private static final String LOG_CATEGORY = Extractor.class.getName();

    //private static transient final Logger mLogger = Logger.getLogger(Extractor.class.getName());
    //private static transient final Localizer mLoc = Localizer.get();
    
    private static final MessageManager MSG_MGR = MessageManager.getManager("com.sun.etl.engine.impl");
    
    private int batchSize = DEFAULT_BATCH_SIZE;
    private String createInsertPoolName = null;
    private Connection destPoolCon = null;
    
    private DBConnectionFactory factory = DBConnectionFactory.getInstance();
    private String selectPoolName = null;
    private Connection srcPoolCon = null;
    
    private ETLTaskNode taskNode;
    
    // Producer/consumer related objects
    private BlockingQueue<PreparedStatement> jobQueue;
    private BlockingQueue<PreparedStatement> preparedStmtPool;
    
    private boolean disableMonitor = false;
    
    /** Creates a new instance of Extractor. */
    public Extractor() {
    }
    
    /** Cleans up the resources used during Extraction. */
    @Override
    public void cleanUp() {
        // Release the connection used in creating tables. This ensures that
        // (flatfiles in AxionDB) a different file can be substituted for the
        // currently used one. Ensure connection gets released at cleanup.
        Logger.print(Logger.DEBUG, LOG_CATEGORY, this, MSG_MGR.getString("MSG_extractor_close_src_conn", srcPoolCon));
	//mLogger.log(Level.INFO,mLoc.loc("INFO081: MSG_extractor_close_src_conn {0}",srcPoolCon));
        factory.closeConnection(srcPoolCon);
        
        // Ensure staging connection gets released at cleanup.
        if (destPoolCon != null) {
	    //mLogger.log(Level.INFO,mLoc.loc("INFO082: MSG_extractor_mark_for_release {0}",destPoolCon));
            Logger.print(Logger.DEBUG, LOG_CATEGORY, this, MSG_MGR.getString("MSG_extractor_mark_for_release", destPoolCon));
            taskNode.getContext().closeAndReleaseLater(destPoolCon);
        }
        
        taskNode = null;
        
        // Call parent implementation to clean up resources associated with the base
        // implementation.
        super.cleanUp();
    }
    
    /**
     * Handles Exception thrown by Engine
     *
     * @param ex BaseException that needs to be handled
     */
    @Override
    public void handleException(ETLException ex) {
        this.cleanUp();
        Logger.printThrowable(Logger.DEBUG, LOG_CATEGORY, "Extractor", "Handling exception for Extractor...", ex);
        //mLogger.log(Level.INFO,mLoc.loc("INFO083: Handling exception for Extractor... {0}",ex));
        // Ensure destination pool connection is released unconditionally if exception is
        // thrown. Source pool connection will be released in cleanUp() method, above.
        if (destPoolCon != null && createInsertPoolName != null) {
            factory.closeConnection(destPoolCon);
            destPoolCon = null;
        }
    }
    
    /**
     * @param node TaskNode that needs to be processed
     * @return Return the Success or Failure String
     * @throws com.sun.etl.engine.utils.ETLException
     *
     */
    @Override
    public String process(ETLTaskNode node) throws ETLException {
        ResultSet rs = null;
        
        try {
            if (node != null) {
                this.taskNode = node;
            } else {
                throw new ETLException("Task node is null....");
            }
            
            Object onePassFlagObj = node.getAttributeMap().getAttributeValue(KEY_ISONEPASS);
            final boolean isOnePass = onePassFlagObj instanceof Boolean && ((Boolean) onePassFlagObj).booleanValue();
            ETLEngine engine = node.getParent();
            String monitorFlag = System.getProperty("JAVACAPS_DATAINTEGRATOR_DISABLE_MONITOR");
            disableMonitor = (monitorFlag == null) ? false : Boolean.parseBoolean(monitorFlag);
        
            if (isOnePass && !disableMonitor) {
                ETLEngineContext.CollabStatistics stats = node.getParent().getContext().getStatistics();
                // Insert new execution entry with current start time in summary table
                final Timestamp startTime = super.createExecutionEntryInSummaryTable(node);//new java.sql.Timestamp(System.currentTimeMillis());//
                stats.setTableStartTime(node.getTableName(), startTime);

                final int executionId = super.getExecutionEntryIdFromSummaryTable(node, startTime);//-1;//
                stats.setTableExecutionId(node.getTableName(), executionId);
                ETLEngineExecEvent evnt = new ETLEngineExecEvent(ETLEngine.STATUS_ACTIVITY_STARTED, node.getTableName(), "" + executionId);
                taskNode.fireETLEngineExecutionEvent(evnt);
            }
            
            if (!StringUtil.isNullString(node.getDisplayName())) {
                DN += " <" + node.getDisplayName().trim() + ">";
            }
            
            String msg = MSG_MGR.getString("MSG_extractor_started");
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",msg));
            Logger.print(Logger.INFO,LOG_CATEGORY, this, msg);
            node.fireETLEngineLogEvent(msg);
            
            List connList = node.getParent().getConnectionDefList();
            AttributeMap attrMap = this.taskNode.getAttributeMap();
            String batchSizeStr = (String) attrMap.getAttributeValue("batchSize"); //NOI18N
            if (!StringUtil.isNullString(batchSizeStr)) {
                int bsInt = StringUtil.getInt(batchSizeStr);
                if (bsInt > 0) {
                    this.batchSize = bsInt;
                }
            }
            
            // get insert SQLPart...
            SQLPart insertSQLPart = node.getStatement(SQLPart.STMT_INSERT); //NOI18N
            if (insertSQLPart == null) {
                throw new ETLException(DN +mMessages.getString("ETLSE-E0420.Missing_insertSQLPart"));
            }
            
            String insertStmt = insertSQLPart.getSQL();
            if (insertStmt == null) {
                throw new ETLException(DN + mMessages.getString("ETLSE-E0421.Missing_insertStatement"));
            }
            
            Map attribMap = taskNode.getParent().getInputAttrMap();
            insertStmt = SQLUtils.replaceTableNameFromRuntimeArguments(insertStmt, attribMap);
            // get target db connection
            createInsertPoolName = insertSQLPart.getConnectionPoolName();
            destPoolCon = getConnection(createInsertPoolName, connList);
            Logger.print(Logger.DEBUG, LOG_CATEGORY, DN + " Got staging connection: " + destPoolCon);
            //mLogger.log(Level.INFO,mLoc.loc("INFO084: Got staging connection: {0}",destPoolCon));
            // drop and create staging table if specified
            setAutoCommitIfRequired(destPoolCon, true);
            dropTable(node, destPoolCon);
            createTable(node, destPoolCon);
            setAutoCommitIfRequired(destPoolCon, false);
            
            // create target table if non-existent, or truncate, if specified
            if (!createBeforeProcess(node, destPoolCon)) {
                truncateBeforeProcess(node, destPoolCon);
            }
            
            // extract data from source db
            rs = getSelectData(node);
            
            // insert the data based on the result set received above
            List types = (List) insertSQLPart.getAttribute(SQLPart.ATTR_JDBC_TYPE_LIST).getAttributeValue(); //NOI18N
            insertData(rs, insertStmt, destPoolCon, types, node);
            
            msg = MSG_MGR.getString("MSG_extractor_finished");
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",msg));
            Logger.print(Logger.INFO,LOG_CATEGORY, this,msg);
            node.fireETLEngineLogEvent(msg);
        } catch (Exception t) {
            handleException(t);
        } finally {
            Statement rsStmt = null;
            if (rs != null) {
                try {
                    rsStmt = rs.getStatement();
                } catch (SQLException e) {
                    // ignore
                }
                closeResultSet(rs);
                closeStatement(rsStmt);
            }
        }
        return ETLTask.SUCCESS;
    }
    
    /**
     * @param t
     * @throws ThreadDeath
     * @throws ETLException
     */
    private void handleException(Throwable t) throws ThreadDeath, ETLException {
        t = unwrapThrowable(t);
        StringBuffer errMsg = new StringBuffer(DN);
        String throwableMsg = t.getMessage();
        if (StringUtil.isNullString(throwableMsg)) {
            throwableMsg = t.toString();
        }
        
        errMsg.append(MSG_MGR.getString("MSG_common_chain_colon", MSG_MGR.getString("MSG_extractor_failed"), throwableMsg));
        String msg = errMsg.toString();
        Logger.print(Logger.DEBUG, LOG_CATEGORY, msg, t);
        //mLogger.log(Level.SEVERE,mLoc.loc("ERRO486: ErrorMsg: {0}",msg), t);
        throw new ETLException(msg, t);
    }
    
    /**
     * Create Table when appropriate, mostly used while extracting source data to temp
     * table, but not used if we are directly extracting to target table. Temp table is
     * required only when data comes from diversified data sources.
     *
     * @param node ETLTaskNode containing create statement
     * @param con Connection to use in executing statement
     * @throws Exception if error occurs during execution
     */
    private void createTable(ETLTaskNode node, Connection con) throws Exception {
        Statement stmt = null;
        String createTableRelatedSql = "";
        
        try {
            SQLPart createSQLPart = node.getStatement(SQLPart.STMT_CREATE); //NO18N
            
            if (createSQLPart == null) {
                return; // user did not choose to create if does not exist
            }
            
            Iterator itr = createSQLPart.getIterator();
            while (itr.hasNext()) {
                stmt = con.createStatement();
                createTableRelatedSql = (String) itr.next();
                Map attribMap = taskNode.getParent().getInputAttrMap();
                createTableRelatedSql = SQLUtils.replaceTableNameFromRuntimeArguments(createTableRelatedSql, attribMap);
                
                String createStartMsg = MSG_MGR.getString("MSG_extractor_create_attempt");
                Logger.print(Logger.INFO,LOG_CATEGORY, this,createStartMsg);
                //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",createStartMsg));
                taskNode.fireETLEngineLogEvent(createStartMsg);
                
                String showSqlMsg = MSG_MGR.getString("MSG_common_using_sql", createTableRelatedSql);
                Logger.print(Logger.INFO,LOG_CATEGORY, this,showSqlMsg);
		//mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",showSqlMsg));
                taskNode.fireETLEngineLogEvent(showSqlMsg);
                
                stmt.executeUpdate(createTableRelatedSql);
                String successMsg = MSG_MGR.getString("MSG_extractor_create_success");
                Logger.print(Logger.INFO,LOG_CATEGORY, this,successMsg);
                //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",successMsg));
                taskNode.fireETLEngineLogEvent(successMsg);
            }
        } catch (SQLException e) {
            if (isObjectAlreadyExistsException(e)) {
                String tableExistsMsg = MSG_MGR.getString("MSG_extractor_create_failure_table_exists");
                Logger.print(Logger.INFO,LOG_CATEGORY, this,tableExistsMsg);
                //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",tableExistsMsg));
                taskNode.fireETLEngineLogEvent(tableExistsMsg);
                return;
            }
            Logger.print(Logger.INFO,LOG_CATEGORY, this,e);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",e));
            //super.logSQLException(Logger.ERROR, LOG_CATEGORY, e);
            throw e;
        } catch (Exception e) {
            String failureMsg = MSG_MGR.getString("MSG_extractor_create_failure");
            Logger.print(Logger.INFO,LOG_CATEGORY, this,failureMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",failureMsg));
            //taskNode.fireETLEngineLogEvent(failureMsg, Logger.DEBUG);
            Logger.printThrowable(Logger.DEBUG, LOG_CATEGORY, this, DN + failureMsg, e);
            //mLogger.log(Level.SEVERE,mLoc.loc("ERRO545: Exception1 {0}",DN + failureMsg), e);
            throw e;
        } finally {
            closeStatement(stmt);
        }
    }
    
    /**
     * Executes a drop command associated with the given ETLTaskNode, using the given
     * Connection.
     *
     * @param node ETLTaskNode containing the drop command to execute
     * @param con Connection to use in executing the drop command
     * @throws SQLException if non-recoverable error occurs during execution
     * @throws Exception if error occurs during execution
     */
    private void dropTable(ETLTaskNode node, Connection con) throws BaseException, SQLException {
        Statement stmt = null;
        try {
            SQLPart checkTablePart = node.getStatement(SQLPart.STMT_CHECKTABLEEXISTS);
            SQLPart dropSQLPart = node.getStatement(SQLPart.STMT_DROP);
            
            if (dropSQLPart == null) {
                return; // user choose not to drop raw/target table
            }
            
            String dropMsg = MSG_MGR.getString("MSG_extractor_drop_attempt");
            Logger.print(Logger.INFO,LOG_CATEGORY, this,dropMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",dropMsg));
            node.fireETLEngineLogEvent(dropMsg, Logger.DEBUG);
            
            String dropSQL = dropSQLPart.getSQL();
            String checkSQL = checkTablePart.getSQL();
            
            String checkStmtMsg = MSG_MGR.getString("MSG_common_using_sql", checkSQL);
            String dropStmtMsg = MSG_MGR.getString("MSG_common_using_sql", dropSQL);
            //mLogger.log(Level.INFO,mLoc.loc("INFO086: Exception {0}",DN + checkStmtMsg));
            Logger.print(Logger.DEBUG, LOG_CATEGORY, DN + checkStmtMsg);
            Logger.print(Logger.DEBUG, LOG_CATEGORY, dropStmtMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",dropStmtMsg));
            node.fireETLEngineLogEvent(dropStmtMsg);
            
            //Replace the Tokenized table name, if any
            Map attribMap = taskNode.getParent().getInputAttrMap();
            checkSQL = SQLUtils.replaceTableNameFromRuntimeArguments(checkSQL, attribMap);
            dropSQL = SQLUtils.replaceTableNameFromRuntimeArguments(dropSQL, attribMap);
            stmt = con.createStatement();
            
            if (isTableExists(con, checkSQL, node)) {
                stmt.executeUpdate(dropSQL);
                if (!con.getAutoCommit()) {
                    con.commit();
                }
            }
            
            String droppedMsg = MSG_MGR.getString("MSG_common_table_dropped");
            Logger.print(Logger.DEBUG, LOG_CATEGORY, droppedMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",droppedMsg));
            node.fireETLEngineLogEvent(droppedMsg);
        } catch (SQLException e) {
            if (isObjectDoesNotExistException(e)) {
                String tableExistsMsg = MSG_MGR.getString("MSG_extractor_drop_failure_table_nonexistent");
                Logger.print(Logger.DEBUG, LOG_CATEGORY, tableExistsMsg);
                //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",tableExistsMsg));
                taskNode.fireETLEngineLogEvent(tableExistsMsg);
                return;
            }
            
            super.logSQLException(Logger.ERROR, LOG_CATEGORY, e);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",e));
            throw e;
        } catch (Exception t) {
            //mLogger.log(Level.SEVERE,mLoc.loc("ERRO088: Unexpected error while dropping table. {0}",Extractor.class.getName()), t);
            Logger.printThrowable(Logger.ERROR, LOG_CATEGORY, this, mMessages.getString("ETLSE-E0422.UnexpectedError_while_dropping_table"), t);
            throw new BaseException(t);
        } finally {
            closeStatement(stmt);
        }
    }
    
    /**
     * Executes a query associated with the given ETLTaskNode, returning a ResultSet
     * representing rows returned in response.
     *
     * @param node ETLTaskNode containing the query to execute
     * @return ResultSet (possibly empty) containing results of the query
     * @throws BaseException if error occurs during execution
     */
    private ResultSet getSelectData(ETLTaskNode node) throws BaseException {
        ResultSet rs;
        PreparedStatement stmt = null;
        String selectStmt = null;
        
        try {
            SQLPart selectSQLPart = node.getStatement(SQLPart.STMT_SELECT); //NOI18N
            if (selectSQLPart == null) {
                throw new ETLException(DN + mMessages.getString("ETLSE-E0420.Missing_selectSQLPart"));
            }
            
            selectStmt = selectSQLPart.getSQL();
            if (selectStmt == null) {
                throw new ETLException(DN + mMessages.getString("ETLSE-E0421.Missing_selectStatement"));
            }
            
            // get Source db connection
            selectPoolName = selectSQLPart.getConnectionPoolName();
            srcPoolCon = getConnection(selectPoolName, node.getParent().getConnectionDefList());
            Logger.print(Logger.DEBUG, LOG_CATEGORY, DN + " got source connection: " + srcPoolCon);
            //mLogger.log(Level.INFO,mLoc.loc("INFO089: got source connection: ",srcPoolCon));
            String startMsg = MSG_MGR.getString("MSG_extractor_select_attempt");
            Logger.print(Logger.DEBUG, LOG_CATEGORY, startMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",startMsg));
            node.fireETLEngineLogEvent(startMsg);
            
            String showSqlMsg = MSG_MGR.getString("MSG_common_using_sql", selectStmt);
            Logger.print(Logger.DEBUG, LOG_CATEGORY, showSqlMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",showSqlMsg));
            node.fireETLEngineLogEvent(showSqlMsg);
            
            // execute select and get result set
            List paramList = new ArrayList();
            Map attribMap = taskNode.getParent().getInputAttrMap();
            
            String ps = SQLUtils.createPreparedStatement(selectStmt, attribMap, paramList);
            stmt = srcPoolCon.prepareStatement(ps);
            stmt.setFetchSize(this.batchSize);
            
            SQLUtils.populatePreparedStatement(stmt, attribMap, paramList);
            rs = stmt.executeQuery();
            if (rs == null) {
                throw new ETLException(DN + mMessages.getString("ETLSE-E0423.Failed_to_get_ResultSet") + Extractor.class.getName() + mMessages.getString("ETLSE-E0424.SQL_execution_failed"));
            }
            
            String successMsg = MSG_MGR.getString("MSG_extractor_select_success");
            Logger.print(Logger.DEBUG, LOG_CATEGORY, successMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",successMsg));
            node.fireETLEngineLogEvent(successMsg);
        } catch (Exception e) {
            closeStatement(stmt);
            throw new BaseException(mMessages.getString("ETLSE-E0425.Failed_to_get_the_ResultSet_for")+ selectStmt, e);
        }
        
        return rs;
    }
    
    /**
     * Inserts data contained in given ResultSet into a temporary table, using the given
     * insert statement, Connection, List of datatypes associated with each column of the
     * ResultSet, and the ETLTaskNode associated with this execution.
     *
     * @param rs ResultSet containing data to be inserted into the temporary table
     * @param insertStmt insert DML statement to be executed
     * @param dbCon Connection to use in inserting data
     * @param types List of JDBC datatypes associated with each column in the ResultSet
     * @param node ETLTaskNode associated with this statement
     * @throws BaseException if error occurs during execution
     */
    private void insertData(ResultSet rs, String insertStmt, Connection dbCon,
            List types, ETLTaskNode node) throws BaseException {

        ETLEngineContext context = node.getParent().getContext();
        ETLEngineContext.CollabStatistics stats = context.getStatistics();

        // If one-pass, insert execution entry into summary table.
        Object onePassFlagObj = node.getAttributeMap().getAttributeValue(KEY_ISONEPASS);
        final boolean isOnePass = onePassFlagObj instanceof Boolean && ((Boolean) onePassFlagObj).booleanValue();
        
        String jobQ = (String) node.getAttributeMap().getAttributeValue(KEY_JOBQUEUESIZE);
        int jobQueueSize = 2;
        try {
            jobQueueSize = Integer.parseInt(jobQ);
        } catch(Exception ex) {
            // use the default.
        }
        
        jobQueue = new ArrayBlockingQueue<PreparedStatement>(jobQueueSize, true);
        preparedStmtPool =new ArrayBlockingQueue<PreparedStatement>(jobQueueSize);
        
        PreparedStatement prepStmt = null;
        try {
            ResultSetMetaData rsMeta = rs.getMetaData();
            
            if (rsMeta == null) {
                Logger.print(Logger.DEBUG, LOG_CATEGORY, "Unable to fetch source table metadata");
                //mLogger.log(Level.INFO,mLoc.loc("INFO128: Unable to fetch source table metadata"));
                taskNode.fireETLEngineLogEvent("Unable to fetch source table metadata");
                throw new BaseException("Unable to fetch source table metadata");
            }
            
            String insertStartMsg = MSG_MGR.getString("MSG_extractor_insert_attempt");
            Logger.print(Logger.DEBUG, LOG_CATEGORY, insertStartMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",insertStartMsg));
            taskNode.fireETLEngineLogEvent(insertStartMsg);
            
            String prepStmtMsg = MSG_MGR.getString("MSG_extractor_show_prep_stmt", insertStmt);
            Logger.print(Logger.DEBUG, LOG_CATEGORY, prepStmtMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",prepStmtMsg));
            taskNode.fireETLEngineLogEvent(prepStmtMsg);
            
            prepStmt = dbCon.prepareStatement(insertStmt);
            if (prepStmt == null) {
                String errMsg = MSG_MGR.getString("MSG_extractor_invalid_insert");
                Logger.print(Logger.DEBUG, LOG_CATEGORY, errMsg);
                //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",errMsg));
                taskNode.fireETLEngineLogEvent(errMsg);
                throw new BaseException("Invalid insert statement.");
            }
            
            long localBatchSize = 0;
            long insertCount = 0;
            ExecutorService consumerThread = Executors.newSingleThreadExecutor();
            BatchProcessingConsumer consumer = new BatchProcessingConsumer();
            Future<Long> result = consumerThread.submit(consumer);
            while (rs.next()) {
                for (int i = 1; i <= rsMeta.getColumnCount(); i++) {
                    int colType = rsMeta.getColumnType(i);
                    switch (colType) {
                        // Must treat timestamp specially, as Oracle's implementation
                        // of ResultSet.getObject() does not return an impl instance
                        // of java.sql.Timestamp, but a concrete Oracle implementation.
                        // Otherwise it will break extraction into non-Oracle db from
                        // Oracle.
                        case Types.TIMESTAMP:
                            Timestamp ts = rs.getTimestamp(i);
                             if (!rs.wasNull() && ts != null) {
                                prepStmt.setTimestamp(i, ts);
                            } else {
                                setNull(rsMeta, i, prepStmt, (String)types.get(i-1));
                            }
                            break;

                        // JDBC/ODBC bridge JDK1.4 brings back -9 for nvarchar columns in
                        // MS SQL Server tables.
                        // -8 is ROWID in Oracle.
                        // JDBC introduced NCHAR(-15), and NVARCHAR (-9)
                        case Types.CHAR:
                        case Types.VARCHAR:
                        case Types.LONGVARCHAR:
                        case -15:
                        case -9:
                        case -8: {
                            String sdata = rs.getString(i);
                            if (!rs.wasNull() && sdata != null) {
                                prepStmt.setString(i, sdata);
                            } else {
                                setNull(rsMeta, i, prepStmt, (String)types.get(i-1));
                            }
                        }
                        break;
                        
                        case Types.BINARY:
                        case Types.VARBINARY:
                        case Types.LONGVARBINARY: {
                            byte[] bdata = rs.getBytes(i);
                            if (!rs.wasNull() && bdata != null) {
                                prepStmt.setBytes(i, bdata);
                            } else {
                                setNull(rsMeta, i, prepStmt, (String)types.get(i-1));
                            }
                        }
                        break;
                        
                        case Types.BLOB: {
                            Blob blob = rs.getBlob(i);
                            if (!rs.wasNull() && blob != null) {
                                // BLOB exists, so try to read the data from it
                                byte[] blobData = null;
                                if (blob != null) {
                                    blobData = blob.getBytes(1, (int) blob.length());
                                }
                                prepStmt.setBinaryStream(i, new ByteArrayInputStream(blobData), blobData.length);
                            } else {
                                setNull(rsMeta, i, prepStmt, (String)types.get(i-1));
                            }
                        }
                        break;
                        
                        case Types.CLOB: {
                            Clob clob = rs.getClob(i);
                            if (!rs.wasNull() && clob != null) {
                                String cld = clob.getSubString(0, (int)clob.length());
                                prepStmt.setCharacterStream(i, new StringReader(cld), cld.length());
                            } else {
                                setNull(rsMeta, i, prepStmt, (String)types.get(i-1));
                            }
                        }
                        break;
                        
                        default:
                            Object value = rs.getObject(i);
                            if (!rs.wasNull() && value != null) {
                                prepStmt.setObject(i, value);
                            } else {
                                setNull(rsMeta, i, prepStmt, (String)types.get(i-1));
                            }
                            break;
                    }
                }

                if (batchSize > 1) {
                    // This needs to be revisited. Axion is poor in this operation
                    prepStmt.addBatch();
                    localBatchSize++;
                    if (localBatchSize == batchSize) {
                        String engineState = (String) taskNode.getContext().getValue("engineState"); //NO18N
                        if (engineState != null && engineState.trim().equalsIgnoreCase("not-active")) { //NOI18N
                            prepStmt.close();
                            return; // -- engine no longer active
                        }
                        // add to the job queue.
                        jobQueue.put(prepStmt);
                        prepStmt = preparedStmtPool.poll();
                        //if (null == prepStmt) { // kk 6842966
                            prepStmt = dbCon.prepareStatement(insertStmt);
                        //}

                        localBatchSize = 0;
                    }
                } else {
                    prepStmt.executeUpdate();
                    prepStmt.getConnection().commit();
                    insertCount++;
                }
            }
            if (batchSize > 1) {
                
                if (localBatchSize != 0) {
                    jobQueue.put(prepStmt);
                }
                consumer.stopConsuming();
                consumerThread.shutdown();
                insertCount += result.get();
            }

            // Use task node table name as key to represent total number of rows
            // processed.
            stats.setRowsExtractedCount(node.getTableName(), insertCount);
            if (isOnePass) {
                updateInsertStats(node, insertCount);

                ETLEngineExecEvent evnt = new ETLEngineExecEvent(ETLEngine.STATUS_ACTIVITY_COMPLETED, node.getTableName(), "" + stats.getTableExecutionId(node.getTableName()));
                taskNode.fireETLEngineExecutionEvent(evnt);
            }

            if (!dbCon.getAutoCommit()) {
                dbCon.commit();
            }

            String successMsg = MSG_MGR.getString("MSG_extractor_insert_success", new Long(insertCount));
            Logger.print(Logger.DEBUG, LOG_CATEGORY, successMsg);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",successMsg));
            taskNode.fireETLEngineLogEvent(successMsg);
        } catch (Exception se) {
            String errMsg = MSG_MGR.getString("MSG_common_sql_failed_show", se.getMessage());
            Logger.printThrowable(Logger.ERROR, LOG_CATEGORY, null, errMsg, se);
            //mLogger.log(Level.SEVERE,mLoc.loc("INFO085: {0}",errMsg), se);
            //mLogger.log(Level.INFO,mLoc.loc("INFO085: {0}",errMsg));
            taskNode.fireETLEngineLogEvent(errMsg);

            if (isOnePass) {
                // Set insert count to zero, as failure has occurred.
                // TODO Add exception description to statistics context for logging and
                // display.
                updateInsertStats(node, 0);
                ETLEngineExecEvent evnt = new ETLEngineExecEvent(ETLEngine.STATUS_ACTIVITY_EXCEPTION, node.getTableName(), "" + stats.getTableExecutionId(node.getTableName()));
                evnt.setCause(se);
                taskNode.fireETLEngineExecutionEvent(evnt);
            }
            
            throw new BaseException(se);
        } finally {
            closeResultSet(rs);
            closeStatement(prepStmt);
        }
    }
    
    /**
     * Indicates whether the given SQLException reports that an object referenced by a DML
     * statement already exists.
     *
     * @param e SQLException to be evaluated
     * @return true if e indicates that a referenced object already exists; false
     *         otherwise
     */
    private boolean isObjectAlreadyExistsException(SQLException e) {
        String sqlState = e.getSQLState();
        int vendorCode = e.getErrorCode();
        
        // TODO Add more SQLState/vendor code combinations for supported DBs.
        // DB2: SQLSTATE 42710
        // Oracle: SQLSTATE 42000 + vendorCode 00955
        return "42710".equals(sqlState) || ("42000".equals(sqlState) && 955 == vendorCode);
    }
    
    /**
     * Indicates whether the given SQLException reports that an object referenced by a DML
     * statement does not exist.
     *
     * @param e SQLException to be evaluated
     * @return true if e indicates that a referenced object already exists; false
     *         otherwise
     */
    private boolean isObjectDoesNotExistException(SQLException e) {
        String sqlState = e.getSQLState();
        int vendorCode = e.getErrorCode();
        
        // TODO Add more SQLState/vendor code combinations for supported DBs.
        // DB2: SQLSTATE 42704
        // Oracle: SQLSTATE 42000 + vendorCode 00942
        return "42704".equals(sqlState) || ("42000".equals(sqlState) && 942 == vendorCode);
    }

    private void setNull(ResultSetMetaData rsMeta, int i, PreparedStatement prepStmt, String type) throws SQLException, BaseException {
        if (rsMeta.isNullable(i) == ResultSetMetaData.columnNullable) {
            prepStmt.setNull(i, StringUtil.getInt(type));
        } else {
            throw new BaseException("Column " + rsMeta.getColumnType(i) + " is not nullable.");
        }
    }
    
    /**
     * Update statistics associated with the given task node, setting the insert count to
     * the given value.
     *
     * @param insertCt count of rows inserted
     */
    private void updateInsertStats(ETLTaskNode node, long insertCount) {
        ETLEngineContext context = node.getParent().getContext();
        ETLEngineContext.CollabStatistics stats = context.getStatistics();
        
        // Record end time of table execution.
        final Timestamp endDate = new Timestamp(System.currentTimeMillis());
        stats.setTableFinishTime(node.getTableName(), endDate);
        
        // extraction count == insertion count for one pass.
        stats.setRowsInsertedCount(node.getTableName(), insertCount);
    }
    
    private class BatchProcessingConsumer implements Callable<Long> {
        
        private AtomicBoolean stopFlag = new AtomicBoolean(false);
        
        public Long call() throws Exception {
            long insertCount = 0;
            try {
                while(!stopFlag.get()) {
                    PreparedStatement prepStmt = jobQueue.take();
                    if(prepStmt != null) {
                        int[] rows = null;
                        try {
                            rows = prepStmt.executeBatch();
                            if(!prepStmt.getConnection().getAutoCommit() ){
                                prepStmt.getConnection().commit();
                            }
                            insertCount += rows.length;
                        } catch (SQLException ex) {
                            handleException(ex);
                        }
                        prepStmt.clearBatch();
                        if(!preparedStmtPool.offer(prepStmt)) {
                            try {
                                prepStmt.close();                                
                            } catch (SQLException ex) {
                                handleException(ex);
                                // unable to close.
                            }
                        }
                    }
                }
            } catch (InterruptedException ex) {
                // come out and finish the remaining jobs.
            }
            // execute the remaining jobs and exit.
            for(int i = 0, I = jobQueue.size(); i < I; i++) {
                PreparedStatement prepStmt = jobQueue.poll();
                if(prepStmt != null) {
                    int[] rows = null;
                    try {
                        rows = prepStmt.executeBatch();
                        if(!prepStmt.getConnection().getAutoCommit() ){
                            prepStmt.getConnection().commit();
                        }
                        insertCount += rows.length;
                        prepStmt.close();
                    } catch (SQLException ex) {
                        // log this.
                        ex.printStackTrace();
                    }
                }
            }
            return insertCount;
        }
        
        public void stopConsuming() {
            stopFlag.set(true);
        }
    }
}