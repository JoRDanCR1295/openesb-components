package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;

import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.ArrayList;

/**
 * ExternalTablePollingStream.java
 *
 * Poll an external table, two modes:
 * 
 * (1) Polling periodically based on polling interval
 *    fetch set of records limited by record size.
 *    The last fetch record's primary key is stored,
 *    so that next time we fetch records > last fetched record
 *  
 *     optinally  Also delete the records once they are fetched
 *     
 * (2) Always poll table from the begining, no primary key
 *    is used, this is assuming the records are limited in size
 *    and gets updated peridically. in this case delete record should always
 *    be specified.
 *    
 *         
 *    
 *
 * @author Ritesh Adval
 * 
 */
public class ExternalTablePollingStream extends AbstractOperator  {
    
    public static final String INTERVAL_NANOSECOND = "nanosecond";
    public static final String INTERVAL_MICROSECOND = "microsecond";
    public static final String INTERVAL_MILLISECOND = "millisecond";
    public static final String INTERVAL_SECOND = "second";
    public static final String INTERVAL_MINUTE = "minute";
    public static final String INTERVAL_HOUR = "hour";
    public static final String INTERVAL_DAY = "day";
    public static final String INTERVAL_WEEK = "week";
    
    
    
    
    private static final Messages mMessages = Messages.getMessages(ExternalTablePollingStream.class);
    private Logger mLogger = Logger.getLogger(ExternalTablePollingStream.class.getName());
    
    private List<String> mFromColumnList;
    private List<String> mToColumnList; // exp -> asName
    private List<String> mFromClause;
    private String mWhereClause;
    private int mPollingInterval;
    private String mPollingIntervalTimeUnit;
    private int mPollingRecordSize = -1;
    private String mPollingDatabaseJndiName;
    private String mRecordIdentifierColumnsSchemaName;
    private Schema mRecordIdentifierColumnsSchema;
    private boolean mIsRecordIdentifierColumnsSpecified = false;
    private String mIsDeletePolledRecords;
    private boolean mIsPreserveLastFetchedRecord = true;
    private String mLastFetchedRecordTable;
    
    
    private Properties mConfigProp = null;
    
    private PreparedStatement[] mPollingOperatorStmts;
    
    private List lastFetchedRecordIdentifierColumnValues = new ArrayList();
    
    private List currentFetchedRecordIdentifierColumnValues = new ArrayList();
    
    private List<String> mRecordIdentifierColumnNames = new ArrayList<String>();
    
    private Connection mSource;
    private Connection mTarget;
    
    private long mStartTime = 0;
    private long mEndTime = 0;
    
    private boolean mIsRowFetched = false;
    
    public ExternalTablePollingStream(Map prop) {
        initialize(prop);
        mFromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mToColumnList = getStrList((String)prop.get(PROP_TO_COLUMN_LIST));
        mFromClause = getStrList((String)prop.get(PROP_FROM_CLAUSE));
        mWhereClause = (String)prop.get(PROP_WHERE_CLAUSE);
        String pollingInterval = (String) prop.get(PROP_POLLING_INTERVAL);
        try {
            mPollingInterval = Integer.parseInt(pollingInterval);
        } catch(NumberFormatException ex) {
            mMessages.log(Level.SEVERE, "ExternalTablePollingStream." +
                    "Failed_to_parse_polling_interval",ex);
        }
        
            
        mPollingIntervalTimeUnit = (String) prop.get(PROP_POLLING_INTERVAL_TIME_UNIT);
        String pollingRecordSize = (String) prop.get(PROP_POLLING_RECORD_SIZE);
        try {
            mPollingRecordSize = Integer.parseInt(pollingRecordSize);
        } catch(NumberFormatException ex) {
            mLogger.log(Level.SEVERE, "ExternalTablePollingStream." +
                    "Failed_to_parse_polling_record_size", ex);
        }
        
        //record identifier specified or not
        mRecordIdentifierColumnsSchemaName = (String) prop.get(PROP_RECORD_IDENTIFIER_COLUMNS_SCHEMA);
        if(mRecordIdentifierColumnsSchemaName != null) {
            mRecordIdentifierColumnsSchema = mQueryPlan.getSchema(mRecordIdentifierColumnsSchemaName);
            if(mRecordIdentifierColumnsSchema != null) {
                if(mRecordIdentifierColumnsSchema.getColumnCount() > 0) {
                    mIsRecordIdentifierColumnsSpecified = true;
                }
            }
            
            try {
                if(mIsRecordIdentifierColumnsSpecified) {
                    String[] columnNames = mRecordIdentifierColumnsSchema.getColumnNames();
                    for(int i =0; i < columnNames.length; i++){
                        String column = columnNames[i];
                        mRecordIdentifierColumnNames.add(column);
                    }
                }
            } catch(Exception ex) {
                mMessages.log(Level.SEVERE, 
                        "ExternalTablePollingStream." +
                        "Failed_to_get_columns_of_record_identifier_from_schema"
                        , mRecordIdentifierColumnsSchemaName, ex);
            }
            
        }
        mPollingDatabaseJndiName = (String) prop.get(PROP_DATABASE_JNDI_NAME);
        mIsDeletePolledRecords = (String) prop.get(PROP_IS_DELETE_RECORDS);
        mIsPreserveLastFetchedRecord =  PropertyUtil.getboolean(prop, PROP_IS_PRESERVE_LAST_FETCHED_RECORD, true);
        mLastFetchedRecordTable = (String) prop.get(PROP_LAST_FETCHED_RECORD_TABLE);
        
        mConfigProp = (Properties)prop.get(PROP_CONFIG_PROPERTIES);
    }
    
    public String getOutputType() {
        return IO_TYPE_STREAM;
    }

    @Override
    public void unsetRuntimeConnection() {
        super.unsetRuntimeConnection();
        cleanup();
    }
    
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        super.createOutputQueue(con);
        
        //create a table where we keep last value of identifer columns
        if(mIsRecordIdentifierColumnsSpecified) {
                try {
                    
                    String recordIdentifierTable = getRecordIdentifierTableName();
                    
                    int status = mDbSpecial.checkTableStatus(con, mDbSchemaName, recordIdentifierTable, mRecordIdentifierColumnsSchema, new HashSet());
                    if(isPreserveLastFetchedRecord()) {
                    //if isPreserveLastFetchedRecord flag is true 
                    //then we recrete table only if it does not 
                    //exist or schema is changed
                    //otherwise we keep the old values in table    
                        switch (status) {
                            case TS_UNKNOWN:
                                Util.dropTable(con, recordIdentifierTable);
                                mDbSpecial.createTable(con, recordIdentifierTable, mRecordIdentifierColumnsSchema);
                                break;
                            case TS_NAME_NOT_EXIST:
                                mDbSpecial.createTable(con, recordIdentifierTable, mRecordIdentifierColumnsSchema);
                                break;
                            case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                                Util.dropTable(con, recordIdentifierTable);
                                mDbSpecial.createTable(con, recordIdentifierTable, mRecordIdentifierColumnsSchema);
                                break;
                        }
                    } else {
                        //if isPreserveLastFetchedRecord is false
                        //we always create a new table
                        //if table exist we drop it and create it
                        switch (status) {
                            case TS_UNKNOWN:
                                Util.dropTable(con, recordIdentifierTable);
                                mDbSpecial.createTable(con, recordIdentifierTable, mRecordIdentifierColumnsSchema);
                                break;
                            case TS_NAME_NOT_EXIST:
                                mDbSpecial.createTable(con, recordIdentifierTable, mRecordIdentifierColumnsSchema);
                                break;
                            case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                                Util.dropTable(con, recordIdentifierTable);
                                mDbSpecial.createTable(con, recordIdentifierTable, mRecordIdentifierColumnsSchema);
                                break;
                            case TS_TABLE_EXIST:
                                Util.dropTable(con, recordIdentifierTable);
                                mDbSpecial.createTable(con, recordIdentifierTable, mRecordIdentifierColumnsSchema);
                                break;    
                        }
                    }
                    
                }catch(Exception ex) {
                    String err = mMessages.getString(
                            "ExternalTablePollingStream.Failed_to_create_table",
                            getRecordIdentifierTableName());
                    mLogger.log(Level.SEVERE, err, ex);
                    throw ex;
                }
        }
        
    }
    
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        super.dropOutputQueue(con);
        
        //if not global (isPreserveLastFetchedRecord=false) then we do not need to keep the 
        //table where we keep last fetched value of record identifier colums
        if (!isPreserveLastFetchedRecord() && mIsRecordIdentifierColumnsSpecified) {
          String recordIdentifierTable = getRecordIdentifierTableName();
          mDbSpecial.dropTable(recordIdentifierTable, con);
        }
    }
    
    @Override
    protected void createCleanOutputStmt(Connection con) throws Exception {
        super.createCleanOutputStmt(con);
    }
    @Override
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }
    
    
    @Override
    public boolean hasWorkToDo(Timestamp timestampToProcess) {
        return false;
    }
    
    public void operate(Timestamp timestampToProcess) {
        long startTime = System.currentTimeMillis();
        try {
            if (mGarbageCollectionEnabled && mCleanOutputStmt != null) {
                // clean up output queue using previous timestamp
                mCleanOutputStmt.executeUpdate();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_operate", getName(), e);
            Util.rollback(mRuntimeConnection);
        } finally {
            mProcessingTime += (System.currentTimeMillis() - startTime);
        }
    }
    
    
    public List<String> getFromColumnList() {
        return mFromColumnList;
    }

    public List<String> getToColumnList() {
        return mToColumnList;
    }
    
    public List<String> getFromClause() {
        return mFromClause;
    }
    
    public String getWhereClause() {
        return mWhereClause;
    }
    
    public int getPollingInterval() {
        return mPollingInterval;
    }
    
    public String getPollingIntervalTimeUnit() {
        return mPollingIntervalTimeUnit;
    }
    
    public int getPollingRecordSize() {
        return mPollingRecordSize;
    }
    
    public String getDatabaseJndiName() {
        return mPollingDatabaseJndiName;
    }
    
    public Schema getRecordIdentifierColumnsSchema() {
        return mRecordIdentifierColumnsSchema;
    }
    
    public boolean isDeletePolledRecords() {
        if(mIsDeletePolledRecords == null) {
            return false;
        }
        
        return Boolean.parseBoolean(mIsDeletePolledRecords);
    }
    
    public boolean isPreserveLastFetchedRecord() {
        return mIsPreserveLastFetchedRecord;
    }
    
    public String getLastFetchedRecordTable() {
        return mLastFetchedRecordTable;
    }
    
    public void createStatements(Connection source, Connection target) throws Exception {
        if (target != null && source != null) {
            //call clean up since when su is stopped and
            //restarted we get same plan with same old
            //external polling table which may have
            //old connection and lastFetched record etc
            
            cleanup();
            
            mSource = source;
            mTarget = target;
            mPollingOperatorStmts = mDbSpecial.getExternalTablePollingStreamDb().createOperateStatements(source, target, this);
            
            //get data of last fetched record identifiers
            //so as we know what we have processed since last time
            //iep was running
            ResultSet rs = mPollingOperatorStmts[0].executeQuery();
            if(rs.next()) {
                for(int i =0; i < mRecordIdentifierColumnNames.size(); i++) {
                    String columnName = mRecordIdentifierColumnNames.get(i);
                    Object val = rs.getObject(columnName);
                    lastFetchedRecordIdentifierColumnValues.add(val);
                }
            }
        }
    }
    
    public void executeStatements() throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mStartTime = System.nanoTime();
        }
        
        if(this.mIsRecordIdentifierColumnsSpecified) {
            executeStatementRecordIdentiferSpecified();
        } else {
            executeStatementNoRecordIdentiferSpecified();
        }
        
        //log execution time if we have fetched new rows in when
        //we excute select statment on source
        if (mLogger.isLoggable(Level.FINE) && mIsRowFetched) {
            mEndTime = System.nanoTime();
            long timeToExecute = mEndTime - mStartTime;
            
            mLogger.log(Level.FINE, mMessages.getString("ExternalTablePollingStream.time_spend_in_polling"), new Object[] {timeToExecute});
        }
        
    }
    
    
    private void executeStatementRecordIdentiferSpecified() throws Exception {
        if(mPollingOperatorStmts == null) {
            return;
        }
        
        
        
        //we have 6 prepared statment
        if(mPollingOperatorStmts.length != 6) {
            String msg = mMessages.
                  getString("ExternalTablePollingStream.Expected_6_statments_found_only",
                  mPollingOperatorStmts.length);
            throw new Exception(msg);
        }
        
        
        
        //third statement is select from polled table
        //when we do not have any last fetched identifer column values.
        //first time last fetched record will be empty,
        //so we execute 2 and 4 statement
        if(lastFetchedRecordIdentifierColumnValues.size() == 0) {
            executePollingStatement(mPollingOperatorStmts[2], mPollingOperatorStmts[4]);
        } else {
            //once we have last fetched record we use it
            //to restrict records we fetch. we fetch
            //records > last fetched records.
            //we execute 3 and 4 statement. statement 3 uses last fetched
            //record in where clause to restrict new records fetched.
            
            //fourth statement is select from polled table
            //when we do not have any last fetched identifer column values.
            
            
            //fifth statement is insert into our operator table
            for(int i =0; i < lastFetchedRecordIdentifierColumnValues.size(); i++) {
                Object val = lastFetchedRecordIdentifierColumnValues.get(i);
                mPollingOperatorStmts[3].setObject(i +1, val);
            }
            
            executePollingStatement(mPollingOperatorStmts[3], mPollingOperatorStmts[4]);
            
        }
        
        
        //if we have fetched some rows its time to update our
        //last fetched row table
        if(currentFetchedRecordIdentifierColumnValues.size() != 0) {
            lastFetchedRecordIdentifierColumnValues.clear();
            lastFetchedRecordIdentifierColumnValues.addAll(currentFetchedRecordIdentifierColumnValues);
            
            //second statment is delete last fetched identifer record.
            mPollingOperatorStmts[1].executeUpdate();
            
            
            //    last statment is insert last fetched row for our record keeping
            for(int i =0; i < lastFetchedRecordIdentifierColumnValues.size(); i++) {
                Object val = lastFetchedRecordIdentifierColumnValues.get(i);
                mPollingOperatorStmts[5].setObject(i +1, val);
            }
            
            mPollingOperatorStmts[5].executeUpdate();
        }
        
    }
    
    
    private void executePollingStatement(PreparedStatement selectFromPolledTableStmt, PreparedStatement insertToOpTableStmt) throws Exception {
       
        int pollingRecordSize = getPollingRecordSize();
        if(pollingRecordSize != -1 && !hasJoin()) {
            selectFromPolledTableStmt.setMaxRows(pollingRecordSize);
            selectFromPolledTableStmt.setFetchSize(pollingRecordSize);
        } 
        
        //reset mIsRowFetched
        mIsRowFetched = false;
        int counter = 0;
        //first statment is select, second is insert
        ResultSet rs = selectFromPolledTableStmt.executeQuery();
        while(rs.next()) {
            mIsRowFetched = true;
            //clear current fetch records
            currentFetchedRecordIdentifierColumnValues.clear();
            
            List<String> toColumnList = getToColumnList();
            Iterator<String> it = toColumnList.iterator();
            int paramIndex = 1;
            
            while(it.hasNext()) {
                String columnName = it.next();
                Object val = Util.getTypeSpecificValue(rs, paramIndex);
                insertToOpTableStmt.setObject(paramIndex, val);
                
                if(mRecordIdentifierColumnNames.contains(columnName)) {
                    currentFetchedRecordIdentifierColumnValues.add(val);
                }
                paramIndex++;
            }
            insertToOpTableStmt.executeUpdate();
            
            if(isDeletePolledRecords()) {
                rs.deleteRow();
            }
            
            //if we have a join then we can not
            //use polling record size
            //in this case we need to fetch all the rows
            //of a join irrespective of polling record size
            //otherwise if we only fetch some row of join 
            //we have no way to know how to fetch remain rows
            //of a join
        
            if(pollingRecordSize != -1 && counter == pollingRecordSize -1
               && !hasJoin()) {
                break;
            } 
            
            
            counter++;
            
        }
    }
    
    
    private void executeStatementNoRecordIdentiferSpecified() throws Exception {
        if(mPollingOperatorStmts == null) {
            return;
        }
        
        executePollingStatement(mPollingOperatorStmts[0], mPollingOperatorStmts[1]);
    }
        
        
    public String getRecordIdentifierTableName() {
        //if isPreserveLastFetchedRecord true then it is user's reposibility to
        //specify unique table name which will remain after undeployment
        if(isPreserveLastFetchedRecord()) {
            return getLastFetchedRecordTable();
        }
        
        return getQueueName() + "_poll";
    }
    
    public String getQueueName() {
        return "q_" + mQueryPlan.getId() + "_" + getId();
    }
    
    private boolean hasJoin() {
        boolean hasJoin = false;
        if(getFromClause() != null && getFromClause().size() > 1) {
            hasJoin = true;
        }
        
        return hasJoin;
    }
    
    private void closeStatement() {
        if (mPollingOperatorStmts != null) {
            for (int i = 0, I = mPollingOperatorStmts.length; i < I; i++) { 
                Util.close(mPollingOperatorStmts[i]);
                mPollingOperatorStmts[i] = null;
            }
            mPollingOperatorStmts = null;
        }
    }
    
    private void cleanup() {
        closeStatement();
        if(mSource != null) {
            Util.close(mSource);
        }
        
        if(mTarget != null) {
            Util.close(mTarget);
        }
        mSource = null;
        mTarget = null;
        
        lastFetchedRecordIdentifierColumnValues.clear();
        currentFetchedRecordIdentifierColumnValues.clear();
    }
}
