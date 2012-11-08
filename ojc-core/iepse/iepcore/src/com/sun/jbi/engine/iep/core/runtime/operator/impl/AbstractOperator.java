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
 * @(#)AbstractOperator.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;
import java.sql.Connection;
import java.sql.Timestamp;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.NameUtil;
import java.sql.Statement;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Level;

/**
 * AbstractOperator.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public abstract class AbstractOperator implements Operator {
    private static final Messages mMessages = Messages.getMessages(AbstractOperator.class);

    protected Map mOpMap;
    protected String mId;
    protected String mName;
    protected String mDescription;
    protected String mOperation;
    protected QueryPlan mQueryPlan;
    protected int mTopoScore;
    protected List<String> mInputIdList;
    protected List<Operator> mInputOperatorList;
    protected List<Operator> mStaticInputOperatorList;
    protected Schema mOutputSchema;
    //============================
    protected boolean mIsGlobal;
    protected String mGlobalId;
    //============================
    protected boolean mGarbageCollectionEnabled;
    protected String mDbSchemaName;
    
    protected Connection mRuntimeConnection;
    protected PreparedStatement mOperateStmt[] = null;
    protected PreparedStatement mCheckInputStmt[] = null;
    protected PreparedStatement mUpdateInputUsageStmt[] = null;
    protected PreparedStatement mCleanOutputStmt = null;
    protected Timestamp mPrevTimestampToProcess = new Timestamp(0);
    protected Properties mProcessingState = new Properties(); 
    protected int mOperationStatus;
    
    // performance monitoring
    protected long mProcessingTime;
 
    // db special 
    protected DbSpecial mDbSpecial = null;
    
    //debug related information. Timestamp state can be linked to debug enabled
    // property
    protected Timestamp mPrevTimestampToProcessForDebug = new Timestamp(0);
    protected Timestamp mTimestampToProcessForDebug = new Timestamp(0);
    
    private boolean mIsDataAccessEnabled = false;
    private String mExtTable = null;
    protected boolean isReinitNeeded = false;
    //
    public static List<String> getStrList(String value) {
        List<String> ret = new ArrayList<String>();
        StringTokenizer st = new StringTokenizer(value, "\n\\");

        while (st.hasMoreTokens()) {
            String t = st.nextToken().trim();
            ret.add(t);
        }
        return ret;
    }

    protected void initialize(Map prop) {
        mOpMap = prop;
        Properties configProp = (Properties)prop.get(PROP_CONFIG_PROPERTIES);
        mQueryPlan = (QueryPlan)prop.get(PROP_QUERY_PLAN);
        mId = (String)prop.get(PROP_ID);
        mName = (String)prop.get(PROP_NAME);
        mDescription = (String)prop.get(PROP_DESCRIPTION);
        mOperation = NameUtil.makeJavaId(mName);
        mTopoScore = PropertyUtil.getint(prop, PROP_TOPO_SCORE, 0);
        mInputIdList = getStrList((String)prop.get(PROP_INPUT_ID_LIST));
        mInputOperatorList = new ArrayList<Operator>();
        for (int i = 0; i < mInputIdList.size(); i++) {
            QueryPlan plan = null;
            String s = mInputIdList.get(i);
            int idx = s.lastIndexOf('.');
            if (idx < 0) {
                plan = mQueryPlan;
            } else {
                String instanceId = s.substring(0, idx);
                plan = Util.getPlanByInstanceId(configProp, instanceId);
                if (plan == null) {
                    String planFullName = s.substring(0, idx) + ".iep";
                    instanceId = NameUtil.makeInstanceId(planFullName);
                    plan = Util.getPlanByInstanceId(configProp, instanceId);
                }
            }
            String opId = s.substring(idx + 1);
            Operator op = plan.getOperatorById(opId);
            mInputOperatorList.add(op);
        }
        List<String> staticInputIdList = getStrList((String)prop.get(PROP_STATIC_INPUT_ID_LIST));
        mStaticInputOperatorList = new ArrayList<Operator>();
        for (int i = 0; i < staticInputIdList.size(); i++) {
            QueryPlan plan = null;
            String s = staticInputIdList.get(i);
            int idx = s.lastIndexOf('.');
            if (idx < 0) {
                plan = mQueryPlan;
            } else {
                String instanceId = s.substring(0, idx);
                plan = Util.getPlanByInstanceId(configProp, instanceId);
                if (plan == null) {
                    String planFullName = s.substring(0, idx) + ".iep";
                    instanceId = NameUtil.makeInstanceId(planFullName);
                    plan = Util.getPlanByInstanceId(configProp, instanceId);
                }
            }
            String opId = s.substring(idx + 1);
            Operator op = plan.getOperatorById(opId);
            mStaticInputOperatorList.add(op);
        }

        String outputSchemaId = (String)prop.get(PROP_OUTPUT_SCHEMA_ID);
        mOutputSchema = mQueryPlan.getSchema(outputSchemaId);
        
        //=========================================================
        mIsGlobal = PropertyUtil.getboolean(prop, PROP_IS_GLOBAL, false);
        mGlobalId = (String)prop.get(PROP_GLOBAL_ID);
        mDbSpecial = (DbSpecial)prop.get(PROP_DB_SPECIAL);

        mGarbageCollectionEnabled = PropertyUtil.getboolean(configProp, PROP_GARBAGE_COLLECTION_ENABLED, true);
        mDbSchemaName = (String)configProp.get(PROP_DB_SCHEMA);
    }
    
    protected AbstractOperator() {
    }
    
    protected void registerInputTableUsage(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String fullId = planId + "." + mId;
        for (int i = 0; i < mInputOperatorList.size(); i++) {
            Operator inputOp = mInputOperatorList.get(i);
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                Util.initializeTableUsage(con, inputPlanId, inputTableName, mId);
            } else {
                Util.initializeTableUsage(con, inputPlanId, inputTableName, fullId);
            }
        }
    }
    
    protected void unregisterInputTableUsage(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String fullId = mQueryPlan.getId() + "." + mId;
        for (int i = 0; i < mInputOperatorList.size(); i++) {
            Operator inputOp = mInputOperatorList.get(i);
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                Util.deleteTableUsage(con, inputPlanId, inputTableName, mId);
            } else {
                Util.deleteTableUsage(con, inputPlanId, inputTableName, fullId);
            }
        }
    }

    // template method: used by deploy()
    protected void createOutputQueue(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        if (getOutputType().equals(IO_TYPE_STREAM)) {
            mDbSpecial.createStream(con, tableName, schema, new ArrayList<ColumnMetadata>(), true);
        } else if (getOutputType().equals(IO_TYPE_RELATION)) {
            mDbSpecial.createRelation(con, tableName, schema, new ArrayList<ColumnMetadata>(), false);
        }
    }
    
    // template method: used by undeploy()
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        Util.dropTable(con, tableName);
    }
    
    // utility methods
    protected String getIndexName(String planId, String opId) {
        return "i_" + planId + "_" + opId;
    }

    // template method: used by deploy()
    protected void createSynopsis(Connection con) throws Exception {
    }
    
    // template method: used by undeploy()
    protected void dropSynopsis(Connection con) throws Exception {
    }
    
    // template method: used by setConnection(..)
    protected void createCheckInputStmt(Connection con) throws Exception {
        int inputOperatorCount = mInputOperatorList.size();
        mCheckInputStmt = new PreparedStatement[inputOperatorCount];
        for (int i = 0;  i < inputOperatorCount; i++) {
            Operator op = mInputOperatorList.get(i);
            String inputTableName = op.getQueueName();
            mCheckInputStmt[i] = Util.createHasRowsBetweenTimestampStmt(con, inputTableName);
           // mCheckInputStmt[i] = Util.createModHasRowsAfterTimestampStmt(con, inputTableName);
            mCheckInputStmt[i].setFetchSize(1);
            mCheckInputStmt[i].setMaxRows(1);
        }
    }
        
    // template method: used by setConnection(..)
    protected void createUpdateInputUsageStmt(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String fullId = mQueryPlan.getId() + "." + mId;
        int inputCount = mInputOperatorList.size();
        mUpdateInputUsageStmt = new PreparedStatement[inputCount];
        for (int i = 0;  i < inputCount; i++) {
            Operator inputOp = mInputOperatorList.get(i);
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                mUpdateInputUsageStmt[i] = Util.createUpdateTableUsageStmt(con, inputPlanId, inputTableName, mId);
            } else {
                mUpdateInputUsageStmt[i] = Util.createUpdateTableUsageStmt(con, inputPlanId, inputTableName, fullId);
            }
        }
    }

    // template method: used by setConnection(..)
    protected void createOperateStmt(Connection con) throws Exception {
    }

    // template method: used by setConnection(..)
    protected void createCleanOutputStmt(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String outputTableName = getQueueName();
        if (getOutputType().equals(IO_TYPE_STREAM)) {
            mCleanOutputStmt = Util.createCleanStreamByMinUsageTimeStmt(con, planId, outputTableName);
        } else if (getOutputType().equals(IO_TYPE_RELATION)) {
            mCleanOutputStmt = mDbSpecial.createCleanRelationByMinUsageTimeStmt(con, planId, outputTableName);
        }
    }
    
    // template method: used by operate(..)
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        try {
            if (mOperateStmt != null) {
                for (int i = 0; i < mOperateStmt.length; i++) {
                    mOperateStmt[i].setTimestamp(1, prevT);
                    mOperateStmt[i].setTimestamp(2, curT);
                    mOperateStmt[i].executeUpdate();
                }
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Operator.Name_Id", new Object[]{mName, mId}), e);
        }
    }
    
    // template method: used by operate(..)
    protected void executeUpdateInputUsageStmt(Timestamp curT) throws Exception {
        if (mUpdateInputUsageStmt != null) {
            for (int i = 0, I = mUpdateInputUsageStmt.length; i < I; i++) { 
                mUpdateInputUsageStmt[i].setTimestamp(1, curT);
                mUpdateInputUsageStmt[i].executeUpdate();
            }
        }
    }

    public final String getId() {
        return mId;
    }
    
    public final String getName() {
        return mName;
    }
    
    public final String getOperation() {
        return mOperation;
    }
    
    public final QueryPlan getPlan() {
        return mQueryPlan;
    }
    
    public final Schema getOutputSchema() {
        return mOutputSchema;
    }

    public String getQueueName() {
        if (isGlobal()) {
            return getGlobalId();
        }
        return "q_" + mQueryPlan.getId() + "_" + getId();
    }
    
    public final int getTopoScore() {
        return mTopoScore;
    }
    
    public List<Operator> getInputOperatorList() {
        return mInputOperatorList;
    }

    public List<Operator> getStaticInputOperatorList() {
        return mStaticInputOperatorList;
    }
    
    public void initProcessingState(Properties processingState) {
        mProcessingState = processingState;
        try {
            String sv = mProcessingState.getProperty(PS_PREV_TIMESTAMP_TO_PROCESS);
            
            // remove prevTimetampToProcess so that it won't be persisted
            // prevTimetampToProcess is persisted as plan level state
            mProcessingState.remove(PS_PREV_TIMESTAMP_TO_PROCESS);
            
            mPrevTimestampToProcess = new Timestamp(Long.parseLong(sv));
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_initialize_processing_state", getName(), e);
            mPrevTimestampToProcess = new Timestamp(0);
        }
    }
    
    public Properties getProcessingState() {
        // update mProcessingState with operator level state
        return mProcessingState;
    }
    
    public void setRuntimeConnection(Connection con) {
        if (con ==  null) {
            return;
        }
        unsetRuntimeConnection();
        mRuntimeConnection = con;
        try {
            createCheckInputStmt(con);

            createOperateStmt(con);

            createUpdateInputUsageStmt(con);

            createCleanOutputStmt(con);
            
            isReinitNeeded = false;
            
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_set_up", getName(), e);
        }
    }

    public void unsetRuntimeConnection() {
        if (mOperateStmt != null) {
            for (int i = 0, I = mOperateStmt.length; i < I; i++) { 
                Util.close(mOperateStmt[i]);
                mOperateStmt[i] = null;
            }
            mOperateStmt = null;
        }
        
        if (mCheckInputStmt != null) {
            for (int i = 0, I = mCheckInputStmt.length; i < I; i++) { 
                Util.close(mCheckInputStmt[i]);
                mCheckInputStmt[i] = null;
            }
            mCheckInputStmt = null;
        }

        if (mUpdateInputUsageStmt != null) {
            for (int i = 0, I = mUpdateInputUsageStmt.length; i < I; i++) { 
                Util.close(mUpdateInputUsageStmt[i]);
                mUpdateInputUsageStmt[i] = null;
            }
            mUpdateInputUsageStmt = null;
        }
        
        Util.close(mCleanOutputStmt);
        mCleanOutputStmt = null;

        mRuntimeConnection = null;
    }
    
    public PreparedStatement getOperateStatement(int i) {
        return mOperateStmt[i];
    }

    // input: data, timestamp, tag
    // output: data, timestamp
    public void operate(Timestamp timestampToProcess) {
        long startTime = System.currentTimeMillis();
        try {
            if (mGarbageCollectionEnabled && mCleanOutputStmt != null) {
                // clean up output queue using previous timestamp
                mCleanOutputStmt.executeUpdate();
            }
            
            executeOperateStmt(mPrevTimestampToProcess, timestampToProcess);
            
            executeUpdateInputUsageStmt(timestampToProcess);
            //one statement assign for debug purposes only
            // assuming debug operation is called always after the execution for that
            // debug information.
            mPrevTimestampToProcessForDebug = mPrevTimestampToProcess;
            mTimestampToProcessForDebug = timestampToProcess;
            //
            mPrevTimestampToProcess = timestampToProcess;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_operate", getName(), e);
            Util.rollback(mRuntimeConnection);
        } finally {
            mProcessingTime += (System.currentTimeMillis() - startTime);
        }
    }
    
    public boolean hasWorkToDo(Timestamp timestampToProcess) {
        if (mInputOperatorList.size() == 0) {
            return true;
        }
        if (mCheckInputStmt == null) {
            return false;
        }
        ResultSet rs = null;
        boolean result = false;
        try {
            for (int i = 0, I = mCheckInputStmt.length; i < I && !result; i++) {
                mCheckInputStmt[i].setTimestamp(1, mPrevTimestampToProcess);
                mCheckInputStmt[i].setTimestamp(2, timestampToProcess);
                rs = mCheckInputStmt[i].executeQuery();
                if (rs.next()) {
                   //ts =  rs.getTimestamp(1) ;
                    //if(ts!= null && ts.after(mPrevTimestampToProcess)){
                    //if(rs.getInt(1)>0){
                    // If one input queue is not empty, that's the work to do
                    // This is the case after enumerating all relation-stream, 
                    // relation-relation, stream-relation operators 
                    //Util.close(rs);
                        result =  true;
                    }
                    //}
                
                Util.close(rs);
            }
        } catch (Exception e) {
            Util.close(rs);
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_check_input_status", getName(), e);
        } 
        return result;
    }
    
    protected void registerOutput(Connection con) throws Exception {
    }
    public void deploy(Connection con) throws Exception {
        try {
            registerInputTableUsage(con);
            createOutputQueue(con);
            createSynopsis(con);
            registerOutput(con);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_deploy", getName(), e);
            throw e;
        }    
    }
    
    protected void unregisterOutput(Connection con) throws Exception {
    }
    public void undeploy(Connection con) {
        try {
            unregisterInputTableUsage(con);
            dropOutputQueue(con);
            dropSynopsis(con);
            unregisterOutput(con);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_undeploy", getName(), e);
        }    
    }
    
    public void undeployExternalResource() {
    }

    public void deployExternalResource() throws Exception {
    }
    
    public boolean hasReport() {
        return true;
    }

    public void resetReport() {
        mProcessingTime = 0;
    }
    
    public String getReport(int indentation) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < indentation; i++) {
            sb.append("\t");
        }
        sb.append(mMessages.getString("Operator.Processing_time_milliseconds"));
        sb.append(mProcessingTime);
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (o instanceof AbstractOperator) {
            AbstractOperator op = (AbstractOperator)o;
            return op.mQueryPlan.getId().equals(mQueryPlan.getId()) &&
                    op.getId().equals(getId());
        }
        return false;
    }
    
    public int hashCode() {
        return 5*mQueryPlan.getId().hashCode() + 3*mId.hashCode() + 1;
    }
    
    //========================================
    public boolean isGlobal() {
        return mIsGlobal;
    }
    
    public String getGlobalId() {
        return mGlobalId;
    }
    
    public boolean getGarbageCollectionEnabled() {
        return mGarbageCollectionEnabled;
    }
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = new HashMap<String,Object>();
        map.put(PROP_NAME,this.getName());
        map.put(PROP_ID,this.getId());
        return map;    
    }
    
    public PreparedStatement getPreparedStatmentForDebugResult(Connection con) throws Exception {

        if (getOutputType().equals(IO_TYPE_RELATION)) {
            String[] inputColumnName = getOutputSchema().getColumnNames();
            PreparedStatement stmt = mDbSpecial.getRelationResultForDebugInfo(con, getQueueName(), inputColumnName);
            stmt.setTimestamp(1, mPrevTimestampToProcessForDebug);
            stmt.setTimestamp(2, mTimestampToProcessForDebug);
            return stmt;
        } else {
            return null;
        }
    }

    public void setAdministrableProperty(String propName, Object propValue) {
        //Do Nothing for now Need to implement after normalizing mech. of propName > propValue type .
    }
    /*
    public void checkForPause() {
    int inputIdCount = mInputIdList.size();
    for (int i = 0;  i < inputIdCount; i++) {
    String inputId = (String)mInputIdList.get(i);
    Operator parentOpr = mQueryPlan.getOperatorById(inputId);
    if(parentOpr.isPaused()){
    pause();
    return;
    }
    }
    }
     */

    
    public void setDataAccessEnabled(boolean daEnabled){
        mIsDataAccessEnabled = daEnabled;
    }
    public boolean isDataAccessEnabled(){
        return mIsDataAccessEnabled ;
    }

    public void setExtTable(String tabName) {
        mExtTable = tabName;
    }
    public String getExtTable() {
        return mExtTable;
    }
    public DbSpecial getDBSpecial() {
        return mDbSpecial;
    }
    

    
    public Map getOperatorProperties() {
        return mOpMap;
    }
    public boolean needsReset() {
        return isReinitNeeded;
    }
    
}