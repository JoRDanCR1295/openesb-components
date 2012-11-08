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
 * @(#)TupleBasedAggregator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.Map;
import java.util.List;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;

/**
 * TupleBasedAggregator.java
 * 
 * Created on August 1, 2005, 2:31 PM
 * 
 * @author: Bing Lu
 */
public class TupleBasedAggregator extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(TupleBasedAggregator.class);

    private List<String> mFromColumnList;
    private List<String> mToColumnList; // exp -> asName
    private String mFromClause;
    private String mWhereClause;
    private String mRWhereClause;
    private List<String> mGroupByColumnList; // hence not necessorily map mSize tuples to 1 tuple
    private long mStart; // inclusive
    private long mIncrement;
    private long mSize;
    // mStart is inclusive, mPrevSeqidToProcess must be >= mStart - 1 
    // see constructor and initProcessingState()
    private long mPrevSeqidToProcess; 
    
    public TupleBasedAggregator(Map prop) {
        initialize(prop);
        mFromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mToColumnList = getStrList((String)prop.get(PROP_TO_COLUMN_LIST));
        mFromClause = " " + (String)prop.get(PROP_FROM_CLAUSE) + " ";
        mWhereClause = (String)prop.get(PROP_WHERE_CLAUSE);
        if (mWhereClause != null) {
            mWhereClause = mWhereClause.trim();
        }
        if (mWhereClause.equals("")) {
            mRWhereClause = "";
        } else {
            Map<String, String> tableNameMap = new HashMap<String, String>();
            Operator inputOp = mInputOperatorList.get(0);
            String inputName = inputOp.getName();
            String inputTableName = inputOp.getQueueName();
            tableNameMap.put(inputName, inputTableName);
            for (Operator o : mStaticInputOperatorList) {
                String staticInputName = o.getName();
                String staticInputTableName = o.getQueueName();
                tableNameMap.put(staticInputName, staticInputTableName);
            }
            mRWhereClause = StringUtil.replacePrefixNotStartWithDot(mWhereClause, tableNameMap);
        }
        mGroupByColumnList = getStrList((String)prop.get(PROP_GROUP_BY_COLUMN_LIST));
        mStart = PropertyUtil.getint(prop, PROP_START, 0);
        mIncrement = PropertyUtil.getint(prop, PROP_INCREMENT, 1);
        mSize = PropertyUtil.getint(prop, PROP_SIZE, 1);
        mPrevSeqidToProcess = mStart - 1;
    }
    
    public String getOutputType() {
        return IO_TYPE_STREAM;
    }

    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }

    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        mOperateStmt = mDbSpecial.getTupleBasedAggregatorDb().createOperateStatements(con, this);
    }
        
    // template method: used by operate(..)
    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mDbSpecial.getTupleBasedAggregatorDb().executeOperateStatements(this, prevT, curT);
    }

    // template method: used by setConnection(..)
    @Override
    protected void createUpdateInputUsageStmt(Connection con) throws Exception {
        mUpdateInputUsageStmt = new PreparedStatement[2];
        String planId = mQueryPlan.getId();
        String inputTableName = mInputOperatorList.get(0).getQueueName();

        // SELECT Timestamp FROM S1 WHERE SeqId = prevSeqidToProcess - mSize
        StringBuffer sb = new StringBuffer();
        sb.append("SELECT " + COL_TIMESTAMP + " FROM "); 
        sb.append(inputTableName);
        sb.append(" WHERE " + COL_SEQID + " = ?");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.UpdateTableUsageStmt_i", new Object[]{getName(), "0", sqlStr});
        }    
        mUpdateInputUsageStmt[0] = con.prepareStatement(sqlStr);
        
        // UPDATE usageTableName SET COL_TIMESTAMP  = ? 
        // WHERE COL_TABLE_NAME = 'S1' AND COL_USER_ID = 'mId';
        String usageTableName = Util.getTableUsageTableName(planId);
        sb = new StringBuffer();
        sb.append("UPDATE ");
        sb.append(usageTableName); 
        sb.append(" SET " + COL_TIMESTAMP + " = ?");
        sb.append(" WHERE " + COL_TABLE_NAME + " = '");
        sb.append(inputTableName);
        sb.append("' AND " + COL_USER_ID + " = '");
        sb.append(mId);
        sb.append("'");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.UpdateTableUsageStmt_i", new Object[]{getName(), "1", sqlStr});
        }    
        mUpdateInputUsageStmt[1] = con.prepareStatement(sqlStr);
    }

    // template method: used by operate(..)
    protected void executeUpdateInputUsageStmt(Timestamp curT) throws Exception {
        ResultSet rs = null;
        try {
            // executeUpdateInputUsageStmt is called after executeOperateStmt in operate()
            long seqId = getPrevSeqidToProcess() - mSize;
            if (seqId < 1) {
                return;
            }
            mUpdateInputUsageStmt[0].setLong(1, seqId);
            rs = mUpdateInputUsageStmt[0].executeQuery();
            if (rs.next()) {
                Timestamp ts = rs.getTimestamp(1);
                long milliInSec = 1000;
                long lv = ts.getTime() - milliInSec;
                ts.setTime(lv);
                mUpdateInputUsageStmt[1].setTimestamp(1, ts);
                mUpdateInputUsageStmt[1].executeUpdate();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_update_input_usage", getName(), e);
            throw e;
        } finally {
            Util.close(rs);
        }
    }
    
    public void initProcessingState(Properties processingState) {
        mProcessingState = processingState;
        
        long lv = PropertyUtil.getlong(mProcessingState, PS_PREV_TIMESTAMP_TO_PROCESS, 0);
        // remove prevTimetampToProcess so that it won't be persisted
        // prevTimetampToProcess is persisted as plan level state
        mProcessingState.remove(PS_PREV_TIMESTAMP_TO_PROCESS);
        mPrevTimestampToProcess = new Timestamp(lv);
        
        setPrevSeqidToProcess(PropertyUtil.getint(mProcessingState, PS_PREV_SEQID_TO_PROCESS, 0));
        if (getPrevSeqidToProcess() < (mStart - 1)) {
            setPrevSeqidToProcess(mStart - 1);
        }
    }
    
    public Properties getProcessingState() {
        // update mProcessingState with operator level state
        mProcessingState.setProperty(PS_PREV_SEQID_TO_PROCESS, "" + getPrevSeqidToProcess());
        return mProcessingState;
    }
    
    public List<String> getFromColumnList() {
        return mFromColumnList;
    }

    public List<String> getToColumnList() {
        return mToColumnList;
    }
    
    public String getFromClause() {
        return mFromClause;
    }
    
    public String getWhereClause() {
        return mWhereClause;
    }
    
    public String getRWhereClause() {
        return mRWhereClause;
    }

    public List<String> getGroupByColumnList() {
        return mGroupByColumnList;
    }
    
    public long getStart() {
        return mStart;
    }
    
    public long getSize() {
        return mSize;
    }
    
    public long getIncrement() {
        return mIncrement;
    }

    @Override
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_WHERE_CLAUSE, getWhereClause());
        map.put(PROP_FROM_CLAUSE, getFromClause());
        return map;
    }
    


    public long getPrevSeqidToProcess() {
        return mPrevSeqidToProcess;
    }

    public void setPrevSeqidToProcess(long prevSeqidToProcess) {
        this.mPrevSeqidToProcess = prevSeqidToProcess;
    }

}

        
