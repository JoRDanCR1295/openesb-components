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
 * @(#)TupleSerialCorrelation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.sql.ResultSet;
import java.util.Map;
import java.util.List;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.util.HashMap;
import java.util.logging.Level;

/**
 * TupleSerialCorrelation.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class TupleSerialCorrelation extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(TupleSerialCorrelation.class);

    private List<String> mFromColumnList;
    private int mSize;
    private int mIncrement;

    public TupleSerialCorrelation(Map prop) {
        initialize(prop);
        mFromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mSize = PropertyUtil.getint(prop, PROP_SIZE, 0);
        mIncrement = PropertyUtil.getint(prop, PROP_INCREMENT, 1);
    }
    
    public String getOutputType() {
        return IO_TYPE_STREAM;
    }

    @Override
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }

    // template method: used by setConnection(..)
    @Override
    protected void createUpdateInputUsageStmt(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        mUpdateInputUsageStmt = new PreparedStatement[2];
        String inputTableName = mInputOperatorList.get(0).getQueueName();
        //   SELECT t1.Timstamp 
        //   FROM S1 t0, S1 t1 
        //   WHERE t0.seqId = (SELECT MAX(seqId) FROM S1 WHERE MOD(seqId - mSize, mIncrement) = 0 AND Timetamp <= ?)
        //     AND t1.seqId = to.seqId - mSize + 1
        String usageTableName = Util.getTableUsageTableName(planId);
        StringBuffer sb = new StringBuffer();
        sb.append("SELECT t1." + COL_TIMESTAMP + " FROM "); 
        sb.append(inputTableName + " t0, " + inputTableName + " t1 WHERE ");
        sb.append("t0." + COL_SEQID + " = (SELECT MAX(" + COL_SEQID + ") FROM " + inputTableName);
        sb.append(" WHERE MOD(" + COL_SEQID + " - " + mSize + ", " + mIncrement + ") = 0 AND " + COL_TIMESTAMP + " <= ?) AND t1." + COL_SEQID + " = t0." + COL_SEQID + " - " + (mSize-1));
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.UpdateTableUsageStmt_i", new Object[]{getName(), "0", sqlStr});
        }    
        mUpdateInputUsageStmt[0] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        // UPDATE usageTableName SET COL_TIMESTAMP  = ? 
        // WHERE COL_TABLE_NAME = 'S1' AND COL_USER_ID = 'mId';
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

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        mOperateStmt = mDbSpecial.getTupleSerialCorrelationDb().createOperateStatements(con, this);
    }
        
    // template method: used by operate(..)
    @Override
    protected void executeUpdateInputUsageStmt(Timestamp curT) throws Exception {
        ResultSet rs = null;
        try {
            mUpdateInputUsageStmt[0].setTimestamp(1, curT);
            rs = mUpdateInputUsageStmt[0].executeQuery();
            if (rs.next()) {
                long lv = rs.getTimestamp(1).getTime();
                Timestamp ts = new Timestamp(lv - 1000);
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
    
    public List<String> getFromColumnList() {
        return mFromColumnList;
    }
    
    public int getSize() {
        return mSize;
    }
    
    public int getIncrement() {
        return mIncrement;
    }
    
    
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_SIZE, new Integer(mSize));
        map.put(PROP_INCREMENT, new Integer(mIncrement));
        
        return map;
    }
    
}

        
