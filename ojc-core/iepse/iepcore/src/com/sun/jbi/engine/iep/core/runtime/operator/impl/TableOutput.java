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
 * @(#)TableOutput.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.HashSet;
import java.util.logging.Level;

/**
 * TableOutput.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class TableOutput extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(TableOutput.class);

    private Schema mActualOutputSchema;
    public TableOutput(Map prop) {
        initialize(prop);
        try {
            Schema schema = getOutputSchema();
            List<String> columnMetadataList = new ArrayList<String>(schema.getColumnMetadataAsList());
            ColumnMetadata cm = new ColumnMetadata(COL_SEQID, SQL_TYPE_BIGINT);
            columnMetadataList.add(cm.getColumnName());
            columnMetadataList.add(cm.getColumnType());
            columnMetadataList.add(cm.getColumnSize() + "");
            columnMetadataList.add(cm.hasColumnScale() + "");
            mActualOutputSchema = new Schema("actualOutputSchema", columnMetadataList);
        } catch (Exception e) {
            
        }
    }

    public String getOutputType() {
        return IO_TYPE_NONE;
    }

    @Override
    protected void registerOutput(Connection con) throws Exception {
        String planInstanceId = mQueryPlan.getInstanceId();
        String outputTableName = getQueueName();
        PreparedStatement stmt = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("INSERT INTO ");
            sb.append(TABLE_EMS_OUTPUT);
            sb.append(" VALUES (?, ?, ?, ?)");
            stmt = con.prepareStatement(sb.toString());
            stmt.setString(1, planInstanceId);
            stmt.setString(2, outputTableName);
            stmt.setString(3, mDescription);
            stmt.setTimestamp(4, new Timestamp(0));
            stmt.executeUpdate();
        } catch (Exception e) {
            throw new Exception(mMessages.getString("TableOutput.Fail_to_register_table_output_at_ems_table", planInstanceId + "." + outputTableName), e);
        } finally {
            Util.close(stmt);
        }
    }

    @Override
    protected void unregisterOutput(Connection con) throws Exception {
        String planInstanceId = mQueryPlan.getInstanceId();
        String outputTableName = getQueueName();
        PreparedStatement stmt = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("DELETE FROM ");
            sb.append(TABLE_EMS_OUTPUT);
            sb.append(" WHERE " + COL_PLAN_INSTANCE_ID + " = ? AND " + COL_OUTPUT_NAME + " = ?");
            stmt = con.prepareStatement(sb.toString());
            stmt.setString(1, planInstanceId);
            stmt.setString(2, outputTableName);
            stmt.executeUpdate();
        } catch (Exception e) {
            throw new Exception(mMessages.getString("TableOutput.Fail_to_unregister_table_output_from_ems_table", planInstanceId + "." + outputTableName), e);
        } finally {
            Util.close(stmt);
        }
    }

    // template method: used by deploy()
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        if (isGlobal()) {
            int status = mDbSpecial.checkTableStatus(con, mDbSchemaName, tableName, mActualOutputSchema, new HashSet());
            switch (status) {
                case TS_UNKNOWN:
                    Util.dropTable(con, tableName);
                    mDbSpecial.createTable(con, tableName, mActualOutputSchema);
                    break;
                case TS_NAME_NOT_EXIST:
                    mDbSpecial.createTable(con, tableName, mActualOutputSchema);
                    break;
                case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                    Util.dropTable(con, tableName);
                    mDbSpecial.createTable(con, tableName, mActualOutputSchema);
                    break;
                case TS_TABLE_EXIST:
                    Util.cleanTable(con, tableName);
                    break;
            }   
            return;
        } 
        mDbSpecial.createTable(con, tableName, mActualOutputSchema);
    }
    
    // template method: used by undeploy()
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        boolean isGlobal = this.isGlobal();
        if (!isGlobal()) {
            String tableName = getQueueName();
            Util.dropTable(con, tableName);
        }
    }
    
    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        // Prepare mOperateStmt:
        String inputTableName = mInputOperatorList.get(0).getQueueName();
        Schema inputSchema = mInputOperatorList.get(0).getOutputSchema();
        String outputTableName = getQueueName();

        mOperateStmt = new PreparedStatement[4]; 
        StringBuffer sb = new StringBuffer();
        // INSERT INTO T1 
        //     SELECT Name, Value, SeqId FROM R1 WHERE 7 < Timestamp AND Timestamp <= 10 AND Tag='+'
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append(COL_SEQID + " FROM ");
        sb.append(inputTableName);
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ? AND "  + COL_TAG + " = '+'");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{getName(), "0", sqlStr});
        }    
        mOperateStmt[0] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        // DELETE FROM T1 WHERE EXISTS (
        //     SELECT 'x' FROM R1 WHERE SeqId=T1.SeqId AND 7 < Timestamp AND Timestamp <= 10 AND Tag='-'
        sb.append("DELETE FROM ");
        sb.append(outputTableName);
        sb.append(" WHERE EXISTS (SELECT 'x' FROM ");
        sb.append(inputTableName);
        sb.append(" WHERE " + COL_SEQID + " = " + outputTableName + "." + COL_SEQID + " AND ");
        sb.append("? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ? AND "  + COL_TAG + " = '-')");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{getName(), "1", sqlStr});
        }    
        mOperateStmt[1] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        // SELECT MAX(Timestamp) FROM R1 WHERE 7 < Timestamp AND Timestamp <= 10
        sb.append("SELECT MAX(" + COL_TIMESTAMP + ")");
        sb.append(" FROM " + inputTableName);
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ?");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{getName(), "2", sqlStr});
        }    
        mOperateStmt[2] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        // UPDATE TABLE_EMS_OUTPUT SET LastModified = ? WHERE PlanInstanceId = ? AND OutputName = ?
        sb.append("UPDATE " + TABLE_EMS_OUTPUT + " SET " + COL_OUTPUT_LAST_UPDATED + " = ?");
        sb.append(" WHERE " + COL_PLAN_INSTANCE_ID + " = ? AND " + COL_OUTPUT_NAME + " = ?");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{getName(), "3", sqlStr});
        }    
        mOperateStmt[3] = con.prepareStatement(sqlStr);
    }
    
    // template method: used by operate(..)
    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        String planInstanceId = mQueryPlan.getInstanceId();
        String outputTableName = getQueueName();
        ResultSet rs = null;
        try {
            for (int i = 0; i < 2; i++) {
                mOperateStmt[i].setTimestamp(1, prevT);
                mOperateStmt[i].setTimestamp(2, curT);
                mOperateStmt[i].executeUpdate();
            }
            mOperateStmt[2].setTimestamp(1, prevT);
            mOperateStmt[2].setTimestamp(2, curT);
            rs = mOperateStmt[2].executeQuery();
            if (rs.next()) {
                Timestamp ts = rs.getTimestamp(1);
                mOperateStmt[3].setTimestamp(1, ts);
                mOperateStmt[3].setString(2, planInstanceId);
                mOperateStmt[3].setString(3, outputTableName);
                mOperateStmt[3].executeUpdate();
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Operator.Name_Id", new Object[]{mName, mId}), e);
        } finally {
            Util.close(rs);
        }
    }

    // template method: used by setConnection(..)
    @Override
    protected final void createCleanOutputStmt(Connection con) throws Exception {
    }
    
}


