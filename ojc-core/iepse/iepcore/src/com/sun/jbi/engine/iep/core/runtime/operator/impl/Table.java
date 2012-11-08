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
import java.util.Map;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.logging.Level;

/**
 * TableOutput.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class Table extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(Table.class);

    public Table(Map prop) {
        initialize(prop);
    }

    public String getOutputType() {
        return IO_TYPE_TABLE;
    }

    // template method: used by deploy()
    protected void createOutputQueue(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        mDbSpecial.createRelation(con, tableName, schema, new ArrayList<ColumnMetadata>(), false);
    }
    
    // template method: used by undeploy()
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        Util.dropTable(con, tableName);
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
        //     SELECT Name, Value, SeqId, Timestamp FROM R1 WHERE 7 < Timestamp AND Timestamp <= 10 AND Tag='+'
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ",'+' FROM ");
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
    }
    
    // template method: used by operate(..)
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
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Operator.Name_Id", new Object[]{mName, mId}), e);
        } finally {
            Util.close(rs);
        }
    }

    // template method: used by setConnection(..)
    protected final void createCleanOutputStmt(Connection con) throws Exception {
    }
    
}


