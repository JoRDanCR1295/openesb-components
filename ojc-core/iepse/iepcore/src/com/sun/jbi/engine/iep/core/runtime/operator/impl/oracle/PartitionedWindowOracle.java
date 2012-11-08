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
 * @(#)PartitionedWindowOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.oracle;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.PartitionedWindow;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.PartitionedWindowDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class PartitionedWindowOracle implements PartitionedWindowDb {
    private static final Messages mMessages = Messages.getMessages(PartitionedWindowOracle.class);
    
    private OracleSpecial mOracleSpecial;
    
    public PartitionedWindowOracle(OracleSpecial oracleSpecial) {
        mOracleSpecial = oracleSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, PartitionedWindow op) throws Exception {
        // Prepare mOperateStmt:
        PreparedStatement[] ret = new PreparedStatement[3];
        
        QueryPlan plan = op.getPlan();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        List attributeList = op.getAttributeList();
        long size = op.getSize();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        int attrCnt = attributeList.size();
        String attrName[] = new String[attrCnt];
        for (int i = 0; i < attrCnt; i++) {
            attrName[i] = (String)attributeList.get(i);
        }
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("0," +  COL_SEQID + "," + COL_TIMESTAMP + ",'+' FROM ");
        sb.append(inputTableName + " u1");
        sb.append(" WHERE ? < u1." + COL_TIMESTAMP + " AND u1." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("(SELECT COUNT(*) FROM " + inputTableName + " u2 WHERE ");
        sb.append("u2." + COL_TIMESTAMP + " = u1." + COL_TIMESTAMP + " AND ");
        for (int i = 0; i < attrCnt; i++) {
            sb.append("u2." + attrName[i] + " = u1." + attrName[i] + " AND ");
        }
        sb.append("u2." + COL_SEQID + " > u1." + COL_SEQID + ") < " + size);
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }
        ret[0] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("Update ");
        sb.append(outputTableName);
        sb.append(" t1 Set " + COL_PSEQID + " = ( SELECT COUNT(*) FROM ");
        sb.append(outputTableName);
        sb.append(" t2 WHERE ");
        for (int i = 0; i < attrCnt; i++) {
            sb.append("t1." + attrName[i] + " = t2." + attrName[i] + " AND ");
        }
        sb.append("t2." + COL_TAG + " = '+' AND t2." + COL_SEQID + " <= t1." + COL_SEQID);
        sb.append(") WHERE t1." + COL_TAG + " = '+'");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1." + cm.getColumnName() + ",");
        }
        sb.append("0, t1." + COL_SEQID + ",t2." + COL_TIMESTAMP + ",'-' FROM ");
        sb.append(outputTableName + " t1, " + outputTableName + " t2");
        sb.append(" WHERE ? < t2." + COL_TIMESTAMP + " AND t2." + COL_TIMESTAMP + " <= ? AND ");
        for (int i = 0; i < attrCnt; i++) {
            sb.append("t1." + attrName[i] + " = t2." + attrName[i] + " AND ");
        }
        sb.append("t1." + COL_TAG + " = '+' AND ");
        sb.append("t2." + COL_TAG + " = '+' AND ");
        sb.append("t2." + COL_PSEQID + " = t1." + COL_PSEQID + " + " + size);
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "2", sqlStr});
        }    
        ret[2] = con.prepareStatement(sqlStr);
        
        return ret;
    }
}
