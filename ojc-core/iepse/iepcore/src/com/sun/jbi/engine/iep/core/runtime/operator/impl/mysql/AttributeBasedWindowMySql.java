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
 * @(#)AttributeBasedWindowMySql.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AttributeBasedWindow;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AttributeBasedWindowDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class AttributeBasedWindowMySql implements AttributeBasedWindowDb {
    private static final Messages mMessages = Messages.getMessages(AttributeBasedWindowMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public AttributeBasedWindowMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, AttributeBasedWindow op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];
        
        QueryPlan plan = op.getPlan();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String attribute = op.getAttribute();
        String attributeType = op.getAttributeType();
        double size = op.getSize();
        if (attributeType.equals(SQL_TYPE_TIMESTAMP)) {
            size = Math.ceil(size/1000);  // Up round size to integer seconds
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
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ",'+' FROM ");
        sb.append(inputTableName + " u1");
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ? AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + inputTableName + " u2");
        sb.append(" WHERE u1." + COL_TIMESTAMP + " = u2." + COL_TIMESTAMP + " AND ");
        sb.append("u1." + attribute + " <= (u2." + attribute + " - " + size + "))");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);
        

        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1.");
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("t1." + COL_SEQID + ",MIN(t2." + COL_TIMESTAMP + "),'-' FROM ");
        sb.append(outputTableName + " t1, " + outputTableName + " t2 ");
        sb.append("WHERE t1." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " t");
        sb.append(" WHERE t." + COL_SEQID + " = t1." + COL_SEQID + " AND ");
        sb.append("t." + COL_TAG + " = '-') AND ");
        sb.append("t2." + COL_TAG + " = '+' AND ");
        sb.append("? < t2." + COL_TIMESTAMP + " AND t2." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("t1." + attribute + " <= (t2." + attribute + " - " + size + ")");
        sb.append(" GROUP BY ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1.");
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("t1." + COL_SEQID);
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);

        return ret;
    }
    
    public void executeOperateStatements(AttributeBasedWindow op, Timestamp prevT, Timestamp curT) throws Exception {
        op.getOperateStatement(0).setTimestamp(1, prevT);
        op.getOperateStatement(0).setTimestamp(2, curT);
        op.getOperateStatement(0).executeUpdate();

        op.getOperateStatement(1).setTimestamp(1, prevT);
        op.getOperateStatement(1).setTimestamp(2, curT);
        op.getOperateStatement(1).executeUpdate();
    }
}
