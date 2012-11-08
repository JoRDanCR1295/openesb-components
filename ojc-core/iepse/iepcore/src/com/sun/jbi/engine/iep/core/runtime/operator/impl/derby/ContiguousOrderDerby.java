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
 * @(#)ContiguousOrderDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ContiguousOrder;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ContiguousOrderDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class ContiguousOrderDerby implements ContiguousOrderDb {
    private static final Messages mMessages = Messages.getMessages(ContiguousOrderDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public ContiguousOrderDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }

    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE contiguousOrderOperate1 (");
        sb.append(" IN inputTableName VARCHAR(20),");
        sb.append(" IN tableName VARCHAR(20),");
        sb.append(" IN outputTableName VARCHAR(20),");
        sb.append(" IN columns VARCHAR(800),");
        sb.append(" IN attribute VARCHAR(40),");
        sb.append(" IN attributes VARCHAR(800),");
        sb.append(" IN ts1 TIMESTAMP,");
        sb.append(" IN ts00 TIMESTAMP,");
        sb.append(" IN ts01 TIMESTAMP");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.ContiguousOrder.operate'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure",  "contiguousOrderOperate1", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE contiguousOrderOperate1");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "contiguousOrderOperate1", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, ContiguousOrder op) throws Exception {
        // Prepare mOperateStmt:
        PreparedStatement[] ret = new PreparedStatement[7];

        String tableName = op.getTableName();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        List<String> attributeList = op.getAttributeList();
        String attribute = op.getAttribute();
        long start = op.getStart();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        int attrCnt = attributeList.size();
        String[] attrName = new String[attrCnt];
        for (int i = 0; i < attrCnt; i++) {
            attrName[i] = attributeList.get(i);
        }
        String[] columnName = inputSchema.getColumnNames();

        // UPDATE T WITH '-'
        StringBuffer sb = new StringBuffer();
        sb.append("UPDATE " + tableName + " SET " + COL_UPDATE + " = '-'");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareCall(sqlStr);

        sb = new StringBuffer();
        sb.append("UPDATE " + tableName + " t SET " + COL_UPDATE + " = '+' WHERE ");
        sb.append("(t." + COL_MISS_SEQ + " - t." + COL_MAX_SEQ + " = 1");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
        sb.append("? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND t." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND u." + attribute + " >= t." + COL_MISS_SEQ + "))");
        sb.append(" OR ");
        sb.append("(t." + COL_MISS_SEQ + " - t." + COL_MAX_SEQ + " <> 1");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
        sb.append("? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND t." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND u." + attribute + " = t." + COL_MISS_SEQ + "))");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareCall(sqlStr);

        // INSERT INTO T
        sb = new StringBuffer();
        sb.append("INSERT INTO " + tableName + " (");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(attrName[i] + ", ");
        }
        sb.append(COL_MISS_SEQ + ", " + COL_MAX_SEQ + ")(");
        sb.append(" SELECT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("u." + attrName[i] + " AS " + attrName[i] + ", ");
        }
        sb.append("MIN(u." + attribute + ") + 1 AS " + COL_MISS_SEQ + ", ");
        sb.append((start - 1) + " AS " + COL_MAX_SEQ + " FROM ");
        sb.append(inputTableName + " u");
        sb.append(" WHERE u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND (SELECT t." + COL_UPDATE + " FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(") = '+'");
        sb.append(" AND u." + attribute + " >= (SELECT t." + COL_MISS_SEQ + " FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(") - 1");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append("w." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = u." + attribute + " + 1)");
        if (attrName.length > 0) {
            sb.append(" GROUP BY ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(", ");
                }
                sb.append("u." + attrName[i]);
            }
        }
        sb.append(" UNION ");

        sb.append(" SELECT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("u." + attrName[i] + " AS " + attrName[i] + ", ");
        }
        sb.append("MIN(u." + attribute + ") + 1 AS " + COL_MISS_SEQ + ", ");
        sb.append((start - 1) + " AS " + COL_MAX_SEQ + " FROM ");
        sb.append(inputTableName + " u");
        sb.append(" WHERE u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(")");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append("w." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = " + start + ")");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append("w." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = u." + attribute + " + 1)");
        if (attrName.length > 0) {
            sb.append(" GROUP BY ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(", ");
                }
                sb.append("u." + attrName[i]);
            }
        }
        sb.append(" UNION ");

        sb.append(" SELECT DISTINCT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("u." + attrName[i] + " AS " + attrName[i] + ", ");
        }
        sb.append(start + " AS " + COL_MISS_SEQ + ", ");
        sb.append((start - 1) + " AS " + COL_MAX_SEQ + " FROM ");
        sb.append(inputTableName + " u");
        sb.append(" WHERE u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(")");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append(" w." + COL_TIMESTAMP + " <= ? ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = " + start + ")");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append(" w." + COL_TIMESTAMP + " <= ? ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " > " + start + ")");
        sb.append(")"); // end of INSERT INTO
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "2", sqlStr});
        }    
        ret[2] = con.prepareStatement(sqlStr);

        // UPDATE T
        sb = new StringBuffer();
        sb.append("UPDATE " + tableName + " t SET ");
        sb.append(COL_MAX_SEQ + " = (SELECT MAX(u." + attribute + ")FROM " + inputTableName + " u WHERE ");
        sb.append("u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND u." + attrName[i] + " = t." + attrName[i]);
        }
        sb.append(")," + COL_TIMESTAMP + " = (SELECT MAX(u." + COL_TIMESTAMP + ") FROM " + inputTableName + " u WHERE ");
        sb.append("u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND u." + attrName[i] + " = t." + attrName[i]);
        }
        sb.append("), " + COL_PROCESSING_TIME + " = ? WHERE t." + COL_MAX_SEQ + " = " + (start - 1));
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "3", sqlStr});
        }    
        ret[3] = con.prepareStatement(sqlStr);

        // {CALL contiguousOrderOperate1('S1', 'T', 'S', 'Symbol|Price|Seq', 'Seq', 'Symbol', ?, ?)}
        String columns = mDerbySpecial.getString(columnName, DELIM);
        String attributes = mDerbySpecial.getString(attrName, DELIM);
        sb = new StringBuffer();
        sb.append("{CALL contiguousOrderOperate1(");
        sb.append("'" + inputTableName + "', ");
        sb.append("'" + tableName + "', ");
        sb.append("'" + outputTableName + "', ");
        sb.append("'" + columns + "', ");
        sb.append("'" + attribute + "', ");
        sb.append("'" + attributes + "', ");
        sb.append("?, ?, ?)}");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "4", sqlStr});
        }    
        ret[4] = con.prepareCall(sqlStr);

        // DELETE FROM T
        sb = new StringBuffer();
        sb.append("DELETE FROM ");
        sb.append(tableName);
        sb.append(" t WHERE EXISTS (SELECT 'x' FROM ");
        sb.append(tableName);
        sb.append(" s WHERE ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("s." + attrName[i] + " = t." + attrName[i] + " AND ");
        }
        sb.append("t." + COL_PROCESSING_TIME + " < s." + COL_PROCESSING_TIME + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "5", sqlStr});
        }    
        ret[5] = con.prepareStatement(sqlStr);

        // Min Usage Timetamp
        sb = new StringBuffer();
        sb.append("SELECT MIN(u." + COL_TIMESTAMP + ") FROM " + inputTableName + " u WHERE");
        sb.append(" u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND EXISTS (SELECT 'x' FROM ");
        sb.append(tableName + " t WHERE ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("t." + attrName[i] + " = u." + attrName[i] + " AND ");
        }
        sb.append("t." + COL_MISS_SEQ + " - 1 <= u." + attribute + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "6", sqlStr});
        }    
        ret[6] = con.prepareStatement(sqlStr);
        return ret;
    }

}
