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
 * @(#)UnionDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.Union;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.UnionDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class UnionDerby implements UnionDb {
    private static final Messages mMessages = Messages.getMessages(UnionDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public UnionDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }

    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE unionOperate0 (");
        sb.append(" IN outRName VARCHAR(20),");
        sb.append(" IN inRName VARCHAR(800),");
        sb.append(" IN colNames VARCHAR(800),");
        sb.append(" IN ts0 TIMESTAMP,");
        sb.append(" IN ts1 TIMESTAMP");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.Union.operate'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure",  "unionOperate0", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE unionOperate0");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "unionOperate0", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, Union op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];

        String outputTableName = op.getQueueName();
        String viewName = op.getViewName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String[] columnNames = inputSchema.getColumnNames();

        // {CALL unionOperate0('S1', 'R1', 'colNames', ?, ?)}
        String colNames = mDerbySpecial.getString(columnNames, DELIM);
        StringBuffer sb = new StringBuffer();
        sb.append("{CALL unionOperate0(");
        sb.append("'" + outputTableName + "', ");
        sb.append("'" + viewName + "', ");
        sb.append("'" + colNames + "', ");
        sb.append("?, ?)}");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);

        sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG + ") ");
        sb.append("SELECT DISTINCT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("u." + columnNames[i] + ",");
        }
        sb.append("u." + COL_SEQID + ",");
        sb.append("r." + COL_TIMESTAMP + ",'-' FROM " + outputTableName + " u, " + viewName + " r WHERE ");
        sb.append("r." + COL_TAG + " = '-' AND ");
        sb.append("? < r." + COL_TIMESTAMP + " AND r." + COL_TIMESTAMP + " <= ? AND ");

        sb.append("(SELECT COUNT(*) FROM " + viewName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." + columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " < r." + COL_TIMESTAMP + " AND s." + COL_TAG + " = '-') ");
        sb.append(" < "); // Existing: - < +
        sb.append("(SELECT COUNT(*) FROM " + viewName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." + columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " < r." + COL_TIMESTAMP + " AND s." + COL_TAG + " = '+') AND ");

        sb.append("(SELECT COUNT(*) FROM " + viewName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." + columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " <= r." + COL_TIMESTAMP + " AND s." + COL_TAG + " = '-') ");
        sb.append(" = "); // total(+)=total(-)
        sb.append("(SELECT COUNT(*) FROM " + viewName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." + columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " <= r." + COL_TIMESTAMP + " AND s." + COL_TAG + " = '+') AND ");

        sb.append("u." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " v WHERE v." + COL_SEQID + " = u." + COL_SEQID + " AND v." + COL_TAG + " = '-')");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(" AND u." + columnNames[i] + " = r." + columnNames[i]);
        }
        sb.append(" AND u." + COL_TIMESTAMP + " = (SELECT MAX(v." + COL_TIMESTAMP + ") FROM " + outputTableName + " v WHERE ");
        sb.append("v." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " w WHERE w." + COL_SEQID + " = v." + COL_SEQID + " AND w." + COL_TAG + " = '-')");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(" AND v." + columnNames[i] + " = r." + columnNames[i]);
        }
        sb.append(" AND v." + COL_TIMESTAMP + " < r." + COL_TIMESTAMP + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        return ret;
    }

}
