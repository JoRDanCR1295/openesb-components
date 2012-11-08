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
 * @(#)MinusDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.Minus;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.MinusDb;
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
public class MinusDerby implements MinusDb {
    private static final Messages mMessages = Messages.getMessages(MinusDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public MinusDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }

    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE minusOperate0 (");
        sb.append(" IN outRName VARCHAR(20),");
        sb.append(" IN inRName VARCHAR(20),");
        sb.append(" IN inMVName VARCHAR(20),");
        sb.append(" IN inMRNames VARCHAR(800),");
        sb.append(" IN colNames VARCHAR(800),");
        sb.append(" IN ts0 TIMESTAMP,");
        sb.append(" IN ts1 TIMESTAMP");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.Minus.operate'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure",  "minusOperate0", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE minusOperate0");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "minusOperate0", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, Minus op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];

        String outputTableName = op.getQueueName();
        String viewName = op.getViewName();
        String subtractFromTableName = op.getSubtractFromTableName();
        String[] subtractTableName = op.getSubtractTableName();
        String inRName = subtractFromTableName;
        String inMVName = viewName;
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < subtractTableName.length; i++) {
            if (0 < i) {
                sb.append(DELIM);
            }
            sb.append(subtractTableName[i]);
        }
        String inMRNames = sb.toString();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String[] columnNames = inputSchema.getColumnNames();
        String colNames = mDerbySpecial.getString(columnNames, DELIM);

        // New + in R1, R2
        // {CALL minusOperate0('R', 'R0', 'v_R', 'R1|R2', 'colNames', ?, ?)}
        sb = new StringBuffer();
        sb.append("{CALL minusOperate0(");
        sb.append("'" + outputTableName + "', ");
        sb.append("'" + inRName + "', ");
        sb.append("'" + inMVName + "', ");
        sb.append("'" + inMRNames + "', ");
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
        sb.append("SELECT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("x." + columnNames[i] + ",");
        }
        sb.append("x." + COL_SEQID + ",u." + COL_TIMESTAMP + ",'-' FROM " + outputTableName + " x, " + viewName + " u WHERE ");
        sb.append("u." + COL_TAG + " = '-' AND ");
        sb.append("? < " + "u." + COL_TIMESTAMP + " AND " + "u." + COL_TIMESTAMP + " <= ? AND (");
        sb.append("(SELECT COUNT(*) FROM " + subtractFromTableName + " r WHERE ");
        for (int j = 0; j < columnNames.length; j++) {
            sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
        }
        sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
        sb.append("r." + COL_TAG + " = '-')");
        sb.append(" < "); // Existing: R0: - < +
        sb.append("(SELECT COUNT(*) FROM " + subtractFromTableName + " r WHERE ");
        for (int j = 0; j < columnNames.length; j++) {
            sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
        }
        sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
        sb.append("r." + COL_TAG + " = '+')");
        for (int i = 0; i < subtractTableName.length; i++) {
            sb.append(" AND (SELECT COUNT(*) FROM " + subtractTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
            sb.append(" = "); // Existing: for all MRi: - = +
            sb.append("(SELECT COUNT(*) FROM " + subtractTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '+')");
        }
        sb.append(") AND (");
        sb.append("(SELECT COUNT(*) FROM " + subtractFromTableName + " r WHERE ");
        for (int j = 0; j < columnNames.length; j++) {
            sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
        }
        sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
        sb.append("r." + COL_TAG + " = '+')");
        sb.append(" = "); // for R0: total(+)=total(-)
        sb.append("(SELECT COUNT(*) FROM " + subtractFromTableName + " r WHERE ");
        for (int j = 0; j < columnNames.length; j++) {
            sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
        }
        sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
        sb.append("r." + COL_TAG + " = '-')");
        for (int i = 0; i < subtractTableName.length; i++) {
            sb.append(" OR (SELECT COUNT(*) FROM " + subtractTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " = u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
            sb.append(" < "); // for some Ri: - < +
            sb.append("(SELECT COUNT(*) FROM " + subtractTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " = u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '+')");
        }
        sb.append(") AND x." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " y WHERE y." + COL_SEQID + " = x." + COL_SEQID + " AND y." + COL_TAG + " = '-') AND ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("x." + columnNames[i] + " = u." + columnNames[i] + " AND ");
        }
        sb.append("x." + COL_TIMESTAMP + " = (SELECT MAX(y." + COL_TIMESTAMP + ") FROM " + outputTableName + " y WHERE y." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " z WHERE z." + COL_SEQID + " = y." + COL_SEQID + " AND z." + COL_TAG + " = '-') AND ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("y." + columnNames[i] + " = u." + columnNames[i] + " AND ");
        }
        sb.append("y." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        return ret;
    }
    
}
