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
 * @(#)MinusOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.oracle;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.Minus;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.MinusDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class MinusOracle implements MinusDb {
    private static final Messages mMessages = Messages.getMessages(MinusOracle.class);
    
    private OracleSpecial mOracleSpecial;
    
    public MinusOracle(OracleSpecial oracleSpecial) {
        mOracleSpecial = oracleSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, Minus op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];
        
        QueryPlan plan = op.getPlan();
        String seqName = Util.getSequenceName(plan.getId(), op.getId());
        String outputTableName = op.getQueueName();
        String subtractFromTableName = op.getSubtractFromTableName();
        String[] subtractTableName = op.getSubtractTableName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String[] columnNames = inputSchema.getColumnNames();
        String viewName = op.getViewName();
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG + ") ");
        sb.append("SELECT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(seqName + ".NextVal," + COL_TIMESTAMP + ",'+' FROM (SELECT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_TIMESTAMP + " FROM " + viewName + " u WHERE ");
        sb.append("u." + COL_TAG + " = '+' AND ");
        sb.append("? < " + "u." + COL_TIMESTAMP + " AND " + "u." + COL_TIMESTAMP + " <= ? AND (");
        sb.append("(SELECT COUNT(*) FROM " + subtractFromTableName + " r WHERE ");
        for (int j = 0; j < columnNames.length; j++) {
            sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
        }
        sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
        sb.append("r." + COL_TAG + " = '+')");
        sb.append(" = ");  // Existing: R0 + = -
        sb.append("(SELECT COUNT(*) FROM " + subtractFromTableName + " r WHERE ");
        for (int j = 0; j < columnNames.length; j++) {
            sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
        }
        sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
        sb.append("r." + COL_TAG + " = '-')");
        for (int i = 0; i < subtractTableName.length; i++) {
            sb.append("OR (SELECT COUNT(*) FROM " + subtractTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
            sb.append(" < ");  // Existing: for some Ri - < +
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
        sb.append("r." + COL_TAG + " = '-')");
        sb.append(" < ");  // New: R0: total(-) < total(+)
        sb.append("(SELECT COUNT(*) FROM " + subtractFromTableName + " r WHERE ");
        for (int j = 0; j < columnNames.length; j++) {
            sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
        }
        sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
        sb.append("r." + COL_TAG + " = '+')");
        for (int i = 0; i < subtractTableName.length; i++) {
            sb.append(" AND (SELECT COUNT(*) FROM " + subtractTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
            sb.append(" = ");  // New: for all Ri: total(-) = total(+)
            sb.append("(SELECT COUNT(*) FROM " + subtractTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '+')");
        }
        sb.append(") ORDER BY " + COL_TIMESTAMP + ")");
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
        sb.append(" < ");  // Existing: R0: - < +
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
            sb.append(" = ");  // Existing: for all MRi: - = +
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
        sb.append(" = ");  // for R0: total(+)=total(-)
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
            sb.append(" < ");  // for some Ri: - < +
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
        sb.append("y." + COL_TIMESTAMP + " < u."  + COL_TIMESTAMP + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        return ret;
    }
    
}
