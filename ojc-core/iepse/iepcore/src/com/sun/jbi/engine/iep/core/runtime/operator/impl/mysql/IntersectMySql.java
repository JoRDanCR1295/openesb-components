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
 * @(#)IntersectMySql.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.Intersect;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.IntersectDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class IntersectMySql implements IntersectDb {
    private static final Messages mMessages = Messages.getMessages(IntersectMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public IntersectMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, Intersect op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];
        
        String outputTableName = op.getQueueName();
        List<Operator> inputOperatorList = op.getInputOperatorList();
        int inputCount = inputOperatorList.size();
        String inputTableName[] = new String[inputCount];
        for (int i = 0; i < inputCount; i++) {
            inputTableName[i] = inputOperatorList.get(i).getQueueName();
        }
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();

        String[] columnNames = inputSchema.getColumnNames();
        String viewName = op.getViewName();
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" (");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_TIMESTAMP + "," + COL_TAG + ") SELECT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_TIMESTAMP + ", '+' FROM " + viewName + " u WHERE ");
        sb.append("u." + COL_TAG + " = '+' AND ");
        sb.append("? < " + "u." + COL_TIMESTAMP + " AND " + "u." + COL_TIMESTAMP + " <= ? AND (");
        for (int i = 0; i < inputCount; i++) {
            if (0 < i) {
                sb.append(" OR ");
            }
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '+')");
            sb.append(" = ");  // Existing: for some Ri + = -
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
        }
        sb.append(") AND (");
        for (int i = 0; i < inputCount; i++) {
            if (0 < i) {
                sb.append(" AND ");
            }
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
            sb.append(" < ");  // New: for all Ri: total(-) < total(+)
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '+')");
        }
        sb.append(") ORDER BY " + COL_TIMESTAMP);
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" (");
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
        for (int i = 0; i < inputCount; i++) {
            if (0 < i) {
                sb.append(" AND ");
            }
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
            sb.append(" < ");  // Existing: for all Ri: - < +
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '+')");
        }
        sb.append(") AND (");
        for (int i = 0; i < inputCount; i++) {
            if (0 < i) {
                sb.append(" OR ");
            }
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '+')");
            sb.append(" = ");  // for some Ri: total(+)=total(-)
            sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
            }
            sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("r." + COL_TAG + " = '-')");
        }
        sb.append(") AND ");
        sb.append("x." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " y WHERE y." + COL_SEQID + " = x." + COL_SEQID + " AND y." + COL_TAG + " = '-')");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(" AND x." + columnNames[i] + " = u." +  columnNames[i]);
        }
        sb.append(" AND x." + COL_TIMESTAMP + " = (SELECT MAX(y." + COL_TIMESTAMP + ") FROM " + outputTableName + " y WHERE ");
        sb.append("y." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " z WHERE z." + COL_SEQID + " = y." + COL_SEQID + " AND z." + COL_TAG + " = '-')");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(" AND y." + columnNames[i] + " = u." + columnNames[i]);
        }
        sb.append(" AND y." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        return ret;
    }
     
}
