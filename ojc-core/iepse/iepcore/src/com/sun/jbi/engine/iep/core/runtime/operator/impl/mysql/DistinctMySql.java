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
 * @(#)DistinctMySql.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.Distinct;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.DistinctDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class DistinctMySql implements DistinctDb {
    private static final Messages mMessages = Messages.getMessages(DistinctMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public DistinctMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, Distinct op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];
        
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String[] columnNames = inputSchema.getColumnNames();
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_TIMESTAMP + "," + COL_TAG + ") ");
        sb.append("SELECT DISTINCT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_TIMESTAMP + ",'+' FROM " + inputTableName + " r WHERE ");
        sb.append("r." + COL_TAG + " = '+' AND ");
        sb.append("? < r." + COL_TIMESTAMP + " AND r." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("(SELECT COUNT(*) FROM " + inputTableName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." +  columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " < r." +  COL_TIMESTAMP + " AND ");
        sb.append("s." + COL_TAG + " = '+')");
        sb.append(" = "); 
        sb.append("(SELECT COUNT(*) FROM " + inputTableName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." +  columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " < r." +  COL_TIMESTAMP + " AND ");
        sb.append("s." + COL_TAG + " = '-') AND ");
        sb.append("(SELECT COUNT(*) FROM " + inputTableName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." +  columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " = r." +  COL_TIMESTAMP + " AND ");
        sb.append("s." + COL_TAG + " = '-')");
        sb.append(" < "); 
        sb.append("(SELECT COUNT(*) FROM " + inputTableName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." +  columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " = r." +  COL_TIMESTAMP + " AND ");
        sb.append("s." + COL_TAG + " = '+')");
        sb.append(" ORDER BY " + COL_TIMESTAMP);
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
        sb.append("u." + COL_SEQID + ", r." + COL_TIMESTAMP + ", '-' FROM " + outputTableName + " u, " + inputTableName + " r WHERE ");
        sb.append("r." + COL_TAG + " = '-' AND ");
        sb.append("? < r." + COL_TIMESTAMP + " AND r." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("(SELECT COUNT(*) FROM " + inputTableName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." +  columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " <= r." +  COL_TIMESTAMP + " AND ");
        sb.append("s." + COL_TAG + " = '+')");
        sb.append(" = "); 
        sb.append("(SELECT COUNT(*) FROM " + inputTableName + " s WHERE ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("s." + columnNames[i] + " = r." +  columnNames[i] + " AND ");
        }
        sb.append("s." + COL_TIMESTAMP + " <= r." +  COL_TIMESTAMP + " AND ");
        sb.append("s." + COL_TAG + " = '-') AND ");
        sb.append("u." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " v WHERE v." + COL_SEQID + " = u." + COL_SEQID + " AND v." + COL_TAG + " = '-')");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(" AND u." + columnNames[i] + " = r." +  columnNames[i]);
        }
        sb.append(" AND u." + COL_TIMESTAMP + " < r." + COL_TIMESTAMP);
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        return ret;
    }
    
}
