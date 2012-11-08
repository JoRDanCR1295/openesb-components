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
 * @(#)UnionAllMySql.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.UnionAllDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.UnionAll;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class UnionAllMySql implements UnionAllDb {
    private static final Messages mMessages = Messages.getMessages(UnionAllMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public UnionAllMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, UnionAll op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];
        
        String outputTableName = op.getQueueName();
        List<Operator> inputOperatorList = op.getInputOperatorList();
        int inputCount = inputOperatorList.size();
        Schema inputSchema = inputOperatorList.get(0).getOutputSchema();
        String[] columnNames = inputSchema.getColumnNames();
        
        String inputTableName[] = new String[inputCount];
        for (int i = 0; i < inputCount; i++) {
            inputTableName[i] = inputOperatorList.get(i).getQueueName();
        }
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_INPUT_ID + "," + COL_INPUT_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG + ") ");
        for (int i = 0; i < inputCount; i++) {
            if (0 < i) {
                sb.append(" UNION ALL ");
            }
            sb.append("SELECT ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append(columnNames[j] + ",");
            }
            sb.append(i + " AS " + COL_INPUT_ID + ",");
            sb.append(inputTableName[i] + "." + COL_SEQID + " AS " + COL_INPUT_SEQID + ",");
            sb.append(inputTableName[i] + "." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + ", '+'");
            sb.append(" FROM " + inputTableName[i] + " WHERE ");
            sb.append(inputTableName[i] + "." + COL_TAG + " = '+' AND ");
            sb.append("? < " + inputTableName[i] + "." + COL_TIMESTAMP + " AND " + inputTableName[i] + "." + COL_TIMESTAMP + " <= ?");
        }
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
        sb.append(COL_INPUT_ID + "," + COL_INPUT_SEQID + "," + COL_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG + ") ");
        sb.append("SELECT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append("u." + columnNames[i] + ",");
        }
        sb.append("u." + COL_INPUT_ID + ",u." + COL_INPUT_SEQID + ",u." + COL_SEQID + ",v." + COL_TIMESTAMP + ",'-' FROM " + outputTableName + " u, (");
        for (int i = 0; i < inputCount; i++) {
            if (0 < i) {
                sb.append(" UNION ALL ");
            }
            sb.append("SELECT " + i + " AS " + COL_INPUT_ID + ",");
            sb.append(inputTableName[i] + "." + COL_SEQID + " AS " + COL_INPUT_SEQID + ",");
            sb.append(inputTableName[i] + "." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP);
            sb.append(" FROM " + inputTableName[i] + " WHERE ");
            sb.append(inputTableName[i] + "." + COL_TAG + " = '-' AND ");
            sb.append("? < " + inputTableName[i] + "." + COL_TIMESTAMP + " AND " + inputTableName[i] + "." + COL_TIMESTAMP + " <= ?");
        }
        sb.append(") v WHERE u." + COL_INPUT_ID + " = v." + COL_INPUT_ID + " AND u." + COL_INPUT_SEQID + " = v." + COL_INPUT_SEQID);
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        return ret;
    }

    public void executeOperateStatements(UnionAll op, Timestamp prevT, Timestamp curT) throws Exception {
        int inputCount = op.getInputOperatorList().size();
        for (int i = 0; i < inputCount; i++) {
            op.getOperateStatement(0).setTimestamp(2*i+1, prevT);
            op.getOperateStatement(0).setTimestamp(2*i+2, curT);
        }
        op.getOperateStatement(0).executeUpdate();
        for (int i = 0; i < inputCount; i++) {
            op.getOperateStatement(1).setTimestamp(2*i+1, prevT);
            op.getOperateStatement(1).setTimestamp(2*i+2, curT);
        }
        op.getOperateStatement(1).executeUpdate();
    }
    
}
