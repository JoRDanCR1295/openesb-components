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
 * @(#)UnionAllDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.UnionAll;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.UnionAllDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class UnionAllDerby implements UnionAllDb {
    private static final Messages mMessages = Messages.getMessages(UnionAllDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public UnionAllDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }
 
    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE unionAllOperate0 (");
        sb.append(" IN outRName VARCHAR(20),");
        sb.append(" IN inRNames VARCHAR(800),");
        sb.append(" IN colNames VARCHAR(800),");
        sb.append(" IN ts0 TIMESTAMP,");
        sb.append(" IN ts1 TIMESTAMP");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.UnionAll.operate'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure", "unionAllOperate0", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE unionAllOperate0");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "unionAllOperate0", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, UnionAll op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];

        String outputTableName = op.getQueueName();
        List<Operator> inputOperatorList = op.getInputOperatorList();
        int inputCount = inputOperatorList.size();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String[] columnName = inputSchema.getColumnNames();

        String[] inputTableName = new String[inputCount];
        for (int i = 0; i < inputCount; i++) {
            inputTableName[i] = inputOperatorList.get(i).getQueueName();
        }

        // new + in R1, R2, ..
        // {CALL unionAllOperate0('R', 'R1,R2', 'colNames', ?, ?)}
        String outRName = outputTableName;
        String inRNames = mDerbySpecial.getString(inputTableName, DELIM);
        String colNames = mDerbySpecial.getString(columnName, DELIM);
        StringBuffer sb = new StringBuffer();
        sb.append("{CALL unionAllOperate0(");
        sb.append("'" + outRName + "', ");
        sb.append("'" + inRNames + "', ");
        sb.append("'" + colNames + "', ");
        sb.append("?, ?)}");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);

        // new - in R1, R2, ..
        sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < columnName.length; i++) {
            sb.append(columnName[i] + ",");
        }
        sb.append(COL_INPUT_ID + "," + COL_INPUT_SEQID + "," + COL_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG + ") ");
        sb.append("SELECT ");
        for (int i = 0; i < columnName.length; i++) {
            sb.append("u." + columnName[i] + ",");
        }
        sb.append("u." + COL_INPUT_ID + ",u." + COL_INPUT_SEQID + ",u." + COL_SEQID + ",v." + COL_TIMESTAMP + ",'-' FROM " + outputTableName + " u, (");
        for (int i = 0; i < inputCount; i++) {
            if (0 < i) {
                sb.append(" UNION ALL ");
            }
            sb.append("SELECT ");
            sb.append(i + " AS " + COL_INPUT_ID + ",");
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
        op.getOperateStatement(0).setTimestamp(1, prevT);
        op.getOperateStatement(0).setTimestamp(2, curT);
        op.getOperateStatement(0).executeUpdate();
        for (int i = 0; i < inputCount; i++) {
            op.getOperateStatement(1).setTimestamp(2 * i + 1, prevT);
            op.getOperateStatement(1).setTimestamp(2 * i + 2, curT);
        }
        op.getOperateStatement(1).executeUpdate();
    }

}
