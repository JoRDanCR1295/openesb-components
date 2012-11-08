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
 * @(#)MergedStreamInputMySql.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.Merge;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.MergeDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class MergeMySql implements MergeDb {
    private static final Messages mMessages = Messages.getMessages(MergeMySql.class);

    private MySqlSpecial mMySqlSpecial;

    public MergeMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, Merge op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];

        String outputTableName = op.getQueueName();
        List<Operator> inputOperatorList = op.getInputOperatorList();
        int inputCount = inputOperatorList.size();
        Schema outputSchema = op.getOutputSchema();
        String[] columnNames = outputSchema.getColumnNames();

        String[] inputTableName = new String[inputCount];
        for (int i = 0; i < inputCount; i++) {
            inputTableName[i] = inputOperatorList.get(i).getQueueName();
        }

        // INSERT INTO S (column1, column2, Timestamp)
        //     SELECT column1, column1, Timestamp FROM
        //     (
        //            SELECT column1, column2, Timestamp
        //            FROM S1
        //            WHERE  7 < S1.Timestamp AND S1.Timestamp <= 10
        //
        //            UNION ALL
        //
        //            SELECT column1, column2, Timestamp
        //            FROM S2
        //            WHERE 7 < S2.Timestamp AND S2.Timestamp <= 10
        //
        //     ) u ORDER BY Timestamp
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + "(");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_TIMESTAMP + ") SELECT ");
        for (int i = 0; i < columnNames.length; i++) {
            sb.append(columnNames[i] + ",");
        }
        sb.append(COL_TIMESTAMP + " FROM (");
        for (int i = 0; i < inputCount; i++) {
            if (i > 0) {
                sb.append(" UNION ALL ");
            }
            sb.append("SELECT ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append(columnNames[j] + ",");
            }
            sb.append(COL_TIMESTAMP + " FROM " + inputTableName[i]);
            sb.append(" WHERE ? < " + inputTableName[i] + "." + COL_TIMESTAMP + " AND ");
            sb.append(inputTableName[i] + "." + COL_TIMESTAMP + " <= ?");
        }
        sb.append(") u ORDER BY " + COL_TIMESTAMP);
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{op.getName(), "0", sqlStr});
        }
        return new PreparedStatement[]{con.prepareStatement(sqlStr)};
    }

    public void executeOperateStatements(Merge op, Timestamp prevT, Timestamp curT) throws Exception {
        int inputCount = op.getInputOperatorList().size();
        for (int i = 0; i < inputCount; i++) {
            op.getOperateStatement(0).setTimestamp(2*i+1, prevT);
            op.getOperateStatement(0).setTimestamp(2*i+2, curT);
        }
        op.getOperateStatement(0).executeUpdate();
    }

}
