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
 * @(#)NotificationStreamDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.NotificationStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.NotificationStreamDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class NotificationStreamDerby implements NotificationStreamDb {
    private static final Messages mMessages = Messages.getMessages(NotificationStreamDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public NotificationStreamDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }
 
    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE notificationStreamOperate1 (");
        sb.append(" IN inputTableName VARCHAR(20),");
        sb.append(" IN tableName VARCHAR(20),");
        sb.append(" IN outputTableName VARCHAR(20),");
        sb.append(" IN columns VARCHAR(800),");
        sb.append(" IN size BIGINT,");
        sb.append(" IN ts0 TIMESTAMP,");
        sb.append(" IN ts1 TIMESTAMP");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.NotificationStream.operate'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure",  "notificationStreamOperate1", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE notificationStreamOperate1");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "notificationStreamOperate1", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, NotificationStream op) throws Exception {
        // Prepare mOperateStmt:
        PreparedStatement[] ret = new PreparedStatement[1];

        String tableName = op.getTableName();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        long size = op.getSize();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String[] columnName = inputSchema.getColumnNames();

        // {CALL notificationStreamOperate1('R', 'T', 'S', 'Symbol|Price', ?, ?)}
        String columns = mDerbySpecial.getString(columnName, DELIM);
        StringBuffer sb = new StringBuffer();
        sb.append("{CALL notificationStreamOperate1(");
        sb.append("'" + inputTableName + "', ");
        sb.append("'" + tableName + "', ");
        sb.append("'" + outputTableName + "', ");
        sb.append("'" + columns + "', ");
        sb.append(size + ", ");
        sb.append("?, ?)}");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareCall(sqlStr);

        return ret;
    }
  
    public void executeOperateStatements(NotificationStream op, Timestamp prevT, Timestamp curT) throws Exception {
        op.getOperateStatement(0).setTimestamp(1, prevT);
        op.getOperateStatement(0).setTimestamp(2, curT);
        op.getOperateStatement(0).executeUpdate();
    }
    
}
