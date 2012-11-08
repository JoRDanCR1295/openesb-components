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
 * @(#)AttributeBasedWindowDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AttributeBasedWindow;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AttributeBasedWindowDb;
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
public class AttributeBasedWindowDerby implements AttributeBasedWindowDb {
    private static final Messages mMessages = Messages.getMessages(AttributeBasedWindowDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public AttributeBasedWindowDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }

    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE attributeBasedWindowOperate1 (");
        sb.append(" IN sName VARCHAR(20),");
        sb.append(" IN rName VARCHAR(20),");
        sb.append(" IN colNames VARCHAR(800),");
        sb.append(" IN attribute VARCHAR(40),");
        sb.append(" IN attributeType VARCHAR(40),");
        sb.append(" IN size FLOAT,");
        sb.append(" IN ts0 TIMESTAMP,");
        sb.append(" IN ts1 TIMESTAMP");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.AttributeBasedWindow.operate'");
        String sqlStr = sb.toString();

        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure", " attributeBasedWindowOperate1", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE attributeBasedWindowOperate1");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "attributeBasedWindowOperate1", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, AttributeBasedWindow op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[1];

        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String attribute = op.getAttribute();
        String attributeType = op.getAttributeType();
        double size = op.getSize();

        String[] columnName = inputSchema.getColumnNames();
        String colNames = mDerbySpecial.getString(columnName, DELIM);
        // {CALL attributeBasedWindowOperate1('R1', 'Name|Value', 'Value', mSize, ?, ?, ?, ?)}
        StringBuffer sb = new StringBuffer();
        sb.append("{CALL attributeBasedWindowOperate1(");
        sb.append("'" + inputTableName + "', ");
        sb.append("'" + outputTableName + "', ");
        sb.append("'" + colNames + "', ");
        sb.append("'" + attribute + "', ");
        sb.append("'" + attributeType + "', ");
        sb.append(size + ", ");
        sb.append("?, ?)}");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[0] = con.prepareCall(sqlStr);
        return ret;
    }

    public void executeOperateStatements(AttributeBasedWindow op, Timestamp prevT, Timestamp curT) throws Exception {
        op.getOperateStatement(0).setTimestamp(1, prevT);
        op.getOperateStatement(0).setTimestamp(2, curT);
        op.getOperateStatement(0).executeUpdate();
    }
}
