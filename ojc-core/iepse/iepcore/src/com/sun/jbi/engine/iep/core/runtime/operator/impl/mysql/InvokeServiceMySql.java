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
 * @(#)InvokeServiceDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeService;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeServiceDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.logging.Level;


/**
 *
 * @author Bing Lu
 */
public class InvokeServiceMySql implements InvokeServiceDb {
    private static final Messages mMessages = Messages.getMessages(InvokeServiceMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public InvokeServiceMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public PreparedStatement[] createOperateStatements(Connection con, InvokeService op) throws Exception {
        Operator inputOp = op.getInputOperatorList().get(0);
        String inputQueueName = inputOp.getQueueName();
        Schema inputSchema = inputOp.getOutputSchema();

        // Prepare mOperateStmt:
        // SELECT Name, Value, SeqId, Timestamp FROM S1 WHERE ? < Timestamp AND Timestamp <= ?
        String[] inputColumnNames = inputSchema.getColumnNames();
        StringBuffer sb = new StringBuffer();
        sb.append("SELECT ");
        for (int i = 0; i < inputColumnNames.length; i++) {
            sb.append(inputColumnNames[i] + ", ");
        }
        sb.append(COL_SEQID + ", " + COL_TIMESTAMP);
        sb.append(" FROM " + inputQueueName + " WHERE ");
        sb.append("? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ?");
        String stmt0 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", stmt0});
        }    
        
        // INSERT INTO S2 (Name, Value, Amount, Timestamp) VALUES (?, ?, ?, ?)
        String outputQueueName = op.getQueueName();
        Schema outputSchemaSchema = op.getOutputSchema();
        String[] outputColumnNames = outputSchemaSchema.getColumnNames();
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputQueueName + "(");
        for (int i = 0; i < outputColumnNames.length; i++) {
            sb.append(outputColumnNames[i] + ", ");
        }
        sb.append(COL_TIMESTAMP + ") VALUES (");
        for (int i = 0; i < outputColumnNames.length; i++) {
            sb.append("?, ");
        }
        sb.append(", ?)");
        String stmt1 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", stmt1});
        }    

        return new PreparedStatement[]{con.prepareStatement(stmt0), con.prepareStatement(stmt1)};
    }
    

}
