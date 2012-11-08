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
 * @(#)AttributeBasedWindowOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.db2;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AttributeBasedWindow;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AttributeBasedWindowDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 * 
 * Fortent : Based on AttributeBasedWindowOracle.java
 */
public class AttributeBasedWindowDB2 implements AttributeBasedWindowDb {
    private static final Messages mMessages = Messages.getMessages(AttributeBasedWindowDB2.class);
    
    private DB2Special mDB2Special;
    
    public AttributeBasedWindowDB2(DB2Special dB2Special) {
    	mDB2Special = dB2Special;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, AttributeBasedWindow op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];
        
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String attribute = op.getAttribute();
        String attributeType = op.getAttributeType();
        
        /*
         * Fortent (jtaylor) 2008-11-21
         * 
         * For DB2, date, time and timestamp calculations use durations which 
         * must be expressed differently.  The adjustment is now stored in a 
         * String in order to accommodate "labeled durations"
         * 
         * See http://publib.boulder.ibm.com/infocenter/db2luw/v9/index.jsp?topic=/com.ibm.db2.udb.admin.doc/doc/r0023457.htm
         */
        
//      double size = op.getSize();
        String size = new Double( op.getSize() ).toString();
       
        if (attributeType.equals(SQL_TYPE_DATE)) {
            size = size + " days ";
        }
        /*
         * TIME type not available for Oracle
         */
        if (attributeType.equals(SQL_TYPE_TIME)) {
            size = size + " seconds ";
        }
        if (attributeType.equals(SQL_TYPE_TIMESTAMP)) {
            size = size + " seconds ";
        }
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ",'+' FROM ");
        sb.append(inputTableName + " u1");
        sb.append(" WHERE ? < u1." + COL_TIMESTAMP + " AND u1." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + inputTableName + " u2");
        sb.append(" WHERE u1." + COL_TIMESTAMP + " = u2." + COL_TIMESTAMP + " AND ");
        sb.append("u1." + attribute + " <= (u2." + attribute + " - " + size + "))");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append("((SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1.");
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("t1." + COL_SEQID + ",MIN(t2." + COL_TIMESTAMP + "),'-' FROM ");
        sb.append(outputTableName);
        sb.append(" t1, ");
        sb.append(outputTableName);
        sb.append(" t2 ");
        sb.append(" WHERE t1." + COL_TAG + " = '+' AND t2." + COL_TAG + " = '+' AND ");
        sb.append("? < t2." + COL_TIMESTAMP + " AND t2." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("t1." + attribute + " <= (t2." + attribute + " - " + size + ")");
        sb.append(" GROUP BY ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1.");
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("t1." + COL_SEQID + ")");
        
//      sb.append(" MINUS ");
//		Fortent : MINUS replaced by EXCEPT 
        sb.append(" EXCEPT ");
        
        sb.append("(SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1.");
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("t1." + COL_SEQID + ",MIN(t2." + COL_TIMESTAMP + "),'-' FROM ");
        sb.append(outputTableName);
        sb.append(" t1, ");
        sb.append(outputTableName);
        sb.append(" t2 ");
        sb.append(" WHERE t1." + COL_TAG + " = '-' AND t2." + COL_TAG + " = '+' AND ");
        sb.append("? < t2." + COL_TIMESTAMP + " AND t2." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("t1." + attribute + " <= (t2." + attribute + " - " + size + ")");
        sb.append(" GROUP BY ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1.");
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("t1." + COL_SEQID + "))");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        
        return ret;
    }

    public void executeOperateStatements(AttributeBasedWindow op, Timestamp prevT, Timestamp curT) throws Exception {
        op.getOperateStatement(0).setTimestamp(1, prevT);
        op.getOperateStatement(0).setTimestamp(2, curT);
        op.getOperateStatement(0).executeUpdate();

        op.getOperateStatement(1).setTimestamp(1, prevT);
        op.getOperateStatement(1).setTimestamp(2, curT);
        op.getOperateStatement(1).setTimestamp(3, prevT);
        op.getOperateStatement(1).setTimestamp(4, curT);
        op.getOperateStatement(1).executeUpdate();
    }
    
}
