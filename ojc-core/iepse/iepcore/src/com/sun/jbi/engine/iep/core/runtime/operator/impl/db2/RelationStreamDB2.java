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
 * @(#)RelationStreamOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.db2;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationStreamDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 * 
 * Fortent : Based on RelationStreamOracle.java
 */
public class RelationStreamDB2 implements RelationStreamDb {
    private static final Messages mMessages = Messages.getMessages(RelationStreamDB2.class);
    
    private DB2Special mDB2Special;
    
    public RelationStreamDB2(DB2Special dB2Special) {
    	mDB2Special = dB2Special;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, RelationStream op) throws Exception {
        QueryPlan plan = op.getPlan();
        String seqName = Util.getSequenceName(plan.getId(), op.getId());
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append(cm.getColumnName() + ",");
        }
//      sb.append(seqName + ".NextVal," + COL_TIMESTAMP + " FROM " + "(SELECT ");
//		Fortent : sequence change
        sb.append("NEXT VALUE FOR " + seqName + "," + COL_TIMESTAMP + " FROM " + "(SELECT ");

        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t1." + cm.getColumnName() + ",");
        }
        sb.append("t2." + COL_TIMESTAMP + " FROM ");
        sb.append(inputTableName + " t1, ");
        sb.append("(SELECT DISTINCT " + COL_TIMESTAMP + " FROM " + inputTableName);
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ?) t2");
        sb.append(" WHERE t1." + COL_TAG + " = '+' AND ");
        sb.append("t1." + COL_TIMESTAMP + " <= t2." + COL_TIMESTAMP + " AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + inputTableName);
        sb.append(" WHERE " + COL_SEQID + " = t1." + COL_SEQID + " AND ");
        sb.append(COL_TIMESTAMP + " <= t2." + COL_TIMESTAMP + " AND ");
        sb.append(COL_TAG + " = '-')");
        sb.append(" ORDER BY t2." + COL_TIMESTAMP + ")");
//		Fortent : correlation name change
        sb.append( " x" );

        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{op.getName(), sqlStr});
        }    
        return new PreparedStatement[]{con.prepareStatement(sqlStr)};
    }
    
}
