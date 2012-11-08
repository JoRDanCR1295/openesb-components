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
 * @(#)TupleSerialCorrelationOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.db2;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleSerialCorrelation;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleSerialCorrelationDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 * 
 * Fortent : Based on TupleSerialCorrelationOracle.java
 */
public class TupleSerialCorrelationDB2 implements TupleSerialCorrelationDb {
    private static final Messages mMessages = Messages.getMessages(TupleSerialCorrelationDB2.class);
    
	private DB2Special mDB2Special;

	public TupleSerialCorrelationDB2(DB2Special dB2Special) {
		mDB2Special = dB2Special;
	}

    public PreparedStatement[] createOperateStatements(Connection con, TupleSerialCorrelation op) throws Exception {
        QueryPlan plan = op.getPlan();
        String seqName = Util.getSequenceName(plan.getId(), op.getId());
        String outputTableName = op.getQueueName();

        Operator inputOp = op.getInputOperatorList().get(0);
        String inputTableName = inputOp.getQueueName();
        
        int size = op.getSize();
        int increment = op.getIncrement();
        List fromColumnList = op.getFromColumnList();
        String[] fromColumnName = (String[])fromColumnList.toArray(new String[0]);
        
        String[] toColumnName = new String[fromColumnName.length*size];
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < fromColumnName.length; j++) {
                toColumnName[i*fromColumnName.length + j] = fromColumnName[j] + "_" + i;
            }
        }

        int k = size - 1;
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i] + ",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ") SELECT ");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i] + ",");
        }
        
//      sb.append(seqName);
//      sb.append(".NextVal,");
//      sb.append(COL_TIMESTAMP + " FROM (SELECT ");
//		Fortent : sequence change
        sb.append("NEXT VALUE FOR " + seqName + "," + COL_TIMESTAMP + " FROM (SELECT ");
        
        for (int i = 0; i <= k; i++) {
            for (int j = 0; j < fromColumnName.length; j++) {
                sb.append("t" + i + "." + fromColumnName[j]);
                sb.append(" as ");
                sb.append(fromColumnName[j] + "_" + i + ",");
            }
        }
        sb.append("t" + k + "." + COL_TIMESTAMP + " FROM ");
        for (int i = 0; i <= k; i++) {
            if (i > 0) {
                sb.append(",");
            }
            sb.append(inputTableName + " t" + i);
        }
        sb.append(" WHERE MOD(t" + k +  "." + COL_SEQID + " - " + size + ", " + increment + ") = 0 AND ");
        sb.append(" ? < t" + k + "." + COL_TIMESTAMP + " AND ");
        sb.append("t" + k + "." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < k; i++) {
            sb.append(" AND t" + i + "." + COL_SEQID + "=t" + k + "." + COL_SEQID + "-" + (k-i));
        }
        sb.append(" ORDER BY t" + k + "." + COL_TIMESTAMP + ")");
        // correlation name
        sb.append( " x" );
        
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{op.getName(), sqlStr});
        }    
        return new PreparedStatement[]{con.prepareStatement(sqlStr)};
    }
    
}
