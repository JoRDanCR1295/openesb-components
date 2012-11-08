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
 * @(#)TupleBasedAggregatorOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.db2;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleBasedAggregatorDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleBasedAggregator;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 * 
 * Fortent : Based on TupleBasedAggregatorOracle.java
 */
public class TupleBasedAggregatorDB2 implements TupleBasedAggregatorDb {
    private static final Messages mMessages = Messages.getMessages(TupleBasedAggregatorDB2.class);
    
	private DB2Special mDB2Special;

	public TupleBasedAggregatorDB2(DB2Special dB2Special) {
		mDB2Special = dB2Special;
	}

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, TupleBasedAggregator op) throws Exception {
        QueryPlan plan = op.getPlan();
        String seqName = Util.getSequenceName(plan.getId(), op.getId());
        String outputTableName = op.getQueueName();
        Operator inputOp = op.getInputOperatorList().get(0);
        String inputName = inputOp.getName();
        String inputTableName = inputOp.getQueueName();
        List<String> fromColumnList = op.getFromColumnList();
        List<Operator> staticInputOperatorList = op.getStaticInputOperatorList();
        List<String> toColumnList = op.getToColumnList();
        List<String> groupByColumnList = op.getGroupByColumnList();
        
        Map<String, String> tableNameMap = new HashMap<String, String>();
        tableNameMap.put(inputName, inputTableName);
        for (int i = 0; i < staticInputOperatorList.size(); i++) {
            Operator o = staticInputOperatorList.get(i);
            String staticInputName = o.getName();
            String staticInputTableName = o.getQueueName();
            tableNameMap.put(staticInputName, staticInputTableName);
        }
        String[] fromColumnName = new String[fromColumnList.size()];
        for (int i = 0; i < fromColumnName.length; i++) {
            String exp = fromColumnList.get(i);
            if (exp.startsWith("'") && exp.endsWith("'")) {
                fromColumnName[i] = exp;
            } else {
                fromColumnName[i] = StringUtil.replacePrefixNotStartWithDot(exp, tableNameMap);
            }
        }
        
        String[] toColumnName = new String[toColumnList.size()];
        for (int i = 0; i < toColumnName.length; i++) {
            toColumnName[i] = toColumnList.get(i);
        }
        
        String[] groupByColumnName = new String[groupByColumnList.size()];
        for (int i = 0; i < groupByColumnName.length; i++) {
            String exp = groupByColumnList.get(i);
            groupByColumnName[i] = StringUtil.replacePrefixNotStartWithDot(exp, tableNameMap);
        }
        
        // Prepare mOperateStmt 0:
        // SELECT MAX(Seqid) FROM S1 WHERE TIMESTAMP <= 10
        StringBuffer sb = new StringBuffer();
        sb.append("SELECT MAX(" + COL_SEQID + ") FROM ");
        sb.append(inputTableName);
        sb.append(" WHERE " + COL_TIMESTAMP + " <= ?");
        String stmt0 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", stmt0});
        }    
        
        // Prepare mOperateStmt 1:
        // SELECT TIMESTAMP FROM S1 WHERE Seqid = ?
        sb = new StringBuffer();
        sb.append("SELECT " + COL_TIMESTAMP + " FROM " + inputTableName);
        sb.append(" WHERE " + COL_SEQID + " = ?");
        String stmt1 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", stmt1});
        }    
        
        // Prepare mOperateStmt 2:
        // INSERT INTO S2 (toColumn1, toColumn2, Seqid, Timestamp)
        //    SELECT toColumn1, toColumn2, S1_Seqid.nextVal, Timestamp FROM (
        //       SELECT fromColumn1 as toColumn1, fromColumn2 as toColumn2, MAX(Timestamp) FROM fromClause(T1, T2, S1)
        //       WHERE prevSeqidToProcess < S1.Seqid AND S1.Seqid <= seqidToProcess AND whereClause(T1, T2, S1)
        //       GROUP BY groupByColumn1, groupByColumn2);
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i]);
            sb.append(",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ") SELECT ");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i]);
            sb.append(",");
        }
        
//      sb.append(seqName);
//      sb.append(".NextVal,");
//      sb.append(COL_TIMESTAMP + " FROM (SELECT ");
//		Fortent : sequence change
        sb.append("NEXT VALUE FOR " + seqName + "," + COL_TIMESTAMP + " FROM (SELECT ");

        for (int i = 0; i < fromColumnName.length; i++) {
            sb.append(fromColumnName[i]);
            sb.append(" as ");
            sb.append(toColumnName[i]);
            sb.append(",");
        }
        sb.append("MAX(" + inputTableName + "." + COL_TIMESTAMP + ") AS " + COL_TIMESTAMP + " FROM ");
        String fromClause = StringUtil.replacePrefixNotStartWithDot(op.getFromClause(), tableNameMap);
        sb.append(fromClause);
        sb.append(" WHERE ? < ");
        sb.append(inputTableName + "." + COL_SEQID + " AND ");
        sb.append(inputTableName + "." + COL_SEQID + " <= ?");
        if (op.getWhereClause() != null && !op.getWhereClause().equals("")) {
            String whereClause = StringUtil.replacePrefixNotStartWithDot(op.getWhereClause(), tableNameMap);
            sb.append(" AND (" + whereClause + ")");
        }
        if (groupByColumnName.length > 0) {
            sb.append(" GROUP BY ");
            for (int i = 0; i < groupByColumnName.length; i++) {
                if (0 < i) {
                    sb.append(",");
                }
                sb.append(groupByColumnName[i]);
            }
        }
        sb.append(")");
        
//		Fortent : correlation name change
        sb.append( " x" );
        
        String stmt2 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "2", stmt2});
        }    
        return new PreparedStatement[]{
            con.prepareStatement(stmt0),
            con.prepareStatement(stmt1),
            con.prepareStatement(stmt2),
        };
    }
    
    public void executeOperateStatements(TupleBasedAggregator op, Timestamp prevT, Timestamp curT) throws Exception {
        ResultSet rs = null;
        try {
            long seqidToProcess = op.getPrevSeqidToProcess();
            op.getOperateStatement(0).setTimestamp(1, curT);
            rs = op.getOperateStatement(0).executeQuery();
            if (!rs.next()) {
                Util.close(rs);
                return;
            }
            int maxSeqid = rs.getInt(1);
            Util.close(rs);
            //+/- 1: getStart() is an inclusive boundary
            seqidToProcess = op.getStart() + ((maxSeqid - op.getStart() + 1)/op.getIncrement())*op.getIncrement() - 1;
            if ((op.getStart() + op.getIncrement() - 1) <= seqidToProcess && op.getPrevSeqidToProcess() < seqidToProcess) {
                for (long i = 1 + (op.getPrevSeqidToProcess() - op.getStart() + 1)/op.getIncrement(), I = (seqidToProcess - op.getStart() + 1)/op.getIncrement(); i <= I; i++) {
                    op.getOperateStatement(1).setLong(1, op.getStart() + i*op.getIncrement() - 1);
                    rs = op.getOperateStatement(1).executeQuery();
                    if (!rs.next()) {
                        continue;
                    }
                    Timestamp maxTs = rs.getTimestamp(1);
                    Util.close(rs);
                    op.getOperateStatement(2).setLong(1, op.getStart() + i*op.getIncrement() - op.getSize() - 1);
                    op.getOperateStatement(2).setLong(2, op.getStart() + i*op.getIncrement() - 1);
                    op.getOperateStatement(2).executeUpdate();
                    op.setPrevSeqidToProcess(seqidToProcess);
                }
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Operator.Name_Id", new Object[]{op.getName(), op.getId()}), e);
        } finally {
            Util.close(rs);
        }
    }
    
}
