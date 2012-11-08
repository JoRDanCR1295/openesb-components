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
 * @(#)TupleBasedAggregatorDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleBasedAggregator;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleBasedAggregatorDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class TupleBasedAggregatorDerby implements TupleBasedAggregatorDb {
    private static final Messages mMessages = Messages.getMessages(TupleBasedAggregatorDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public TupleBasedAggregatorDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }
 
    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE tupleBasedAggregatorOperate1 (");
        sb.append(" IN inputTableName VARCHAR(20),");
        sb.append(" IN outputTableName VARCHAR(20),");
        sb.append(" IN fromColumnNames VARCHAR(800),");
        sb.append(" IN toColumnNames VARCHAR(800),");
        sb.append(" IN groupByColumnNames VARCHAR(800),");
        sb.append(" IN fromClause VARCHAR(200),");
        sb.append(" IN whereClause VARCHAR(800),");
        sb.append(" IN start BIGINT,");
        sb.append(" IN increment BIGINT,");
        sb.append(" IN size BIGINT,");
        sb.append(" IN prevStp BIGINT,");
        sb.append(" IN stp BIGINT");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.TupleBasedAggregator.operate'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "tupleBasedAggregatorOperate1", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE tupleBasedAggregatorOperate1");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "tupleBasedAggregatorOperate1", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, TupleBasedAggregator op) throws Exception {
        QueryPlan plan = op.getPlan();
        String outputTableName = op.getQueueName();
        Operator inputOp = op.getInputOperatorList().get(0);
        String inputName = inputOp.getName();
        String inputTableName = inputOp.getQueueName();
        List<String> fromColumnList = op.getFromColumnList();
        List<Operator> staticInputOperatorList = op.getStaticInputOperatorList();
        List<String> toColumnList = op.getToColumnList();
        List<String> groupByColumnList = op.getGroupByColumnList();
        long size = op.getSize();
        long increment = op.getIncrement();
        long start = op.getStart();


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
                int len = exp.length();
                fromColumnName[i] = SINGLE_QUOTE_SUB + exp.substring(1, len - 1) + SINGLE_QUOTE_SUB;
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
            groupByColumnName[i] = StringUtil.replaceWord(exp, tableNameMap);
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
        // {CALL tupleBasedAggregatorOperate1('S1',
        //                                    'S2',
        //                                    '$fromColumns',
        //                                    '$toColumnNames',
        //                                    '$groupByColumnNames',
        //                                    '$fromClause',
        //                                    '?',
        //                                    start,
        //                                    size,
        //                                    ?, ?)}
        String fromColumnNames = mDerbySpecial.getString(fromColumnName, DELIM);
        String toColumnNames = mDerbySpecial.getString(toColumnName, DELIM);
        String groupByColumnNames = mDerbySpecial.getString(groupByColumnName, DELIM);
        String fromClause = StringUtil.replacePrefixNotStartWithDot(op.getFromClause(), tableNameMap);
        sb = new StringBuffer();
        sb.append("{CALL tupleBasedAggregatorOperate1(");
        sb.append("'" + inputTableName + "', ");
        sb.append("'" + outputTableName + "', ");
        sb.append("'" + fromColumnNames + "', ");
        sb.append("'" + toColumnNames + "', ");
        sb.append("'" + groupByColumnNames + "', ");
        sb.append("'" + fromClause + "', ");
        sb.append("?, ");
        sb.append(start + ", ");
        sb.append(increment + ", ");
        sb.append(size + ", ");
        sb.append("?, ?)}");
        String stmt1 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", stmt1});
        }    
        return new PreparedStatement[]{con.prepareStatement(stmt0), con.prepareCall(stmt1)};
    }

    public void executeOperateStatements(TupleBasedAggregator op, Timestamp prevT, Timestamp curT) throws Exception {
        ResultSet rs = null;
        try {
            long seqidToProcess = op.getPrevSeqidToProcess();
            op.getOperateStatement(0).setTimestamp(1, curT);
            rs = op.getOperateStatement(0).executeQuery();
            if (rs.next()) {
                long maxSeqid = rs.getLong(1);
                //+/- 1: getStart() is an inclusive boundary
                seqidToProcess = op.getStart() + ((maxSeqid - op.getStart() + 1L) / op.getIncrement()) * op.getIncrement() - 1L;
                if ((op.getStart() + op.getIncrement() - 1) <= seqidToProcess && op.getPrevSeqidToProcess() < seqidToProcess) {
                    op.getOperateStatement(1).setString(1, op.getRWhereClause());
                    op.getOperateStatement(1).setLong(2,op.getPrevSeqidToProcess());
                    op.getOperateStatement(1).setLong(3, seqidToProcess);
                    op.getOperateStatement(1).executeUpdate();
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
