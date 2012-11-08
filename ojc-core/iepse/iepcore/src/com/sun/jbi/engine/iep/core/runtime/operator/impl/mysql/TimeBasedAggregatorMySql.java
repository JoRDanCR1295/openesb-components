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
 * @(#)TimeBasedAggregatorMySql.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TimeBasedAggregatorDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TimeBasedAggregator;
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
 */
public class TimeBasedAggregatorMySql implements TimeBasedAggregatorDb {
    private static final Messages mMessages = Messages.getMessages(TimeBasedAggregatorMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public TimeBasedAggregatorMySql(MySqlSpecial mysqlSpecial) {
        mMySqlSpecial = mysqlSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, TimeBasedAggregator op) throws Exception {
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
        // SELECT MIN(Timestamp) FROM S1 WHERE Timsttamp < 10
        StringBuffer sb = new StringBuffer();
        sb.append("SELECT MIN(" + COL_TIMESTAMP + ") FROM ");
        sb.append(inputTableName);
        sb.append(" WHERE " + COL_TIMESTAMP + " < ?");
        String stmt0 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", stmt0});
        }    
        
        // Prepare mOperateStmt 1:
        // INSERT INTO S2 (toColumn1, toColumn2, Seqid, Timestamp)\
        //    SELECT toColumn1, toColumn2, S1_SeqId.nextVal, Timestamp FROM
        //    (SELECT fromColumn1 as toColumn1,
        //            fromColumn2 as toColumn2,
        //            ? as Timestamp
        //     FROM fromClause(T1, T2, S1)
        //     WHERE ? < S1.Timestamp AND S1.Timestamp <= ? AND whereClause(T1, T2, S1)
        //     GROUP BY groupByColumn1, groupByColumn2
        //     ORDER BY Timestamp);
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i]);
            sb.append(", ");
        }
        sb.append(COL_TIMESTAMP + ") ");
        sb.append("SELECT ");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i]);
            sb.append(", ");
        }
        sb.append(COL_TIMESTAMP + " FROM ");
        sb.append("(SELECT ");
        for (int i = 0; i < fromColumnName.length; i++) {
            sb.append(fromColumnName[i]);
            sb.append(" AS ");
            sb.append(toColumnName[i]);
            sb.append(", ");
        }
        sb.append("? AS " + COL_TIMESTAMP + " FROM ");
        String fromClause = StringUtil.replacePrefixNotStartWithDot(op.getFromClause(), tableNameMap);
        sb.append(fromClause);
        sb.append(" WHERE ? < " + inputTableName + "." + COL_TIMESTAMP + " AND ");
        sb.append(inputTableName + "." + COL_TIMESTAMP + " <= ?");
        if (op.getWhereClause() != null && !op.getWhereClause().equals("")) {
            String whereClause = StringUtil.replacePrefixNotStartWithDot(op.getWhereClause(), tableNameMap);
            sb.append(" AND (" + whereClause + ")");
        }
        if (groupByColumnName.length > 0) {
            sb.append(" GROUP BY ");
            for (int i = 0; i < groupByColumnName.length; i++) {
                sb.append(groupByColumnName[i]);
                if (i < (groupByColumnName.length - 1)) {
                    sb.append(",");
                }
            }
        }
        sb.append(") AS t1");
        String stmt1 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", stmt1});
        }    
        return new PreparedStatement[] {
            con.prepareStatement(stmt0),
            con.prepareStatement(stmt1),
        };
    }

    public void executeOperateStatements(TimeBasedAggregator op, Timestamp prevT, Timestamp curT) throws Exception {
        ResultSet rs = null;
        try {
            op.getOperateStatement(0).setTimestamp(1, curT);
            rs = op.getOperateStatement(0).executeQuery();
            Timestamp curTimeMarker = op.getPrevTimeMarker();
            Timestamp minTv;
            if ((rs.next()) && ((minTv = rs.getTimestamp(1)) != null)) {
                long minLv = minTv.getTime(); // Lv: Long Value; Tv: Timestamp Value
                // performance improvement
                minLv = op.getStart() + ((minLv - op.getStart()) / op.getIncrement()) * op.getIncrement();
                if (op.getPrevTimeMarker().getTime() < minLv) {
                    op.setPrevTimeMarker(new Timestamp(minLv));
                    curTimeMarker = op.getPrevTimeMarker();
                }

                long curLv = curT.getTime();
                curLv = op.getStart() + ((curLv - op.getStart()) / op.getIncrement()) * op.getIncrement();
                if (op.getStart() <= curLv && op.getPrevTimeMarker().getTime() < curLv) {
                    curTimeMarker = new Timestamp(curLv);
                }
            }
            if (op.getPrevTimeMarker().before(curTimeMarker)) {
                long prevTM = op.getPrevTimeMarker().getTime();
                long curTM = curTimeMarker.getTime();
                for (long i = prevTM; i < curTM; i+=op.getIncrement()) {
                    Timestamp tm1 = new Timestamp(i+op.getIncrement()-op.getSize());
                    Timestamp tm2 = new Timestamp(i+op.getIncrement());
                    op.getOperateStatement(1).setTimestamp(1, tm2);
                    op.getOperateStatement(1).setTimestamp(2, tm1);
                    op.getOperateStatement(1).setTimestamp(3, tm2);
                    op.getOperateStatement(1).executeUpdate();
                }
                op.setPrevTimeMarker(curTimeMarker);
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Operator.Name_Id", new Object[]{op.getName(), op.getId()}), e);
        } finally {
            Util.close(rs);
        }
    }
}
