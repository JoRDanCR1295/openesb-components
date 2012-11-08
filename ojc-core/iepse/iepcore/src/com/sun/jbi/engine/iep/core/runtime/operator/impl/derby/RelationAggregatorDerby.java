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
 * @(#)RelationAggregatorDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationAggregator;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationAggregatorDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class RelationAggregatorDerby implements RelationAggregatorDb {
    private static final Messages mMessages = Messages.getMessages(RelationAggregatorDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public RelationAggregatorDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }

    public void createStoredProcedures(Connection con) {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE relationAggregatorOperate0 (");
        sb.append(" IN outRName VARCHAR(20),");
        sb.append(" IN inRName VARCHAR(20),");
        sb.append(" IN inVName VARCHAR(20),");
        sb.append(" IN toColNames VARCHAR(800),");
        sb.append(" IN sColNames VARCHAR(800),");
        sb.append(" IN rFromColNames VARCHAR(800),");
        sb.append(" IN gbColNames VARCHAR(800),");
        sb.append(" IN rWhereClause VARCHAR(800),");
        sb.append(" IN ts0 TIMESTAMP,");
        sb.append(" IN ts1 TIMESTAMP");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.RelationAggregator.operate'");
        String sqlStr = sb.toString();

        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure",  "relationAggregatorOperate0", e);
        } finally {
            Util.close(s);
        }
        return;
    }

    public void dropStoredProcedures(Connection con) {
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute("DROP PROCEDURE relationAggregatorOperate0");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "relationAggregatorOperate0", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement[] createOperateStatements(Connection con, RelationAggregator op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];

        QueryPlan plan = op.getPlan();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        String viewName = op.getViewName();

        List<String> toColumnList = op.getToColumnList();
        String[] toColumnName = new String[toColumnList.size()];
        for (int i = 0; i < toColumnName.length; i++) {
            toColumnName[i] = toColumnList.get(i);
        }

        boolean hasGroupByColumnList = op.hasGroupByColumnList();
        List<String> groupByColumnList = op.getGroupByColumnList();
        String[] groupByColumnName = new String[groupByColumnList.size()];
        String[] supportingColumnName = new String[groupByColumnList.size()];
        for (int i = 0; i < groupByColumnName.length; i++) {
            groupByColumnName[i] = groupByColumnList.get(i);
            supportingColumnName[i] = op.getSupportingColumnName(groupByColumnName[i]);
        }

        // New + in R
        // {CALL relationAggregator0('R', 'R0', 'v_R', 'Symbol,Avg0', 'ems_sr_Name', 'r.Name,AVG(r.Value)', 'Name', ?, ?, ?)}
        String outRName = outputTableName;
        String inRName = op.getInputOperatorList().get(0).getQueueName();
        String inVName = viewName;

        String toColNames = mDerbySpecial.getString(toColumnName, DELIM);
        String sColNames = mDerbySpecial.getString(supportingColumnName, DELIM);

        StringBuffer sb = new StringBuffer();
        List<String> rFromColumnList = op.getRFromColumnList();
        for (int i = 0, I = rFromColumnList.size(); i < I; i++) {
            if (0 < i) {
                sb.append(DELIM);
            }
            String exp = rFromColumnList.get(i);
            if (exp.startsWith("'") && exp.endsWith("'")) {
                int len = exp.length();
                exp = SINGLE_QUOTE_SUB + exp.substring(1, len - 1) + SINGLE_QUOTE_SUB;
                sb.append(exp);
            } else {
                sb.append(exp);
            }
        }
        String rFromColNames = sb.toString();

        String gbColNames = mDerbySpecial.getString(groupByColumnName, DELIM);

        sb = new StringBuffer();
        sb.append("{CALL relationAggregatorOperate0(");
        sb.append("'" + outRName + "', ");
        sb.append("'" + inRName + "', ");
        sb.append("'" + inVName + "', ");
        sb.append("'" + toColNames + "', ");
        sb.append("'" + sColNames + "', ");
        sb.append("'" + rFromColNames + "', ");
        sb.append("'" + gbColNames + "', ");
        sb.append("?, ?, ?)}");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);
        
        // There are two cases when we want to remove an aggregation:
        // 1. There was a new aggregation added to the output table, making the previous one invalid. The
        // first condition of the OR corresponds to this case.  
        // 2. No new aggregation was added, but the aggregation has become invalid because the records
        // from the input relation R1 have become invalid. For example, if at time t1, R1 has a + record
        // for the group IBM, then it will have a corresponding entry in the output relation. If at time
        // t2, a - record appears in R1 for IBM, that means there are no records in R1 that are valid. So,
        // the corresponding aggregation for IBM becomes invalid in the output table (that is, we need to
        //insert a -). The second condition of the OR corresponds to this case. 
        // Note:
        //     "y.Timestamp < u.Timestamp" because there can be (Name, Value) pairs with bigger timestamps
        //           inserted when '+' statement was executed.
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i] + ",");
        }
        if (hasGroupByColumnList) {
            for (int i = 0; i < supportingColumnName.length; i++) {
                sb.append(supportingColumnName[i] + ",");
            }
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG);
        sb.append(") SELECT ");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append("x." + toColumnName[i] + ",");
        }
        if (hasGroupByColumnList) {
            for (int i = 0; i < supportingColumnName.length; i++) {
                sb.append("x." + supportingColumnName[i] + ",");
            }
        }
        sb.append("x." + COL_SEQID + ",u." + COL_TIMESTAMP + ",'-' FROM " + outputTableName + " x, " + viewName + " u WHERE ");
        sb.append("? < " + "u." + COL_TIMESTAMP + " AND " + "u." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("x." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " y WHERE ");
        sb.append("y." + COL_TAG + " = '-' AND ");
        sb.append("y." + COL_SEQID + " = x." + COL_SEQID + ") AND ");
        if (hasGroupByColumnList) {
            for (int i = 0; i < groupByColumnName.length; i++) {
                sb.append("x." + supportingColumnName[i] + " = u." + groupByColumnName[i] + " AND ");
            }
        }
        sb.append("((EXISTS (SELECT 'x' FROM " + outputTableName + " y WHERE ");
        if (hasGroupByColumnList) {
            for (int i = 0; i < groupByColumnName.length; i++) {
                sb.append("y." + supportingColumnName[i] + " = u." + groupByColumnName[i] + " AND ");
            }
        }
        sb.append("y." + COL_TIMESTAMP + " = u." + COL_TIMESTAMP + ") AND ");
        sb.append("x." + COL_TIMESTAMP + " = (SELECT MAX(y." + COL_TIMESTAMP + ") FROM " + outputTableName + " y WHERE ");
        sb.append("y." + COL_TAG + " = '+' AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + outputTableName + " z WHERE z." + COL_SEQID + " = y." + COL_SEQID + " AND z." + COL_TAG + " = '-') AND ");
        if (hasGroupByColumnList) {
            for (int i = 0; i < groupByColumnName.length; i++) {
                sb.append("y." + supportingColumnName[i] + " = u." + groupByColumnName[i] + " AND ");
            }
        }
        sb.append("y." + COL_TIMESTAMP + " < u."  + COL_TIMESTAMP + ")) ");
        sb.append("OR NOT EXISTS (SELECT 'x' FROM " + inputTableName + " r WHERE NOT EXISTS (SELECT 'x' FROM " + inputTableName + " s WHERE ");
        sb.append("s." + COL_TAG + " = '-' AND s." + COL_SEQID + " = r." + COL_SEQID + " AND s." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + ") AND ");
        if (hasGroupByColumnList) {
            for (int i = 0; i < groupByColumnName.length; i++) {
                sb.append("x." + supportingColumnName[i] + " = r." + groupByColumnName[i] + " AND ");
            }
        }
        sb.append("r." + COL_TIMESTAMP + " <= u."  + COL_TIMESTAMP + "))");
        
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        return ret;
    }

    public void executeOperateStatements(RelationAggregator op, Timestamp prevT, Timestamp curT) throws Exception {
        op.getOperateStatement(0).setString(1, op.getRWhereClause());
        op.getOperateStatement(0).setTimestamp(2, prevT);
        op.getOperateStatement(0).setTimestamp(3, curT);
        op.getOperateStatement(0).executeUpdate();

        op.getOperateStatement(1).setTimestamp(1, prevT);
        op.getOperateStatement(1).setTimestamp(2, curT);
        op.getOperateStatement(1).executeUpdate();
    }
}
