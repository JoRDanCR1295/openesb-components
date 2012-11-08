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
 * @(#)RelationAggregator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;

/**
 *
 * @author Bing Lu
 */
public class RelationAggregator implements OperatorConstants {
    public static void operate(String outRName,
            String inRName,
            String inVName,
            String toColNames,
            String sColNames,
            String rFromColNames,
            String gbColNames,
            String rWhereClause,
            Timestamp ts0,
            Timestamp ts1) throws Exception {
        Connection con = DriverManager.getConnection("jdbc:default:connection");
        PreparedStatement s = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            String outputTableName = outRName;
            String inputTableName = inRName;
            String viewName = inVName;
            String[] toColumnNames = Util.getTokens(toColNames, DELIM);
            String[] supportingColumnNames = Util.getTokens(sColNames, DELIM);
            String[] r_fromColumnNames = Util.getColumnExpressions(rFromColNames);
            boolean hasGroupByColumnList = gbColNames != null && !gbColNames.trim().equals("");
            String[] groupByColumnNames = Util.getTokens(gbColNames, DELIM);
            boolean hasWhereClause = rWhereClause != null && !rWhereClause.trim().equals("");
            String whereClause_r = rWhereClause;
            
            //Note that the MINUS/EXCEPT operator filters out aggregations which have not changed since the
            //last time. For example, lets assume that at time t1, an aggregation was calculated for a 
            //particular group from the input relation R1. Then if at time t2, if R1 has changed, but the
            //aggregation for the particular group remains the same, then we don't update the
            //aggregation in the output table. That is, because the aggregation did not change, there
            //is no need to update the output table.            
            
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT ");
            for (int i = 0; i < toColumnNames.length; i++) {
                sb.append(r_fromColumnNames[i] + " AS " + toColumnNames[i] + ",");
            }
            if (hasGroupByColumnList) {
                for (int i = 0; i < supportingColumnNames.length; i++) {
                    sb.append("u." + groupByColumnNames[i] + " AS " + supportingColumnNames[i] + ",");
                }
            }
            sb.append("u." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + ",");
            sb.append("'+' AS " + COL_TAG);
            sb.append(" FROM " + inputTableName + " r, " + viewName + " u WHERE ");
            sb.append("? < " + "u." + COL_TIMESTAMP + " AND " + "u." + COL_TIMESTAMP + " <= ? AND ");
            sb.append("r." + COL_TAG + " = '+' AND ");
            if (hasGroupByColumnList) {
                for (String groupByColumnName : groupByColumnNames) {
                    sb.append("r." + groupByColumnName + " = u." + groupByColumnName + " AND ");
                }
            }
            sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("NOT EXISTS (SELECT 'x' FROM " + inputTableName + " s WHERE ");
            sb.append("s." + COL_TAG + " = '-' AND ");
            sb.append("s." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
            sb.append("s." + COL_SEQID + " = r." + COL_SEQID + ")");
            if (hasWhereClause) {
                sb.append(" AND (" + whereClause_r + ")");
            }
            sb.append(" GROUP BY ");
            if (hasGroupByColumnList) {
                for (String groupByColumnName : groupByColumnNames) {
                    sb.append("r." + groupByColumnName + ",");
                }
                for (String groupByColumnName : groupByColumnNames) {
                    sb.append("u." + groupByColumnName + ",");
                }
            }
            sb.append("u." + COL_TIMESTAMP);
            sb.append(" EXCEPT SELECT ");
            for (int i = 0; i < toColumnNames.length; i++) {
                sb.append(r_fromColumnNames[i] + " AS " + toColumnNames[i] + ",");
            }
            if (hasGroupByColumnList) {
                for (int i = 0; i < supportingColumnNames.length; i++) {
                    sb.append("u." + groupByColumnNames[i] + " AS " + supportingColumnNames[i] + ",");
                }
            }
            sb.append("u." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + ",");
            sb.append("'+' AS " + COL_TAG);
            sb.append(" FROM " + inputTableName + " r, " + viewName + " u WHERE ");
            sb.append("? < " + "u." + COL_TIMESTAMP + " AND " + "u." + COL_TIMESTAMP + " <= ? AND ");
            sb.append("r." + COL_TAG + " = '+' AND ");
            if (hasGroupByColumnList) {
                for (String groupByColumnName : groupByColumnNames) {
                    sb.append("r." + groupByColumnName + " = u." + groupByColumnName + " AND ");
                }
            }
            sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("NOT EXISTS (SELECT 'x' FROM " + inputTableName + " s WHERE ");
            sb.append("s." + COL_TAG + " = '-' AND ");
            sb.append("s." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
            sb.append("s." + COL_SEQID + " = r." + COL_SEQID + ")");
            if (hasWhereClause) {
                sb.append(" AND (" + whereClause_r + ")");
            }
            sb.append(" GROUP BY ");
            if (hasGroupByColumnList) {
                for (String groupByColumnName : groupByColumnNames) {
                    sb.append("r." + groupByColumnName + ",");
                }
                for (String groupByColumnName : groupByColumnNames) {
                    sb.append("u." + groupByColumnName + ",");
                }
            }
            sb.append("u." + COL_TIMESTAMP);
            sb.append(" ORDER BY " + COL_TIMESTAMP);
            String sqlStr = sb.toString();
//            System.out.println("RelationAggregator.operate:\n" + sqlStr);
            s = con.prepareStatement(sqlStr);
            s.setTimestamp(1, ts0);
            s.setTimestamp(2, ts1);
            s.setTimestamp(3, ts0);
            s.setTimestamp(4, ts1);            
            rs = s.executeQuery();
            
            sb = new StringBuffer();
            sb.append("INSERT INTO ");
            sb.append(outputTableName);
            sb.append(" (");
            for (String toColumnName : toColumnNames) {
                sb.append(toColumnName + ", ");
            }
            if (hasGroupByColumnList) {
                for (String supportingColumnName : supportingColumnNames) {
                    sb.append(supportingColumnName + ", ");
                }
            }
            sb.append(COL_TIMESTAMP + "," + COL_TAG + ") VALUES (");
            for (int i = 0, I = toColumnNames.length; i < I; i++) {
                sb.append("?, ");
            }
            if (hasGroupByColumnList) {
                for (int i = 0, I = supportingColumnNames.length; i < I; i++) {
                    sb.append("?, ");
                }
            }
            sb.append("?, ?)"); // for Timestamp, Tag
            sqlStr = sb.toString();
            ps = con.prepareStatement(sqlStr);
            int colTotal = toColumnNames.length;
            if (hasGroupByColumnList) {
                colTotal += supportingColumnNames.length;
            }
            colTotal += 2; // 2 for Timestamp, Tag
            while (rs.next()) {
                for (int i = 1; i <= colTotal; i++) {
                    ps.setObject(i, rs.getObject(i));
                }
                ps.addBatch();
            }
            ps.executeBatch();
        } catch (Exception e) {
            throw e;
        } finally {
            Util.close(s);
            Util.close(rs);
            Util.close(ps);
            Util.close(con);
        }
    }
}
