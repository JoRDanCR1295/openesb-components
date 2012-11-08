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
 * @(#)TimeBasedAggregator.java 
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
public class TimeBasedAggregator implements OperatorConstants {
    public static void operate(String inputTableName,
            String outputTableName,String fromColNames,String toColNames,String groupByColNames,
            String fromClause,
            String whereClause,
            Timestamp mark,
            Timestamp prevTstp,
            Timestamp tstp) throws Exception 
    {
        Connection con = null;
        PreparedStatement s = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            String[] fromColumnNames = Util.getColumnExpressions(fromColNames);
            String[] toColumnNames = Util.getTokens(toColNames, DELIM);
            String[] groupByColumnNames = Util.getTokens(groupByColNames, DELIM);
            
            // SELECT fromColumn1 as toColumn1,
            //       fromColumn2 as toColumn2,
            // FROM fromClause(T1, T2, S1)
            // WHERE ? < S1.Timestamp AND S1.Timestamp <= ? AND whereClause(T1, T2, S1)
            // GROUP BY groupByColumn1, groupByColumn2
            
            // For each (toColumn1, toColumn2) = (tc1, tc2) do
            // INSERT INTO S2 VALUES (tc1, tc2, ?)
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT ");
            for (int i = 0; i < fromColumnNames.length; i++) {
                if (0 < i) {
                    sb.append(",");
                }
                sb.append(fromColumnNames[i] + " AS " + toColumnNames[i]);
            }
            sb.append(" FROM ");
            sb.append(fromClause);
            sb.append(" WHERE ? < " + inputTableName + "." + COL_TIMESTAMP + " AND ");
            sb.append(inputTableName + "." + COL_TIMESTAMP + " <= ?");
            if (whereClause != null && !whereClause.trim().equals("")) {
                sb.append(" AND (" + whereClause + ")");
            }
            if (groupByColumnNames.length > 0) {
                sb.append(" GROUP BY ");
                for (int i = 0; i < groupByColumnNames.length; i++) {
                    sb.append(groupByColumnNames[i]);
                    if (i < (groupByColumnNames.length - 1)) {
                        sb.append(",");
                    }
                }
            }
            String sqlStr = sb.toString();
            s = con.prepareStatement(sqlStr);
            s.setTimestamp(1, prevTstp);
            s.setTimestamp(2, tstp);
            rs = s.executeQuery();
            
            sb = new StringBuffer();
            sb.append("INSERT INTO ");
            sb.append(outputTableName);
            sb.append("(");
            for (String toColumnName : toColumnNames) {
                sb.append(toColumnName + ",");
            }
            sb.append(COL_TIMESTAMP + ")");
            sb.append(" VALUES (");
            for (int i = 0; i < toColumnNames.length; i++) {
                sb.append("?, ");
            }
            sb.append("?) ");
            sqlStr = sb.toString();
            ps = con.prepareStatement(sqlStr);
            while (rs.next()) {
                for (int i = 1; i <= toColumnNames.length; i++) {
                    Object v = rs.getObject(i);
                    if (rs.wasNull()) {
                        return;
                    }
                    ps.setObject(i, rs.getObject(i));
                }
                ps.setTimestamp(toColumnNames.length + 1, mark);
                ps.addBatch(); 
            }
            ps.executeBatch();
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        } finally {
            Util.close(rs);
            Util.close(s);
            Util.close(ps);
            Util.close(con);
        }
    }
    
           
}
