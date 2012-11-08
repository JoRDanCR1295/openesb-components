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
 * @(#)TupleBasedAggregator.java 
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
public class TupleBasedAggregator implements OperatorConstants {
    public static void operate(String inputTableName,
            String outputTableName,String fromColNames,String toColNames,String groupByColNames,
            String fromClause,
            String whereClause,
            long start,
            long increment,
            long size,
            long prevStp,  
            long stp) throws Exception {// Stp: Seqid To Process
        Connection con = null;
        PreparedStatement ps0 = null;
        PreparedStatement ps1 = null;
        PreparedStatement ps2 = null;
        ResultSet rs = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            String[] fromColumnNames = Util.getColumnExpressions(fromColNames);
            String[] toColumnNames = Util.getTokens(toColNames, DELIM);
            String[] groupByColumnNames = Util.getTokens(groupByColNames, DELIM);
            
            //For each i:  1 + (prevStp - start + 1)/increment < i <= (stp - start + 1)/increment
            // SELECT Timestamp as maxTs FROM S1 WHERE Seqid = start + i*increment -1
            //
            // SELECT fromColumn1 as toColumn1, fromColumn2 as toColumn2, maxTs 
            //    FROM fromClause(T1, T2, S1)
            //    WHERE start + i*increment - size - 1 < S1.Seqid <= start + i*increment -1 AND whereClause(T1, T2, S1)
            //    GROUP BY groupByColumn1, groupByColumn2
            // INSERT INTO S2 (toColumn1, toColumn2, Timestamp) VALUES (toColumn1, toColumn2);
            
            // Note that we must use maxTs as the Timestamp for the i-th aggregagation event because
            // it is caused by the (start + i*increment -1)-th raw event. This is consistent with
            // TimebasedAggregator
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT " + COL_TIMESTAMP + " FROM " + inputTableName);
            sb.append(" WHERE " + COL_SEQID + " = ?");
            String sqlStr = sb.toString();
            //System.out.println("TupleBasedAggregator.operate.ps0: \n" + sqlStr);
            ps0 = con.prepareStatement(sqlStr);
            
            sb = new StringBuffer();
            sb.append("SELECT ");
            for (int i = 0; i < fromColumnNames.length; i++) {
                if (0 < i) {
                    sb.append(",");
                }
                sb.append(fromColumnNames[i]);
            }
            sb.append(" FROM ");
            sb.append(fromClause);
            sb.append(" WHERE (? < " + inputTableName + "." + COL_SEQID);
            sb.append(" AND " + inputTableName + "." + COL_SEQID + " <= ?)");
            if (whereClause != null && !whereClause.trim().equals("")) {
                sb.append(" AND (" + whereClause + ")");
            }
            if (groupByColumnNames.length > 0) {
                sb.append(" GROUP BY ");
                for (int i = 0; i < groupByColumnNames.length; i++) {
                    if (0 < i) {
                        sb.append(",");
                    }
                    sb.append(groupByColumnNames[i]);
                }
            }
            sqlStr = sb.toString();
            //System.out.println("TupleBasedAggregator.operate.ps1: \n" + sqlStr);
            ps1 = con.prepareStatement(sqlStr);
            
            sb = new StringBuffer();
            sb.append("INSERT INTO ");
            sb.append(outputTableName);
            sb.append(" (");
            for (String toColumnName : toColumnNames) {
                sb.append(toColumnName + ",");
            }
            sb.append(COL_TIMESTAMP + ") VALUES (");
            for (int i = 0; i < toColumnNames.length; i++) {
                sb.append("?,");
            }
            sb.append("?)");
            sqlStr = sb.toString();
            //System.out.println("TupleBasedAggregator.operate.ps2: \n" + sqlStr);
            ps2 = con.prepareStatement(sqlStr);
            
            //System.out.println("TupleBasedAggregator.operate: \n" + postStr);
            for (long i = 1 + (prevStp - start + 1)/increment, I = (stp - start + 1)/increment; i <= I; i++) {
                //System.out.println("(" + (start + i*increment - size - 1) + ", " + (start + i*increment - 1) + ")");
                ps0.setLong(1, start + i*increment -1);
                rs = ps0.executeQuery();
                if (!rs.next()) {
                    continue;
                }
                Timestamp maxTs = rs.getTimestamp(1);
                Util.close(rs);
                ps1.setLong(1, start + i*increment - size - 1);
                ps1.setLong(2, start + i*increment - 1);
                rs = ps1.executeQuery(); 
                while (rs.next()) {
                    for (int j = 0; j < toColumnNames.length; j++) {
                        ps2.setObject(j+1, rs.getObject(j+1));
                    }
                    ps2.setTimestamp(toColumnNames.length + 1, maxTs);
                    ps2.addBatch(); // ps2.addBatch();ps2.executeUpdate();
                }
                ps2.executeBatch();
                Util.close(rs);
            }
            //ps.executeBatch();
        } catch (Exception e) {
            throw e;
        } finally {
            Util.close(ps0);
            Util.close(ps1);
            Util.close(ps2);
            Util.close(con);
        }
    }
 
}
