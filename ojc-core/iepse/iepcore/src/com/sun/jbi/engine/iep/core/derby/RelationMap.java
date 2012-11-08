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
 * @(#)RelationMap.java 
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
public class RelationMap implements OperatorConstants {
    public static void operate(
            String outRName,
            String inRNames,
            String toColNames,
            String fromColNames,
            String seqIdColNames,
            String fromClause,
            String whereClause,
            Timestamp ts0,
            Timestamp ts1)
            throws Exception {
        Connection con = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        PreparedStatement ps = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            
            String outputTableName = outRName;
            
            String[] inputTableName = Util.getTokens(inRNames, DELIM);
            int inputCount = inputTableName.length;
            String[] toColumnName = Util.getTokens(toColNames, DELIM);
            String[] fromColumnName = Util.getColumnExpressions(fromColNames);
            String[] seqIdColumnName = Util.getTokens(seqIdColNames, DELIM);
            
            StringBuffer sb = new StringBuffer();
            for (int i = 0; i < inputCount; i++) {
                if (i > 0) {
                    sb.append(" UNION ALL ");
                }
                sb.append("SELECT ");
                for (int j = 0; j < fromColumnName.length; j++) {
                    sb.append(fromColumnName[j] + " AS " + toColumnName[j] + ",");
                }
                for (int j = 0; j < inputCount; j++) {
                    sb.append(inputTableName[j] + "." + COL_SEQID + " AS " + seqIdColumnName[j] + ",");
                }
                sb.append(inputTableName[i] + "." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP);
                sb.append(" FROM " + fromClause + " WHERE ");
                for (int j = 0; j < inputCount; j++) {
                    sb.append(inputTableName[j] + "." + COL_TAG + " = '+' AND ");
                    if (j < i) {
                        sb.append(inputTableName[j] + "." + COL_TIMESTAMP + " < " + inputTableName[i] + "." + COL_TIMESTAMP + " AND ");
                    } else if (j == i) {
                        sb.append("? < " + inputTableName[j] + "." + COL_TIMESTAMP + " AND ");
                        sb.append(inputTableName[j] + "." + COL_TIMESTAMP + " <= ? AND ");
                    } else {// j > i
                        sb.append(inputTableName[j] + "." + COL_TIMESTAMP + " <= " + inputTableName[i] + "." + COL_TIMESTAMP + " AND ");
                    }
                    if (j != i) {
                        sb.append("NOT EXISTS (SELECT 'x' FROM ");
                        sb.append(inputTableName[j] + " r WHERE ");
                        sb.append("r." + COL_SEQID + " = " + inputTableName[j] + "." + COL_SEQID + " AND ");
                        sb.append("r." + COL_TAG + " = '-' AND ");
                        sb.append("r." + COL_TIMESTAMP);
                        sb.append(" <= ");
                        sb.append(inputTableName[i] + "." + COL_TIMESTAMP + ") AND ");
                    }
                }
                if (whereClause.trim().equals("")) {
                    // Delete "AND " at end
                    int len = sb.length();
                    sb.delete(len-4, len);
                } else {
                    sb.append("(" + whereClause + ")");
                }
            }
            sb.append(" ORDER BY " + COL_TIMESTAMP);
            String sqlStr = sb.toString();
            //System.out.println("RelationMap.operate: " + sqlStr);
            s = con.prepareStatement(sqlStr);
            for (int i = 0; i < inputCount; i++) {
                s.setTimestamp(2*i+1, ts0);
                s.setTimestamp(2*i+2, ts1);
            }
            rs = s.executeQuery();
            
            sb = new StringBuffer();
            sb.append("INSERT INTO " + outputTableName + " (");
            for (int i = 0; i < toColumnName.length; i++) {
                sb.append(toColumnName[i] + ",");
            }
            for (int i = 0; i < inputCount; i++) {
                sb.append(seqIdColumnName[i] + ",");
            }
            sb.append(COL_TIMESTAMP + "," + COL_TAG + ") VALUES (");
            for (int i = 0; i < toColumnName.length; i++) {
                sb.append("?,");
            }
            for (int i = 0; i < inputCount; i++) {
                sb.append("?,");
            }
            sb.append("?, '+')");
            sqlStr = sb.toString();
            ps = con.prepareStatement(sqlStr);
            int colTotal = toColumnName.length + inputCount + 1; // 1 for Timestamp
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
            Util.close(rs);
            Util.close(s);
            Util.close(ps);
            Util.close(con);
        }
    }
}
