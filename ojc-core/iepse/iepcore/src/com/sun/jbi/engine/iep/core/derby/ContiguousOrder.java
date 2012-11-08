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
 * @(#)ContiguousOrder.java
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
public class ContiguousOrder implements OperatorConstants {
    public static void operate(String inputTableName, String tableName, String outputTableName, String columns, String attribute, String attributes, Timestamp ts1, Timestamp ts00, Timestamp ts01) throws Exception {
        Connection con = null;
        PreparedStatement s = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            String[] columnNames = Util.getTokens(columns, DELIM);
            String[] attrNames = Util.getTokens(attributes, DELIM);
            
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT ");
            for (String columnName : columnNames) {
                sb.append("u." + columnName + " AS " + columnName + ", ");
            }
            sb.append("t." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + " FROM ");
            sb.append(inputTableName);
            sb.append(" u, ");
            sb.append(tableName);
            sb.append(" t WHERE u." + COL_TIMESTAMP + " <= ? ");
            
            sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " v WHERE ");
            for (String attrName : attrNames) {
                sb.append(" v." + attrName + " = u." + attrName + " AND ");
            }
            sb.append("v." + attribute + " = u." + attribute);
            sb.append(" AND v." + COL_SEQID + " < u." + COL_SEQID + ")");
            
            sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + tableName + " s WHERE s." + COL_TIMESTAMP + " <= ?");
            for (String attrName : attrNames) {
                sb.append(" AND s." + attrName + " = u." + attrName);
            }
            sb.append(" AND u." + attribute + " < s." + COL_MISS_SEQ + ")");
            sb.append(" AND t." + COL_PROCESSING_TIME + " > ?");
            for (String attrName : attrNames) {
                sb.append(" AND u." + attrName + " = t." + attrName);
            }
            sb.append(" AND u." + attribute + " < t." + COL_MISS_SEQ);
            sb.append(" ORDER BY " + COL_TIMESTAMP + ", " + attribute);
            
            String sqlStr = sb.toString();
            //System.out.println("ContiguousOrder.operate:\n" + sqlStr);
            s = con.prepareStatement(sqlStr);
            s.setTimestamp(1, ts1);
            s.setTimestamp(2, ts00);
            s.setTimestamp(3, ts01);
            rs = s.executeQuery();
            
            // For each (Symbol, Price, Seq, ems_timestamp) = (s, p, seq, t)
            // INSERT INTO S (Symbol, Price, Seq, ems_timestamp) VALUES (s, p, seq, t)
            sb = new StringBuffer();
            sb.append("INSERT INTO ");
            sb.append(outputTableName);
            sb.append(" (");
            for (String columnName : columnNames) {
                sb.append(columnName + ", ");
            }
            sb.append(COL_TIMESTAMP + ") VALUES (");
            for (int i = 0, I = columnNames.length; i < I; i++) {
                sb.append("?, ");
            }
            sb.append("?)");
            sqlStr = sb.toString();
            ps = con.prepareStatement(sqlStr);
            int colTotal = columnNames.length + 1; // 1 for Timestamp
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
