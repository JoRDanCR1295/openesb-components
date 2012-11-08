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
 * @(#)Intersect.java
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
public class Intersect implements OperatorConstants {
    public static void operate(String outRName, String inVName, String inRNames, String colNames, Timestamp ts0, Timestamp ts1) throws Exception {
        Connection con = null;
        PreparedStatement s = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            
            String outputTableName = outRName;
            String viewName = inVName;
            String[] inputTableName = Util.getTokens(inRNames, DELIM);
            int inputCount = inputTableName.length;
            String[] columnNames = Util.getTokens(colNames, DELIM);

            StringBuffer sb = new StringBuffer();
            sb.append("SELECT ");
            for (int i = 0; i < columnNames.length; i++) {
                sb.append(columnNames[i] + ",");
            }
            sb.append(COL_TIMESTAMP + " FROM " + viewName + " u WHERE ");
            sb.append("u." + COL_TAG + " = '+' AND ");
            sb.append("? < " + "u." + COL_TIMESTAMP + " AND " + "u." + COL_TIMESTAMP + " <= ? AND (");
            for (int i = 0; i < inputCount; i++) {
                if (0 < i) {
                    sb.append(" OR ");
                }
                sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
                for (int j = 0; j < columnNames.length; j++) {
                    sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
                }
                sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
                sb.append("r." + COL_TAG + " = '+')");
                sb.append(" = ");  // Existing: for some Ri + = -
                sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
                for (int j = 0; j < columnNames.length; j++) {
                    sb.append("r." + columnNames[j] + " = u." + columnNames[j] + " AND ");
                }
                sb.append("r." + COL_TIMESTAMP + " < u." + COL_TIMESTAMP + " AND ");
                sb.append("r." + COL_TAG + " = '-')");
            }
            sb.append(") AND (");
            for (int i = 0; i < inputCount; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
                for (String columnName : columnNames) {
                    sb.append("r." + columnName + " = u." + columnName + " AND ");
                }
                sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
                sb.append("r." + COL_TAG + " = '-')");
                sb.append(" < ");  // New: for all Ri: total(-) < total(+)
                sb.append("(SELECT COUNT(*) FROM " + inputTableName[i] + " r WHERE ");
                for (String columnName : columnNames) {
                    sb.append("r." + columnName + " = u." + columnName + " AND ");
                }
                sb.append("r." + COL_TIMESTAMP + " <= u." + COL_TIMESTAMP + " AND ");
                sb.append("r." + COL_TAG + " = '+')");
            }
            sb.append(") ORDER BY " + COL_TIMESTAMP);
            String sqlStr = sb.toString();
            //System.out.println("Intersect.operate0: " + sqlStr);
            s = con.prepareStatement(sqlStr);
            s.setTimestamp(1, ts0);
            s.setTimestamp(2, ts1);
            rs = s.executeQuery();
            
            sb = new StringBuffer();
            sb.append("INSERT INTO " + outputTableName + " (");
            for (String columnName : columnNames) {
                sb.append(columnName + ", ");
            }
            sb.append(COL_TIMESTAMP + ", " + COL_TAG + ") VALUES (");
            for (int i = 0, I = columnNames.length; i < I; i++) {
                sb.append("?, ");
            }
            sb.append("?, '+')");
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
