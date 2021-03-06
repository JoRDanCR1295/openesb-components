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
 * @(#)UnionAll.java 
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
public class Merge implements OperatorConstants {
    public static void operate(String outSName,
            String inSNames,
            String colNames,
            Timestamp ts0,
            Timestamp ts1)
            throws Exception {
        Connection con = null;
        PreparedStatement s = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            String outputTableName = outSName;
            
            String[] inputTableName = Util.getTokens(inSNames, DELIM);
            int inputCount = inputTableName.length;
            String[] columnNames = Util.getTokens(colNames, DELIM);
            
            //     SELECT column1, column2, Timestamp FROM
            //     (
            //            SELECT column1, column2, Timestamp
            //            FROM S1
            //            WHERE  7 < S1.Timestamp AND S1.Timestamp <= 10
            //
            //            UNION ALL
            //
            //            SELECT column1, column2, Timestamp
            //            FROM S2
            //            WHERE 7 < S2.Timestamp AND S2.Timestamp <= 10
            //
            //     ) u ORDER BY Timestamp
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT ");
            for (int i = 0; i < columnNames.length; i++) {
                sb.append(columnNames[i] + ",");
            }
            sb.append(COL_TIMESTAMP);
            sb.append(" FROM (");
            for (int i = 0; i < inputCount; i++) {
                if (0 < i) {
                    sb.append(" UNION ALL ");
                }
                sb.append("SELECT ");
                for (int j = 0; j < columnNames.length; j++) {
                    sb.append(inputTableName[i] + "." + columnNames[j] + ",");
                }
                sb.append(inputTableName[i] + "." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP);
                sb.append(" FROM " + inputTableName[i] + " WHERE ");
                sb.append("? < " + inputTableName[i] + "." + COL_TIMESTAMP + " AND " + inputTableName[i] + "." + COL_TIMESTAMP + " <= ?");
            }
            sb.append(") u ORDER BY " + COL_TIMESTAMP);
            String sqlStr = sb.toString();
            //System.out.println("Merge.operate0: \n" + sqlStr);
            s = con.prepareStatement(sqlStr);
            for (int i = 0; i < inputCount; i++) {
                s.setTimestamp(2*i+1, ts0);
                s.setTimestamp(2*i+2, ts1);
            }
            rs = s.executeQuery();
            
            // For each (Name, Value, Timestamp) = (n, v, t)
            // INSERT INTO S (Name, Value, Timestamp) VALUES (n, v, t)
            sb = new StringBuffer();
            sb.append("INSERT INTO ");
            sb.append(outputTableName);
            sb.append(" (");
            for (int i = 0; i < columnNames.length; i++) {
                sb.append(columnNames[i] + ",");
            }
            sb.append(COL_TIMESTAMP + ") VALUES (");
            for (int i = 0; i < columnNames.length; i++) {
                sb.append("?,");
            }
            sb.append("?)");
            sqlStr = sb.toString();
            //System.out.println("Merge.operate1: \n" + sqlStr);
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
