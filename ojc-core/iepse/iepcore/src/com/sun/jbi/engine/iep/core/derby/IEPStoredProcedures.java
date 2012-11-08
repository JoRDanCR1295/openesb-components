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
 * @(#)IEPStoredProcedures.java
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
import java.sql.Statement;

/**
 *
 * @author Bing Lu
 */
public class IEPStoredProcedures implements OperatorConstants {
    public static void cleanRelationByMinUsageTime(String planId, String tableName) throws Exception {
        Connection con = null;
        Statement s = null;
        ResultSet rs = null;
        PreparedStatement ps = null;
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            // DELETE FROM R1 t1
            // WHERE EXISTS (SELECT 'x' FROM R1 WHERE Tag='-' AND Row_Id = t1.Row_Id AND Timestamp <= min_usage_time)
            
            // SELECT SeqId FROM R1 WHERE Tag='-' AND Timestamp <= min_usage_time
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT " + COL_SEQID + " FROM ");
            sb.append(tableName);
            sb.append(" WHERE " + COL_TAG + "='-' AND ");
            sb.append(COL_TIMESTAMP + " <= (SELECT MIN(" + COL_TIMESTAMP + ") FROM ");
            sb.append(Util.getTableUsageTableName(planId));
            sb.append(" WHERE " + COL_TABLE_NAME + " = '" );
            sb.append(tableName);
            sb.append("')");
            
            s = con.createStatement();
            String sqlStr = sb.toString();
            rs = s.executeQuery(sqlStr);
            
            sb = new StringBuffer();
            sb.append("DELETE FROM " + tableName + " WHERE ");
            sb.append(COL_SEQID + " = ?");
            sqlStr = sb.toString();
            ps = con.prepareStatement(sqlStr);
            while (rs.next()) {
                long id = rs.getLong(1);
                ps.setLong(1, id);
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
