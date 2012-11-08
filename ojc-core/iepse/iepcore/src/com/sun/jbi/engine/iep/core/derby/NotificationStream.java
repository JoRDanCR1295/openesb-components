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
 * @(#)NotificationStream.java 
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
import java.sql.SQLException;
import java.sql.Timestamp;

/**
 *
 * @author Bing Lu
 */
public class NotificationStream implements OperatorConstants {
    public static void operate(String inputTableName, String tableName, String outputTableName, String columns, long size, Timestamp prevT, Timestamp curT) throws Exception {
        Connection con = null;
        PreparedStatement[] ps = new PreparedStatement[8];
        ResultSet[] rs = new ResultSet[3];
        try {
            con = DriverManager.getConnection("jdbc:default:connection");
            String[] columnNames = Util.getTokens(columns, DELIM);
            
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT MIN(" + COL_TIMESTAMP + ") FROM " + inputTableName);
            String sqlStr = sb.toString();
//            System.out.println("NotificationStream.operate 0:\n" + sqlStr);
            ps[0] = con.prepareStatement(sqlStr);
            
            sb = new StringBuffer();
            sb.append("SELECT MIN(" + COL_TIMESTAMP + ") FROM " + tableName);
            sqlStr = sb.toString();
//            System.out.println("NotificationStream.operate 1:\n" + sqlStr);
            ps[1] = con.prepareStatement(sqlStr);

            sb = new StringBuffer();
            sb.append("SELECT ");
            for (String columnName : columnNames) {
                sb.append("t." + columnName + " AS " + columnName + ", ");
            }
            sb.append("t." + COL_SEQID + " AS " + COL_SEQID + ", ");
            sb.append("t." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + " FROM ");
            sb.append(tableName);
            sb.append(" t WHERE ? < t." + COL_TIMESTAMP + " AND t." + COL_TIMESTAMP + " <= ? ");
            sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
            sb.append("u." + COL_TIMESTAMP + " <= t." + COL_TIMESTAMP);
            sb.append(" AND u." + COL_SEQID + " = t." + COL_SEQID);
            sb.append(" AND u." + COL_TAG + " = '-')");
            sb.append(" ORDER BY ");
            sb.append("t." + COL_TIMESTAMP);
            sqlStr = sb.toString();
//            System.out.println("NotificationStream.operate 2:\n" + sqlStr);
            ps[2] = con.prepareStatement(sqlStr);
            
            // For each (Symbol, Price, ems_timestamp) = (s, p, t)
            // INSERT INTO S (Symbol, Price, ems_timestamp) VALUES (s, p, t)
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
//            System.out.println("NotificationStream.operate 3:\n" + sqlStr);
            ps[3] = con.prepareStatement(sqlStr);
            
            // For each (Symbol, Price, ems_timestamp) = (s, p, t)
            // INSERT INTO T (Symbol, Price, ems_seqid, ems_timestamp + size) VALUES (s, p, seq, t)
            sb = new StringBuffer();
            sb.append("INSERT INTO ");
            sb.append(tableName);
            sb.append(" (");
            for (String columnName : columnNames) {
                sb.append(columnName + ", ");
            }
            sb.append(COL_SEQID + ", " + COL_TIMESTAMP + ") VALUES (");
            for (int i = 0, I = columnNames.length; i < I; i++) {
                sb.append("?, ");
            }
            sb.append("?, ?)");
            sqlStr = sb.toString();
//            System.out.println("NotificationStream.operate 4:\n" + sqlStr);
            ps[4] = con.prepareStatement(sqlStr);
            
            // DELETE FROM T t WHERE t.ems_timestamp <= ?
            sb = new StringBuffer();
            sb.append("DELETE FROM " + tableName + " t WHERE t." + COL_TIMESTAMP + " <= ?");
            sqlStr = sb.toString();
//            System.out.println("NotificationStream.operate 5:\n" + sqlStr);
            ps[5] = con.prepareStatement(sqlStr);
            
            // SELECT + FROM R
            sb = new StringBuffer();
            sb.append("SELECT ");
            for (String columnName : columnNames) {
                sb.append("u." + columnName + " AS " + columnName + ", ");
            }
            sb.append("u." + COL_SEQID + " AS " + COL_SEQID + ", ");
            sb.append("u." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + " FROM ");
            sb.append(inputTableName);
            sb.append(" u WHERE ? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ? ");
            sb.append(" AND u." + COL_TAG + " = '+'");
            sqlStr = sb.toString();
//            System.out.println("NotificationStream.operate 6:\n" + sqlStr);
            ps[6] = con.prepareStatement(sqlStr);

            // DELETE FROM T WHERE - in R
            sb = new StringBuffer();
            sb.append("DELETE FROM " + tableName + " t WHERE EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
            sb.append("? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ? ");
            sb.append("AND u." + COL_SEQID + " = t." + COL_SEQID);
            sb.append(" AND u." + COL_TAG + " = '-')");
            sqlStr = sb.toString();
//            System.out.println("NotificationStream.operate 7:\n" + sqlStr);
            ps[7] = con.prepareStatement(sqlStr);
            
            int colTotal = columnNames.length + 2; // 2 for SeqId, and Timestamp
            rs[0] = ps[0].executeQuery();
            rs[1] = ps[1].executeQuery();
            
            Timestamp tsMin = getMinTimestamp(rs[0], rs[1]);
            
            if(tsMin == null) {
            	// never happens (see NotificationStream.hasWorkToDo)            	
            	return;
            }

            if (tsMin.after(curT)) {
                return;
            }
            
            if (tsMin.after(prevT)) {
            	tsMin = createNewTimestamp(tsMin, -1);  // -1 so that the event with tsMin will be processed
            } else {
                tsMin = prevT;
            }
            
            Timestamp tsL = createNewTimestamp(tsMin, 0);
            Timestamp tsH = createNewTimestamp(tsMin, size);
            boolean stop = false;
            while (!stop) {
                if (tsH.after(curT)) {
                    tsH = createNewTimestamp(curT, 0);
                    stop = true;
                }
                ps[2].setTimestamp(1, tsL);
                ps[2].setTimestamp(2, tsH);
                rs[1] = ps[2].executeQuery();
                // For each (Symbol, Price, ems_timestamp) = (s, p, t)
                // INSERT INTO T (Symbol, Price, ems_timestamp + size) VALUES (s, p, t)
                while (rs[1].next()) {
                    for (int i = 1; i <= columnNames.length; i++) {
                        Object value = rs[1].getObject(i);
                        ps[3].setObject(i, value);
                        ps[4].setObject(i, value);
                    }
                    Object seqId = rs[1].getObject(colTotal - 1);
                    Timestamp t = rs[1].getTimestamp(colTotal);
                    ps[3].setTimestamp(colTotal - 1, t);

                    t = createNewTimestamp(t, size);
                    ps[4].setObject(colTotal - 1, seqId);
                    ps[4].setTimestamp(colTotal, t);

                    ps[3].addBatch();
                    ps[4].addBatch();
                }
                ps[3].executeBatch();
                ps[4].executeBatch();

                // DELETE FROM T t WHERE t.ems_timestamp <= tsH
                ps[5].setTimestamp(1, tsH);
                ps[5].executeUpdate();

                // SELECT + FROM R between tsL and tsH
                ps[6].setTimestamp(1, tsL);
                ps[6].setTimestamp(2, tsH);
                rs[2] = ps[6].executeQuery();

                // For each (Symbol, Price, ems_seqid, ems_timestamp) = (s, p, seq, t)
                // INSERT INTO T (Symbol, Price, ems_seqid, ems_timestamp + size) VALUES (s, p, seq, t)
                while (rs[2].next()) {
                    for (int i = 1; i <= columnNames.length; i++) {
                        Object value = rs[2].getObject(i);
                        ps[4].setObject(i, value);
                    }
                    Object seqId = rs[2].getObject(colTotal - 1);
                    Timestamp t = rs[2].getTimestamp(colTotal);
                    t = createNewTimestamp(t, size);
                    ps[4].setObject(colTotal - 1, seqId);
                    ps[4].setTimestamp(colTotal, t);
                    ps[4].addBatch();
                }
                ps[4].executeBatch();
                
                // DELETE FROM T WHERE - in R between tsL and tsH
                ps[7].setTimestamp(1, tsL);
                ps[7].setTimestamp(2, tsH);
                ps[7].executeUpdate();
                
                tsL = createNewTimestamp(tsH, 0);
                tsH = createNewTimestamp(tsH, size);
            }
        } catch (Exception e) {
            throw e;
        } finally {
            for (int i = 0; i < rs.length; i++) {
                Util.close(rs[i]);
            }
            for (int i = 0; i < ps.length; i++) {
                Util.close(ps[i]);
            }
            Util.close(con);
        }
    }
    
    static long THOUSAND = 1000L;
    static long MILLION = 1000000L;
    static long BILLION = 1000000000L;
    private static Timestamp createNewTimestamp(Timestamp originalTs, long millisToOffset){
        long orgSeconds = originalTs.getTime()/THOUSAND;
        long oriNanos = originalTs.getNanos();

        long nanosFromOffset = (millisToOffset % THOUSAND) * MILLION;
        long secondsFromOffset = millisToOffset / THOUSAND;

        // Note -BILLION < newNanos < BILLION
        long newNanos = (oriNanos + nanosFromOffset)%BILLION;
        long newSeconds = orgSeconds + secondsFromOffset + (oriNanos + nanosFromOffset)/BILLION;

        if (newNanos < 0) {
            newNanos += BILLION;
            newSeconds -= 1L;
        }

        Timestamp ts = new Timestamp(newSeconds*THOUSAND);
        ts.setNanos((int) newNanos);

        return ts;
    }
    
    private static Timestamp getMinTimestamp(ResultSet rs0, ResultSet rs1) throws SQLException{
    	Timestamp tsMin = null;
    	
    	Timestamp t0 = null;
        if (rs0.next()) {
            t0 = rs0.getTimestamp(1);
        }
        
        Timestamp t1 = null;
        if (rs1.next()) {
            t1 = rs1.getTimestamp(1);
        }
        
        if (t0 == null && t1 == null) {
            return null;
        }
        
        if (t0 != null && t1 != null) {
        	int comparisonResult = t0.compareTo(t1);
        	if(comparisonResult < 0) {
        		tsMin = t0;
        	} else {
        		tsMin = t1;
        	}
        } else if (t0 != null) {
            tsMin = t0;
        } else { // t1 is not null
            tsMin = t1;                
        }    	
    	return tsMin;
    }    
}
