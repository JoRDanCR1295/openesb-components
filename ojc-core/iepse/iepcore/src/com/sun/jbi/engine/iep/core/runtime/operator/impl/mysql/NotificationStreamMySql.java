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
 * @(#)NotificationStreamMySql.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.NotificationStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.NotificationStream;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class NotificationStreamMySql implements NotificationStreamDb {
    private static final Messages mMessages = Messages.getMessages(NotificationStreamMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public NotificationStreamMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, NotificationStream op) throws Exception {
        PreparedStatement[] ret = new PreparedStatement[7];
        
        QueryPlan plan = op.getPlan();
        String seqName = Util.getSequenceName(plan.getId(), op.getId());
        String tableName = op.getTableName();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        long size = op.getSize();
        long microSecs = (long)(Math.ceil(size/1000f)*1000000);  // Up round size to integer seconds
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        String[] columnName = inputSchema.getColumnNames();
        
        StringBuffer sb = new StringBuffer();
        sb.append("SELECT MIN(" + COL_TIMESTAMP + ") FROM " + inputTableName);
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);   
        
        sb = new StringBuffer();
        sb.append("SELECT MIN(" + COL_TIMESTAMP + ") FROM " + tableName);
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + "(");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append(columnName[i] + ", ");
        }
        sb.append(COL_TIMESTAMP);
        sb.append(") SELECT ");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append("t." + columnName[i] + " AS " + columnName[i] + ", ");
        }
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
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "2", sqlStr});
        }    
        ret[2] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(tableName);
        sb.append(" (");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append(columnName[i] + ", ");
        }
        sb.append(COL_SEQID + ", " + COL_TIMESTAMP + ") ");
        sb.append("SELECT ");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append("t." + columnName[i] + " AS " + columnName[i] + ", ");
        }
        sb.append("t." + COL_SEQID + " AS " + COL_SEQID + ", ");
        sb.append("t." + COL_TIMESTAMP + " + INTERVAL " + microSecs  + " MICROSECOND AS " + COL_TIMESTAMP + " FROM ");
        sb.append(tableName);
        sb.append(" t WHERE ? < t." + COL_TIMESTAMP + " AND t." + COL_TIMESTAMP + " <= ? ");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
        sb.append("u." + COL_TIMESTAMP + " <= t." + COL_TIMESTAMP);
        sb.append(" AND u." + COL_SEQID + " = t." + COL_SEQID);
        sb.append(" AND u." + COL_TAG + " = '-')");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "3", sqlStr});
        }    
        ret[3] = con.prepareStatement(sqlStr);        
        
        sb = new StringBuffer();
        sb.append("DELETE t FROM " + tableName + " t WHERE t." + COL_TIMESTAMP + " <= ?");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "4", sqlStr});
        }    
        ret[4] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(tableName);
        sb.append(" (");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append(columnName[i] + ", ");
        }
        sb.append(COL_SEQID + ", " + COL_TIMESTAMP + ") ");
        sb.append("SELECT ");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append("u." + columnName[i] + " AS " + columnName[i] + ", ");
        }
        sb.append("u." + COL_SEQID + " AS " + COL_SEQID + ", ");
        sb.append("u." + COL_TIMESTAMP + " + INTERVAL " + microSecs  + " MICROSECOND AS " + COL_TIMESTAMP + " FROM ");
        sb.append(inputTableName);
        sb.append(" u WHERE ? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ? ");
        sb.append(" AND u." + COL_TAG + " = '+'");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "5", sqlStr});
        }    
        ret[5] = con.prepareStatement(sqlStr);        

        sb = new StringBuffer();
        sb.append("DELETE t FROM " + tableName + " t WHERE ");
        sb.append("EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
        sb.append("? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ? ");
        sb.append("AND u." + COL_SEQID + " = t." + COL_SEQID);
        sb.append(" AND u." + COL_TAG + " = '-')");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "6", sqlStr});
        }    
        ret[6] = con.prepareStatement(sqlStr);
        return ret;
    }
    

    public void executeOperateStatements(NotificationStream op, Timestamp prevT, Timestamp curT) throws Exception {
        long size = op.getSize();
        ResultSet[] rs = new ResultSet[2];
        try {
            rs[0] = op.getOperateStatement(0).executeQuery();
            rs[1] = op.getOperateStatement(1).executeQuery();
            boolean hasValue0 = rs[0].next();
            Timestamp t0 = null;
            if (hasValue0) {
                t0 = rs[0].getTimestamp(1);
                if (t0 == null) {
                    hasValue0 = false;
                }
            }
            boolean hasValue1 = rs[1].next();
            Timestamp t1 = null;
            if (hasValue1) {
                t1 = rs[1].getTimestamp(1);
                if (t1 == null) {
                    hasValue1 = false;
                }
            }
            if (!hasValue0 && !hasValue1) {
                // never happens (see NotificationStream.hasWorkToDo)
                return;
            }
            Timestamp tsMin = null;
            if (hasValue0 && hasValue1) {
                tsMin = new Timestamp(Math.min(t0.getTime(), t1.getTime()));
            } else if (hasValue0) {
                tsMin = new Timestamp(t0.getTime());
            } else { // hasValue1
                tsMin = new Timestamp(t1.getTime());
            }
            if (tsMin.after(curT)) {
                return;
            }
            if (tsMin.after(prevT)) {
                tsMin.setTime(tsMin.getTime() - 1);  // -1 so that the event with tsMin will be processed
            } else {
                tsMin.setTime(prevT.getTime());
            }
            Timestamp tsL = new Timestamp(tsMin.getTime());
            Timestamp tsH = new Timestamp(tsMin.getTime() + size);
            boolean stop = false;
            while (!stop) {
                if (tsH.after(curT)) {
                    tsH.setTime(curT.getTime());
                    stop = true;
                }
                op.getOperateStatement(2).setTimestamp(1, tsL);
                op.getOperateStatement(2).setTimestamp(2, tsH);
                op.getOperateStatement(2).executeUpdate();

                op.getOperateStatement(3).setTimestamp(1, tsL);
                op.getOperateStatement(3).setTimestamp(2, tsH);
                op.getOperateStatement(3).executeUpdate();

                // DELETE FROM T t WHERE t.ems_timestamp <= tsH
                op.getOperateStatement(4).setTimestamp(1, tsH);
                op.getOperateStatement(4).executeUpdate();

                // SELECT + FROM R between tsL and tsH
                op.getOperateStatement(5).setTimestamp(1, tsL);
                op.getOperateStatement(5).setTimestamp(2, tsH);
                op.getOperateStatement(5).executeUpdate();

                // DELETE FROM T WHERE - in R between tsL and tsH
                op.getOperateStatement(6).setTimestamp(1, tsL);
                op.getOperateStatement(6).setTimestamp(2, tsH);
                op.getOperateStatement(6).executeUpdate();
                
                tsL.setTime(tsH.getTime());
                tsH.setTime(tsH.getTime() + size);
            }
        } catch (Exception e) {
            throw e;
        } finally {
            for (int i = 0; i < rs.length; i++) {
                Util.close(rs[i]);
            }
        }
    }
}
