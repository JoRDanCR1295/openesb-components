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
 * @(#)GapWindowOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.db2;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.GapWindow;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.GapWindowDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 * 
 * Fortent : Based on GapWindowOracle.java
 */
public class GapWindowDB2 implements GapWindowDb {
    private static final Messages mMessages = Messages.getMessages(GapWindowDB2.class);
    
    private DB2Special mDB2Special;
    
    public GapWindowDB2(DB2Special dB2Special) {
    	mDB2Special = dB2Special;
    }

    public PreparedStatement[] createOperateStatements(Connection con, GapWindow op) throws Exception {
        // Prepare mOperateStmt:
        PreparedStatement[] ret = new PreparedStatement[8];
        
        QueryPlan plan = op.getPlan();
        String seqName = Util.getSequenceName(plan.getId(), op.getId());
        String tableName = op.getTableName();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        List attributeList = op.getAttributeList();
        String attribute = op.getAttribute();
        long start = op.getStart();
        int attrCnt = attributeList.size();
        String attrName[] = new String[attrCnt];
        for (int i = 0; i < attrCnt; i++) {
            attrName[i] = (String)attributeList.get(i);
        }
        
        // UPDATE T WITH '-'
        StringBuffer sb = new StringBuffer();
        sb.append("UPDATE " + tableName + " SET " + COL_UPDATE + " = '-'");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareCall(sqlStr);        
        
        sb = new StringBuffer();
        sb.append("UPDATE " + tableName + " t SET " + COL_UPDATE + " = '+' WHERE ");
        sb.append("(t." + COL_MAX_SEQ + " - t." + COL_MISS_SEQ + " < 0");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
        sb.append("? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND t." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND u." + attribute + " >= t." + COL_MISS_SEQ + "))");
        sb.append(" OR ");
        sb.append("(t." + COL_MAX_SEQ + " - t." + COL_MISS_SEQ + " > 0");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " u WHERE ");
        sb.append("? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND t." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND u." + attribute + " = t." + COL_MISS_SEQ + "))");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareCall(sqlStr);        

        // INSERT INTO T
        sb = new StringBuffer();
        sb.append("INSERT INTO " + tableName + " (");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(attrName[i] +  ", ");
        }
        sb.append(COL_MISS_SEQ + ", " + COL_MAX_SEQ + ")(");
        sb.append(" SELECT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("u." + attrName[i] + " AS " + attrName[i] +  ", ");
        }
        sb.append("MIN(u." + attribute + ") + 1 AS " + COL_MISS_SEQ + ", ");
        sb.append((start-1) + " AS " + COL_MAX_SEQ + " FROM ");
        sb.append(inputTableName + " u");
        sb.append(" WHERE u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND (SELECT t." + COL_UPDATE + " FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(") = '+'");
        sb.append(" AND u." + attribute + " >= (SELECT t." + COL_MISS_SEQ + " FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(") - 1");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append("w." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = u." + attribute + " + 1)");
        if (attrName.length > 0) {
            sb.append(" GROUP BY ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(", ");
                }
                sb.append("u." + attrName[i]);
            }
        }         
        sb.append(" UNION ");
        
        sb.append(" SELECT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("u." + attrName[i] + " AS " + attrName[i] + ", ");
        }
        sb.append("MIN(u." + attribute + ") + 1 AS " + COL_MISS_SEQ + ", ");
        sb.append((start-1) + " AS " + COL_MAX_SEQ + " FROM ");
        sb.append(inputTableName + " u");
        sb.append(" WHERE u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(")");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append("w." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = " + start + ")");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append("w." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = u." + attribute + " + 1)");
        if (attrName.length > 0) {
            sb.append(" GROUP BY ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(", ");
                }
                sb.append("u." + attrName[i]);
            }
        }    
        sb.append(" UNION ");
        
        sb.append(" SELECT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("u." + attrName[i] + " AS " + attrName[i] + ", ");
        }
        sb.append(start + " AS " + COL_MISS_SEQ + ", ");
        sb.append((start-1) + " AS " + COL_MAX_SEQ + " FROM ");
        sb.append(inputTableName + " u");
        sb.append(" WHERE u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + tableName + " t");
        if (attrName.length > 0) {
            sb.append(" WHERE ");
            for (int i = 0; i < attrName.length; i++) {
                if (0 < i) {
                    sb.append(" AND ");
                }
                sb.append("t." + attrName[i] + " = u." + attrName[i]);
            }
        }
        sb.append(")");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append(" w." + COL_TIMESTAMP + " <= ? ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " = " + start + ")");
        sb.append(" AND EXISTS (SELECT 'x' FROM " + inputTableName + " w WHERE ");
        sb.append(" w." + COL_TIMESTAMP + " <= ? ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND w." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND w." + attribute + " > " + start + ")");
        sb.append(")"); // end of INSERT INTO
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "2", sqlStr});
        }    
        ret[2] = con.prepareStatement(sqlStr);   
        
        // UPDATE T
        sb = new StringBuffer();
        sb.append("UPDATE " + tableName + " t SET ");
        sb.append(COL_MAX_SEQ + " = (SELECT MAX(u." + attribute + ")FROM " + inputTableName + " u WHERE ");
        sb.append("u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND u." + attrName[i] + " = t." + attrName[i]);
        }
        sb.append(")," + COL_TIMESTAMP + " = (SELECT MAX(u." + COL_TIMESTAMP + ") FROM " + inputTableName + " u WHERE ");
        sb.append("u." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND u." + attrName[i] + " = t." + attrName[i]);
        }
        sb.append("), " + COL_PROCESSING_TIME + " = ? WHERE t." + COL_MAX_SEQ + " = " + (start - 1));
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "3", sqlStr});
        }    
        ret[3] = con.prepareStatement(sqlStr);    

        // DELETE FROM T
        sb = new StringBuffer();
        sb.append("DELETE FROM " + tableName + " t WHERE ");
        sb.append("EXISTS (SELECT 'x' FROM " + tableName + " s WHERE ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("s." + attrName[i] + " = t." + attrName[i] + " AND ");
        }
        sb.append("t." + COL_PROCESSING_TIME + " < s." + COL_PROCESSING_TIME + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "4", sqlStr});
        }    
        ret[4] = con.prepareStatement(sqlStr);   
        
        // INSERT '+' INTO R
        sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(attrName[i] + ", ");
        }
        sb.append(attribute + ", " + COL_SEQID + ", " + COL_TIMESTAMP + ", " + COL_TAG + ") ");
        sb.append("(SELECT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("t." + attrName[i] + " AS " + attrName[i] + ", ");
        }
        sb.append("t." + COL_MISS_SEQ + " AS " + attribute + ", ");
        
//      sb.append(seqName + ".NextVal, ");
//		Fortent : sequence change
        sb.append("NEXT VALUE FOR " + seqName + ", ");
        
        sb.append("t." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + ", ");
        sb.append("'+' AS " + COL_TAG);
        sb.append(" FROM " + tableName + " t WHERE ");
        sb.append("? < t." + COL_TIMESTAMP);
        sb.append(" AND t." + COL_MISS_SEQ + " < t." + COL_MAX_SEQ);
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + outputTableName + " r WHERE ");
        sb.append("r." + attribute + " = t." + COL_MISS_SEQ);
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND r." + attrName[i] + " = t." + attrName[i]);
        }
        sb.append("))");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "5", sqlStr});
        }    
        ret[5] = con.prepareStatement(sqlStr);
        
        // INSERT '-' INTO R
        sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + "(SELECT ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("r." + attrName[i] + " AS " + attrName[i] + ", ");
        }
        sb.append("r." + attribute + " AS " + attribute + ", ");
        sb.append("r." + COL_SEQID + " AS " + COL_SEQID + ", ");
        sb.append("MIN(u." + COL_TIMESTAMP + ") AS " + COL_TIMESTAMP + ", ");
        sb.append("'-' AS " + COL_TAG);
        sb.append(" FROM " + outputTableName + " r, " + inputTableName + " u WHERE ");
        sb.append("u." + COL_TIMESTAMP + " < ?");
        sb.append(" AND r." + COL_TAG + " = '+'");
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + outputTableName + " s WHERE ");
        sb.append("s." + COL_SEQID + " = r." + COL_SEQID);
        sb.append(" AND s." + COL_TAG + " = '-')");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND r." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND r." + attribute + " = u." + attribute);
        sb.append(" GROUP BY ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("r." + attrName[i] + ", ");
        }
        sb.append("r." + attribute + ", ");
        sb.append("r." + COL_SEQID);
        sb.append(")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "6", sqlStr});
        }    
        ret[6] = con.prepareStatement(sqlStr);
        
        // Min Usage Timetamp
        sb = new StringBuffer();
        sb.append("SELECT MIN(u." + COL_TIMESTAMP + ") FROM " + inputTableName + " u WHERE");
        sb.append(" u." + COL_TIMESTAMP + " <= ?");
        sb.append(" AND EXISTS (SELECT 'x' FROM ");
        sb.append(tableName + " t WHERE ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("t." + attrName[i] + " = u." + attrName[i] + " AND ");
        }
        sb.append("t." + COL_MISS_SEQ + " - 1 <= u." + attribute + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "7", sqlStr});
        }    
        ret[7] = con.prepareStatement(sqlStr);
        return ret;
    }
    
}
