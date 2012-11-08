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
 * @(#)ContiguousOrderOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.oracle;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ContiguousOrder;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ContiguousOrderDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class ContiguousOrderOracle implements ContiguousOrderDb {
    private static final Messages mMessages = Messages.getMessages(ContiguousOrderOracle.class);
    
    private OracleSpecial mOracleSpecial;
    
    public ContiguousOrderOracle(OracleSpecial oracleSpecial) {
        mOracleSpecial = oracleSpecial;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, ContiguousOrder op) throws Exception {
    	
    	/*
    	 * Note: Please review this and correct the documentation as necessary. These notes have been
    	 * written by looking at the existing code, so it may not capture all the ideas accurately!
    	 * 
    	 * For ContiguousOrder we have a temp. table  where we hold the information about the
    	 * max_sequence and the missing sequence. This information is stored for each unique value
    	 * of partition key. All the queries always use the value of the partition key to insert or
    	 * update the temp table.
    	 * 
    	 * Statement 0 and 1 are update statements to the temp. table. Initially when no data is present
    	 * the table is empty and the update statements don't have any effect. Once a record is inserted
    	 * we always set the value to - before we do further processing. 
    	 * 
    	 * Statement 1: In statement 1, we detect if any changes are required to the existing records in 
    	 * the temp. table. In the first condition, if all the records have arrived upto a certain sequence, 
    	 * and if a new record arrives greater than or equal to the missing sequence then we need to update
    	 * the temp table. For example if 1,2,3 have arrived previously, then 4 would be the missing sequence. 
    	 * Now if any event with sequence id 4 or more arrives, we want to make changes to the entry in the 
    	 * temp table. In this case, the max sequence id would be changed from 4 to the new sequence id. 
    	 * What is not clear to me is why do we update if the event that arrive is 4 itself. The other condition 
    	 * is used in other cases, for example 1,2,3 and 5 have arrived previously. In this case the missing 
    	 * sequence is 4. The second condition updates the temp table only if 4 arrives.  
    	 * 
    	 * Statement 2: This scans the input table and populates the temp. table with the the missing seq id. 
    	 * It is a union of three statements. The first is used after the temp table has been populated for 
    	 * a particular value of the partition key. The second is used initially and only once for a particular 
    	 * value of a partition key, when the event with the start sequence has arrived. Once the temp table is 
    	 * inserted with the missing sequence id for the particular value of partition key, the first statement 
    	 * is then used for subsequent inserts. Note that the previous entries get deleted and the new one
    	 * takes precedence which happens when the next set of queries are executed. The third is a special 
    	 * case and is used initially when the event with the start sequence has not arrived and no data
    	 * exists in the temp table for that particular value of the partition key.
    	 * 
    	 * Statement 3: Note that when a new missing sequence is inserted into the temp table (as described) in the
    	 * statement 2, the max is set as 0. This query updates the record with the max sequence id as well as the
    	 * timestamp of the latest record. Also, the processing time in the temp table is updated to the current
    	 * timestamp.
    	 * 
    	 * Statement 4: This statement is used to pick the records from the input table which have sequence
    	 * id less than the missing sequence and insert them in the output table. If two events with the same 
    	 * sequence id exist, the latest is picked (first not-exists). Those events which have already been 
    	 * picked are filtered using the second not exists.     
    	 * 
    	 * Statement 5: This is used to delete older records from the temp table for a particular value of the 
    	 * partition key. In statements 0 and 1 the existing entries in the temp table are updated. In 2, new entry 
    	 * is inserted. This query deletes the old ones.  
    	 * 
    	 * Statement 6: This query is used to retrieve or calculate the input usage time so that the method 
    	 * executeUpdateInputUsageStmt() can use the information.
    	 */
    	
    	
        // Prepare mOperateStmt:
        PreparedStatement[] ret = new PreparedStatement[7];
        
        String seqName = Util.getSequenceName(op.getPlan().getId(), op.getId());
        String tableName = op.getTableName();
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        List attributeList = op.getAttributeList();
        String attribute = op.getAttribute();
        long start = op.getStart();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        int attrCnt = attributeList.size();
        String attrName[] = new String[attrCnt];
        for (int i = 0; i < attrCnt; i++) {
            attrName[i] = (String)attributeList.get(i);
        }
        String[] columnName = inputSchema.getColumnNames();
        
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
        
        sb.append(" SELECT DISTINCT ");
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
        
        // INSERT INTO S
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append(columnName[i] + ", ");
        }
        sb.append(seqName + ".NextVal, ");
        sb.append(COL_TIMESTAMP + " FROM (");
        sb.append("SELECT ");
        for (int i = 0, I = columnName.length; i < I; i++) {
            sb.append("u." + columnName[i] + " AS " + columnName[i] + ", ");
        }
        sb.append("t." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + " FROM ");
        sb.append(inputTableName);
        sb.append(" u, ");
        sb.append(tableName);
        sb.append(" t WHERE u." + COL_TIMESTAMP + " <= ? ");
        
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + inputTableName + " v WHERE ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" v." + attrName[i] + " = u." + attrName[i] + " AND ");
        }
        sb.append("v." + attribute + " = u." + attribute);
        sb.append(" AND v." + COL_SEQID + " < u." + COL_SEQID + ")");
        
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + tableName + " s WHERE s." + COL_TIMESTAMP + " <= ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND s." + attrName[i] + " = u." + attrName[i]);
        }
        sb.append(" AND u." + attribute + " < s." + COL_MISS_SEQ + ")");
        sb.append(" AND t." + COL_PROCESSING_TIME + " > ?");
        for (int i = 0; i < attrName.length; i++) {
            sb.append(" AND u." + attrName[i] + " = t." + attrName[i]);
        }
        sb.append(" AND u." + attribute + " < t." + COL_MISS_SEQ);
        sb.append(" ORDER BY " + COL_TIMESTAMP + ", " + attribute + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "4", sqlStr});
        }    
        ret[4] = con.prepareCall(sqlStr);
        
        // DELETE FROM T
        sb = new StringBuffer();
        sb.append("DELETE FROM ");
        sb.append(tableName);
        sb.append(" t WHERE EXISTS (SELECT 'x' FROM ");
        sb.append(tableName);
        sb.append(" s WHERE ");
        for (int i = 0; i < attrName.length; i++) {
            sb.append("s." + attrName[i] + " = t." + attrName[i] + " AND ");
        }
        sb.append("t." + COL_PROCESSING_TIME + " < s." + COL_PROCESSING_TIME + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "5", sqlStr});
        }    
        ret[5] = con.prepareStatement(sqlStr);
        
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
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "6", sqlStr});
        }    
        ret[6] = con.prepareStatement(sqlStr);
        return ret;
    }
    
}
