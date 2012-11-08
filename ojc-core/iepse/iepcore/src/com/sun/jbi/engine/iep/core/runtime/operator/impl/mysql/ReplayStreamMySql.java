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
 * @(#)ExternalTablePollingStreamMySql.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ReplayStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ReplayStreamDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class ReplayStreamMySql implements ReplayStreamDb {
    private static final Messages mMessages = Messages.getMessages(ReplayStreamMySql.class);
    
    private MySqlSpecial mMySqlSpecial;
    
    public ReplayStreamMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public PreparedStatement[] createOperateStatements(Connection source, Connection target, ReplayStream op) throws Exception {
        String outputTableName = op.getQueueName();

        PreparedStatement selectRecordIdColumnsStmt = null;

        PreparedStatement deleteRecordIdColumnsStmt = null;

        Schema recordIdColumnSchema = op.getRecordIdentifierColumnsSchema();
        boolean isRecordIdentiferColumnsSpecified = false;
        if (recordIdColumnSchema != null) {
            if (recordIdColumnSchema.getColumnCount() > 0) {
                isRecordIdentiferColumnsSpecified = true;
            }
        }

        if (isRecordIdentiferColumnsSpecified) {
            //select from record identifier table
            StringBuffer selectStmt = new StringBuffer();
            selectStmt.append("SELECT * FROM ");
            selectStmt.append(op.getRecordIdentifierTableName());
            selectRecordIdColumnsStmt = target.prepareStatement(selectStmt.toString());

            //delete all from record identifer table
            StringBuffer deleteStmt = new StringBuffer();
            deleteStmt.append("DELETE FROM ");
            deleteStmt.append(op.getRecordIdentifierTableName());
            deleteRecordIdColumnsStmt = target.prepareStatement(deleteStmt.toString());
        }

        Map<String, String> toColumnToFromColumnMap = new HashMap<String, String>();

        List<String> fromColumnList = op.getFromColumnList();
        List<String> toColumnList = op.getToColumnList();

        String[] fromColumnName = new String[fromColumnList.size()];

        for (int i = 0; i < fromColumnName.length; i++) {
            String exp = fromColumnList.get(i);
            if (exp.startsWith("'") && exp.endsWith("'")) {
                fromColumnName[i] = exp;
            } else {
                fromColumnName[i] = exp;
            }
        }

        String[] toColumnName = new String[toColumnList.size()];
        for (int i = 0; i < toColumnName.length; i++) {
            toColumnName[i] = toColumnList.get(i);

            toColumnToFromColumnMap.put(toColumnName[i], fromColumnName[i]);
        }
        // Prepare mOperateStmt:
        // INSERT INTO S2 (toColumn1, toColumn2, Timestamp) (
        //       SELECT fromColumn1 as toColumn1, fromColumn2 as toColumn2, S1.Timestamp FROM fromClause(T1, T2, S1)
        //       WHERE 7 < S1.Timestamp AND S1.Timestamp <= 10 AND whereClause(T1, T2, S1)
        //       );

        //execute insert on target

        StringBuffer targetSql = new StringBuffer();
        targetSql.append("INSERT INTO ");
        targetSql.append(outputTableName);
        targetSql.append(" (");
        StringBuffer targetValues = new StringBuffer();

        for (int i = 0; i < toColumnName.length; i++) {
            targetSql.append(toColumnName[i]);
            targetSql.append(",");

            targetValues.append("?,");
        }


        targetSql.append(COL_TIMESTAMP + ") ");

        targetSql.append("VALUES (");
        targetSql.append(targetValues.toString());
        targetSql.append("CURRENT_TIMESTAMP");
        targetSql.append(")");

        PreparedStatement targetStmt = target.prepareStatement(targetSql.toString());

        //execute select on source
        //we have two source sql
        //first one does simple select
        //second one does a select and limits the select result based
        //on previously fetched records
        //so first one is only used to get some initial set of records
        //then we always use second one.
        //Also note that second sql may be null if user does not specify
        //columns to limit the records
        StringBuffer sourceSql = new StringBuffer();
        StringBuffer sourceSql2 = null;

        sourceSql.append("(SELECT ");
        for (int i = 0; i < fromColumnName.length; i++) {
            sourceSql.append(fromColumnName[i]);
            sourceSql.append(" as ");
            sourceSql.append(toColumnName[i]);
            if (i != fromColumnName.length - 1) {
                sourceSql.append(",");
            }

        }
        sourceSql.append(" FROM ");
        List<String> fromClause = op.getFromClause();
        if (fromClause != null && fromClause.size() > 0) {
            Iterator<String> fromIt = fromClause.iterator();
            while (fromIt.hasNext()) {
                String fromTable = fromIt.next();
                sourceSql.append(fromTable);
                if (fromIt.hasNext()) {
                    sourceSql.append(", ");
                }
            }
        }

        String whereClause = op.getWhereClause();
        if (whereClause != null && !whereClause.trim().equals("")) {
            sourceSql.append(" WHERE ");
//            whereClause = StringUtil.replacePrefixNotStartWithDot(whereClause, tableNameMap);
            sourceSql.append(whereClause);
        }
        
        sourceSql2 = new StringBuffer(sourceSql.toString());
        sourceSql.append(")");
        
        //add order by in  sourceSql
        if (isRecordIdentiferColumnsSpecified) {
            String[] columns = recordIdColumnSchema.getColumnNames();
            addOrderBy(sourceSql, columns);
        }
        
        

        //this is for sourceSql2, we execute sourceSql2
        //once we get first rows of values from sourceSql which
        //we wan to use in sourceSql2
        if (isRecordIdentiferColumnsSpecified) {
            if (sourceSql2.indexOf("WHERE") == -1) {
                sourceSql2.append(" WHERE ");
            } else {
                //already has WHERE from above
                sourceSql2.append(" AND ");
            }
            String[] columns = recordIdColumnSchema.getColumnNames();
            for (int i = 0; i < columns.length; i++) {
                String c = columns[i];
                String fromColumn = toColumnToFromColumnMap.get(c);
                sourceSql2.append(fromColumn);
                sourceSql2.append(" > ?");
                if (i != columns.length - 1) {
                    sourceSql2.append(" AND ");
                }
            }
            
            //add order by in  sourceSql2
            sourceSql2.append(")");
            addOrderBy(sourceSql2, columns);
            
        }


        PreparedStatement sourceStmt = null;
        PreparedStatement sourceStmt2 = null;

        
        sourceStmt = source.prepareStatement(sourceSql.toString());

        if (sourceSql2 != null) {
            sourceStmt2 = source.prepareStatement(sourceSql2.toString());
        }


        

        PreparedStatement insertRecordIdColumnsStmt = null;

        if (isRecordIdentiferColumnsSpecified) {
            //insert into record identifier column table
            StringBuffer insertRecordIdStmt = new StringBuffer();
            insertRecordIdStmt.append("INSERT INTO ");
            insertRecordIdStmt.append(op.getRecordIdentifierTableName());
            insertRecordIdStmt.append(" VALUES(");

            int columnCount = recordIdColumnSchema.getColumnCount();

            for (int i = 0; i < columnCount; i++) {
                insertRecordIdStmt.append("?");
                if (i != columnCount - 1) {
                    insertRecordIdStmt.append(",");
                }
            }

            insertRecordIdStmt.append(")");
            insertRecordIdColumnsStmt = target.prepareStatement(insertRecordIdStmt.toString());
        }


        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{op.getName(), targetSql.toString() + " " + sourceSql.toString()});
        }

        
        return new PreparedStatement[]{selectRecordIdColumnsStmt, deleteRecordIdColumnsStmt, sourceStmt, sourceStmt2, targetStmt, insertRecordIdColumnsStmt};
        
    }
    
    private void addOrderBy(StringBuffer sql, String[] columns) {
        sql.append(" ORDER BY ");
        for (int i = 0; i < columns.length; i++) {
            sql.append(columns[i]);
            
            if (i != columns.length - 1) {
                sql.append(",");
            }
        }
    }

}
