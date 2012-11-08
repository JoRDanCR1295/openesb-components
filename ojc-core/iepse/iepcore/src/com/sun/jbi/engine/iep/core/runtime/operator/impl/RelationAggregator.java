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
 * @(#)RelationAggregator.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.ArrayList;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.Statement;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import java.util.logging.Level;

/**
 * RelationAggregator.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class RelationAggregator extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(RelationAggregator.class);

    private List<String> mFromColumnList;
    private List<String> mRFromColumnList;
    private List<String> mToColumnList; // exp -> asName
    private List<String> mGroupByColumnList;
    private boolean mHasGroupByColumnList;
    private String mWhereClause;
    private boolean mHasWhereClause;
    private String mRWhereClause;
    
    public RelationAggregator(Map prop) throws Exception {
        initialize(prop);
        Operator inputOp = mInputOperatorList.get(0);
        Schema inputSchema = inputOp.getOutputSchema();
        String[] columnNames = inputSchema.getColumnNames();

        // Create the map for inputName.columnName -> columnName
        Map<String, String> prefix_columnNameMap = new HashMap<String, String>();
        String inputName = inputOp.getName();
        for (String columnName : columnNames) {
            prefix_columnNameMap.put(inputName + "." + columnName, columnName);
        }
        // Create the map for columnName -> r.columnName
        Map<String, String> r_columnNameMap = new HashMap<String, String>();
        for (String columnName : columnNames) {
            r_columnNameMap.put(columnName, "r." + columnName);
        }
        
        // inputName.columName -> columnName
        List<String> fromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mFromColumnList = new ArrayList<String>(fromColumnList.size());
        for (String fromColumn : fromColumnList) {
            mFromColumnList.add(StringUtil.replaceWord(fromColumn, prefix_columnNameMap));
        }
        mRFromColumnList = new ArrayList<String>(mFromColumnList.size());
        for (String s : mFromColumnList) {
            mRFromColumnList.add(StringUtil.replaceWord(s, r_columnNameMap));
        }
        
        mToColumnList = getStrList((String)prop.get(PROP_TO_COLUMN_LIST));

        // inputName.columName -> columnName
        List<String> groupByList = getStrList((String)prop.get(PROP_GROUP_BY_COLUMN_LIST));
        mGroupByColumnList = new ArrayList<String>(groupByList.size());
        for (String s : groupByList) {
            mGroupByColumnList.add(StringUtil.replaceWord(s, prefix_columnNameMap));
        }
        mHasGroupByColumnList = mGroupByColumnList != null && !mGroupByColumnList.isEmpty();
        String whereClause = (String)prop.get(PROP_WHERE_CLAUSE);
        mHasWhereClause = whereClause != null && !whereClause.trim().equals("");
        if (mHasWhereClause) {
            // inputName.columName -> columnName
            mWhereClause = StringUtil.replaceWord(whereClause, prefix_columnNameMap);
            
            mRWhereClause = StringUtil.replaceWord(mWhereClause, r_columnNameMap);
        } else {
            mRWhereClause = "";
        }
    }
    
    public String getSupportingColumnName(String columnName) {
        return "ems_sc_" + columnName;
    }
    
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    private String getIndexName() {
        return getIndexName(mQueryPlan.getId(), mId) + "_all";
    }
        
    private void createIndex(Connection con) throws Exception {
        if (!mHasGroupByColumnList) {
            return;
        }
        // CREATE INDEX I_planId_opId ON R (ems_sc_groupByColumnList);
        String indexName = getIndexName();
        String tableName = getQueueName();
        List<String> columnNameList = new ArrayList<String>();
        for (int i = 0, I = mGroupByColumnList.size();  i < I; i++) {
            String columnName = mGroupByColumnList.get(i);
           columnNameList.add(getSupportingColumnName(columnName));
        }
        mDbSpecial.createIndex(con, indexName, tableName, columnNameList);
    }
    
    private void dropIndex(Connection con) throws Exception {
        if (!mHasGroupByColumnList) {
            return;
        }
        // DROP INDEX I_planId_opId
        String indexName = getIndexName();
        String tableName = getQueueName();
        mDbSpecial.dropIndex(con, indexName, tableName);
    }

    public String getViewName() {
        return "v_" + mQueryPlan.getId() + "_" +  mId;
    }
        
    private void createView(Connection con) throws Exception {
        String name = getViewName();
        Statement stmt = null;
        //Note: If the RelationAggregator has fields f1, f2 and f3, and the aggregation is done only on f1, then
        //f2 and f3 will need to be in the group by clause. In that case, the view will be created by selecting
        //distinct Timestamp, filed2 and field3.  If all the fields are used for aggregation and therefore no group 
        //by fields exist, then the view will be created by only selecting distinct Timestamp. In the former case,
        //we want to do the aggregation for every distinct timestamp, f2 and f3. In the latter case, we want to do
        //aggregation for any distinct timestamp, irrespective of what the values of f1, f2 or f3 are. The view
        //helps in doing this aggregation.
        try {
            String inputTableName = mInputOperatorList.get(0).getQueueName();
            String[] columnNames = mHasGroupByColumnList ? 
            		(String[])mGroupByColumnList.toArray(new String[0]) : new String[0];
            StringBuffer sb = new StringBuffer();
            sb.append("CREATE VIEW " + name + " (");
            for (int i = 0; i < columnNames.length; i++) {
                sb.append(columnNames[i] + ", ");
            }
            sb.append(COL_TIMESTAMP + ") AS ");
            sb.append("SELECT DISTINCT ");
            for (int j = 0; j < columnNames.length; j++) {
                sb.append(columnNames[j] + ", ");
            }
            sb.append(COL_TIMESTAMP + " FROM " + inputTableName);
            if (mHasWhereClause) {
                sb.append(" WHERE " + mWhereClause);
            }
            stmt = con.createStatement();
            stmt.execute(sb.toString());
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_create_view", name, e);
        } finally {
            Util.close(stmt);
        }
    }
    
    private void dropView(Connection con) throws Exception {
        // DROP VIEW v_planId_opId
        String name = getViewName();
        Statement stmt = null;
        try {
            stmt = con.createStatement();
            stmt.execute("DROP VIEW " + name);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_drop_view", name, e);
        } finally {
            Util.close(stmt);
        }
    }
    
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        Schema inSchema = mInputOperatorList.get(0).getOutputSchema();
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        List<ColumnMetadata> extraColumns = new ArrayList<ColumnMetadata>();
        for (int i = 0, I = mGroupByColumnList.size(); i < I; i++) {
            String columnName = mGroupByColumnList.get(i);
            ColumnMetadata cm = inSchema.getColumnMetadata(columnName);
            extraColumns.add(new ColumnMetadata(getSupportingColumnName(columnName),
                                               cm.getColumnType(), 
                                               cm.getColumnSize(), 
                                               cm.getColumnScale()));
        }
        mDbSpecial.createRelation(con, tableName, schema, extraColumns, true);
        createIndex(con);
        createView(con);
    }

    // template method: used by undeploy()
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        // drop index before drop the table on which it is defined
        dropView(con);
        dropIndex(con);
        Util.dropTable(con, tableName);
    }

    @Override
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }
    
    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        // Prepare mOperateStmt:
        mOperateStmt = mDbSpecial.getRelationAggregatorDb().createOperateStatements(con, this);
    }

    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mDbSpecial.getRelationAggregatorDb().executeOperateStatements(this, prevT, curT);
    }

    public List<String> getRFromColumnList() {
        return mRFromColumnList;
    }

    public List<String> getToColumnList() {
        return mToColumnList;
    }
    
    public List<String> getGroupByColumnList() {
        return mGroupByColumnList;
    }
    
    public boolean hasGroupByColumnList() {
        return mHasGroupByColumnList;
    }
    
    public boolean hasWhereClause() {
        return mHasWhereClause;
    }
    
    public String getRWhereClause() {
        return mRWhereClause;
    }
    
}

        
