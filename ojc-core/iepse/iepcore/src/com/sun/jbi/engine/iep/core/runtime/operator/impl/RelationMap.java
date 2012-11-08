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
 * @(#)RelationMap.java 
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
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;

/**
 * RelationMap.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class RelationMap extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(RelationMap.class);
    
    private static final String SEQID_COLUMN_APPEND = "_seqid";
    private List<String> mFromColumnList;
    private List<String> mToColumnList; // exp -> asName
    private String mFromClause;
    private String mWhereClause;
    private String mRWhereClause;
    
    public RelationMap(Map prop) {
        initialize(prop);
        mFromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mToColumnList = getStrList((String)prop.get(PROP_TO_COLUMN_LIST));
        mFromClause = " " + (String)prop.get(PROP_FROM_CLAUSE) + " ";
        mWhereClause = (String)prop.get(PROP_WHERE_CLAUSE);
        if (mWhereClause != null && !mWhereClause.trim().equals("")) {
            Map<String, String> tableNameMap = new HashMap<String, String>();
            for (int i = 0, I = mInputOperatorList.size(); i < I; i++) {
                Operator inputOp = mInputOperatorList.get(i);
                String inputName = inputOp.getName();
                String inputTableName = inputOp.getQueueName();
                tableNameMap.put(inputName, inputTableName);
            }
            mRWhereClause = StringUtil.replacePrefixNotStartWithDot(mWhereClause, tableNameMap);
        } else {
            mRWhereClause = "";
        }
    }
    
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    private String getIndexName() {
        return getIndexName(mQueryPlan.getId(), mId) + "_all";
    }
        
    private void createIndex(Connection con) throws Exception {
        // CREATE INDEX I_planId_opId ON Table (R1_SeqId, R2_SeqId, ...);
        String indexName = getIndexName();
        String tableName = getQueueName();
        List<String> columnNameList = new ArrayList<String>();
        for (int i = 0, I = mInputOperatorList.size();  i < I; i++) {
            String inputTableName = mInputOperatorList.get(i).getQueueName();
            columnNameList.add(getSeqIdColumnName(inputTableName));
        }
        mDbSpecial.createIndex(con, indexName, tableName, columnNameList);
    }
    
    private void dropIndex(Connection con) throws Exception {
        // DROP INDEX I_planId_opId
        String indexName = getIndexName();
        String tableName = getQueueName();
        mDbSpecial.dropIndex(con, indexName, tableName);
    }
    
    // template method: used by deploy()
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        //CREATE TABLE tableName (OutputSchema.columns, input1_SeqId Numeric, input2_SeqId Numeric, SeqId Numeric, Ts Timestamp, Tag Char, ...);
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        List<ColumnMetadata> extraColumns = new ArrayList<ColumnMetadata>();
        for (int i = 0, I = mInputOperatorList.size();  i < I; i++) {
            String inputTableName = mInputOperatorList.get(i).getQueueName();
            ColumnMetadata cm = new ColumnMetadata(getSeqIdColumnName(inputTableName), SQL_TYPE_NUMERIC);
            extraColumns.add(cm);
        }
        mDbSpecial.createRelation(con, tableName, schema, extraColumns, true);
        createIndex(con);
    }

    // template method: used by undeploy()
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        // drop index before drop the table on which it is defined
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
        mOperateStmt = mDbSpecial.getRelationMapDb().createOperateStatements(con, this);
    }

    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mDbSpecial.getRelationMapDb().executeOperateStatements(this, prevT, curT);
    }

    public List<String> getFromColumnList() {
        return mFromColumnList;
    }

    public List<String> getToColumnList() {
        return mToColumnList;
    }
    
    public String getFromClause() {
        return mFromClause;
    }
    
    public String getWhereClause() {
        return mWhereClause;
    }
    
    public String getRWhereClause() {
        return mRWhereClause;
    }
    
    public String getSeqIdColumnName(String inputTableName) {
        return inputTableName + SEQID_COLUMN_APPEND;
    }
    
}

        
