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


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.sql.Timestamp;
import java.sql.Connection;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.Collections;

/**
 * UnionAll.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class UnionAll extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(UnionAll.class);

    private static final String SEQID_COLUMN_APPEND = "_seqid";
    
    public UnionAll(Map prop) {
        initialize(prop);
    }
    
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    private String getIndexName() {
        return getIndexName(mQueryPlan.getId(), mId) + "_all";
    }
        
    private void createIndex(Connection con) throws Exception {
        // CREATE INDEX I_planId_opId_all ON R (InputId, InputSeqId);
        String indexName = getIndexName();
        String tableName = getQueueName();
        List<String> columnNameList = new ArrayList<String>();
        columnNameList.add(COL_INPUT_ID);
        columnNameList.add(COL_INPUT_SEQID);
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
        //CREATE TABLE tableName (OutputSchema.columns, InputId Numeric, InputSeqId Numeric, SeqId Numeric, Ts Timestamp, Tag Char, ...);
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        List<ColumnMetadata> extraColumns = new ArrayList<ColumnMetadata>();
        ColumnMetadata cm = new ColumnMetadata(COL_INPUT_ID, SQL_TYPE_NUMERIC);
        extraColumns.add(cm);
        cm = new ColumnMetadata(COL_INPUT_SEQID, SQL_TYPE_NUMERIC);
        extraColumns.add(cm);
        mDbSpecial.createRelation(con, tableName, schema, extraColumns, true);
        createIndex(con);
    }

    // template method: used by undeploy()
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        // drop index before drop the table on which it is defined
        dropIndex(con);
        Util.dropTable(con, tableName);
    }

    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }
    
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        // Prepare mOperateStmt:
        mOperateStmt = mDbSpecial.getUnionAllDb().createOperateStatements(con, this);
    }

    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mDbSpecial.getUnionAllDb().executeOperateStatements(this, prevT, curT);
    }

    public String getSeqIdColumnName(String inputTableName) {
        return inputTableName + SEQID_COLUMN_APPEND;
    }
    
}

        

