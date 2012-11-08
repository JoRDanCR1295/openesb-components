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
 * @(#)PartitionedWindow.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.util.HashMap;
import java.util.logging.Level;

/**
 * PartitionedWindow.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class PartitionedWindow extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(PartitionedWindow.class);

    protected List<String> mAttributeList;
    protected int mSize;
    
    public PartitionedWindow(Map prop) {
        initialize(prop);
        mAttributeList = getStrList((String)prop.get(PROP_ATTRIBUTE_LIST));
        mSize = PropertyUtil.getint(prop, PROP_SIZE, 0);
    }

    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    private String getIndexName() {
        return getIndexName(mQueryPlan.getId(), mId) + "_" + COL_PSEQID;
    }

    private void createIndex(Connection con) throws Exception {
        // CREATE INDEX I_planId_opId ON Table (R1_SeqId, R2_SeqId, ...);
        String indexName = getIndexName();
        String tableName = getQueueName();
        List<String> columnNameList = new ArrayList<String>();
        columnNameList.add(COL_PSEQID);
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
        //CREATE TABLE tableName (OutputSchema.columns, PSeqId Numeric, SeqId Numeric, Timestamp Timestamp, Tag Char);
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        List<ColumnMetadata> extraColumns = new ArrayList<ColumnMetadata>();
        extraColumns.add(new ColumnMetadata(COL_PSEQID, "INTEGER"));
        boolean autoGenIfPossible = false;
        mDbSpecial.createRelation(con, tableName, schema, extraColumns, autoGenIfPossible);
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
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        mOperateStmt = mDbSpecial.getPartitionedWindowDb().createOperateStatements(con, this);
    }
    
    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mOperateStmt[0].setTimestamp(1, prevT);
        mOperateStmt[0].setTimestamp(2, curT);
        mOperateStmt[0].executeUpdate();

        mOperateStmt[1].executeUpdate();

        mOperateStmt[2].setTimestamp(1, prevT);
        mOperateStmt[2].setTimestamp(2, curT);
        mOperateStmt[2].executeUpdate();
    }

    public List<String> getAttributeList() {
        return mAttributeList;
    }
    
    public int getSize() {
        return mSize;
    }
    
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_SIZE, new Long(mSize));
        map.put(PROP_ATTRIBUTE_LIST,mAttributeList);
        return map;
    }

    
}        
