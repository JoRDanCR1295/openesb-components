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
 * @(#)GapWindow.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.Statement;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.ResultSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Level;


/**
 * GapWindow.java
 *
 * Created on April 8, 2007, 12:47 AM
 *
 * @author Bing Lu
 */
public class GapWindow extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(GapWindow.class);

    private long mStart;
    protected String mAttribute; // sequence id 
    private List<String> mAttributeList; // partition key
    private Timestamp mInputUsageTimestamp = new Timestamp(0L);
    
    public GapWindow(Map prop) throws Exception {
        initialize(prop);
        mStart = PropertyUtil.getlong(prop, PROP_START, 0);
        mAttribute = PropertyUtil.getString(prop, PROP_ATTRIBUTE, "");
        mAttributeList = getStrList((String)prop.get(PROP_ATTRIBUTE_LIST));
    }
    
    public String getTableName() {
        return "t_" + mQueryPlan.getId() + "_" +  mId;
    }
        
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    private void createTable(Connection con) throws Exception {
        String name = getTableName();
        Statement stmt = null;
        try {
            Schema inSchema = mInputOperatorList.get(0).getOutputSchema();
            List<String> columnMetadataAsList = new ArrayList<String>();
            for (String columnName : mAttributeList) {
                ColumnMetadata cm = inSchema.getColumnMetadata(columnName);
                columnMetadataAsList.add(cm.getColumnName());
                columnMetadataAsList.add(cm.getColumnType());
                columnMetadataAsList.add(cm.getColumnSize() + "");
                columnMetadataAsList.add(cm.getColumnScale() + "");
            }
            
            ColumnMetadata cm = inSchema.getColumnMetadata(mAttribute);
            
            columnMetadataAsList.add(COL_MISS_SEQ);
            columnMetadataAsList.add(cm.getColumnType());
            columnMetadataAsList.add(cm.getColumnSize() + "");
            columnMetadataAsList.add(cm.getColumnScale() + "");
            
            columnMetadataAsList.add(COL_MAX_SEQ);
            columnMetadataAsList.add(cm.getColumnType());
            columnMetadataAsList.add(cm.getColumnSize() + "");
            columnMetadataAsList.add(cm.getColumnScale() + "");
            
            columnMetadataAsList.add(COL_TIMESTAMP);
            columnMetadataAsList.add(SQL_TYPE_TIMESTAMP);
            columnMetadataAsList.add(ColumnMetadata.SIZE_NOT_SPECIFIED + "");
            columnMetadataAsList.add(ColumnMetadata.SCALE_NOT_SPECIFIED + "");
            
            columnMetadataAsList.add(COL_PROCESSING_TIME);
            columnMetadataAsList.add(SQL_TYPE_TIMESTAMP);
            columnMetadataAsList.add(ColumnMetadata.SIZE_NOT_SPECIFIED + "");
            columnMetadataAsList.add(ColumnMetadata.SCALE_NOT_SPECIFIED + "");
            
            columnMetadataAsList.add(COL_UPDATE);
            columnMetadataAsList.add(SQL_TYPE_CHAR);
            columnMetadataAsList.add("1");
            columnMetadataAsList.add(ColumnMetadata.SCALE_NOT_SPECIFIED + "");

            Schema tableSchema = new Schema("temp", columnMetadataAsList);
            mDbSpecial.createTable(con, name, tableSchema);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_create_table", name, e);
        } finally {
            Util.close(stmt);
        }
    }
    
    private void dropTable(Connection con) throws Exception {
        String name = getTableName();
        Util.dropTable(con, name);      
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
    protected void createOutputQueue(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        mDbSpecial.createRelation(con, tableName, schema, new ArrayList<ColumnMetadata>(),  true);
        createTable(con);
    }

    // template method: used by undeploy()
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        Util.dropTable(con, tableName);
        dropTable(con);
    }

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        // Prepare mOperateStmt:
        mOperateStmt = mDbSpecial.getGapWindowDb().createOperateStatements(con, this);
    }

    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mOperateStmt[0].executeUpdate();

        mOperateStmt[1].setTimestamp(1, prevT);
        mOperateStmt[1].setTimestamp(2, curT);
        mOperateStmt[1].setTimestamp(3, prevT);
        mOperateStmt[1].setTimestamp(4, curT);
        mOperateStmt[1].executeUpdate();
        
        mOperateStmt[2].setTimestamp(1, curT);
        mOperateStmt[2].setTimestamp(2, curT);
        mOperateStmt[2].setTimestamp(3, curT);
        mOperateStmt[2].setTimestamp(4, curT);
        mOperateStmt[2].setTimestamp(5, curT);
        mOperateStmt[2].setTimestamp(6, curT);
        mOperateStmt[2].setTimestamp(7, curT);
        mOperateStmt[2].setTimestamp(8, curT);
        mOperateStmt[2].executeUpdate();

        mOperateStmt[3].setTimestamp(1, curT);
        mOperateStmt[3].setTimestamp(2, curT);
        mOperateStmt[3].setTimestamp(3, curT);
        mOperateStmt[3].executeUpdate();
        
        mOperateStmt[4].executeUpdate();

        mOperateStmt[5].setTimestamp(1, prevT);
        mOperateStmt[5].executeUpdate();

        mOperateStmt[6].setTimestamp(1, curT);
        mOperateStmt[6].executeUpdate();
        
        mOperateStmt[7].setTimestamp(1, curT);
        ResultSet rs = mOperateStmt[7].executeQuery();
        if (rs.next()) {
            Timestamp t = rs.getTimestamp(1);
            if (t == null) {
                mInputUsageTimestamp.setTime(curT.getTime());
            } else {
                mInputUsageTimestamp.setTime(t.getTime() - 1);
            }
        }
    }

    @Override
    protected void executeUpdateInputUsageStmt(Timestamp curT) throws Exception {
        Timestamp min = curT.before(mInputUsageTimestamp)? curT : mInputUsageTimestamp;
        if (mUpdateInputUsageStmt != null) {
            for (int i = 0, I = mUpdateInputUsageStmt.length; i < I; i++) { 
                mUpdateInputUsageStmt[i].setTimestamp(1, min);
                mUpdateInputUsageStmt[i].executeUpdate();
            }
        }
    }
    
    public long getStart() {
        return mStart;
    }

    public List<String> getAttributeList() {
        return mAttributeList;
    }

    public String getAttribute() {
        return mAttribute;
    }
    @Override
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_START, new Long(mStart));
        map.put(PROP_ATTRIBUTE, mAttribute);
        map.put(PROP_ATTRIBUTE_LIST, mAttributeList);
        //map.put(MAXIMUM_DELAY_UNIT, unit);
        return map;
    }
    
}

        
