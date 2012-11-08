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
import java.sql.PreparedStatement;
import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Level;


/**
 * NotificationStream.java
 *
 * Created on April 8, 2007 AM
 *
 * @author Bing Lu
 */
public class NotificationStream extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(NotificationStream.class);

    protected long mSize;
    
    public NotificationStream(Map prop) throws Exception {
        initialize(prop);
        double size = PropertyUtil.getdouble(prop, PROP_SIZE, 0.001);
        String unit = PropertyUtil.getString(prop, PROP_UNIT, TIME_UNIT_SECOND);
        mSize = PropertyUtil.getMiliseconds(size, unit);
    }
    
    public String getOutputType() {
        return IO_TYPE_STREAM;
    }

    public String getTableName() {
        return "t_" + mQueryPlan.getId() + "_" +  mId;
    }
        
    private void createTable(Connection con) throws Exception {
        String name = getTableName();
        Statement stmt = null;
        try {
            Schema inSchema = mInputOperatorList.get(0).getOutputSchema();
            List<String> columnMetadataAsList = new ArrayList<String>();
            columnMetadataAsList.addAll(inSchema.getColumnMetadataAsList());

            columnMetadataAsList.add(COL_SEQID);
            columnMetadataAsList.add(SQL_TYPE_NUMERIC);
            columnMetadataAsList.add(ColumnMetadata.SIZE_NOT_SPECIFIED + "");
            columnMetadataAsList.add(ColumnMetadata.SCALE_NOT_SPECIFIED + "");
            
            columnMetadataAsList.add(COL_TIMESTAMP);
            columnMetadataAsList.add(SQL_TYPE_TIMESTAMP);
            columnMetadataAsList.add(ColumnMetadata.SIZE_NOT_SPECIFIED + "");
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
    protected void createOutputQueue(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        mDbSpecial.createStream(con, tableName, schema, new ArrayList<ColumnMetadata>(), true);
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
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }
    
    @Override
    protected void createCheckInputStmt(Connection con) throws Exception {
        int inputIdCount = mInputOperatorList.size();
        mCheckInputStmt = new PreparedStatement[inputIdCount + 1];  // 1 for table T
        for (int i = 0;  i < inputIdCount; i++) {
            String inputTableName = mInputOperatorList.get(i).getQueueName();
            mCheckInputStmt[i] = Util.createHasRowsBetweenTimestampStmt(con, inputTableName);
            //mCheckInputStmt[i].setFetchSize(1);
            mCheckInputStmt[i].setMaxRows(1);
            
        }
        String tableName = getTableName();
        mCheckInputStmt[inputIdCount] = Util.createHasRowsBetweenTimestampStmt(con, tableName);
        //mCheckInputStmt[inputIdCount].setFetchSize(1);
        mCheckInputStmt[inputIdCount].setMaxRows(1);
    }
    
    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        // Prepare mOperateStmt:
        mOperateStmt = mDbSpecial.getNotificationStreamDb().createOperateStatements(con, this);
    }

    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mDbSpecial.getNotificationStreamDb().executeOperateStatements(this, prevT, curT);
    }

    public long getSize() {
        return mSize;
    }

    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_SIZE, new Long(mSize));
        return map;
    }

    public void setAdministrableProperty(String propName, Object propValue) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}

        
