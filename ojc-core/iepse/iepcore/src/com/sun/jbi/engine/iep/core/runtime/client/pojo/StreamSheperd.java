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
 * @(#)StreamSheperd.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;

/**
 * StreamPrinter.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */
public class StreamSheperd implements Sheperd {
    private static final Messages mMessages = Messages.getMessages(StreamSheperd.class);

    private int mBatchSize;
    private int mRepeat;
    private List mRowList;
    private String[] mColumnNames;
    
    private Connection mCon = null;
    private PreparedStatement mStmt = null;
    private PreparedStatement mClearStmt = null;
    
    private QueryPlan mPlan = null;
    private Operator mOperator = null;
    
    public StreamSheperd() {
    }
    
    /**
     * SHEPERD_INSTANCE_ID
     * SHEPERD_OP_ID
     * SHEPERD_BATCH_SIZE
     * SHEPERD_REPEAT
     * SHEPERD_ROW_LIST
     * SHEPERD_COLUMN_NAMES
     */
    public void init(Map prop) {
        try {
            String instanceId = (String)prop.get(SHEPERD_INSTANCE_ID);
            mPlan = Util.getPlanByInstanceId(System.getProperties(), instanceId);

            String opId = PropertyUtil.getString(prop, SHEPERD_OP_ID, "");
            mOperator = mPlan.getOperatorById(opId);
            
            mBatchSize = PropertyUtil.getint(prop, SHEPERD_BATCH_SIZE, 1);
            mRepeat = PropertyUtil.getint(prop, SHEPERD_REPEAT, 1);
            mRowList = (List)prop.get(SHEPERD_ROW_LIST);
            mColumnNames = (String[]) prop.get(SHEPERD_COLUMN_NAMES);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "StreamSheperd.init_fails", e);
        } 
    }

    public void begin() throws Exception {
        mCon = Util.getConnection();
        String instanceId = mPlan.getInstanceId();
        String opId = mOperator.getId();
        String outputTableName = Util.getQueueName(System.getProperties(), instanceId, opId);

        mStmt = ((Inserter)mOperator).getInsertStatement(mCon,true);
        
        mClearStmt = Util.createCleanStreamByMinUsageTimeStmt(mCon, mPlan.getId(), outputTableName);
    }
    
    public int input(int index) throws Exception {
        int listSize = mRowList.size();
        int total = mRepeat * listSize;
        String outputTableName = Util.getQueueName(System.getProperties(), mPlan.getInstanceId(), mOperator.getId());
        Timestamp minUsageTimeStamp = Util.getTableUsage(mCon, mPlan.getId(), outputTableName);
        int colCnt = mColumnNames.length;
        if (minUsageTimeStamp != null) {
            if (mOperator.getGarbageCollectionEnabled()) {
                mClearStmt.executeUpdate();
            }
            Object[] row = null;
            for (int i = 0; i < mBatchSize; i++) {
                if (index < total) {
                    row = (Object[])mRowList.get(index%listSize);
                    index++;
                    for (int k = 0; k < colCnt; k++) {
                        mStmt.setObject(k+1, row[k]);
                    }
                    mStmt.addBatch();
                }
            }
            mStmt.executeBatch();
        }
        return index;
    }
    
    public void end() {
        Util.close(mStmt);
        Util.close(mClearStmt);
        Util.close(mCon);
    }
    
}
