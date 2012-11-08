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
 * @(#)RandomInsertCommand.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

/**
 * RandomInsertCommand.java
 *
 * Created on June 2, 2006, 11:35 AM
 *
 * @author Rahul Dwivedi
 */
public class RandomInsertCommand implements Command {
    private static final Messages mMessages = Messages.getMessages(RandomInsertCommand.class);
    
    private static SimpleDateFormat mSDF = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
    
    private Properties mConfigProp;
    private Connection mCon = null;
    private PreparedStatement mStmt = null;
    private PreparedStatement mClearStmt = null;
    private QueryPlan mPlan = null;
    private Operator mOperator = null;
    private RandomEventGenerator mRandomEventGenerator = null;
    String mBatcheSize = null;
    String mTotalBatches = null;
    private Runnable mWait = null;
    
    /*
     *command.5.name=RandomInserts
    command.5.type=com.sun.jbi.engine.iep.core.runtime.client.pojo.RandomInsertCommand
    command.5.props=instanceId=zmart_iep;opId=o0;rFile=randomFileDescription
     */
    
    
    
    public RandomInsertCommand() {
        super();
    }
    
    
    public void init(NWaySheperd sheperd, String props) {
        
        try {
            mConfigProp = PropertyUtil.getPropertiesFromString(props, "=", ";");
            mConfigProp.putAll(System.getProperties());
            mCon = Util.getConnection(mConfigProp);
            String instanceId = (String)mConfigProp.get("instanceId");
            String opId = PropertyUtil.getString(mConfigProp, "opId", "");
            String randomDescriptionFile = PropertyUtil.getString(mConfigProp, "rFile", "");
            mBatcheSize = PropertyUtil.getString(mConfigProp, "batchSize", "");
            String pauseTimeInSeconds = PropertyUtil.getString(mConfigProp, "pauseTime", "");
            mTotalBatches = PropertyUtil.getString(mConfigProp, "tBatches", "");
            mPlan = Util.getPlanByInstanceId(mConfigProp, instanceId);
            mOperator = mPlan.getOperatorById(opId);
            mRandomEventGenerator = new RandomEventGenerator(randomDescriptionFile);
            mStmt = ((Inserter)mOperator).getInsertStatement(mCon,true);
            String outputTableName = mOperator.getQueueName();
            mClearStmt = Util.createCleanStreamByMinUsageTimeStmt(mCon, mPlan.getId(), outputTableName);
            String[] args = {pauseTimeInSeconds,pauseTimeInSeconds};
            WaitCommand wait = new WaitCommand();
            mWait = wait.createRunnable(null,args);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "RandomInsertCommand.init_fails", e);
        }
    }
   
    public Runnable createRunnable(final Map<String, Input> inputTable, final String[] args) {
        return new Runnable() {
            public void run() {
                //Input input = (Input)inputTable.get(args[1]);
                //assuming there is only one row of description .
                List<RandomData> row = mRandomEventGenerator.getRowDescriptionList().get(0);
                int batches = 1;
                try {
                     batches = Integer.parseInt(mBatcheSize);
                } catch (Exception e) {
                }
                int tNofBatches = 1;
                try {
                     tNofBatches = Integer.parseInt(mTotalBatches);
                } catch (Exception e) {
                }
                try {
                    String outputTableName = mOperator.getQueueName();
                    int colCnt = mOperator.getOutputSchema().getColumnCount();
                    Timestamp minUsageTimeStamp = Util.getTableUsage(mCon, mPlan.getId(), outputTableName);
                    for (int j = 0; j < tNofBatches; j++) {
                    if (minUsageTimeStamp != null) {
                        StringBuffer sb = new StringBuffer();
                        mClearStmt.executeUpdate();
                        for (int i = 0; i < batches; i++) {
                            if (0 < i) {
                                sb.append("\n");
                            }
                            sb.append("(");
                            //String data = input.nextData();
                            //Object[] row = input.getCurRow();
                            for (int k = 0; k < colCnt; k++) {
                                
                                mStmt.setObject(k+1, row.get(k).getData());
                                if (0 < k) {
                                    sb.append(",");
                                }
                                sb.append(row.get(k).getData().toString());
                            }
                            mStmt.addBatch();
                            sb.append(")");
                        }
                        mStmt.executeBatch();
                        //System.out.println("Insert into " + outputTableName + ": \n" + sb.toString() + "\nat " + mSDF.format(new Date()));
                    }
                    mWait.run();
                    }
                } catch (Exception e) {
                    mMessages.log(Level.SEVERE, "RandomInsertCommand.run_fails", e);
                }
            }
        };
    }

    public void destroy() {
        Util.close(mCon);
    }
    
}
