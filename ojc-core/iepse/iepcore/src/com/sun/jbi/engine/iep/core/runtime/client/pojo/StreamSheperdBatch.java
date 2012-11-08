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
 * @(#)StreamSheperdBatch.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.text.SimpleDateFormat;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Date;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.logging.Level;

/**
 * StreamSheperdBatch.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */
public class StreamSheperdBatch  {
    private static final Messages mMessages = Messages.getMessages(StreamSheperdBatch.class);

    private SimpleDateFormat mSDF = new SimpleDateFormat("yyyy.MM.dd G 'at' HH:mm:ss z");
    private QueryPlan mPlan = null;
    private Operator mOperator = null;
    private String mFileName;
    private int mRepeat;
    
    public StreamSheperdBatch(String instanceId, String opId, String fileName, int repeat) {
        mPlan = Util.getPlanByInstanceId(System.getProperties(), instanceId);
        mOperator = mPlan.getOperatorById(opId);
        mFileName = fileName;
        mRepeat = repeat;
    }

    public void run() throws Exception {
        // read and cache all input
        List<String[]> rowList = new ArrayList<String[]>();
        BufferedReader fileIn = null;
        int colCnt = 0;
        try {
            fileIn = new BufferedReader(new InputStreamReader(IOUtil.getResourceAsStream(mFileName)));
            String colNames = fileIn.readLine();
            StringTokenizer st = new StringTokenizer(colNames, ",");
            colCnt = st.countTokens();
            while (true) {
                String record = fileIn.readLine();
                if (record == null) {
                    break;
                }
                String[] row = new String[colCnt];
                st = new StringTokenizer(record, ",");
                for (int i = 0; i < colCnt; i++) {
                    if (st.hasMoreTokens()) {
                        row[i] = st.nextToken();
                    } else {
                        row[i] = "";
                    }
                }
                rowList.add(row);
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("StreamSheperdBatch.run_fails"), e);
        } finally {
            try {
                if (fileIn != null) {
                    fileIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "StreamSheperdBatch.Closing_fileIn_fails", e1);
            }
        }
        
        Connection con = null;
        PreparedStatement insertStmt = null;
        PreparedStatement clearStmt = null;
        try {
            String planId = mPlan.getId();
            String outputTableName = mOperator.getQueueName();
            con = Util.getConnection();
            con.setAutoCommit(false);
            insertStmt = ((Inserter)mOperator).getInsertStatement(con,true);
            clearStmt = Util.createCleanStreamByMinUsageTimeStmt(con, planId, outputTableName);

            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "StreamSheperdBatch.Start_Processing_at", mSDF.format(new Date()));
            }    
            long startTime = System.currentTimeMillis();
            for (int i = 0; i < mRepeat; i++) {
                for (int j = 0, J = rowList.size(); j < J; j++) {
                    String[] row = rowList.get(j);
                    
                    for (int k = 0; k < colCnt; k++) {
                        insertStmt.setObject(k+1, row[k]);
                    }
                    insertStmt.addBatch();
                }
            }
            Timestamp minUsageTimeStamp = Util.getTableUsage(con, planId, outputTableName);
            if (minUsageTimeStamp != null) {
                if (mOperator.getGarbageCollectionEnabled()) {
                    clearStmt.executeUpdate();
                }
                insertStmt.executeBatch();
                con.commit();
            }
            if (mMessages.isLoggable(Level.FINE)) {
                long endTime = System.currentTimeMillis();
                mMessages.log(Level.FINE, "StreamSheperdBatch.Finish_processing_at", mSDF.format(new Date()));
                long totalTime =  endTime - startTime;
                mMessages.log(Level.FINE, "StreamSheperdBatch.records_are_processed_in_milliseconds", new Object[]{mRepeat * rowList.size(), totalTime});
            }    
            
        } catch (Exception e) {
            Util.rollback(con);
            mMessages.log(Level.SEVERE, "StreamSheperdBatch.run_fails", e);
            throw e;
        } finally {
            Util.close(insertStmt);
            Util.close(clearStmt);
            Util.close(con);
        }
    }                    
    
    public static void main(String[] args) {
        if (args.length < 4) {
            mMessages.logOriginal(Level.INFO, "java com.sun.jbi.engine.iep.core.runtime.util.StreamSheperdBatch instanceId opId fileName repeat");
            return;
        }
        try {
            String instanceId = args[0];
            String opId = args[1];
            String fileName = args[2];
            int repeat = Integer.parseInt(args[3]);
            StreamSheperdBatch ss = new StreamSheperdBatch(instanceId, opId, fileName, repeat);
            ss.run();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "StreamSheperdBatch.main_fails", e);
        }
    }
    
}
