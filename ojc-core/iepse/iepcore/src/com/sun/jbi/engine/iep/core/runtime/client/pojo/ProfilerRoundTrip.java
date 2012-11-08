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
 * @(#)ProfilerRoundTrip.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Notification;
import com.sun.jbi.engine.iep.core.runtime.operator.Notifier;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Properties;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class ProfilerRoundTrip implements OperatorConstants {
    private static final Messages mMessages = Messages.getMessages(ProfilerRoundTrip.class);
    
    private Properties mProperties;
    private QueryPlan mPlan;
    private Inserter mInput;
    private Notifier mOutput;
    private String[] mRowTemplate;
    private ArrayList mRows;
    private int mInputBatchSize;
    private int mOutputBatchSize;
    private int mTotalBatches;
    private String mLogFile;

    public ProfilerRoundTrip(Properties properties) {
        mProperties = properties;    
        String inputTemplateFile = mProperties.getProperty("InputTemplateFile");
        mLogFile = mProperties.getProperty("LogFile");

        mInputBatchSize = PropertyUtil.getint(mProperties, "InputBatchSize", 10);
        mOutputBatchSize = PropertyUtil.getint(mProperties, "OutputBatchSize", 10);
        mTotalBatches = PropertyUtil.getint(mProperties, "TotalBatches", 1);
        
        String instanceId = mProperties.getProperty("InstanceId");
        String streamIn = mProperties.getProperty("StreamInput");
        String streamOut = mProperties.getProperty("StreamOutput");
        
        mPlan = Util.getPlanByInstanceId(mProperties, instanceId);
        mInput = (Inserter)mPlan.getOperatorByName(streamIn);
        mOutput = (Notifier)mPlan.getOperatorByName(streamOut);

        BufferedReader fileIn = null;
        int colCnt = 0;
        try {
            fileIn = new BufferedReader(new InputStreamReader(IOUtil.getResourceAsStream(inputTemplateFile)));
            String record = fileIn.readLine();
            if (record == null) {
                return;
            }
            // skip comments
            while (record.startsWith("#")) {
                record = fileIn.readLine(); 
                if (record == null) {
                    return;
                }
            }
            // load template 
            StringTokenizer st = new StringTokenizer(record, ",");
            colCnt = st.countTokens();
            mRowTemplate = new String[colCnt];
            for (int i = 0; i < colCnt; i++) {
                mRowTemplate[i] = st.nextToken();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "ProfilerRoundTrip.Fail_to_load_data_template", inputTemplateFile, e);
            return;
        } finally {
            try {
                if (fileIn != null) {
                    fileIn.close();
                }
            } catch (Exception e1) {
            }
        }
            
        // prepare data
        mRows = new ArrayList(mInputBatchSize);
        for (int i = 0; i < mInputBatchSize; i++) {
            String[] row = new String[mRowTemplate.length];
            for (int j = 0; j < mRowTemplate.length; j++) {
                row[j] = StringUtil.replaceAll(mRowTemplate[j], "${counter}", i + "");
            }    
            mRows.add(row);
        }    
    }

    public void run() throws Exception {
        Thread feeder = null;
        Thread consumer = null;
        long totalTime = 0;
        StringBuffer report = new StringBuffer();
        report.append("Configuration:\n");
        report.append("\tDatabaseType = " + mProperties.get("DatabaseType") + "\n");
        report.append("\tDatabaseHostname = " + mProperties.get("DatabaseHostname") + "\n");
        report.append("\tDatabaseUsername = " + mProperties.get("DatabaseUsername") + "\n");
        report.append("\tDatabasePassword = " + mProperties.get("DatabasePassword") + "\n");
        report.append("\tDatabaseSid = " + mProperties.get("DatabaseSid") + "\n");
        report.append("\tDatabaseSchemaName = " + mProperties.get("DatabaseSchemaName") + "\n");
        report.append("\tInstanceId = " + mProperties.get("InstanceId") + "\n");
        report.append("\tStreamInput = " + mProperties.get("StreamInput") + "\n");
        report.append("\tStreamOutput = " + mProperties.get("StreamOutput") + "\n");
        report.append("\tInputTemplateFile = " + mProperties.get("InputTemplateFile") + "\n");
        report.append("\tInputBatchSize = " + mProperties.get("InputBatchSize") + "\n");
        report.append("\tOutputBatchSize = " + mProperties.get("OutputBatchSize") + "\n");
        report.append("\tTotalBatches = " + mProperties.get("TotalBatches") + "\n");
        report.append("Throughput:\n");
        for (int i = 0; i < mTotalBatches; i++) {
            mMessages.printlnOriginal("Starting benchmark run (" + i + ") .. Message batch size (" + mInputBatchSize + ")");
            feeder = new Thread("Feeder") {
                @Override
                public void run() {
                    Connection con = null;                    
                    PreparedStatement insertStmt = null;
                    String queueName = ((Operator)mInput).getQueueName();
                    try {
                        con = Util.getConnection(mProperties);
                        con.setAutoCommit(false);
                        insertStmt = mInput.getInsertStatement(con, true);
                        if (mRows.size() > 1) {
                            insertStmt.clearBatch();
                            for (int i = 0, I = mRows.size(); i < I; i++) {
                                Object[] row = (Object[]) mRows.get(i);
                                for (int j = 0; j < row.length; j++) {
                                    insertStmt.setObject(j + 1, row[j]);
                                }
                                insertStmt.addBatch();
                            }
                            insertStmt.executeBatch();
                        } else {
                            Object[] row = (Object[]) mRows.get(0);
                            for (int j = 0; j < row.length; j++) {
                                insertStmt.setObject(j + 1, row[j]);
                            }
                            insertStmt.executeUpdate();
                        }
                        con.commit();
                    } catch (Exception e) {
                        mMessages.log(java.util.logging.Level.SEVERE, "ProfilerRoundTrip.Fail_to_insert_data_into_table", queueName, e);
                    } finally {
                        Util.close(insertStmt);
                        Util.close(con);
                    }                    
                }
            };
            consumer = new Thread("Consumer") {
                @Override
                public void run() {
                    Connection con = null;                    
                    String queueName = ((Operator)mInput).getQueueName();
                    try {
                        con = Util.getConnection(mProperties);
                        con.setAutoCommit(false);
                        int totalFetched = 0;
                        Timestamp prevTs = new Timestamp(0);
                        while (totalFetched < mOutputBatchSize) {
                            Timestamp ts = Util.getPrevTimestampToCheck(con, mPlan.getId());
                            while (!prevTs.before(ts)) {
                                try {
                                    sleep(1000);
                                } catch (InterruptedException e) {
                                }
                                ts = Util.getPrevTimestampToCheck(con, mPlan.getId());
                            }
                            mOutput.fetch(con, ts);
                            List<Notification> batch = mOutput.getNotificationBatch(mOutput.getCacheSize());
                            prevTs = ts;
                            totalFetched += batch.size();
                            mOutput.removeNotificationBatch(con, batch);
                            con.commit();
                        }    
                    } catch (Exception e) {
                        mMessages.log(java.util.logging.Level.SEVERE, "ProfilerRoundTrip.Fail_to_read_data_from_table", queueName, e);
                    } finally {
                        Util.close(con);
                    }  
                    
                }
            };
            long startTime = System.currentTimeMillis();
            feeder.start();
            consumer.start();

            /*
             *It is expected that the sender has completed sending messages before the receiver returns,
             *as the receiver would not return before receiving all the messages, hence we dont need to
             *join on the sender.
             */
            feeder.join();
            consumer.join();
            long endTime = System.currentTimeMillis();
            double throughput = mInputBatchSize * 1000.0 / (endTime - startTime);
            totalTime += (endTime - startTime);
            String msg = "Successfully completed benchmark run: " + i + " in " + (endTime - startTime) + " miliseconds. Throughput(msg per second): " + throughput;
            report.append("\t" + msg + "\n");
            mMessages.printlnOriginal(msg);
        }
        double avgThroughput = mInputBatchSize * mTotalBatches  * 1000.0 / totalTime;
        String msg = "Benchmark completed. Total number of runs: " + mTotalBatches + ". Average throughput (msg per second): " + avgThroughput;
        report.append("\t" + msg + "\n");
        mMessages.printlnOriginal(msg);
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(mLogFile);
            IOUtil.copy(new StringReader(report.toString()), fos);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (fos != null) {
                try {
                    fos.close();
                } catch (Exception e) {
                }
            }    
        }
    }
    
    public static void main(String[] args) {
        if (args.length < 1) {
            mMessages.printlnOriginal("java com.sun.jbi.engine.iep.core.runtime.client.pojo.ProfilerRoundTrip config.properties");
            mMessages.printlnOriginal("config.properties:");
            mMessages.printlnOriginal("\tDatabaseType=...");
            mMessages.printlnOriginal("\tDatabaseHostname=...");
            mMessages.printlnOriginal("\tDatabaseUsername=...");
            mMessages.printlnOriginal("\tDatabasePassword=...");
            mMessages.printlnOriginal("\tDatabaseSid=...");
            mMessages.printlnOriginal("\tDatabaseSchemaName=...");
            mMessages.printlnOriginal("\tInstanceId=...");
            mMessages.printlnOriginal("\tStreamInput=...");
            mMessages.printlnOriginal("\tStreamOutput=...");
            mMessages.printlnOriginal("\tInputTemplateFile=...");
            mMessages.printlnOriginal("\tInputBatchSize=...");
            mMessages.printlnOriginal("\tOutputBatchSize=...");
            mMessages.printlnOriginal("\tTotalBatches=...");
            mMessages.printlnOriginal("\tLogFile=...");
            return;
        }
        String configFileName = args[0];
        //Load the properties to resolve relative path of logFile and inputTemplateFile to absolute path
        File configFile = new File(configFileName);
        String path = configFile.getParentFile().getAbsolutePath();
        Properties configProp = new Properties();
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(configFile);
            configProp.load(fis);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "ProfilerRoundTrip.Fail_to_load_configuration_file", configFileName, e);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (Exception e) {
                }    
            }
        }   
        Util.setDbSpecial(configProp);
        String inputTemplateFile = path + File.separator + configProp.getProperty("InputTemplateFile");
        configProp.put("InputTemplateFile", inputTemplateFile);
        String logFile = path + File.separator + configProp.getProperty("LogFile");
        configProp.put("LogFile", logFile);
        
        try {
            ProfilerRoundTrip prt = new ProfilerRoundTrip(configProp);
            prt.run();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "ProfilerRoundTrip.main_fails", e);
        } 
    }

}    
