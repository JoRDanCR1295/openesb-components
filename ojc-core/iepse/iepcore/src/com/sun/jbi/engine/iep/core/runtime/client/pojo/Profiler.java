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
 * @(#)Profiler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.sql.Connection;
import java.sql.Timestamp;
import java.util.Properties;
import java.io.PrintWriter;
import java.io.FileOutputStream;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.logging.Level;

/**
 * Profiler.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */
public class Profiler implements Runnable, OperatorConstants {
    private static final Messages mMessages = Messages.getMessages(Profiler.class);

    protected SimpleDateFormat mSDF = new SimpleDateFormat("yyyy.MM.dd G 'at' HH:mm:ss z");

    protected String mInstanceId;
    protected String mId;
    protected String mPlanId;
    protected String mInputId;
    protected int mTotalRowCnt;
    protected String mFileName;
    protected long mInterval;
    protected Object mMonitor = new Object();
    protected transient boolean mStop = false;
    
    protected void registerInputTableUsage(Connection con) throws Exception {
        unregisterInputTableUsage(con);
        String inputTableName = Util.getQueueName(new Properties(System.getProperties()), mInstanceId, mInputId);
        Util.initializeTableUsage(con, mPlanId, inputTableName, mId);
    }
    
    protected void unregisterInputTableUsage(Connection con) throws Exception {
        String inputTableName = Util.getQueueName(new Properties(System.getProperties()), mInstanceId, mInputId);
        Util.deleteTableUsage(con, mPlanId, inputTableName, mId);
    }
    
    public Profiler() {
    }
    
    public void initialize(String name, String instanceId, String inputId, int totalRowCnt, String fileName, long interval) {
        mId = name;
        mInstanceId = instanceId;
        mPlanId = Util.getPlanId(instanceId);
        mInputId = inputId;
        mTotalRowCnt = totalRowCnt;
        mFileName = fileName;
        mInterval = interval;
        if (mInterval < 0) {
            mInterval = 1000;
        }
    }        
    
    public void run() {
        PrintWriter prw = null;
        Connection con = null;
        try {
            if (mFileName.equals("stdout")) {
                prw = new PrintWriter(System.out);
            } else {
                prw = new PrintWriter(new FileOutputStream(mFileName));
            }
            con = Util.getConnection();
            registerInputTableUsage(con);
            String inputTableName = Util.getQueueName(new Properties(System.getProperties()), mInstanceId, mInputId);
            Timestamp timestamp = new Timestamp(0);
            int totalRowCnt = 0;
            long startingTime = 0;
            long endTime = 0;
            while (true) {
                synchronized (mMonitor) {
                    try {
                        mMonitor.wait(mInterval);
                    } catch (InterruptedException e) {
                        continue;
                    }
                }
                if (mStop) {
                    break;
                }
                Timestamp newTimestamp = Util.getPrevTimestampToProcess(con, mPlanId);
                if (newTimestamp == null || !newTimestamp.after(timestamp)) {
                    continue;
                }
                timestamp = newTimestamp;
                int rowCnt = Util.getRowCount(con, inputTableName, timestamp);

                // Mark all rows in the input queue that has timestamp <= timestampToProcess
                Util.updateTableUsage(con, inputTableName, mPlanId, mId, timestamp);
                if (rowCnt > 0) {
                    if (totalRowCnt == 0) {
                        startingTime = System.currentTimeMillis();
                        prw.println(mMessages.getString("Profiler.The_1st_row_is_receieved_at", startingTime));
                    }
                    endTime = System.currentTimeMillis();
                    totalRowCnt += rowCnt;
                    if (totalRowCnt >= mTotalRowCnt) {
                        break;
                    }
                }
            }
            long totalTime = endTime - startingTime;
            prw.println(mMessages.getString("Profiler.Stop_at", endTime));
            prw.println(totalRowCnt + mMessages.getString("Profiler.rows_are_processed_in_milli_secs", new Object[]{totalRowCnt, totalTime})); 
            prw.println(mMessages.getString("Profiler.rows_are_processed_per_second", totalRowCnt * 1000.0 / totalTime));
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Profiler.Run_fails", e);
        } finally {
            try {
                if (prw != null) {
                    prw.close();
                }
                unregisterInputTableUsage(con);
                Util.close(con);
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "Profiler.Clean_up_fails", e1);
            }
        }
        //System.exit(0);
    }                    

    public void stop() {
        mStop = true;
        synchronized (mMonitor) {
            mMonitor.notifyAll();
        }
    }

    public static void main(String[] args) {
        if (args.length < 6) {
            mMessages.printlnOriginal("java com.sun.jbi.engine.iep.core.runtime.util.Profiler name instanceId inputId totalRowCnt fileName|stdout interval");
            return;
        }
        BufferedReader userIn = null;
        try {
            String name = args[0];
            String instanceId = args[1];
            String inputId = args[2];
            int totalRowCnt = Integer.parseInt(args[3]);
            String fileName = args[4];
            long interval = Long.parseLong(args[5]);
            Profiler profiler = new Profiler();
            profiler.initialize(name, instanceId, inputId, totalRowCnt, fileName, interval);
            Thread thread = new Thread(profiler);
            thread.start();
            userIn = new BufferedReader(new InputStreamReader(System.in));
            while (true) {
                mMessages.println("Profiler.Stop", "(y)");
                String ans = userIn.readLine();
                if (ans == null) {
                    break;
                }
                if (ans.equals("y")) {
                    profiler.stop();
                    break;
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Profiler.main_fails", e);
        } finally {
            try {
                userIn.close();
            } catch (Exception e) {
                mMessages.log(Level.SEVERE, "Profiler.Closing_userIn_fails", e);
            }
        }
    }
    
}
