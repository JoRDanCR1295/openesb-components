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
 * @(#)Printer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import java.text.SimpleDateFormat;
import java.sql.Connection;
import java.sql.Timestamp;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Collections;
import java.util.Properties;
import java.io.PrintWriter;
import java.io.FileOutputStream;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.RowSection;
import java.util.logging.Level;

/**
 * Printer.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */
public abstract class Printer implements Runnable, OperatorConstants {
    private static final Messages mMessages = Messages.getMessages(Printer.class);

    protected SimpleDateFormat mSDF = new SimpleDateFormat("yyyy.MM.dd G 'at' HH:mm:ss z");

    protected String mId;
    protected String mInstanceId;
    protected String mPlanId;
    protected String mOutputId;
    protected String mFileName;
    protected long mInterval;
    protected Object mMonitor = new Object();
    protected boolean mStop = false;
    
    protected void registerInputTableUsage(Connection con) throws Exception {
        unregisterInputTableUsage(con);
        String inputTableName = Util.getQueueName(new Properties(System.getProperties()), mInstanceId, mOutputId);
        Util.initializeTableUsage(con, mPlanId, inputTableName, mId);
    }
    
    protected void unregisterInputTableUsage(Connection con) throws Exception {
        String inputTableName = Util.getQueueName(new Properties(System.getProperties()), mInstanceId, mOutputId);
        Util.deleteTableUsage(con, mPlanId, inputTableName, mId);
    }
    
    protected void printRow(Connection con, String inputTableName, PrintWriter prw, int rowIdx, Object[] row) throws Exception {
    }

    public Printer() {
    }
    
    public void initialize(String name, String instanceId, String outputId, String fileName, long interval) {
        mId = name;
        mInstanceId = instanceId;
        mPlanId = Util.getPlanId(instanceId);
        mOutputId = outputId;
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
            String inputTableName = Util.getQueueName(new Properties(System.getProperties()), mInstanceId, mOutputId);
            Timestamp timestamp = new Timestamp(0);
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
                ArrayList<String> attributeList = new ArrayList<String>();
                attributeList.add(COL_TIMESTAMP);
                Map<RowSection, List<Object[]>> partitionMap = Util.getPartitionedRows(con, inputTableName, attributeList, timestamp);
                
                // Mark all rows in the input queue that has timestamp <= timestampToProcess
                Util.updateTableUsage(con, mPlanId, inputTableName, mId, timestamp);
                
                List<RowSection> timestampList = new ArrayList<RowSection>(partitionMap.keySet());
                Collections.sort(timestampList);
                for (RowSection r : timestampList) {
                    List<Object[]> inputRowList = partitionMap.get(r);
                    for (int i = 0, I = inputRowList.size(); i < I; i++) {
                        Object[] row = inputRowList.get(i);
                        printRow(con, inputTableName, prw,i, row);
                    }
                    prw.println("---------------------------------------------------");
                    prw.flush();
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Printer.Run_fails", e);
        } finally {
            try {
                if (prw != null) {
                    prw.close();
                }
                unregisterInputTableUsage(con);
                Util.close(con);
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "Printer.Clean_up_fails", e1);
            }
        }
    }                    

    public void stop() {
        mStop = true;
        synchronized (mMonitor) {
            mMonitor.notifyAll();
        }
    }

}
