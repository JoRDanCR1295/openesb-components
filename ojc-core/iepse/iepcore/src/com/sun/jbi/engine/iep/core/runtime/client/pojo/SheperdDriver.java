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
 * @(#)SheperdDriver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.text.SimpleDateFormat;
import java.util.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.logging.Level;

/**
 * SheperdDriver.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */
public class SheperdDriver  {
    private static final Messages mMessages = Messages.getMessages(SheperdDriver.class);

    private SimpleDateFormat mSDF = new SimpleDateFormat("yyyy.MM.dd G 'at' HH:mm:ss z");
    private Sheperd mSheperd = null;
    private long mInterval;
    private int mRepeat;
    private int mBatchSize;
    private List<String[]> mRowList;
    private String[] mColumnNames;
    
    private List<String[]> loadInput(String dataFileName) {
        // read and cache all input
        List<String[]> rowList = new ArrayList<String[]>();
        BufferedReader fileIn = null;
        int colCnt = 0;
        try {
            fileIn = new BufferedReader(new InputStreamReader(IOUtil.getResourceAsStream(dataFileName)));
            String colNames = fileIn.readLine();
            StringTokenizer st = new StringTokenizer(colNames, ",");
            colCnt = st.countTokens();
            mColumnNames = new String[colCnt];
            for (int i = 0; i < colCnt; i++) {
                mColumnNames[i] = st.nextToken();
            }
            while (true) {
                String record = fileIn.readLine();
                if (record == null) {
                    break;
                }
                if (record.trim().equals("")) {// skip empty line
                    continue;
                }
                if (record.startsWith("#")) { // skip comments
                    continue;
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
            mMessages.log(Level.SEVERE, "SheperdDriver.loadInput_fails", e);
        } finally {
            try {
                if (fileIn != null) {
                    fileIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "SheperdDriver.Closing_fileIn_fails", e1);
            }
        }
        return rowList;
    }
    
    @SuppressWarnings("unchecked") // Map prop = new HashMap(sheperdProp);
    public SheperdDriver(String sheperdPropFile,
                         String dataFileName, 
                         int batchSize, 
                         long interval, 
                         int repeat) throws Exception 
    {
        Properties sheperdProp = new Properties();
        sheperdProp.load(IOUtil.getResourceAsStream(sheperdPropFile));
        String sheperdName = sheperdProp.getProperty(Sheperd.SHEPERD_NAME);
        mSheperd = (Sheperd)Class.forName(sheperdName).newInstance();

        mBatchSize = batchSize;
        mInterval = interval;
        mRepeat = repeat;
        mRowList = loadInput(dataFileName);

        Map prop = new HashMap(sheperdProp);
        prop.put(Sheperd.SHEPERD_BATCH_SIZE, batchSize + "");
        prop.put(Sheperd.SHEPERD_REPEAT, repeat + "");
        prop.put(Sheperd.SHEPERD_ROW_LIST, mRowList);
        prop.put(Sheperd.SHEPERD_COLUMN_NAMES, mColumnNames);
        mSheperd.init(prop);
    }

    public void run() throws Exception {
        BufferedReader userIn = null;
        try {
            if (mInterval < 0) {
                userIn = new BufferedReader(new InputStreamReader(System.in));
            }
            mSheperd.begin();
            long startTime = System.currentTimeMillis();
            //System.out.println("Start Processing at " +  startTime + ": " + mSDF.format(new Date()));
            int listSize = mRowList.size();
            int total = mRepeat * listSize;
            int index = 0;
            Object[] row = null;
            while (index < total) {
                if (mInterval < 0) {// interactive
                    StringBuffer sb = new StringBuffer();
                    for (int i = 0; i < mBatchSize; i++) {
                        if (index + i < total) {
                            row = mRowList.get((index + i)%listSize);
                            sb.append("\n\t(");
                            for (int k = 0; k < row.length; k++) {
                                if (k > 0) {
                                    sb.append(", ");
                                }
                                sb.append(row[k]);
                            }
                            sb.append(")");
                        } else {
                            break;
                        }
                    }    
                    sb.append("\n? (y/n/q)");
                    mMessages.log(Level.INFO, "SheperdDriver.insert_record", sb.toString());
                    String ans = userIn.readLine();
                    if (ans == null) {
                        continue;
                    }
                    if (ans.equals("n")) {
                        index+=mBatchSize;
                        continue;
                    } 
                    if (ans.equals("q")) {
                        break;
                    }
                    index = mSheperd.input(index);
                } else {
                    sleep(mInterval);
                    index = mSheperd.input(index);
                }
            }
            if (mInterval > 0) {
                if (mMessages.isLoggable(Level.FINE)) {
                    if (index == total) {
                        long endTime = System.currentTimeMillis();
                        mMessages.log(Level.FINE, "SheperdDriver.Finish_processing_at", mSDF.format(new Date()));
                        long totalTime =  endTime - startTime;
                        mMessages.log(Level.FINE, "SheperdDriver.records_are_processed_in_milliseconds", new Object[]{total, totalTime});  
                        mMessages.log(Level.FINE, "SheperdDriver.records_per_second", "" + (total * 1000.0 / totalTime));
                    } else {
                        mMessages.log(Level.FINE, "SheperdDriver.index_total", new Object[]{index, total}); 
                    }
                }    
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "SheperdDriver.run_fails", e);
            throw e;
        } finally {
            mSheperd.end();
        }
    }     
    
    private static void sleep(long time) {
        if (time <= 0) {
            return;
        }
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            mMessages.log(Level.SEVERE, "SheperdDriver.sleep_interrupted", e);
        }
    }
    
    public static void main(String[] args) {
        if (args.length < 5) {
            System.out.println("java com.sun.jbi.engine.iep.core.runtime.pojo.SheperdDriver sheperdPropFile dataFile batchSize interval repeat");
            return;
        }
        try {
            String sheperdPropFile = args[0];
            String dataFile = args[1];
            int batchSize = Integer.parseInt(args[2]);
            long interval = Long.parseLong(args[3]);
            int repeat = Integer.parseInt(args[4]);
            
            SheperdDriver ss = new SheperdDriver(sheperdPropFile, dataFile, batchSize, interval, repeat);
            ss.run();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "SheperdDriver.main_fails", e);
        }
    }
    
}
