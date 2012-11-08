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
 * @(#)JmqCleanupHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author pbhagat
 */
public class JmqCleanupHelper {

    private static Logger LOGGER = Logger.getLogger(JmqCleanupHelper.class.getName());

    private String listTxn = null;
    private String queryTxn = null;
    private String queryCxn = null;
    private String rollbackTxn = null;

    JmqCleanupHelper(String imqcmdString){

        listTxn = imqcmdString + " list txn";
        queryTxn = imqcmdString + " query txn -n ";
        queryCxn = imqcmdString + " query cxn -n ";
        rollbackTxn = imqcmdString + " rollback txn -n ";
    }

    public static void main(String [] args) {
        try {
            JmqCleanupHelper jmqCleanupHelper = new JmqCleanupHelper(System.getProperty("DRIVER_TEST_DIR") + "/recovery/RecoveryTesting/recoverydriver/imq/bin/imqcmd.exe -javahome ${alaska_root}/build-tools/jdk1.5.0_09 -passfile passfile.txt -u admin -b localhost:20000");
            jmqCleanupHelper.rollbackStuckTxns();
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, "Exception occured while doing the processing for rolling back stuck transactions.", ex);
        }
    }

    public void rollbackStuckTxns() throws Exception {

        List listOfTxnsToRollback = getTxnIdsForRollback();
        if(listOfTxnsToRollback.isEmpty()) {
            return;
        }

        Map mapOfCxnIds = getCxnIds(listOfTxnsToRollback);
        List txnsIdsWithBrokenCxn = getTxnsIdsWithBrokenCxns(mapOfCxnIds);
        rollbackTxnsIdsWithBrokenCxns(txnsIdsWithBrokenCxn);
    }

    public List  getTxnIdsForRollback() throws Exception {
        ArrayList listOfTxnsToRollback = new ArrayList();
        InputStream stdin = null;
        InputStreamReader isr = null;
        BufferedReader br = null;

        try{
            Process listTxnProc = Runtime.getRuntime().exec(listTxn);
            stdin = listTxnProc.getInputStream();
            isr = new InputStreamReader(stdin);
            br = new BufferedReader(isr);

            String line = null;
            while ( (line = br.readLine()) != null) {
                if(line.contains("STARTED")) {
                    listOfTxnsToRollback.add(line.substring(0, 19));
                    LOGGER.log(Level.INFO, "Found a transaction with STARTED state which could be potentially be rolledback. Line: " + line);                    
                }
            }

        }  finally {
            if(br != null){
                br.close();
            }
            if(isr != null){
                isr.close();
            }
            if(stdin != null){
                stdin.close();
            }
        }
        return listOfTxnsToRollback;
    }


    public Map getCxnIds(List listOfTxnsToRollback) throws Exception {
        HashMap mapOfCxnIds = new HashMap();

        Iterator iter = listOfTxnsToRollback.iterator();
        while (iter.hasNext()) {
            InputStream stdin = null;
            InputStreamReader isr = null;
            BufferedReader br = null;

            try {
                String txnId = (String)iter.next();
                Process queryTxnProc = Runtime.getRuntime().exec(queryTxn + txnId);
                stdin = queryTxnProc.getInputStream();
                isr = new InputStreamReader(stdin);
                br = new BufferedReader(isr);

                String line = null;
                while ( (line = br.readLine()) != null) {
                    if(line.contains("Connection ID (created transaction)")) {
                        String cxnId = line.substring(line.length()-19, line.length());
                        mapOfCxnIds.put(txnId, cxnId.trim());
                        break;
                    }
                }

            } finally {
                if(br != null){
                    br.close();
                }
                if(isr != null){
                    isr.close();
                }
                if(stdin != null){
                    stdin.close();
                }
            }
        }
        return mapOfCxnIds;
    }

    public List getTxnsIdsWithBrokenCxns(Map mapOfCxnIds) throws Exception {
        ArrayList txnsIdsWithBrokenCxn = new ArrayList();

        Iterator iter = mapOfCxnIds.keySet().iterator();
        while (iter.hasNext()) {
            InputStream stdin = null;
            InputStreamReader isr = null;
            BufferedReader br = null;

            try {
                String txnId = (String)iter.next();
                String cxnId = (String) mapOfCxnIds.get(txnId);
                Process queryCxnProc = Runtime.getRuntime().exec(queryCxn + cxnId);
                stdin = queryCxnProc.getErrorStream();
                isr = new InputStreamReader(stdin);
                br = new BufferedReader(isr);

                String line = null;
                while ( (line = br.readLine()) != null) {
                    if(line.contains("No such connection: " + cxnId)) {
                        txnsIdsWithBrokenCxn.add(txnId);
                        break;
                    }
                }

            } finally {
                if(br != null){
                    br.close();
                }
                if(isr != null){
                    isr.close();
                }
                if(stdin != null){
                    stdin.close();
                }
            }
        }
        return txnsIdsWithBrokenCxn;
    }

    public void rollbackTxnsIdsWithBrokenCxns(List txnsIdsWithBrokenCxns) throws Exception {
        Iterator iter = txnsIdsWithBrokenCxns.iterator();
        while (iter.hasNext()) {
            String txnId = (String)iter.next();

            OutputStream stdout = null;
            OutputStreamWriter osw = null;
            BufferedWriter bw = null;

            try {
                Process rollbackTxnProc = Runtime.getRuntime().exec(rollbackTxn + txnId);
                stdout = rollbackTxnProc.getOutputStream();
                osw = new OutputStreamWriter(stdout);
                bw = new BufferedWriter(osw);
                bw.write("y");
                bw.flush();

            } finally {
                if(bw != null){
                    bw.close();
                }
                if(osw != null){
                    osw.close();
                }
                if(stdout != null){
                    stdout.close();
                }
            }

            LOGGER.log(Level.INFO, "Rolledback a stuck transaction with id " + txnId);
            //To be on the safe side, give 5 seconds for JMS server to do the cleanup
            Thread.sleep(1000);
        }
    }
}
