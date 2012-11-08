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
 * @(#)TableSheperd.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;
import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.StringTokenizer;
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
public class TableSheperd  {
    private static final Messages mMessages = Messages.getMessages(TableSheperd.class);

    private String mTableName;
    private String mFileName;
    
    public TableSheperd(String tableName, String fileName) {
        mTableName = tableName;
        mFileName = fileName;
    }

    public void run() throws Exception {
        BufferedReader fileIn = null;
        Connection con = null;
        try {
            fileIn = new BufferedReader(new InputStreamReader(IOUtil.getResourceAsStream(mFileName)));
            String colNames = fileIn.readLine();
            StringTokenizer st = new StringTokenizer(colNames, ",");
            int colCnt = st.countTokens();
            
            con = Util.getConnection();
            while (true) {
                String record = fileIn.readLine();
                if (record == null) {
                    return;
                }
                if (record.startsWith("#")) { // skip comments
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.logOriginal(Level.FINE, record);
                    }    
                    continue;
                }
                Object[] row = new Object[colCnt];
                st = new StringTokenizer(record, ",");
                for (int i = 0; i < colCnt; i++) {
                    if (st.hasMoreTokens()) {
                        row[i] = st.nextToken();
                    } else {
                        row[i] = "";
                    }
                }
                ArrayList<Object[]> valueList = new ArrayList<Object[]>();
                valueList.add(row);
                
                Util.insertRows(con, mTableName, valueList);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "TableSheperd.run_fails", e);
            throw e;
        } finally {
            Util.close(con);
        }
    }                    
    
    public static void main(String[] args) {
        if (args.length < 2) {
            mMessages.logOriginal(Level.INFO, "java com.sun.jbi.engine.iep.core.runtime.util.TableSheperd tableName fileName");
            return;
        }
        try {
            String tableName = args[0];
            String fileName = args[1];
            TableSheperd ss = new TableSheperd(tableName, fileName);
            ss.run();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "TableSheperd.main_fails", e);
        }
    }
    
}
