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
 * @(#)Input.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.util.ArrayUtil;
import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.logging.Level;

/**
 * Input.java
 *
 * Created on March 25, 2005, 10:58 AM
 *
 *
 * ================Example dataFile =====================
 * #comments start with #
 * symbol,price,amount,action
 * SBYN,10.12,25000,BOUGHT
 * SBYN,10.13,4000,SOLD
 * ....
 * IBM,20.12,200,SOLD
 * ===============End of example dataFile ===============
 *
 * ===============Example templateFile: batchSize=2 ===================
 * <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
 *              xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
 *              xmlns:xsd="http://www.w3.org/1999/XMLSchema"
 *              xmlns:tns="http://seebeyond.com/test_iep/"
 *              xmlns="test_iep">
 *   <SOAP-ENV:Header/>
 *   <SOAP-ENV:Body>
 *     <StreamInput0>
 *       <evts>
 *         <evt>
 *           <name>$symbol.0</name>        
 *           <price>$price.0</price>       
 *           <act>$action.0<act>           
 *         </evt>
 *         <evt>
 *           <name>$symbol.1</name>        
 *           <price>$price.1</price>       
 *           <act>$action.1<act>           
 *         </evt>
 *       </evts>
 *     </StreamInput0>
 *   </SOAP-ENV:Body>
 * </SOAP-ENV:Envelope>
 * ===============End of Example templateFile============
 *
 *
 * @author Bing Lu
 */
public class Input {
    private static Messages mMessages = Messages.getMessages(Input.class);

    private String mName;
    private String mTemplate;
    private String[] mColumnNames;
    private String[] mCurRow;
    private List<String[]> mRowList;
    private int mRowCount;
    private int mCurRowIndex;
    private int mBatchSize;

    private List<String[]> loadData(String dataFileName) {
        // read and cache all input
        List<String[]> rowList = new ArrayList<String[]>();
        BufferedReader fileIn = null;
        int colCnt = 0;
        try {
            fileIn = new BufferedReader(new InputStreamReader(IOUtil.getResourceAsStream(dataFileName)));
            String record = fileIn.readLine();
            if (record == null) {
                return rowList;
            }
            while (record.startsWith("#")) {
                record = fileIn.readLine(); // skip comments
                if (record == null) {
                    return rowList;
                }
            }
            StringTokenizer st = new StringTokenizer(record, ",");
            colCnt = st.countTokens();
            mColumnNames = new String[colCnt];
            for (int i = 0; i < colCnt; i++) {
                mColumnNames[i] = st.nextToken();
            }
            while (true) {
                record = fileIn.readLine();
                if (record == null) {
                    break;
                }
                if (record.startsWith("#")) {
                    continue;  // skip comments
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
            mMessages.log(Level.SEVERE, "Input.loadData_fails", e);
        } finally {
            try {
                if (fileIn != null) {
                    fileIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "Input.Closing_fileIn_fails", e1);
            }
        }
        return rowList;
    }

    private void incrementRow() {
        if (mCurRowIndex < mRowCount - 1) {
            mCurRowIndex++;
        } else {
            mCurRowIndex = 0;
        }
    }
    
    public Input(String name, String templateFile, String dataFile, int batchSize) {
        mName = name;
        try {
            mTemplate = IOUtil.getText(templateFile, "UTF-8");
        } catch (IOException e) {
            mMessages.log(Level.SEVERE, "Input.Constructor_fails", e);
        }
        mRowList = loadData(dataFile);
        mRowCount = mRowList.size();
        mCurRowIndex = 0;
        mBatchSize = batchSize;
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE,"Input.templateFile", templateFile);
            mMessages.log(Level.FINE,"Input.dataFile", dataFile);
        }    
    }
    
    public String getName() {
        return mName;
    }
    
    public String nextData() {
        String ret = null;
        String s = mTemplate;
        for (int i = 0; i < mBatchSize; i++) {
            mCurRow = mRowList.get(mCurRowIndex);
            incrementRow();
            for (int j = 0, J = mCurRow.length; j < J; j++) {
                String data = (String)mCurRow[j];
                String placeHolder = "${" + mColumnNames[j] + "." + i + "}";
                s = StringUtil.replaceAll(s, placeHolder, data);
            }
        }
        return s;
    }
    
    public String[] getCurRow() {
        return ArrayUtil.duplicate(mCurRow);
    }
    
    public String[] getColumnNames() {
        return ArrayUtil.duplicate(mColumnNames);
    }
        
}
