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

package com.sun.jbi.engine.iep.core.runtime.client.soap;

import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.soap.SOAPException;
import javax.xml.transform.stream.StreamSource;

import java.io.StringReader;
import java.util.logging.Level;
import java.util.List;
import java.util.Map;

import com.sun.jbi.engine.iep.core.runtime.client.pojo.Sheperd;
import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;

/**
 * StreamSheperd.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class StreamSheperd implements Sheperd {
    private static final Messages mMessages = Messages.getMessages(StreamSheperd.class);

    public static final String SHEPERD_SOAP_DESTINATION = "soapDestination";
    public static final String SHEPERD_SOAP_ACTION = "soapAction";
    public static final String SHEPERD_INPUT_TEMPLATE_FILE = "inputTemplateFile";
    
    private int mBatchSize;
    private int mRepeat;
    private List mRowList;
    private String[] mColumnNames;
    
    private String mSoapDestination;
    private String mSoapAction;
    private String mInputTemplate;
    
    private SOAPConnectionFactory mSoapConnFactory;
    private SOAPConnection mConnection;
    private MessageFactory mMessageFactory;
    
    /** Creates a new instance of StreamSheperd */
    public StreamSheperd() {
    }

    /**
     * SHEPERD_PLAN_ID
     * SHEPERD_OP_ID
     * SHEPERD_BATCH_SIZE
     * SHEPERD_REPEAT
     * SHEPERD_ROW_LIST
     * SHEPERD_COLUMN_NAMES
     */
    public void init(Map prop) {
        try {
            mBatchSize = PropertyUtil.getint(prop, SHEPERD_BATCH_SIZE, 1);
            mRepeat = PropertyUtil.getint(prop, SHEPERD_REPEAT, 1);
            mRowList = (List)prop.get(SHEPERD_ROW_LIST);
            mColumnNames = (String[]) prop.get(SHEPERD_COLUMN_NAMES);
            
            // properties only related to this sheperd
            mSoapDestination = PropertyUtil.getString(prop, SHEPERD_SOAP_DESTINATION, null);
            mSoapAction = PropertyUtil.getString(prop, SHEPERD_SOAP_ACTION, null);
            String inputTemplateFile = PropertyUtil.getString(prop, SHEPERD_INPUT_TEMPLATE_FILE, null);
            mInputTemplate = IOUtil.getText(inputTemplateFile, "UTF-8");
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "StreamSheperd.init_fails", e);
        }
    }

    public void begin() throws Exception {
        // Set up the mConnection and factories
        mSoapConnFactory = SOAPConnectionFactory.newInstance();
        mConnection = mSoapConnFactory.createConnection();
        mMessageFactory = MessageFactory.newInstance();
    }
    
    public int input(int index) throws Exception {
        int listSize = mRowList.size();
        int total = mRepeat * listSize;
        int colCnt = mColumnNames.length;
        Object[] row = null;
        for (int i = 0; i < mBatchSize; i++) {
            if (index < total) {
                row = (Object[])mRowList.get(index%listSize);
                sendRow(row);
                index++;
            }
        }
        return index;
    }
    
    public void end() {
        try {
            mConnection.close();
            mConnection = null;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "StreamSheperd.end_fails", e);
        }
    }


    /**
     * Test of inbound SOAP Request processing.
     */
    private void sendRow(Object[] row) throws Exception {
        //Prepare input content
        StringBuffer sb = new StringBuffer(mInputTemplate);
        for (int i = 0, I = row.length; i < I; i++) {
            String data = (String)row[i];
            String placeHolder = "${" + mColumnNames[i] + "}";
            int idx = sb.indexOf(placeHolder);
            if (idx >= 0) {
                sb.replace(idx,  idx + placeHolder.length(), data);
            }
        }
        String input = sb.toString();
        
        //Create and populate the message
        SOAPMessage message = mMessageFactory.createMessage();
        SOAPPart soapPart = message.getSOAPPart();
        if (mSoapAction != null) {
            message.getMimeHeaders().addHeader("soapaction", mSoapAction);
        }
        StreamSource inputSrc = new StreamSource(new StringReader(input));
        soapPart.setContent(inputSrc);
        message.saveChanges();

        // Send the message
        try {
            mConnection.call(message, mSoapDestination);
        } catch (SOAPException e) {
            mMessages.log(Level.SEVERE, "StreamSheperd.sendRow_fails", e);
        }
    }

}
