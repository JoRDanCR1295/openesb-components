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
 * @(#)SendCommand.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.soap;

import com.sun.jbi.engine.iep.core.runtime.client.pojo.Command;
import com.sun.jbi.engine.iep.core.runtime.client.pojo.Input;
import com.sun.jbi.engine.iep.core.runtime.client.pojo.NWaySheperd;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.transform.stream.StreamSource;

/**
 * SendCommand.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class SendCommand implements Command {
    private static final Messages mMessages = Messages.getMessages(SendCommand.class);

    private static SimpleDateFormat mSDF = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
    private String mDestination;
    private String mExpectedHttpWarning;
    private String mAction;
    private SOAPConnectionFactory mSoapConnFactory;
    private SOAPConnection mConnection;
    private MessageFactory mMessageFactory;
    

    /** Creates a new instance of SendCommand */
    public SendCommand() {
        try {
            // Set up the mConnection and factories
            mSoapConnFactory = SOAPConnectionFactory.newInstance();
            mConnection = mSoapConnFactory.createConnection();
            mMessageFactory = MessageFactory.newInstance();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "SendCommand.Constructor_fails", e);
        }
    }
    
    public void init(NWaySheperd sheperd, String props) {
        Properties p = PropertyUtil.getPropertiesFromString(props, "=", ";");
        mDestination = p.getProperty("destination");
        mExpectedHttpWarning = p.getProperty("httpwarning");      
        mAction=p.getProperty("action");
    }
    
    public Runnable createRunnable(final Map<String, Input> inputTable, final String[] args) {
        return new Runnable() {
            public void run() {
                Input input = inputTable.get(args[1]);
                int batches = 1;
                try {
                     batches = Integer.parseInt(args[2]);
                } catch (Exception e) {
                }
                for (int i = 0; i < batches; i++) {
                    try {
                        String data = input.nextData();
                        SOAPMessage message = mMessageFactory.createMessage();
                        message.getMimeHeaders().addHeader("soapaction", mAction);
                        SOAPPart soapPart = message.getSOAPPart();
                        soapPart.setContent(new StreamSource(new StringReader(data)));
                        message.saveChanges();
                        sendMessage(input.getName(), false, mDestination, message, mExpectedHttpWarning);
                        String[] row = input.getCurRow();
                        StringBuffer sb = new StringBuffer();
                        sb.append("(");
                        for (int j = 0; j < row.length; j++) {
                            if (j > 0) {
                                sb.append(",");
                            }
                            sb.append(row[j]);
                        }
                        sb.append(")");
                        if (mMessages.isLoggable(Level.FINE)) {
                            mMessages.log(Level.FINE,"SendCommand.Send_to_at", new Object[]{sb.toString(), mAction, mSDF.format(new Date())}); 
                        }    
                    } catch (Exception e) {
                        mMessages.log(Level.SEVERE, "SendCommand.run_fails", e);
                    }
                }
            }   
        };
    }
    
    /**
     * Send a soap message 
     * @param destination URL to send to 
     * @param message message to send
     * @param expectedHttpStatus expected http status code or null if success is expected
     * @return reply soap message
     */    
    private SOAPMessage sendMessage(String logPrefix, boolean logDetails, String destination, SOAPMessage message, String expectedHttpWarning) throws SOAPException {
        
        // Store standard error output temporarily if we expect a certain error as we do not want 
        // to see the SAAJ output in this case 
        java.io.PrintStream origErr = null;
        java.io.ByteArrayOutputStream bufferedErr = null;
        java.io.PrintStream stdErr = null;
        if (expectedHttpWarning != null) {
            origErr =  System.err;
            bufferedErr = new java.io.ByteArrayOutputStream();
            stdErr = new java.io.PrintStream(bufferedErr);
            System.setErr(stdErr);
        }
        
        // Send the message and get a reply
        SOAPMessage reply = null;
        long start = 0;
        if (logDetails) {
            start = System.currentTimeMillis();
        }
       
        boolean httpSuccess = true;
        try {
            reply = mConnection.call(message, destination);
        } catch (SOAPException ex) {
            httpSuccess = false;
            // This currently relies on the implementation details
            // to check for the HTTP status as no standard way is currently provide by saaj
            // It expectes an exception message of the format "Bad response: (404Error" 
            // - where 404 is the status code in this example
            if (expectedHttpWarning != null && bufferedErr.toString().indexOf(expectedHttpWarning) < 0) {
                if (stdErr != null) {
                    System.setErr(origErr);
                    stdErr.flush();
                    stdErr.close();
                    origErr.print(bufferedErr.toString());
                }
                throw ex;
            } 
        }
        long end = 0;
        if (logDetails) {
            end = System.currentTimeMillis();        
        }
        
        // Ensure standard error isn't redirected/buffered anymore
        if (origErr != null) {
            System.setErr(origErr);
            if (stdErr != null) {
                stdErr.close();
            }
        }
        
        if (logDetails) {
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "SendCommand.Prefix_Call_took_milliseconds", new Object[]{logPrefix, (end-start)}); 
            }    
        }
        return reply;
    }
    
    
    public void destroy() {
        try {
            mConnection.close();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "SendCommand.Closing_DB_Connection_fails", e);
        }
    }
}
