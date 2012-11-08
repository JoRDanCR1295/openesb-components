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
 * @(#)FileBasedJMSMessageReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.performance.jdbcjmsbd;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jms.JMSException;
import javax.jms.QueueReceiver;
import javax.jms.QueueSession;
import javax.jms.TextMessage;

/**
 * @author Bing Lu
 *
 */
public class JMSMessageReceiver extends Thread {

    //private static Logger LOGGER = Logger.getLogger("com.sun.jbi.recoverytest.JMSMessageReceiver");
    private static Logger LOGGER = Logger.getLogger(JMSMessageReceiver.class.getName());
    private Properties properties;
    
    static final int serverConnectionRetries = 10;
    static final int serverConectionRetryInterval = 2000;
    private static int retryCount = 0;
    
    private String jmsServerHostName;
    private String jmsServerPort;
    private String consumerDestName;
    private int outputBatchSize;
    private long endTime;
    
    public JMSMessageReceiver(Properties properties) {
        this.properties = properties;
        this.jmsServerHostName = properties.getProperty("JmsServerHostName");
        this.jmsServerPort = properties.getProperty("JmsServerPort");
        this.consumerDestName = properties.getProperty("ConsumerQueue");
        this.outputBatchSize = Integer.parseInt(properties.getProperty("OutputBatchSize"));
    }

    public void run() {
        try {
            recvMessages();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Exception occurred while receiving messages.", e);
            System.exit(0);
        }
    }
    
    /**
     * @throws Exception
     */
    private void recvMessages() throws Exception {
        HashMap hashMap = JMSHelper.createQueueReceiver(jmsServerHostName, jmsServerPort, consumerDestName);
        QueueReceiver queueReceiver = (QueueReceiver)hashMap.get("QueueReceiver");
        QueueSession queueSession = (QueueSession)hashMap.get("QueueSession");
        
        try {
            TextMessage receivedTextMessage = null;
            int receivedCounter = 0;
            do {
                try {
                    receivedTextMessage = (TextMessage) queueReceiver.receive();
                    if (receivedTextMessage == null) {
                        // bug in JMS queue implementation. when the queue is closed,
                        // instead of throwing an exception it returns a null
                        hashMap = JMSHelper.createQueueReceiver(jmsServerHostName, jmsServerPort, consumerDestName);
                        queueReceiver = (QueueReceiver)hashMap.get("QueueReceiver");
                        queueSession = (QueueSession)hashMap.get("QueueSession");
                        
                        continue;
                    }
                } catch (JMSException ex) {
                    LOGGER.log(Level.INFO, "JMS Error.. Possible JMS Server crash.. Retrying again..Retry count (" + retryCount +")");
                    hashMap = retryConnection();
                    if (hashMap != null) {
                        // reset retry count
                        retryCount = 0;
                        queueReceiver = (QueueReceiver)hashMap.get("QueueReceiver");
                        queueSession = (QueueSession)hashMap.get("QueueSession");
                        continue;
                    } else {
                        throw ex;
                    }
                }
                
                try {
                    String receivedMessage = ((TextMessage) receivedTextMessage).getText();
                    receivedCounter++;
                } catch (JMSException ex) {
                    throw new RuntimeException(ex);
                }
            } while (receivedCounter < outputBatchSize);
            
            LOGGER.log(Level.INFO,"All Messages of this batch of size (" + outputBatchSize + ") Received");
        } catch (RuntimeException ex) {
            throw ex;
        } finally {
            JMSHelper.closeConnectionAndSession(hashMap);
        }
    }
    
    private HashMap retryConnection() {
        try {
            if (retryCount++ < serverConnectionRetries) {
                
                LOGGER.log(Level.INFO, "Waiting " + serverConectionRetryInterval + " secs before restarting JMS server");
                try {
                    Thread.sleep(serverConectionRetryInterval * 1000);
                } catch(InterruptedException e) {
                    LOGGER.log(Level.SEVERE, "Exception occurred while the thread was sleeping.", e);
                }
                
                return JMSHelper.createQueueReceiver(jmsServerHostName, jmsServerPort, consumerDestName);
            }
        } catch (Exception e) {
            if (retryCount < serverConnectionRetries) {
                return retryConnection();
            }
        }
        return null;
    }
    
}
