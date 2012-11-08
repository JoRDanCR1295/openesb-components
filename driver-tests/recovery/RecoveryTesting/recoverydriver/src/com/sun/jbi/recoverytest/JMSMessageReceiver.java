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
 * @(#)JMSMessageReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

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
 * @author mbhasin, pbhagat
 *
 */
public class JMSMessageReceiver extends Thread {

    //private static Logger LOGGER = Logger.getLogger("com.sun.jbi.recoverytest.JMSMessageReceiver");
    private static Logger LOGGER = Logger.getLogger(JMSMessageReceiver.class.getName());
    private Properties properties;
    private Properties testCaseProperties;    
    
    static final int serverConnectionRetries = 10;
    static final int serverConectionRetryInterval = 2000;
    private static int retryCount = 0;
    
    private String jmsServerHostName;
    private String jmsServerPort;
    private String consumerDestName;
    private int batchSize;
    private static int endCounter = 0;

    private List expectedMessages = new ArrayList();
    private List duplicateMessages = new ArrayList();
    private List otherMessages = new ArrayList();

    private String messageToSend;
    
    private long messageTimeout;
    private long endTime;
    
    public JMSMessageReceiver(Properties properties, Properties testCaseProperties) {
        
        this.properties = properties;
        this.testCaseProperties = testCaseProperties;        
        this.jmsServerHostName = properties.getProperty("JmsServerHostName");
        this.jmsServerPort = properties.getProperty("JmsServerPort");
        this.consumerDestName = testCaseProperties.getProperty("ConsumerQueue");
        this.batchSize = Integer.parseInt(properties.getProperty("BatchSize"));
        this.messageTimeout = Long.parseLong(properties.getProperty("MessageTimeout"));
        this.messageToSend = testCaseProperties.getProperty("MessageToSend");
        
        endCounter = endCounter + batchSize;
        for(int i = endCounter - batchSize + 1; i <= endCounter; i++) {
            expectedMessages.add(new Integer(i));
        }
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
        
        endTime = Calendar.getInstance().getTimeInMillis() + messageTimeout*1000;

        try {
            TextMessage receivedTextMessage = null;
            int receivedCounter = -1;
            boolean doContinue = true;
            do {
                
                if(endTime - Calendar.getInstance().getTimeInMillis() < 0){
                    //BatchTimeoutt time has elaspsed. Notify by email
                    String message = "The MessageTimeout of " + messageTimeout 
                            + " seconds has reached and no message was received. Terminating the test.";
                    LOGGER.log(Level.SEVERE,  message);
                    new EmailHelper().sendmail(properties, message);
                    System.exit(1);
                }
                
                try {
                    receivedTextMessage = (TextMessage) queueReceiver.receive(messageTimeout * 1000);
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
                    receivedCounter = (int) receivedTextMessage.getLongProperty("counter");
                    String receivedMessage = ((TextMessage) receivedTextMessage).getText();

                    if (receivedMessage.equals(messageToSend)) {
                        endTime = Calendar.getInstance().getTimeInMillis() + messageTimeout*1000;
                        boolean containsMessage = expectedMessages.remove(new Integer(receivedCounter));
                        
                        if (containsMessage) {
                            LOGGER.log(Level.INFO, "Received counter id (" + receivedCounter + ") message (" + receivedMessage + ")");
                        } else {
                            //Check whether duplicate messages are allowed.
                            if("yes".equalsIgnoreCase(properties.getProperty("AllowDuplicateMessages"))){
                                LOGGER.log(Level.INFO, "Received a duplicate message; continuing the test since the property AllowDuplicateMessages is set to yes.");
                                LOGGER.log(Level.INFO, "Received counter id (" + receivedCounter + ") message (" + receivedMessage + ")");
                                duplicateMessages.add(receivedTextMessage);
                            } else {
                                String message = "Terminating the test because a duplicate message was received.";
                                LOGGER.log(Level.SEVERE,  message);
                                LOGGER.log(Level.INFO, "Received counter id (" + receivedCounter + ") message (" + receivedMessage + ")");
                                new EmailHelper().sendmail(properties, message);
                                System.exit(1);
                            }
                        }
                    } else {
                        LOGGER.log(Level.INFO, "Received a message with unexpected text.");
                        LOGGER.log(Level.INFO, "Received counter id (" + receivedCounter + ") message (" + receivedMessage + ")");
                        otherMessages.add(receivedTextMessage);
                    }
                    
                } catch (JMSException ex) {
                    throw new RuntimeException(ex);
                }
                if (expectedMessages.size() == 0) {
                    doContinue = false;
                }
            } while (doContinue);
            
            LOGGER.log(Level.INFO,"All Messages of this batch of size (" + batchSize + ") Received");
            printResults();
            
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
    
    private void printResults() throws JMSException {
        TextMessage retMesg = null;
        int msgId;
        
        Iterator iter = duplicateMessages.iterator();
        Utility.print("");
        while (iter.hasNext()) {
            retMesg = (TextMessage)iter.next();
            msgId = (int) retMesg.getLongProperty("counter");
            LOGGER.log(Level.INFO, "** Duplicate counter id (" + msgId + ") Messages Received, message (" + retMesg.getText() + ")(not expected)");
        }

        Utility.print("");
        iter = otherMessages.iterator();
        while (iter.hasNext()) {
            retMesg = (TextMessage)iter.next();
            msgId = (int) retMesg.getLongProperty("counter");
            LOGGER.log(Level.INFO, "** Other counter id (" + msgId + ") Messages Received, message (" + retMesg.getText() + ")(not expected)");
        }
    }
}
