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

package com.sun.jbi.recoverytest;

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
public class FileBasedJMSMessageReceiver extends Thread {

    //private static Logger LOGGER = Logger.getLogger("com.sun.jbi.recoverytest.JMSMessageReceiver");
    private static Logger LOGGER = Logger.getLogger(JMSMessageReceiver.class.getName());
    private Properties properties;
    private Properties testCaseProperties;    
    
    static final int serverConnectionRetries = 360;
    static final int serverConectionRetryInterval = 10;
    private static int retryCount = 0;
    
    private String jmsServerHostName;
    private String jmsServerPort;
    private String consumerDestName;
    private int batchSize;
    private static int endCounter = 0;

    private List sentMessages = new ArrayList();
    private List expectedMessages = new ArrayList();
    private List duplicateMessages = new ArrayList();
    private List otherMessages = new ArrayList();

    private String testMessageTemplate;
    private String templateDirectory;
    private int messageTimeout;
    private long endTime;
    
    public FileBasedJMSMessageReceiver(Properties properties, Properties testCaseProperties) {
        
        this.properties = properties;
        this.testCaseProperties = testCaseProperties;        
        this.jmsServerHostName = properties.getProperty("JmsServerHostName");
        this.jmsServerPort = properties.getProperty("JmsServerPort");
        this.consumerDestName = testCaseProperties.getProperty("ConsumerQueue");
        this.batchSize = Integer.parseInt(properties.getProperty("BatchSize"));
        this.messageTimeout = Integer.parseInt(properties.getProperty("MessageTimeout"));
        String outputTemplateFile = testCaseProperties.getProperty("OutputTemplateFile");
        try {
            this.testMessageTemplate = Utility.readTextFromFile(outputTemplateFile, "UTF-8"); 
            this.templateDirectory = (new File(outputTemplateFile)).getParent();
        } catch (IOException e) {
            LOGGER.severe(e.getMessage());
        }
        
        endCounter = endCounter + batchSize;
        for(int i = endCounter - batchSize + 1; i <= endCounter; i++) {
            String testMessage = Utility.replaceAll(testMessageTemplate, "${counter}", i + "");
            sentMessages.add(new MessageWithCounter(testMessage, i));
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
    
    private int getCounter(String msg) {
        for (int i = 0; i < sentMessages.size(); i++) {
            MessageWithCounter mwc = (MessageWithCounter) sentMessages.get(i);
            if (mwc.msg.equals(msg)) {
                return mwc.counter;
            }
        }
        return -1;
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
                    for (int i = 0; i < expectedMessages.size(); i++) {
                        int id = ((Integer)expectedMessages.get(i)).intValue() - 1;
                        int idx = id % batchSize;
                        LOGGER.log(Level.SEVERE,  "Missing message(" + id + "): " + ((MessageWithCounter)sentMessages.get(idx)).msg);
                    }
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
                    String receivedMessage = ((TextMessage) receivedTextMessage).getText();
                    receivedCounter = getCounter(receivedMessage);
                    if (receivedCounter > 0) {
                        Integer rc = new Integer(receivedCounter);
                        
                        endTime = Calendar.getInstance().getTimeInMillis() + messageTimeout*1000;
                        boolean containsMessage = expectedMessages.remove(new Integer(receivedCounter));
                        
                        if (containsMessage) {
                            LOGGER.log(Level.INFO, "Received counter id (" + receivedCounter + ") message (" + receivedMessage + ")");
                        } else {
                            //Check whether duplicate messages are allowed.
                            if("yes".equalsIgnoreCase(properties.getProperty("AllowDuplicateMessages"))){
                                LOGGER.log(Level.INFO, "Received a duplicate message; continuing the test since the property AllowDuplicateMessages is set to yes.");
                                LOGGER.log(Level.INFO, "Received counter id (" + receivedCounter + ") message (" + receivedMessage + ")");
                                duplicateMessages.add(new Integer(receivedCounter));
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
                        LOGGER.log(Level.INFO, "Received message (" + receivedMessage + ")");
                        Utility.writeTextToFile(templateDirectory + File.separator + "unexpected.txt", receivedMessage, "UTF-8");
                        otherMessages.add(receivedMessage);
                    }
                    
                } catch (JMSException ex) {
                    throw new RuntimeException(ex);
                }
                if (expectedMessages.size() == 0) {
                    doContinue = false;
                    sentMessages.clear();
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
        String retMesg = null;
        int msgId;
        
        Iterator iter = duplicateMessages.iterator();
        Utility.print("");
        while (iter.hasNext()) {
            int counterValue = ((Integer)iter.next()).intValue();
            retMesg = ((MessageWithCounter)sentMessages.get(counterValue - 1)).msg;
            LOGGER.log(Level.INFO, "** Duplicate counter id (" + counterValue + ") Messages Received, message (" + retMesg + ")(not expected)");
        }

        Utility.print("");
        iter = otherMessages.iterator();
        while (iter.hasNext()) {
            retMesg = (String)iter.next();
            LOGGER.log(Level.INFO, "** Other Messages Received, message (" + retMesg + ")(not expected)");
        }
    }
    
    static class MessageWithCounter {
        public String msg;
        public int counter;
        public MessageWithCounter(String msg, int counter) {
            this.msg = msg;
            this.counter = counter;
        }
    }
}
