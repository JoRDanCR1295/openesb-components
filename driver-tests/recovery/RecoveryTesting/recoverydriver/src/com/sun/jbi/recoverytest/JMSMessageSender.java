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
 * @(#)JMSMessageSender.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jms.DeliveryMode;
import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TextMessage;

/**
 * @author mbhasin
 *
 */
public class JMSMessageSender extends Thread {

    //private static Logger LOGGER = Logger.getLogger("com.sun.jbi.recoverytest.JMSMessageSender");    
    private static Logger LOGGER = Logger.getLogger(JMSMessageSender.class.getName());

    private Properties properties;
    private Properties testCaseProperties;    
    
    private static final int serverConnectionRetries = 10;
    private static final int serverConectionRetryInterval = 2000;
    private static int retryCount = 0;
    private static int counter = 1;

    private String jmsServerHostName;
    private String jmsServerPort;
    private String producerDestName;
    private int batchSize;
    private String testMessage;
    
    public JMSMessageSender(Properties properties, Properties testCaseProperties) {
        
        this.properties = properties;
        this.testCaseProperties = testCaseProperties;
        this.jmsServerHostName = properties.getProperty("JmsServerHostName");
        this.jmsServerPort = properties.getProperty("JmsServerPort");
        this.producerDestName = testCaseProperties.getProperty("PublisherQueue");
        this.batchSize = Integer.parseInt(properties.getProperty("BatchSize"));
        this.testMessage = testCaseProperties.getProperty("MessageToSend"); 
    }
    
    public void run() {
        try {
            sendMessages();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Exception occurred while sending messages.", e);            
            System.exit(0);
        }
    }
    
    /**
     * @throws Exception
     */
    private void sendMessages() throws Exception {
        long iterationCount = 1;
        QueueSender queueSender = null;
        QueueSession queueSession = null;
        HashMap hashMap = JMSHelper.createQueueSender(jmsServerHostName, jmsServerPort, producerDestName);
        
        queueSender = (QueueSender) hashMap.get("QueueSender");
        queueSession = (QueueSession) hashMap.get("QueueSession");

        try {
            do {
                try {
                    TextMessage textMessage = queueSession.createTextMessage();
                    textMessage.setJMSDeliveryMode(DeliveryMode.PERSISTENT);
                    textMessage.setText(testMessage);
                    textMessage.setLongProperty("counter", counter);
                    LOGGER.log(Level.INFO,"Sending counter id  (" + counter + ") message (" + textMessage.getText() + ")");
                    queueSender.send(textMessage);
                    iterationCount++;
                    counter++;
                } catch (JMSException ex) {
                    Utility.print("JMS Error.. Possible JMS Server crash.. Retrying again..Retry count");
                    hashMap = retryConnection();
                    if (hashMap != null) {
                        // reset retry count
                        retryCount = 0;
                        queueSender = (QueueSender) hashMap.get("QueueSender");
                        queueSession = (QueueSession) hashMap.get("QueueSession");
                        continue;
                    } else {
                        throw ex;
                    }
                }
            } while (iterationCount <= batchSize);
            
        } catch (JMSException e) {
            throw e;
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
                
                return JMSHelper.createQueueSender(jmsServerHostName, jmsServerPort, producerDestName);                
            } 
        } catch (Exception e) {
            if (retryCount < serverConnectionRetries) {
                return retryConnection();
            }
        }
        return null;
    }
}
