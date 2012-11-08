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
 * @(#)FileBasedJMSMessageSender.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.performance.jmsbd;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jms.DeliveryMode;
import javax.jms.JMSException;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.TextMessage;

/**
 * @author Bing Lu
 *
 */
public class JMSMessageSender extends Thread implements Configuration {

    //private static Logger LOGGER = Logger.getLogger("com.sun.jbi.recoverytest.JMSMessageSender");    
    private static Logger LOGGER = Logger.getLogger(JMSMessageSender.class.getName());

    private static final int serverConnectionRetries = 10;
    private static final int serverConectionRetryInterval = 2000;
    private static int retryCount = 0;

    private Properties mProperties;
    private String mName;
    private String mJmsServerHostName;
    private String mJmsServerPort;
    private String mJmsQueue;
    private int mBatchSize;
    private String mMessageTemplate;
    private int mMessageDeliveryMode = DeliveryMode.NON_PERSISTENT;
    private ArrayList mInputMessageList = new ArrayList();
    
    public JMSMessageSender(Properties properties) {
        mProperties = properties;
        mName = properties.getProperty(NAME);
        mJmsServerHostName = properties.getProperty(JMS_SERVER_HOST_NAME);
        mJmsServerPort = properties.getProperty(JMS_SERVER_PORT);
        mJmsQueue = properties.getProperty(JMS_QUEUE);
        mBatchSize = Integer.parseInt(properties.getProperty(BATCH_SIZE));
        mMessageDeliveryMode = "PERSISTENT".equals(properties.getProperty(MESSAGE_DELIVERY_MODE))?
                DeliveryMode.PERSISTENT : DeliveryMode.NON_PERSISTENT;
        String inputTemplateFile = properties.getProperty(TEMPLATE_FILE);
        try {
            mMessageTemplate = Utility.readTextFromFile(inputTemplateFile, "UTF-8");
        } catch (IOException e) {
            LOGGER.severe(e.getMessage());
        }
        for (int i = 0; i < mBatchSize; i++) {
            String  msg = Utility.replaceAll(mMessageTemplate, "${counter}", i + "");
            mInputMessageList.add(msg);
        }    
    }

    @Override
    public void run() {
        try {
            sendMessages();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, mName + ": Exception occurred while sending messages.", e);
            System.exit(0);
        }
    }
    
    /**
     * @throws Exception
     */
    private void sendMessages() throws Exception {
        int counter = 0;
        QueueSender queueSender = null;
        QueueSession queueSession = null;
        HashMap hashMap = JMSHelper.createQueueSender(mJmsServerHostName, mJmsServerPort, mJmsQueue);
        
        queueSender = (QueueSender) hashMap.get("QueueSender");
        queueSession = (QueueSession) hashMap.get("QueueSession");

        try {
            do {
                try {
                    TextMessage textMessage = queueSession.createTextMessage();
                    textMessage.setJMSDeliveryMode(mMessageDeliveryMode);
                    textMessage.setText((String)mInputMessageList.get(counter));
                    LOGGER.log(Level.FINEST, mName + ": Sending counter id  (" + counter + ") message (" + textMessage.getText() + ")");
                    queueSender.send(textMessage);
                } catch (JMSException ex) {
                    Utility.print(mName + ": JMS Error.. Possible JMS Server crash.. Retrying again..Retry count");
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
                counter++;
            } while (counter < mBatchSize);
            
        } catch (JMSException e) {
            throw e;
        } finally {
            JMSHelper.closeConnectionAndSession(hashMap);
        }
    }
    
    private HashMap retryConnection() {
        try {
            if (retryCount++ < serverConnectionRetries) {
                LOGGER.log(Level.INFO, mName + ": Waiting " + serverConectionRetryInterval + " secs before restarting JMS server");
                try {
                    Thread.sleep(serverConectionRetryInterval * 1000);
                } catch(InterruptedException e) {
                    LOGGER.log(Level.SEVERE, mName + ": Exception occurred while the thread was sleeping.", e);
                }                
                
                return JMSHelper.createQueueSender(mJmsServerHostName, mJmsServerPort, mJmsQueue);
            } 
        } catch (Exception e) {
            if (retryCount < serverConnectionRetries) {
                return retryConnection();
            }
        }
        return null;
    }
    
}
