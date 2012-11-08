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
 * @(#)JMSHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.performance.jdbcjmsbd;

import com.sun.messaging.QueueConnectionFactory;
import java.util.HashMap;
import javax.jms.DeliveryMode;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueReceiver;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TextMessage;

public class JMSHelper {

    /**
     * The QueueSession and the QueueConnection must be closed by the caller
     */
    public static HashMap createQueueSender(String imqBrokerHostName, String imqBrokerHostPort, 
            String queueToSendMessagesTo) throws Exception {
    
        QueueConnection queueConnection = createQueueConnection(imqBrokerHostName, imqBrokerHostPort);        
        QueueSession queueSession = queueConnection.createQueueSession(false, Session.AUTO_ACKNOWLEDGE);
        Queue queue = new com.sun.messaging.Queue(queueToSendMessagesTo);
        QueueSender queueSender = queueSession.createSender(queue);
        
        HashMap hashMap = new HashMap();
        hashMap.put("QueueConnection", queueConnection);
        hashMap.put("QueueSession", queueSession);
        hashMap.put("QueueSender", queueSender);
        return hashMap;
    }    

    /**
     * The QueueSession and the QueueConnection must be closed by the caller
     */    
    public static HashMap createQueueReceiver(String imqBrokerHostName, String imqBrokerHostPort, 
            String queueToReadMessageFrom) throws Exception {
    
        QueueConnection queueConnection = createQueueConnection(imqBrokerHostName, imqBrokerHostPort);        
        QueueSession queueSession = queueConnection.createQueueSession(false, Session.AUTO_ACKNOWLEDGE);
        Queue queue = new com.sun.messaging.Queue(queueToReadMessageFrom);
        QueueReceiver queueReceiver = queueSession.createReceiver(queue);
        queueConnection.start();
        
        HashMap hashMap = new HashMap();
        hashMap.put("QueueConnection", queueConnection);
        hashMap.put("QueueSession", queueSession);
        hashMap.put("QueueReceiver", queueReceiver);
        return hashMap;
    }        
    
    public static void closeConnectionAndSession(HashMap hashMap) {

        if (hashMap.get("QueueSender") != null) {
            try {
                ((QueueSender)hashMap.get("QueueSender")).close();
            } catch (Exception e) {
            }            
        }        
        
        if (hashMap.get("QueueReceiver") != null) {
            try {
                ((QueueReceiver)hashMap.get("QueueReceiver")).close();
            } catch (Exception e) {
            }            
        }        
        
        if (hashMap.get("QueueSession") != null) {
            try {
                ((QueueSession)hashMap.get("QueueSession")).close();
            } catch (Exception e) {
            }            
        }        
        
        if (hashMap.get("QueueConnection") != null) {
            try {
                ((QueueConnection)hashMap.get("QueueConnection")).close();
            } catch (Exception e) {
            }            
        }
    }
    
    /*
     * Creates and returns a QueueConnection. It is the responsibility of the
     * caller to close the connection. 
     */
    private static QueueConnection createQueueConnection(String imqBrokerHostName, 
            String imqBrokerHostPort) throws Exception {
        QueueConnectionFactory queueConnFactory = new QueueConnectionFactory();
        queueConnFactory.setProperty("imqBrokerHostName", imqBrokerHostName);        
        queueConnFactory.setProperty("imqBrokerHostPort", imqBrokerHostPort);
        return queueConnFactory.createQueueConnection();        
    }
}
