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
 * @(#)Util.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import javax.jms.DeliveryMode;
import javax.jms.Session;
import javax.jms.Message;
import javax.jms.TextMessage;
import javax.jms.StreamMessage;
import javax.jms.MapMessage;
import javax.jms.ObjectMessage;
import javax.jms.BytesMessage;

import com.sun.jbi.jmsbc.extensions.JMSConstants;

/**
 * Utility class for JMS related things 
 */
public class Util {
    
    public static String toStringMessageType (int msgType) {
        switch (msgType) {
            case Channel.MESSAGE_TYPE_BYTES:
                return JMSConstants.BYTES_MESSAGE;
            case Channel.MESSAGE_TYPE_TEXT:
                return JMSConstants.TEXT_MESSAGE;
            case Channel.MESSAGE_TYPE_STREAM:
                return JMSConstants.STREAM_MESSAGE;
            case Channel.MESSAGE_TYPE_MAP:
                return JMSConstants.MAP_MESSAGE;
            case Channel.MESSAGE_TYPE_OBJECT:
                return JMSConstants.OBJECT_MESSAGE;
            case Channel.MESSAGE_TYPE_MESSAGE:
                return JMSConstants.MESSAGE_MESSAGE;
            default:
                return "Unknown JMS Message Type";
        }
    }

    public static String toStringDeliveryMode (int deliveryMode) {
        switch (deliveryMode) {
            case DeliveryMode.PERSISTENT:
                return JMSConstants.DELIVERYMODE_PERSISTENT;
            case DeliveryMode.NON_PERSISTENT:
                return JMSConstants.DELIVERYMODE_NON_PERSISTENT;
            default:
                return JMSConstants.DELIVERYMODE_NON_PERSISTENT;
        }        
    }

    public static String toStringDestinationType (int destinationType) {
        switch (destinationType) {
            case Channel.DESTINATION_TYPE_TOPIC:
                return "Topic";
            case Channel.DESTINATION_TYPE_QUEUE:
                return "Queue";
            default:
                return "Unknown Destination Type";
        }
    }

    
    public static int toIntDestinationType (String destinationType) {
        if (destinationType.equalsIgnoreCase("Topic")) {
            return Channel.DESTINATION_TYPE_TOPIC;
        } else if (destinationType.equalsIgnoreCase("Queue")) {
            return Channel.DESTINATION_TYPE_QUEUE;
        } else {
            return Channel.DESTINATION_TYPE_UNKNOWN;
        }
    }
    
    public static int toIntMessageType (String msgType) {
        if (msgType.equals(toStringMessageType(Channel.MESSAGE_TYPE_BYTES))) {
            return Channel.MESSAGE_TYPE_BYTES;
        } else if (msgType.equals(toStringMessageType(Channel.MESSAGE_TYPE_TEXT))) {
            return Channel.MESSAGE_TYPE_TEXT;
        } else if (msgType.equals(toStringMessageType(Channel.MESSAGE_TYPE_STREAM))) {
            return Channel.MESSAGE_TYPE_STREAM;
        } else if (msgType.equals(toStringMessageType(Channel.MESSAGE_TYPE_MAP))) {
            return Channel.MESSAGE_TYPE_MAP;
        } else if (msgType.equals(toStringMessageType(Channel.MESSAGE_TYPE_OBJECT))) {
            return Channel.MESSAGE_TYPE_OBJECT;
        } else if (msgType.equals(toStringMessageType(Channel.MESSAGE_TYPE_MESSAGE))) {
            return Channel.MESSAGE_TYPE_MESSAGE;
        } else {
            return -1;
        }
    }
    
    public static int toIntAcknowledgementMode (String ackMode) {
        if (ackMode.equals(JMSConstants.AUTO_ACKNOWLEDGE)) {
            return Session.AUTO_ACKNOWLEDGE;
        } else if (ackMode.equals(JMSConstants.CLIENT_ACKNOWLEDGE)) {
            return Session.CLIENT_ACKNOWLEDGE;
        } else if (ackMode.equals(JMSConstants.DUPS_OK_ACKNOWLEGE)) {
            return Session.DUPS_OK_ACKNOWLEDGE;
        } else { // Unknown
            return -1;
        }
    }
    
    public static int toIntDeliveryMode (String deliveryMode) {
        if (deliveryMode.equals(JMSConstants.DELIVERYMODE_PERSISTENT)) {
            return DeliveryMode.PERSISTENT;
        } else if (deliveryMode.equals(JMSConstants.DELIVERYMODE_NON_PERSISTENT)) {
            return DeliveryMode.NON_PERSISTENT;
        } else {
            return DeliveryMode.NON_PERSISTENT;            
        }
    }         

    static public String jmsMessageObjectTypeToString (Message jmsMsg) {
        return jmsMsg.getClass().getName();
    }
    
    static public ClassLoader setContextClassLoader(ClassLoader cls){
    	ClassLoader result = Thread.currentThread().getContextClassLoader();
    	if(cls != null && cls != result){
    		Thread.currentThread().setContextClassLoader(cls);
    	}
    	return result;
    }
}
