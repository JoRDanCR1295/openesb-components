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
 * @(#)SendChannel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import javax.transaction.Transaction;

import javax.jms.Destination;
import javax.jms.Message;

/**
 *
 * SendChannel is an abstraction of a JMS message out flow "pipeline".
 * Messages flow to the JMS Server.
 */
public interface SendChannel extends Channel {
    
    /**
     * Creates a JMS Message of a particular type.
     * The channel must be opened first before calling this method.
     *
     * @param jmsMessageType The JMS message type (see JMSConstants)
     * @return A JMS message instance of the specified type.
     * @throws ChannelException upon error.
     */
    public Message createMessage(String jmsMessageType) throws ChannelException;
    
    /**
     * Sends a Message on this Channel to the default destination.
     * Blocks until the send is completed before returning.
     *
     * @param msg The JMS Message to send
     * @throws ChannelException upon error
     */    
    public void send (Message msg) throws ChannelException;    
    
    /**
     * Sends a Message on this Channel to the default destination.
     * Blocks until the send is completed before returning.
     *
     * @param msg The JMS Message to send
     * @param msg The Transaction in which the send is operated.
     *
     * @throws ChannelException upon error
     */    
    //public void send (Message msg, Transaction tx) throws ChannelException;    

    /**
     * Sends a Message on this Channel to the specified destination.
     * Blocks until the send is completed before returning.
     *
     * @param msg The JMS Message to send
     * @param msg The JMS Destination to send to
     * @throws ChannelException upon error
     */    
    public void send (Message msg, Destination dest) throws ChannelException;    
    
    /**
     * Sends a Message on this Channel to the specified destination.
     * Blocks until the send is completed before returning.
     *
     * @param msg The JMS Message to send
     * @param msg The JMS Destination to send to
     * @param msg The Transaction in which the send is operated.
     *
     * @throws ChannelException upon error
     */    
    //public void send (Message msg, Destination dest, Transaction tx) throws ChannelException;  
    

    /**
     * Sends a Message on this Channel to the default destination and
     * receive a reply on a temporary destination created by the channel.
     *
     * @return The JMS "reply" message.
     *
     * @param msg The JMS Message to send
     * @param timeout Timeout specified in milliseconds waiting for the reply message.
     *
     * @throws ChannelException upon error
     */    
    public Message send (Message msg,
                         long timeout) throws ChannelException;     

    /**
     * Receive a message from the the default destination.
     * 
     * @param timeout - wait for the message for specified time.
     * @return Message - if could not receive message in the specified time then return null
     * @throws ChannelException
     */
    public Message synchronousReceive(long timeout) throws ChannelException;     

    /**
     * Receive a message from the specified destination.
     * 
     * @param dest
     * @param isTopic
     * @param timeout
     * @return Message - if could not receive message in the specified time then return null
     * @throws ChannelException
     */
    public Message synchronousReceive(String dest, boolean isTopic, long timeout) throws ChannelException;     
    /**
     * Send message to this destination and with specified replyTo destination.
     * 
     * @param msg
     * @param dest
     * @param isDestTopic
     * @param replyToDest
     * @param isReplyToDestTopic
     * @param timeout
     * @return Message - if could not receive message in the specified time then return null
     * @throws ChannelException
     */
    public Message send (Message msg, String dest, boolean isDestTopic, String replyToDest, boolean isReplyToDestTopic, long timeout) throws ChannelException;    

    /**
     * Send message to this destination and with specified replyTo destination.
     * 
     * @param msg
     * @param dest
     * @param isDestTopic
     * @param timeout
     * @return
     * @throws ChannelException
     */
    public Message send (Message msg, String dest, boolean isDestTopic, long timeout) throws ChannelException;    
    /**
     * Send message to this destination and with specified replyTo destination.
     * 
     * @param msg
     * @param dest
     * @param isDestTopic
     * @param replyToDest
     * @param isReplyToDestTopic
     * @throws ChannelException
     */
    public void send (Message msg, String dest, boolean isDestTopic, String replyToDest, boolean isReplyToDestTopic) throws ChannelException;    

    /**
     * Send message to this destination.
     * 
     * @param msg
     * @param dest
     * @param isDestTopic
     * @throws ChannelException
     */
    public void send (Message msg, String dest, boolean isDestTopic) throws ChannelException;    
}
