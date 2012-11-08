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
 * @(#)Channel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.msmq;

import javax.transaction.Transaction;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.MSMQPayLoad;
import com.sun.jbi.msmqbc.exception.MSMQException;
import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQOutput;
import com.sun.jbi.msmqbc.extensions.MSMQMessage;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;

/**
 * A channel allows for sending and receiving MSMQ messages
 * It is associated with the web services endpoint and operation
 * 
 * @author Sun Microsystems
 */
public interface Channel {

    /**
     * Get the Endpoint associated with this channel
     *
     * @return The assoicated endpoint
     */
    public Endpoint getEndpoint();

    /**
     * Get the MSMQ operation associated with this channel
     *
     * @return The associated MSMQ operation
     */
    public MSMQOperation getMSMQOperation();

    /**
     * Get the MSMQ Input associated with this channel
     *
     * @return The associated MSMQ Input message
     */
    public MSMQInput getMSMQOperationInput();

    /**
     * Get the MSMQ Output associated with this channel
     *
     * @return The associated MSMQ Output message
     */
    public MSMQOutput getMSMQOperationOutput();

    /**
     * Get the MSMQ Message associated with this channel
     *
     * @return The associated MSMQ message
     */
    public MSMQMessage getMSMQMessage();

    /**
     * Opens the channel by doing the following:
     *
     * Creates connection to MQMQ Queue for InOnly Inbound 
     *
     * @throws MSMQException upon error
     */
    public void open() throws MSMQException;

    /**
     * Closes the channel by doing the following:
     *
     * Closes and opened connection.
     * 
     */
    public void close() throws MSMQException;

    /**
     * send msmq Message on this channel
     * Connection is created if not already created.
     *
     * @param msg the msmq message to be send
     * @throws MSMQException upon error
     */
    public void send(MSMQPayLoad msg) throws MSMQException;

    /**
     * send msmq Message on this channel
     * Connection is created if not already created.
     *
     * @param msg the msmq message to be send
     * @param tx transaction value
     * @throws MSMQException upon error
     */
    public void send(MSMQPayLoad msg, Transaction tx) throws MSMQException;

    /**
     * send msmq Message and receive ack msg on this channel
     * Connection is created if not already created.
     *
     * @param msg the msmq message to be send
     * @throws MSMQException upon error
     */
    public MSMQPayLoad sendAckResponse(MSMQPayLoad msg) throws MSMQException;
    
    /**
     * receive msmq message on this channel
     * Connection is created if not already created.
     *
     * @throws MSMQException upon error
     */
    public MSMQPayLoad receive() throws MSMQException;

    /**
     * receive msmq message on this channel
     * Connection is created if not already created.
     *
     * @throws MSMQException upon error
     */
    public MSMQPayLoad receiveAckResponseMessage(String messageId) throws MSMQException;
    
    /**
     * Get the XAResource associate with this channel
     */
    public XAResource getXAResource() throws XAException;
}
