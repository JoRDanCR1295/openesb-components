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
 * @(#)AbstractChannel.java 
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
 * Abstract implementation of com.sun.jbi.msmqbc.msmq.Channel
 * 
 * @author Sun Microsystems
 */
public abstract class AbstractChannel implements Channel {

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#close()
     */
    public void close() throws MSMQException {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#getEndpoint()
     */
    public Endpoint getEndpoint() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#getMSMQOperation()
     */
    public MSMQOperation getMSMQOperation() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#getMSMQOperationInput()
     */
    public MSMQInput getMSMQOperationInput() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#getMSMQOperationOutput()
     */
    public MSMQOutput getMSMQOperationOutput() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#getMSMQMessage()
     */
    public MSMQMessage getMSMQMessage() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#open()
     */
    public void open() throws MSMQException {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#receive()
     */
    public MSMQPayLoad receive() throws MSMQException {
        // TODO Auto-generated method stub
        return null;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#receiveAckResponseMessage()
     */
    public MSMQPayLoad receiveAckResponseMessage(String messageId) throws MSMQException{
    	// TODO Auto-generated method stub
    	return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#send(com.sun.jbi.msmqbc.msmq.MSMQPayLoad)
     */
    public void send(MSMQPayLoad msg) throws MSMQException {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#send(com.sun.jbi.msmqbc.msmq.MSMQPayLoad,javax.transaction.Transaction)
     */
    public void send(MSMQPayLoad msg, Transaction tx) throws MSMQException {
        // TODO Auto-generated method stub
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#send(com.sun.jbi.msmqbc.msmq.MSMQPayLoad)
     */
    public MSMQPayLoad sendAckResponse(MSMQPayLoad msg) throws MSMQException {
        // TODO Auto-generated method stub
    	return null;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.msmqbc.msmq.Channel#getXAResource()
     */

    public XAResource getXAResource() throws XAException {
        // TODO Auto-generated method stub        
        return null;
    }

}
