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
 * @(#)Request.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.msmq;

import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.msmqbc.MSMQPayLoad;
import com.sun.jbi.msmqbc.exception.MSMQException;
import com.sun.jbi.msmqbc.util.MSMQUtil;



/**
 * Request allows receiving of a MSMQ message as a response.
 * 
 */
public class Request {
	
    private static final Messages mMessages = Messages.getMessages(Request.class);

    private static final Logger mLogger = Messages.getLogger(Request.class);
    
    private ReceiveChannelImpl mMSMQChannel;
    
    protected Request (ReceiveChannelImpl channel) {
    	mMSMQChannel = channel;	
    }

    public MSMQPayLoad receive(byte[] messageId) throws MSMQException{
    	
    	MSMQPayLoad msg;
    	try{
    		msg = mMSMQChannel.receiveAckResponseMessage(messageId.toString());
    	}catch(MSMQException e){
    		throw new MSMQException(MSMQUtil.getStackTraceAsString(e));
    	}
    	mLogger.log(Level.INFO, "Request_RECEIVE_SUCCESS");
    	
    	return msg;
    }
    
}
