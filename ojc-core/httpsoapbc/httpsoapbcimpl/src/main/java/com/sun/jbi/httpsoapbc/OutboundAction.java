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
 * @(#)OutboundAction.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.messaging.MessageExchange;


/**
 *
 */
public class OutboundAction implements Runnable {
    
    private final OutboundMessageProcessorPool mPool;
    private final MessageExchange mExchange;
    private static final Messages mMessages = Messages.getMessages(OutboundAction.class);
    private static final Logger mLog = Messages.getLogger(OutboundAction.class);
    
    /** Creates a new instance of OutboundAction */
    public OutboundAction(OutboundMessageProcessorPool pool, MessageExchange exchange) {
	mPool = pool;
        mExchange = exchange;
    }

    public void run() {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Processing started for message exchange " + mExchange.getExchangeId());
        }
        try {
	    OutboundMessageProcessor processor = mPool.retrieve();
            processor.processMessage(mExchange);
	    mPool.relinquish(processor);
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-E00775.Exception_during_exchange_processing", mExchange.getExchangeId());
            mLog.log(Level.SEVERE, text, ex);
        }
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Processing completed for message exchange " + mExchange.getExchangeId());
        }
    }
    
}
