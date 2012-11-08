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
 * @(#)MessageExchangeSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.jbi.internationalization.Messages;

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 *
 */
public class MessageExchangeSupport {

    private static final Messages mMessages =
            Messages.getMessages(MessageExchangeSupport.class);
    private final static Logger mLogger =
            Messages.getLogger(MessageExchangeSupport.class);
    static Map<String, RedeliveryListener> redeliveryListeners =
            Collections.synchronizedMap(new HashMap<String, RedeliveryListener>());

    /** Creates a new instance of MessageExchangeSupport */
    private MessageExchangeSupport() {
    }

    /**
     * Add a redelivery listener for a message exchange
     * @param messageExchangeId the ID of the exchange to register a reply listener for
     * @param listener The redelivery listener to notify when redelivery criteria is met
     * @param exchangeMetaData the metadata needed to reinitiate a message exchange
     */
    public static void addRedeliveryListener(String messageExchangeId, RedeliveryListener listener) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Adding redelivery listener for messsage exchange:" + messageExchangeId);
        }
        redeliveryListeners.put(messageExchangeId, listener);
    }

    /**
     * Remove the redelivery listener 
     * @param messageExchangeId the exchange ID for the redelivery listener to remove
     */
    public static void removeRedeliveryListener(String messageExchangeId) {
        RedeliveryListener listener = redeliveryListeners.remove(messageExchangeId);
    }

    /**
     * Notify the redelivery listener of a re-send of the message exchange if the redelivery
     * criteria is met.
     * Use <code>removeRedeliveryListener<code> to explicitly remove the listener when retry limit is exhausted.
     * @param mep the message exchange instance
     */
    public static void notifyOfRedelivery(MessageExchange mep) throws MessagingException {
        String messageExchangeId = mep.getExchangeId();
        RedeliveryListener listener = (RedeliveryListener) redeliveryListeners.get(messageExchangeId);

        if (listener != null) {
            listener.onRedelivery(mep);
        } else {
            throw new MessagingException("Cannot redelivery the message exchange " + messageExchangeId + " because no redelivery listener has been registered");
        }
    }
}
