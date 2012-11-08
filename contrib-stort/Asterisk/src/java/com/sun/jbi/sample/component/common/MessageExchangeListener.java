/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: MessageExchangeListener.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

public interface MessageExchangeListener {
    /**
     * MessageExchangeSupport will call this method to notify the lister implementation that a 
     * message exchange is received from the delivery channel.
     * @param me MessageExchange Object
     */
    void messageExchangeReceived(ExchangeStatus status, MessageExchange me);
}
