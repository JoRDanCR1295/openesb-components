/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: MessageExchangeHandler.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

public interface MessageExchangeHandler {
    /**
     * this method will be invoked to process the MessageExchange
     * object.
     * @param msgExchange  MessageExchange object to process.
     */
    void processMessageExchange(ExchangeStatus status, MessageExchange msgExchange);
    
}
