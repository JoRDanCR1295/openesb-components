/*
 * MessageExchangeListener.java
 */

package com.sun.jbi.sample.component.common;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

/**
 * This interface defines the notification mechanism with which the message exchange object received
 * from the delivery channel is processed by the interesting parties. A particular service endpoint
 * implementation would first register the implementation of this interface with MessageExchangeSupport
 * to get the notification of a received message exchange on a particular service endpoint and the
 * implementation of the interface would process the message exchange when notified.
 *
 * When the com.sun.jbi.sample.component.common.DefaultMessageExchangeReceiver receives the message exchange
 * object, it asks the MessageExchangeSupport to notify the MessageExchangeLister that is registered for the
 * received message exchange object to process the message exchange object.
 * @see com.sun.jbi.sample.component.common.MessageExchangeSupport
 * @see com.sun.jbi.sample.component.common.DefaultMessageExchangeReceiver
 * @see com.sun.jbi.sample.component.common.deployment.ProviderEndpoint
 * @see com.sun.jbi.sample.component.common.deployment.ProviderEndpoint
 * @author chikkala
 */
public interface MessageExchangeListener {
    /**
     * MessageExchangeSupport will call this method to notify the lister implementation that a 
     * message exchange is received from the delivery channel.
     * @param me MessageExchange Object
     */
    void messageExchangeReceived(ExchangeStatus status, MessageExchange me);
}
