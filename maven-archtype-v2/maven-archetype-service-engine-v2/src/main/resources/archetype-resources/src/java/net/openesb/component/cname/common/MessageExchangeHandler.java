#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * MessageExchangeHandler.java
 *
 */

package net.openesb.component.${artifactId}.common;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

/**
 * This interface is a Handler to perform message exchanges when the component
 * receives the MessageExchange object from the delivery channel.
 * Implementation of this interface should implement the processing of
 * the active, error, done status of the MessageExchange object according to
 * the MEP for which the MessageExchange object is created.
 *
 * @author chikkala
 */
public interface MessageExchangeHandler {
    /**
     * this method will be invoked to process the MessageExchange
     * object.
     * @param msgExchange  MessageExchange object to process.
     */
    void processMessageExchange(ExchangeStatus status, MessageExchange msgExchange);
    
}
