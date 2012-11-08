package com.sun.jbi.common.qos.messaging.filter;

import java.util.List;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 *
 * @author VFrolkin
 */
public interface FilterChain {

    /**
     * Add specified filer as first filter to this chains
     * @param e Filter to add
     */
    void addFirst(ExchangeFilter e);

    /**
     * Add specified filer as last filter to this chains
     * @param e Filter to add
     */
    void addLast(ExchangeFilter e);

    /**
     * Closes filter chains, closing all filters it contains
     * @throws MessagingException
     */
    void close() throws MessagingException;

    /**
     * Removes specified filter from chain
     * @param e Filter to remove
     * @return True if filter was contained in chain, false otherwise.
     */
    boolean remove(ExchangeFilter e);

    /**
     * Processes an exchange sent via the NMR.
     * @param msg The sent exchange.
     * @param params Optional params needed by filter, provided by <code>FilterBase</code>.
     * @return The message exchange to send or <code>null</code>.
     * @throws MessagingException if an error occurs processing.
     */
    public MessageExchange processOutgoingExchange(MessageExchange msg, Object... params) throws MessagingException;

    /**
     * Processes an exchange received from the NMR.
     * @param msg The received exchange.
     * @param params Optional params needed by filter, provided by <code>FilterBase</code>.
     * @return The received message exchange or <code>null</code>.
     * @throws MessagingException if an error occurs processing.
     */
    public MessageExchange processIncomingExchange(MessageExchange msg, Object... params) throws MessagingException;

    List<ExchangeFilter> getFilterList();
}
