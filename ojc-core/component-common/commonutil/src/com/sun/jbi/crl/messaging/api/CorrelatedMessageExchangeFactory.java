package com.sun.jbi.crl.messaging.api;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;

public interface CorrelatedMessageExchangeFactory {

	/**
	 * create InOnly child MessageExchange given a parent MessageExchange.
	 * @param parentExchange Parent MessageExchange
	 * @return InOnly MessageExchange
	 */
	public InOnly createInOnlyExchange(MessageExchange parentExchange) throws CorrelatedMessageExchangeException;
	
	/**
	 * create InOut child MessageExchange given a parent MessageExchange
	 * @param parentExchange Parent MessageExchange
	 * @return InOut MessageExchange
	 */
	public InOut createInOutExchange(MessageExchange parentExchange) throws CorrelatedMessageExchangeException;
	
	/**
	 * send this message exchange asynchronously
	 * @param messageExchange The MessageExchange to send. Should have been created by this factory.
	 * @throws CorrelatedMessageExchangeException
	 */
	public void send(MessageExchange messageExchange) throws CorrelatedMessageExchangeException;
	    
	/**
	 * send this message exchange synchronously
	 * @param messageExchange The MessageExchange to send. Should have been created by this factory.
	 * @throws CorrelatedMessageExchangeException
	 */
	public void sendSync(MessageExchange messageExchange) throws CorrelatedMessageExchangeException;
	
	
	/**
	 * send this message exchange synchronously
	 * @param messageExchange The MessageExchange to send. Should have been created by this factory.
	 * @param timeout Time-out
	 * @throws CorrelatedMessageExchangeException
	 */
	public void sendSync(MessageExchange messageExchange, long timeout) throws CorrelatedMessageExchangeException;
	
}
