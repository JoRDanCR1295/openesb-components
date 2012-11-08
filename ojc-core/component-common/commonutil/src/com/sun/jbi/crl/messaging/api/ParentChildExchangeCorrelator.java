package com.sun.jbi.crl.messaging.api;

import javax.jbi.messaging.MessageExchange;

public interface ParentChildExchangeCorrelator {

	/**
	 * Do assignment or propagation of properties from
	 * parent exchange to child exchange. 
	 * @param parentExchange parent Message Exchange.
	 * @param childExchange child Message Exchange.
	 */
	void assignChildExchange(MessageExchange parentExchange, 
							 MessageExchange childExchange) throws CorrelatedMessageExchangeException;
	
}
