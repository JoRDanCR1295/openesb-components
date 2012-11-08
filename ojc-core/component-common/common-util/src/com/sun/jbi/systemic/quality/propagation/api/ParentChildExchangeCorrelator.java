package com.sun.jbi.systemic.quality.propagation.api;

import javax.jbi.messaging.MessageExchange;
/**
 * This is used to do assignment of properties (Transaction/Security etc) from
 * parent exchange to child exchange.
 * @author radval
 *
 */
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
