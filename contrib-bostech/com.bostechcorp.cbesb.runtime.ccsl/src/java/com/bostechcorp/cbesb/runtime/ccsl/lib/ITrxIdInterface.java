package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.util.HashMap;
import java.util.LinkedList;
import org.apache.commons.logging.Log;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;

/**
 * Classes for TrxID must implement this interface
 */

public interface ITrxIdInterface {
	/**
	 * The default method to call to extract TrxID from the MessageExchange and return as the String.
	 * @param logger  - the loggger object
	 * @param exhange  - the MessageExchange object passed in
	 * @return - the String to represent the TrxID for the exchange
	 */
	public String process(Log logger, MessageExchange exchange);

	/**
	 * The default method to call to extract TrxID from the MessageExchange and return as the String.
	 * @param logger   - the loggger object
	 * @param exchange - the MessageExchange object passed in
	 * @param content - the String content of the exchange object
	 * @return - the String to represent the TrxID for the exchange
	 */
	
	public String processAsString(Log logger, MessageExchange exchange, String content);

	
}
