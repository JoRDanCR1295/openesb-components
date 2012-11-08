package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.util.Map;
import java.util.LinkedList;
import org.apache.commons.logging.Log;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;

/**
 * Classes for Script Components must implement this interface
 */

public interface IScriptObject {
	/**
	 * Start mode can be used for initialization. Consumer endpoints
	 * that do not use timed mode should set up a thread to generate
	 * exchanges in start mode.
	 * 
	 * @param logger - the loggger object
	 * @param rootDir - the root direcory of the SU
	 * @param componentContext - the componentContext object
	 * @param channel - the delivery channel object
	 * @param exchange - the MessageExchange object passed in
	 * @param params - the user paramaters in name/value pair
	 * @throws Exception
	 */
	public void start(Log logger, String rootDir, ComponentContext componentContext,
			DeliveryChannel channel, Map<String, String> params) throws Exception;

	/**
	 * The stop method should clean up resources
	 * 
	 * @param logger - the loggger object
	 * @param rootDir - the root direcory of the SU
	 * @param componentContext - the componentContext object
	 * @param channel - the delivery channel object
	 * @param params - the user paramaters in name/value pair
	 * @throws Exception
	 */
	public void stop(Log logger, String rootDir, ComponentContext componentContext,
			DeliveryChannel channel, Map<String, String> params) throws Exception;

	/**
	 * The run method is called when an exchange is sent to the endpoint.
	 * 
	 * @param logger - the loggger object
	 * @param rootDir - the root direcory of the SU
	 * @param componentContext - the componentContext object
	 * @param channel - the delivery channel object
	 * @param exchange - the MessageExchange object passed in
	 * @param params - the user paramaters in name/value pair
	 * @throws Exception
	 */
	public void run(Log logger, String rootDir, ComponentContext componentContext,
			DeliveryChannel channel, MessageExchange exchange, Map<String, String> params) throws Exception;

	/**
	 * The time method is called for a timed mode consumer endpoint.
	 * It returns a linked list of exchanges to be sent from the
	 * endpoint.
	 * 
	 * @param logger - the loggger object
	 * @param rootDir - the root direcory of the SU
	 * @param componentContext - the componentContext object
	 * @param channel - the delivery channel object
	 * @param exchange - the MessageExchange object passed in
	 * @param params - the user paramaters in name/value pair
	 * @return - the LinkList containning one of many MessageExchange objects
	 * @throws Exception
	 */
	public LinkedList time(Log logger, String rootDir, ComponentContext componentContext,
			DeliveryChannel channel, MessageExchange exchange, Map<String, String> params) throws Exception;
}
