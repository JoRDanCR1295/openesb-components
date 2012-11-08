 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.cxf;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceInvoker;

import java.util.List;

import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageContentsList;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.invoker.Invoker;

/**
 * Always single-threaded CXF Service invoker. 
 */
public class Jbi4CorbaServiceInvokerInterceptor extends
		AbstractPhaseInterceptor<Message> {

	/**
	 * Logger.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ProviderServiceInvoker.class);

	public Jbi4CorbaServiceInvokerInterceptor() {
		super(Phase.INVOKE);
	}

	@SuppressWarnings("unchecked")
	public void handleMessage(final Message message) {
		final Exchange exchange = message.getExchange();
		final Endpoint endpoint = exchange.get(Endpoint.class);
		final Service service = endpoint.getService();
		final Invoker invoker = service.getInvoker();

		LOG.debug("Executing the invocation in the SAME thread ["
				+ Thread.currentThread().getId() + "]");

		Exchange runableEx = message.getExchange();

		Object result = invoker.invoke(runableEx, getInvokee(message));
		if (!exchange.isOneWay()) {
			Endpoint ep = exchange.get(Endpoint.class);

			Message outMessage = runableEx.getOutMessage();
			if (outMessage == null) {
				outMessage = ep.getBinding().createMessage();
				exchange.setOutMessage(outMessage);
			}
			copyJaxwsProperties(message, outMessage);
			if (result != null) {
				MessageContentsList resList = null;
				if (result instanceof MessageContentsList) {
					resList = (MessageContentsList) result;
				} else if (result instanceof List) {
					resList = new MessageContentsList((List) result);
				} else if (result.getClass().isArray()) {
					resList = new MessageContentsList((Object[]) result);
				} else {
					outMessage.setContent(Object.class, result);
				}
				if (resList != null) {
					outMessage.setContent(List.class, resList);
				}
			}
		}

	}

	private Object getInvokee(Message message) {
		Object invokee = message.getContent(List.class);
		if (invokee == null) {
			invokee = message.getContent(Object.class);
		}
		return invokee;
	}
	
	private void copyJaxwsProperties(Message inMsg, Message outMsg) {
		outMsg.put(Message.WSDL_OPERATION, inMsg.get(Message.WSDL_OPERATION));
		outMsg.put(Message.WSDL_SERVICE, inMsg.get(Message.WSDL_SERVICE));
		outMsg.put(Message.WSDL_INTERFACE, inMsg.get(Message.WSDL_INTERFACE));
		outMsg.put(Message.WSDL_PORT, inMsg.get(Message.WSDL_PORT));
		outMsg.put(Message.WSDL_DESCRIPTION, inMsg
				.get(Message.WSDL_DESCRIPTION));
	}	
}
