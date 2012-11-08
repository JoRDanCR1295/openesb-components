/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: DeliveryHandler.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.util.LinkedList;
import java.util.ListIterator;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchange.Role;

import org.apache.commons.logging.Log;

public class DeliveryHandler {
	private static final String SENDER_ENDPOINT_PROPERTY = "org.apache.servicemix.senderEndpoint";
	private CcslConfig config;
	
	public DeliveryHandler(CcslConfig config) {
		this.config = config;
	}
	
	public MessageExchange accept(ComponentContext componentContext, DeliveryChannel channel) throws MessagingException {
		return acceptHandler(componentContext, channel, -1l);
	}
	
	public MessageExchange accept(ComponentContext componentContext, DeliveryChannel channel, long waitTime) throws MessagingException {
		return acceptHandler(componentContext, channel, waitTime);
	}
	
	private MessageExchange acceptHandler(ComponentContext componentContext, DeliveryChannel channel, long waitTime) throws MessagingException {
		Log log = config.getLog();
		boolean saveErrors = config.getDefaultSaveErrors();
		boolean stripRecord = config.getDefaultStripRecord();
		boolean useSendMessage = config.getUseSendMessage();
		EndpointConfig endpoint = null;
		UpocConfig postAcceptUpoc = null;
		
		// try to get an exchange
		MessageExchange me = null;
		if (waitTime < 0) me = channel.accept();
		else me = channel.accept(waitTime);

		// do UPOC and other processing
		if (me != null && me.getStatus() == ExchangeStatus.ACTIVE ) {
			log.debug("acceptHandler: MessageExchange.getStatus() == ACTIVE");
			NormalizedMessage msg = null;
			String whichMessage = null;
			String destinationEndpointKey = null;
			try {
				// The provider role processes the in message 
				if (me.getRole() == MessageExchange.Role.PROVIDER) {
					whichMessage = "in";
					destinationEndpointKey = me.getEndpoint().getServiceName()+":"+me.getEndpoint().getEndpointName();;
				} else if (me.getRole() == MessageExchange.Role.CONSUMER) {
					whichMessage = "out";
					destinationEndpointKey = (String)me.getProperty(SENDER_ENDPOINT_PROPERTY);
				} else {
					MessagingException e = new MessagingException("invalid role: "+me.getRole());
					log.error(e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
					throw e;
				}
				log.debug("acceptHandler: using \""+whichMessage+"\" message");
				msg = me.getMessage(whichMessage);				
			} catch (Exception e) {
				log.error("\n\n\nException getting '"+whichMessage+"' message: "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
			}
			
			log.debug("acceptHandler: destinationEndpointKey = " + destinationEndpointKey);
			if (destinationEndpointKey != null && destinationEndpointKey.length() > 0) {
				// Try to find the endpoint configuration
				endpoint = config.getEndpointConfig(destinationEndpointKey);
				if (endpoint != null) {
					log.debug("acceptHandler: found endpoint settings");
					saveErrors = endpoint.getSaveErrors();
					stripRecord = endpoint.getStripRecord();
					useSendMessage = endpoint.getSendMessage();
					postAcceptUpoc = endpoint.getUpoc("postaccept");
					log.debug("acceptHandler: useSendMessage = " + useSendMessage);
				}	else
					log.debug("acceptHandler: no endpoint settings, using component defaults");

				log.debug("acceptHandler: postAcceptUpoc=" + postAcceptUpoc +
						   "  saveErrors=" + saveErrors +
						   "  stripRecord=" + stripRecord +
						   "  useSendMessage=" + useSendMessage);
				
				if (postAcceptUpoc != null) {
					// Run UPOCs
					try {
						// run startup if required
						if (endpoint.getNeedToRunStart()) {
							UpocConfig startup = endpoint.getUpoc("start");
							if (startup != null) {
								CcslUpoc.runScript(log, startup.getRootDir(), "start", startup.getType(), startup.getClassName(), startup.getMethod(), 
										componentContext, channel, me,null, this);
								endpoint.setNeedToRunStart(false);
								endpoint.setNeedToRunStop(true);
							}
						}
						// run postAccept
						CcslUpoc.runScript(log, postAcceptUpoc.getRootDir(), "postaccept", postAcceptUpoc.getType(), postAcceptUpoc.getClassName(), postAcceptUpoc.getMethod(), 
								componentContext, channel, me,null, this);
					}
					catch (Exception e) {
						log.error("UPOC exception "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
						if (saveErrors) ErrorDb.write(e, me, config.getLog());
						MessagingException eThrow;
						if (e instanceof MessagingException) eThrow = (MessagingException)e;
						else eThrow = new MessagingException(e);
						throw eThrow;
					}
				}
	
			}

			// Strip XML records from the data envelope
			if (stripRecord) RecordHelper.stripXmlRecord(msg);
			
			// add the SendMessage to the message content.
			if (useSendMessage && (me.getRole() == MessageExchange.Role.CONSUMER)) {
				log.debug("acceptHandler: add sendMessage response");
				RecordHelper.addSendMessage(msg, me, true);
			}
		}
		return me;
	}

	
	public void send(ComponentContext componentContext, DeliveryChannel channel, MessageExchange exchange) throws MessagingException {
		sendOrSendSync(false, -1L, componentContext, channel, exchange);
	}
	
	public boolean sendSync(ComponentContext componentContext, DeliveryChannel channel, MessageExchange exchange) throws MessagingException {
		return sendOrSendSync(true, -1L, componentContext, channel, exchange);
	}

	public boolean sendSync(ComponentContext componentContext, DeliveryChannel channel, MessageExchange exchange, long timeout) throws MessagingException {
		return sendOrSendSync(true, timeout, componentContext, channel, exchange);
	}

	private boolean sendOrSendSync(boolean isSendSync, long syncTimeout, ComponentContext componentContext, 
			DeliveryChannel channel, MessageExchange exchange) throws MessagingException {
		boolean result = true;
		boolean saveError = config.getDefaultSaveErrors();
		boolean addRecord = config.getDafaultAddRecord();
		boolean useSendMessage = config.getUseSendMessage();
		EndpointConfig endpoint = null;
		UpocConfig preSendUpoc = null;
		UpocConfig postSendUpoc = null;
		LinkedList<MessageExchange> sendList = new LinkedList<MessageExchange>();
		Log log = config.getLog();
		
		// Try to get the sender endpoint from exchange properties
		String senderEndpointKey = null;
		log.debug("sendOrSendSync: exchange.getStatus() is " + exchange.getStatus());
		if (exchange.getStatus() == ExchangeStatus.ACTIVE) {
			if (exchange.getRole() == Role.CONSUMER) {
				log.debug("sendOrSendSync: exchange.getRole() is Role.CONSUMER.");
				// The consumer role must extract the source endpoint from a Servicemix property.
				// This is not JBI compliant but JBI gives us no way to determine the sender endpoint.
				senderEndpointKey = (String)exchange.getProperty(SENDER_ENDPOINT_PROPERTY);
			} else {
				log.debug("sendOrSendSync: exchange.getRole() is not Role.CONSUMER.");
				// The provider role uses the target endpoint
				senderEndpointKey = exchange.getEndpoint().getServiceName()+":"+exchange.getEndpoint().getEndpointName();
			}
		} else {
			// if the exchange is not active just pass it to the channel.
			if (isSendSync) {
				if (syncTimeout == -1L) result = channel.sendSync(exchange);
				else result = channel.sendSync(exchange, syncTimeout);
			} else {
				channel.send(exchange);
			}
			return result;
		}

		
		log.debug("sendOrSendSync: senderEndpointKey = " + senderEndpointKey);
		if (senderEndpointKey != null && senderEndpointKey.length() > 0) {
			// Try to find the endpoint configuration
			endpoint = config.getEndpointConfig(senderEndpointKey);
			if (endpoint != null) {
				saveError = endpoint.getSaveErrors();
				addRecord = endpoint.getAddRecord();
				useSendMessage = endpoint.getSendMessage();
				log.debug("sendOrSendSync: endpoint config useSendMessage = " + useSendMessage);
				preSendUpoc = endpoint.getUpoc("presend");
				if (isSendSync) postSendUpoc = endpoint.getUpoc("postsend");
			}
		}
		
		// Convert <SendMessage> envelope into nmh style.
		if (useSendMessage && exchange.getRole() == Role.CONSUMER) {
			log.debug("sendOrSendSync: ready to stripSendMessage");
			RecordHelper.stripSendMessage(exchange.getMessage("in"), exchange, false);
		}
		
		// add the XML record to the message content
		if (addRecord) {
			if (exchange.getRole() == Role.PROVIDER) {
				RecordHelper.addXmlRecord(exchange.getMessage("out"));
			} else {
				RecordHelper.addXmlRecord(exchange.getMessage("in"));
			}
		}

		// if there is no presend UPOC then the sendList is just the original exchange
		if (preSendUpoc == null) {
			sendList.add(exchange);
		} else {
			// Run UPOCs
			try {
				// run startup if required
				if (endpoint.getNeedToRunStart()) {
					UpocConfig startup = endpoint.getUpoc("start");
					if (startup != null) {
						CcslUpoc.runScript(log, startup.getRootDir(), "start", startup.getType(), startup.getClassName(), startup.getMethod(), 
								componentContext, channel, exchange, null, this);
						endpoint.setNeedToRunStart(false);
						endpoint.setNeedToRunStop(true);
					}
				}
				// run presend and get sendList
				Object rtn = CcslUpoc.runScript(log, preSendUpoc.getRootDir(), "presend", preSendUpoc.getType(), preSendUpoc.getClassName(), preSendUpoc.getMethod(), 
						componentContext, channel, exchange,null, this);
				if (rtn instanceof LinkedList) sendList = (LinkedList)rtn;
				else if (rtn instanceof MessageExchange) {
					sendList = new LinkedList<MessageExchange>();
					sendList.add((MessageExchange)rtn);
				}
			}
			catch (Exception e) {
				log.error("UPOC exception "+e);
				e.printStackTrace();
				if (saveError) ErrorDb.write(e, exchange, config.getLog());
				MessagingException me;
				if (e instanceof MessagingException) me = (MessagingException)e;
				else me = new MessagingException(e);
				throw me;
			}
		}
		
		// Send out everything in the send list
		MessagingException sendException = null;
		String originalId = exchange.getExchangeId();
		MessageExchange originalExchange = null;
		for (ListIterator iter = sendList.listIterator(); iter.hasNext() || originalExchange != null;) {
			// save the original exchange for last so it comes back as the response for in-out
			MessageExchange sendExchange = null;
			if (iter.hasNext()) {
				sendExchange = (MessageExchange)iter.next();
				if (sendExchange.getExchangeId().equals(originalId)) {
					originalExchange = sendExchange;
					continue;
				}
			}
			else {
				sendExchange = originalExchange;
				originalExchange = null;
			}
			// send the exchange
			try {
				if (isSendSync) {
					if (syncTimeout == -1L) result = channel.sendSync(sendExchange);
					else result = channel.sendSync(sendExchange, syncTimeout);
					if (postSendUpoc != null) {
						// Run postsend UPOC
						try {
							CcslUpoc.runScript(log, postSendUpoc.getRootDir(), "postsend", postSendUpoc.getType(), postSendUpoc.getClassName(), postSendUpoc.getMethod(), 
									componentContext, channel, exchange, null, this);
						}
						catch (Exception e) {
							log.error("UPOC exception "+e);
							e.printStackTrace();
							if (saveError) ErrorDb.write(e, exchange, config.getLog());
							MessagingException me;
							if (e instanceof MessagingException) me = (MessagingException)e;
							else me = new MessagingException(e);
							throw me;
						}
					}
				} else {
					channel.send(sendExchange);
				}
			}
			catch (Exception e) {
				if (saveError) ErrorDb.write(e, sendExchange, config.getLog());
				if (sendException != null) {
					// if this is not the first exception then log the previous one
					log.error(e.toString()+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
				}
				if (e instanceof MessagingException) sendException = (MessagingException)e;
				else sendException = new MessagingException(e);
			}
		}
		// throw the last exception if there was one
		if (sendException != null) throw sendException;
		return result;
	}
}
