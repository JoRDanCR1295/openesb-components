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
 * $Id: CcslDeliveryChannel.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.base;

import java.util.HashMap;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;

import com.bostechcorp.cbesb.runtime.ccsl.lib.DeliveryHandler;


public class CcslDeliveryChannel implements DeliveryChannel {
	DeliveryChannel realDeliveryChannel;
	Log log;
	CcslComponent ccslComponent;
	DeliveryHandler deliveryHandler;
	
	class FactoryPair {
		MessageExchangeFactory realMessageExchangeFactory;
		CcslMessageExchangeFactory ccslMessageExchangeFactory;
	};
	
	FactoryPair defaultFactory;
	
	HashMap<QName, FactoryPair> qnameFactoryMap = new HashMap<QName, FactoryPair>();
	HashMap<ServiceEndpoint, FactoryPair> endpointFactoryMap = new HashMap<ServiceEndpoint, FactoryPair>();
	HashMap<QName, FactoryPair> serviceQnameFactoryMap = new HashMap<QName, FactoryPair>();
	
	CcslDeliveryChannel(CcslComponent comp, DeliveryChannel chan, Log log) {
		ccslComponent = comp;
		realDeliveryChannel = chan;
		this.log = log;
		deliveryHandler = new DeliveryHandler(ccslComponent.getConfig());
	}

	public MessageExchange accept() throws MessagingException {
		return deliveryHandler.accept(ccslComponent.realComponentContext, realDeliveryChannel);
	}

	public MessageExchange accept(long arg0) throws MessagingException {
		return deliveryHandler.accept(ccslComponent.realComponentContext, realDeliveryChannel, arg0);
	}

	public void close() throws MessagingException {
		realDeliveryChannel.close();
	}

	public MessageExchangeFactory createExchangeFactory() {
		if (defaultFactory == null) {
			defaultFactory = new FactoryPair();
			defaultFactory.realMessageExchangeFactory = realDeliveryChannel.createExchangeFactory();
			defaultFactory.ccslMessageExchangeFactory = new CcslMessageExchangeFactory(defaultFactory.realMessageExchangeFactory, log);
		}
		return defaultFactory.ccslMessageExchangeFactory;
	}

	public MessageExchangeFactory createExchangeFactory(QName arg0) {
		FactoryPair fp = qnameFactoryMap.get(arg0);
		if (fp == null) {
			fp = new FactoryPair();
			qnameFactoryMap.put(arg0, fp);
			fp.realMessageExchangeFactory = realDeliveryChannel.createExchangeFactory(arg0);
			fp.ccslMessageExchangeFactory = new CcslMessageExchangeFactory(fp.realMessageExchangeFactory, log);
		}
		return fp.ccslMessageExchangeFactory;
	}

	public MessageExchangeFactory createExchangeFactory(ServiceEndpoint arg0) {
		FactoryPair fp = endpointFactoryMap.get(arg0);
		if (fp == null) {
			fp = new FactoryPair();
			endpointFactoryMap.put(arg0, fp);
			fp.realMessageExchangeFactory = realDeliveryChannel.createExchangeFactory(arg0);
			fp.ccslMessageExchangeFactory = new CcslMessageExchangeFactory(fp.realMessageExchangeFactory, log);
		}
		return fp.ccslMessageExchangeFactory;
	}

	public MessageExchangeFactory createExchangeFactoryForService(QName arg0) {
		FactoryPair fp = serviceQnameFactoryMap.get(arg0);
		if (fp == null) {
			fp = new FactoryPair();
			serviceQnameFactoryMap.put(arg0, fp);
			fp.realMessageExchangeFactory = realDeliveryChannel.createExchangeFactory(arg0);
			fp.ccslMessageExchangeFactory = new CcslMessageExchangeFactory(fp.realMessageExchangeFactory, log);
		}
		return fp.ccslMessageExchangeFactory;
	}

	public void send(MessageExchange arg0) throws MessagingException {
		deliveryHandler.send(ccslComponent.realComponentContext, realDeliveryChannel, arg0);
	}

	public boolean sendSync(MessageExchange arg0) throws MessagingException {
		return deliveryHandler.sendSync(ccslComponent.realComponentContext, realDeliveryChannel, arg0);
	}

	public boolean sendSync(MessageExchange arg0, long arg1) throws MessagingException {
	return deliveryHandler.sendSync(ccslComponent.realComponentContext, realDeliveryChannel, arg0, arg1);
	}

}
