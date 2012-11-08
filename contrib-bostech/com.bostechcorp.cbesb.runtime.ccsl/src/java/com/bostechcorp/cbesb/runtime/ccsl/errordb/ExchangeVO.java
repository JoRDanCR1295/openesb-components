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
 * $Id: ExchangeVO.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.errordb;

import java.net.URI;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

public class ExchangeVO extends AbstractVO {
	private MessageExchange exchange;
	private long exchangeId;

	public ExchangeVO(MessageExchange e) {
		exchange = e;
	}
	
	public String getRole() {
		MessageExchange.Role role = exchange.getRole();
		String strRole = null;
		if (role == MessageExchange.Role.CONSUMER) strRole = "consumer";
		else if (role == MessageExchange.Role.PROVIDER) strRole = "provider";
		else strRole = "unknown("+role+")";
		return strRole;
	}
	
	public String getEndpointService() {
		String result = null;
		ServiceEndpoint se = exchange.getEndpoint();
		if (se != null) {
			QName serviceName = se.getServiceName();
			if (serviceName != null) result = serviceName.toString();
		}
		return result;
	}
	
	public String getEndpointName() {
		String result = /*null*/"";
		ServiceEndpoint endpoint = exchange.getEndpoint();
		if (endpoint != null) result = endpoint.getEndpointName();
		return result;
	}
	
	public String getExchangeContainerId() {
		return exchange.getExchangeId();
	}
	
	public String getInterfaceName() {
		QName intName = exchange.getInterfaceName();
		return (intName != null) ? intName.toString() : /*null*/"";
	}
	
	public String getOperation() {
		QName n = exchange.getOperation();
		return (n != null) ? n.toString() : /*null*/"";
	}
	
	public String getPattern() {
		URI pattern = exchange.getPattern();
		return (pattern != null) ? pattern.toString() : /*null*/"";
	}
	
	public String getService() {
		QName service = exchange.getService();
		return (service != null) ? service.toString() : /*null*/"";
	}
	
	public String getExchangeStatus() {
		ExchangeStatus status = exchange.getStatus();
		return (status != null) ? status.toString() : /*null*/"";
	}
	
	public long getExchangeId() {
		return exchangeId;
	}

	public void setExchangeId(long id) {
		exchangeId = id;
	}
}
