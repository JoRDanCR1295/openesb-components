/*
 * ChainBuilder ESB
 * 		Visual Enterprise Integration
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
 * $Id: ProviderProcessor.java,v 1.1.1.1 2007/04/09 17:49:31 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip.processors;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging.BaseProviderProcessor;
import com.bostechcorp.cbesb.runtime.component.tcpip.TcpipEndpoint;

public class ProviderProcessor extends BaseProviderProcessor {

	protected final transient Log logger = LogFactory.getLog(getClass());

	private TcpipEndpoint endpoint;

	public ProviderProcessor(TcpipEndpoint endpoint) {
		super(endpoint);
		this.endpoint = endpoint;
	}
	
	@Override
	public void processInMessage(QName service, QName operation,
			NormalizedMessage in, MessageExchange exchange) throws Exception {

		logger.debug("Tcpip provider processInMessage");
		endpoint.getSendQueue().put(exchange);
		Object rtn = endpoint.getReturnQueue().take();
		logger.debug("Tcpip provider return from handler");
		if (rtn instanceof Exception) throw (Exception)rtn;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging.BaseProviderProcessor#processInOutMessage(javax.xml.namespace.QName,
	 *      javax.xml.namespace.QName, javax.jbi.messaging.NormalizedMessage,
	 *      javax.jbi.messaging.NormalizedMessage, boolean)
	 */
	public boolean processInOutMessage(QName service, QName operation,
			NormalizedMessage in, NormalizedMessage out, 
			boolean optionalOut, MessageExchange exchange)
			throws Exception {
		
		logger.debug("Tcpip provider processInOutMessage");
		endpoint.getSendQueue().put(exchange);
		Object rtn = endpoint.getReturnQueue().take();
		logger.debug("Tcpip provider return from handler");
		if (rtn instanceof Exception) throw (Exception)rtn;
		return true;
	}
}