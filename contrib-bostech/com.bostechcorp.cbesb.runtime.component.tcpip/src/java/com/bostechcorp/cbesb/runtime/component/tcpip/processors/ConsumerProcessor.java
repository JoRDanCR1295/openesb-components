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
 * $Id: ConsumerProcessor.java,v 1.1.1.1 2007/04/09 17:49:30 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip.processors;

import java.util.concurrent.atomic.AtomicBoolean;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging.BaseConsumerProcessor;
import com.bostechcorp.cbesb.runtime.component.tcpip.TcpipEndpoint;

public class ConsumerProcessor extends BaseConsumerProcessor {

	protected final transient Log logger = LogFactory.getLog(getClass());
	protected AtomicBoolean running = new AtomicBoolean(false);

	public ConsumerProcessor(TcpipEndpoint endpoint) {
		super(endpoint);
	}

	protected void doStart() throws Exception {

		try {
		} catch (Exception e) {
			throw new Exception(e.getMessage());
		}

		synchronized (running) {
			running.wait();
		}

	}
	
	protected void doStop() throws Exception {
		synchronized(running) {
			running.set(false);
			running.notify();
		}
	}


	@Override
	protected void doProcessFault(NormalizedMessage nm, String fault)
			throws Exception {
		// TODO Auto-generated method stub

	}

	/* 
	 * Process out situation
	 * @see com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging.BaseConsumerProcessor#doProcessOut(javax.jbi.messaging.NormalizedMessage, java.lang.String)
	 */
	@Override
	protected void doProcessOut(NormalizedMessage nm, String s, MessageExchange exchange)
			throws Exception {
		
	}
}