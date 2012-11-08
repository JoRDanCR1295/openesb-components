/*
 * ChainBuilder ESB
 * 		Visual Enterprise Integration
 * 
 * Copyright (C) 2007 Bostech Corporation
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
 * $Id: ITcpipHandler.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import javax.jbi.messaging.MessageExchange;

/*
 * A TCPIP protocol handler must implement this interface.
 */
public interface ITcpipHandler {

	/* Initialize and receive the component's context object.
	 */
	public void init(ITcpipContext context);

	/*
	 * Return a description used for browsing in the UI.
	 */
	public String getDescription();
	
	/*
	 * In consumer mode, The component will call this method when socket
	 * data is available.
	 */
	public int gotReceiveData(byte[] bytes) throws Exception;

	/*
	 * In consumer mode, the container will call this when a socket
	 * timeout occurs.
	 */
	public void gotReceiveTimeout() throws Exception;
	
	/*
	 * In consumer mode, the component will call this method when
	 * a socket error occurs. Usually this indicates the component
	 * is shutting down or a peer disconnected.
	 */
	public void gotSocketError(Exception e) throws Exception;
	
	/*
	 * In provider mode, the component calls this to process an outbound
	 * message exchange. Only one thread at a time will run
	 * these methods.
	 */

	/*
	 * These are the default byte array based callbacks. For an in-out exchange,
	 * the out message bytes are returned.
	 */
	public void processInOnlyBytes(byte[] bytes) throws Exception;

	public byte[] processInOutBytes(byte[] bytes) throws Exception;
	
	/*
	 * These are the MessageExchange based calls. The handler enables these
	 * callbacks by calling setExchangeBasedProvider(true) on the tcpip context.
	 * For an in-out exchange, the out message should be populated before returning.
	 */
	public void processInOnlyExchange(MessageExchange e) throws Exception;

	public void processInOutExchange(MessageExchange e) throws Exception;
}
