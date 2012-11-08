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
 * $Id: ITcpipContext.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

/*
 * The TCPIP binding component implements this interface and makes it available to 
 * a TCPIP protocol handler. This interface provides container services to the handler.
 */

public interface ITcpipContext {

	/*
	 * Send data out over the socket. Blocks until data is sent.
	 */
	public void sendSocket(byte[] bytes) throws Exception;
	
	/*
	 * Receive data from the socket. Blocks until some data is
	 * received or the timeout occurs. Throws a socket timeout exception 
	 * if a timeout occurs.
	 */
	public byte[] receiveSocket() throws Exception;

	/*
	 * Set the socket read timeout in milliseconds.
	 */
	public void setSocketTimeout(int timeOutMillis);
	
	/*
	 * Creates an inbound message exchange using the provided bytes. If the 
	 * consumer default-MEP is in-out, it returns the reply message bytes.
	 */
	public byte[] createInbound(byte[] bytes) throws Exception;
	
	/*
	 * This sets the callback method used for processing provider-mode
	 * exchanges. If this is true then the component will call
	 * the process methods with a MessageExchange argument. If this
	 * is false then it will call the methods with a byte array argument.
	 * 
	 * Default is false (byte array call).
	 */
	public void setExchangeBasedProvider(boolean isEnabled);

	/*
	 * This allows setting the send mode to asynchronous
	 */
	public void setIsAsyncSend(boolean isAsync);

}
