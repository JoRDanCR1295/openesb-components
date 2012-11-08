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
 * $Id: MllpHandler.java,v 1.2 2007/04/25 20:45:59 afung Exp $
 */
package com.bostechcorp.cbesb.custom.protocol;

import javax.jbi.messaging.MessageExchange;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.runtime.ccsl.lib.DumpNormalizedMessage;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipContext;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipHandler;

public class MllpHandler implements ITcpipHandler {
	protected static final short INPUT_WANT_0B = 0;
	protected static final short INPUT_WANT_1C = 1;
	protected static final short INPUT_WANT_0D = 2;
	protected static final int READ_TIMEOUT = 7000;

	protected Log logger = LogFactory.getLog(getClass());
	protected ITcpipContext context;	
	protected StringBuffer inMessage;
	protected short inputState;
	protected boolean gotInMessage;

	
	public String getDescription() { return "TCPIP MLLP protocol handler"; }

	
	public void init(ITcpipContext ctx) {
		context = ctx;
		context.setSocketTimeout(0);
		inMessage = new StringBuffer();
		gotInMessage = false;
		inputState = INPUT_WANT_0B;
	}

	
	/*
	 * This handles received data in consumer mode.
	 */
	public int gotReceiveData(byte[] bytes) throws Exception {
System.out.println("\n\n\nMllpHandler received:\n"+DumpNormalizedMessage.dumpBytesAsHex(bytes)+"\n\n");
		context.setSocketTimeout(READ_TIMEOUT);
		int bytesConsumed = receiveStateMachine(bytes);
		if (gotInMessage) {
			// create the inbound exchange and possibly return the out message
			byte[] inBytes = (new String(inMessage)).getBytes("ISO8859-1");
			logger.debug("got inbound message ("+inMessage.length()+" bytes)\n"+DumpNormalizedMessage.dumpBytesAsHex(inBytes));
System.out.println("\n\n\nMllpHandler creating inbound:\n"+DumpNormalizedMessage.dumpBytesAsHex(inBytes)+"\n\n");
			byte[] outBytes = context.createInbound(inBytes);
			if (outBytes != null) {
System.out.println("\n\n\nMllpHandler reply bytes:\n"+DumpNormalizedMessage.dumpBytesAsHex(outBytes)+"\n\n");
sendMessage(outBytes);
                        }			
			// reset the input state
			inMessage = new StringBuffer();
			inputState = INPUT_WANT_0B;
			gotInMessage = false;
			context.setSocketTimeout(0);
		}
		return bytesConsumed;
	}


	/*
	 * Flush input state on a timeout
	 */
	public void gotReceiveTimeout() throws Exception {
		logger.info("TCPIP handler timed out on input");
		inMessage = new StringBuffer();
		inputState = INPUT_WANT_0B;
		context.setSocketTimeout(0);
	}
	
	
	/*
	 * No special processing for a socket error
	 */
	public void gotSocketError(Exception e) throws Exception {}

	
	/*
	 * Send an in-only message
	 */
	public void processInOnlyBytes(byte[] bytes) throws Exception {
		sendMessage(bytes);
	}

	
	/*
	 * Send a message and return a reply
	 */
	public byte[] processInOutBytes(byte[] bytes) throws Exception {
		byte[] returnBytes = null;
		sendMessage(bytes);
		byte[] received;
		context.setSocketTimeout(READ_TIMEOUT);
		while ((received = context.receiveSocket()) != null) {
			for (int bytesLeft=0; (bytesLeft=received.length) > 0;) {
				int consumed = receiveStateMachine(received);
				if (gotInMessage) break;
				if (consumed > 0) {
					byte[] newBytes = new byte[bytesLeft-consumed];
					for (int i=0; i<newBytes.length; i++) newBytes[i] = received[consumed++];
					received = newBytes;
				}
			}
			if (gotInMessage) {
				returnBytes = (new String(inMessage)).getBytes("ISO8859-1");
				inMessage = new StringBuffer();
				inputState = INPUT_WANT_0B;
				gotInMessage = false;
				context.setSocketTimeout(0);
				break;
			}
		}
		return returnBytes;
	}

	/*
	 * I don't use these since i'm using the byte array methods
	 */
	public void processInOnlyExchange(MessageExchange e) throws Exception {}
	public void processInOutExchange(MessageExchange e) throws Exception {}

	
	/*
	 * Add the MLLP wrapper and send a message
	 */
	protected void sendMessage(byte[] bytes) throws Exception {
		byte[] message = new byte[bytes.length+3];
		message[0] = 0x0B;
		int i;
		for (i=0; i<bytes.length; i++) message[i+1] = bytes[i];
		message[++i] = 0x1C;
		message[++i] = 0x0D;
		context.sendSocket(message);
	}
	
	/*
	 * Detect messages in a 0B-->1C0D MLLP wrapper
	 */
	protected int receiveStateMachine(byte[] bytes) throws Exception {
		int bytesConsumed = bytes.length;
		int position = 0;
		if (bytes.length < 1) throw new Exception("zero bytes received"); 
		switch (inputState) {
		case INPUT_WANT_0B:
			// ignore everything up to the first 0x0B
			int pos_0b=findByte(bytes, position, (byte)0x0B);
			if (pos_0b < 0) break;
			position = pos_0b+1;
			inputState = INPUT_WANT_1C;
			context.setSocketTimeout(READ_TIMEOUT);
			// don't break here since we want to process the next state
			
		case INPUT_WANT_1C:
			// capture everything up to then next 0x1C
			int pos_1c=findByte(bytes, position, (byte)0x1C);
			if (pos_1c < 0) {
				int length = bytes.length-position;
				if (length > 0) inMessage.append(new String(bytes, position, length, "ISO8859-1"));
				break;
			}
			inMessage.append(new String(bytes, position, pos_1c-position, "ISO8859-1"));
			inputState = INPUT_WANT_0D;
			position = pos_1c+1;
			// don't break here since we want to process the next state
						
		case INPUT_WANT_0D:
			// if the next byte is 0D then we have a message, otherwise keep capturing
			if (position >= bytes.length) break;
			if (bytes[position] == (byte)0x0D) {
				gotInMessage = true;
				bytesConsumed = position+1;
			} else {
				inMessage.appendCodePoint(0x1C);
				inputState=INPUT_WANT_1C;
				bytesConsumed = position;
			}
			break;
		}
		return bytesConsumed;
	}

	
	/*
	 * find the first occurrence of a value in a byte array 
	 */
	protected int findByte(byte[] bytes, int start, byte want) {
		int result = -1;
		if (bytes != null) {
			for (int i=start; i<bytes.length; i++) 
				if (bytes[i] == want) {
					result = i;
					break;
				}
		}
		return result;
	}

}
