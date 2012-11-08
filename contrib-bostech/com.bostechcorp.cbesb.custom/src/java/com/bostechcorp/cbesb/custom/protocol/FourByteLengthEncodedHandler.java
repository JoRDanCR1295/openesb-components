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
 * $Id: FourByteLengthEncodedHandler.java,v 1.1 2007/04/12 02:04:12 mpreston Exp $
 */
package com.bostechcorp.cbesb.custom.protocol;

import javax.jbi.messaging.MessageExchange;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.runtime.ccsl.lib.DumpNormalizedMessage;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipContext;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipHandler;

public class FourByteLengthEncodedHandler implements ITcpipHandler {
	protected static final short INPUT_WANT_LENGTH = 0;
	protected static final short INPUT_WANT_DATA = 1;
	protected static final short LENGTH_BYTES = 4;
	protected static final int INPUT_TIMEOUT = 3000;

	protected Log logger = LogFactory.getLog(getClass());
	protected ITcpipContext context;	
	protected StringBuffer inMessage;
	protected short inputState;
	protected boolean gotInMessage;
	protected int bytesLeft;
	protected byte[] lengthBytes = new byte[LENGTH_BYTES];

	
	public String getDescription() { return "TCPIP four byte length encoded protocol handler"; }

	
	public void init(ITcpipContext ctx) {
		context = ctx;
		context.setSocketTimeout(0);
		inMessage = new StringBuffer();
		gotInMessage = false;
		bytesLeft = LENGTH_BYTES;
		inputState = INPUT_WANT_LENGTH;
	}

	
	/*
	 * This handles received data in consumer mode.
	 */
	public int gotReceiveData(byte[] bytes) throws Exception {
		context.setSocketTimeout(INPUT_TIMEOUT);
		int bytesConsumed = receiveStateMachine(bytes);
		if (gotInMessage) {
			// create the inbound exchange and possibly return the out message
			byte[] inBytes = (new String(inMessage)).getBytes("ISO8859-1");
			logger.debug("got inbound message ("+inMessage.length()+" bytes)\n"+DumpNormalizedMessage.dumpBytesAsHex(inBytes));
			byte[] outBytes = context.createInbound(inBytes);
			if (outBytes != null) sendMessage(outBytes);
			
			// reset the input state
			inMessage = new StringBuffer();
			inputState = INPUT_WANT_LENGTH;
			bytesLeft = LENGTH_BYTES;
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
		inputState = INPUT_WANT_LENGTH;
		bytesLeft = LENGTH_BYTES;
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
		context.setSocketTimeout(INPUT_TIMEOUT);
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
				inputState = INPUT_WANT_LENGTH;
				gotInMessage = false;
				bytesLeft = 4;
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
	 * Add the byte count and send a message
	 */
	protected void sendMessage(byte[] bytes) throws Exception {
		byte[] message = new byte[bytes.length+4];
		int length = bytes.length+LENGTH_BYTES;
		for (int i=LENGTH_BYTES-1; i>=0; i--) {
			message[i] = (byte)(length & 0xFF);
			length >>= 8;
		}
		int i;
		for (i=0; i<bytes.length; i++) message[i+4] = bytes[i];
		context.sendSocket(message);
	}
	
	/*
	 * Detect messages
	 */
	protected int receiveStateMachine(byte[] bytes) throws Exception {
		int bytesConsumed = bytes.length;
		int position = 0;
		if (bytes.length < 1) throw new Exception("zero bytes received"); 
		switch (inputState) {
		case INPUT_WANT_LENGTH:
			while (bytesLeft > 0 && position < bytes.length) {
				lengthBytes[--bytesLeft] = bytes[position++];
			}

			if (bytesLeft > 0) {
				bytesConsumed = position;
				break;
			}
			inputState = INPUT_WANT_DATA;
			bytesLeft=0;
			for (int i=LENGTH_BYTES-1; i>=0; i--)
				bytesLeft = 256*bytesLeft + lengthBytes[i];
			bytesLeft -= LENGTH_BYTES;
			// don't break here since we want to process the next state

		case INPUT_WANT_DATA:
			int length = bytes.length-position;
			if (length < bytesLeft) {
				inMessage.append(new String(bytes, position, length, "ISO8859-1"));
				bytesLeft -= length;
			} else {
				inMessage.append(new String(bytes, position, bytesLeft, "ISO8859-1"));
				position += bytesLeft;
				bytesConsumed = position;
				gotInMessage = true;
			}
			break;
		}
		return bytesConsumed;
	}

}
