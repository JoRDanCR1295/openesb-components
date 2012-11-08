/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)MLLPv2Decoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.llp;

import java.io.ByteArrayOutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IdleStatus;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolDecoderOutput;
import org.apache.mina.filter.codec.ProtocolCodecException;
import com.sun.jbi.hl7bc.I18n;

/**
 * This class 
 * 
 * a) unwraps the block formatting characters and pass-on the pure HL7 v3 xml message to the message listener
 * b) executes the recommended steps for Message Receiver, presecribed by MLLP v2 specification.
 * 
 * MLLP v2 specification prescribed steps for Message Receiver are:
 * 
 * 1. Receive and ignore any received bytes until the start of a Block is found.
 * 2. Continue to receive bytes until the end of a Block is found, or until a Timeout occurs.
 * 3. In case of a Timeout, ignore all bytes received thus far; go to step 1.   
 * 
 *   
 * 
 *
 */

public class MLLPv2Decoder extends MLLPDecoder {

	private static final Logger mLog = Logger.getLogger(MLLPv2Decoder.class.getName());

	private static final String SBCHAR_FOUND = "sbCharFound";

	private static final String TIMEOUTOCCURED = "timeOutOccured";

	private static final String READBYTES = "readBytes";

	private static final String CURRENTINPUT = "currentInput";

	private static final String TRUE = "true";

	private static final String FALSE = "false";

	//Fixed the read time out value at 40 sec. Later this needs to be replaced by wsdl configurable value for timeout 
	private static final int waitForReadTime = 40;

	private String logMsg;

	private char mSBChar, mEBChar, mEDChar;

	public void decode(IoSession session, ByteBuffer in,
			ProtocolDecoderOutput out) throws ProtocolCodecException {
		java.nio.ByteBuffer nioBuf = in.buf();
		int offset = nioBuf.position();
		int length = nioBuf.limit() - offset;
		if (!nioBuf.hasArray()) {
			ByteBuffer heapBuf = ByteBuffer.allocate(length, false);
			heapBuf.put(in);
			heapBuf.flip();
			nioBuf = heapBuf.buf();
			offset = 0;
		}
		ByteArrayOutputStream baos = (ByteArrayOutputStream) session
				.getAttribute(CURRENTINPUT);
		byte[] bytes = nioBuf.array();
		baos.write(bytes, offset, length);
		byte[] message = baos.toByteArray();
		// Receive and ignore any received bytes until the start of a Block is
		// found.
		if (FALSE.equals((String) session.getAttribute(SBCHAR_FOUND))) {
			boolean sbFound = false;
			for (int k = 0; k < message.length; k++) {
				if (getStartBlockChar() == (char) message[k]) {
					sbFound = true;
				}
			}
			if (!sbFound) {
				baos.reset();
				return;
			}
			session.setAttribute(SBCHAR_FOUND, TRUE);
			// set the Timeout. Ignore the accumulated bytes when timeout occurs
			session.setIdleTime(IdleStatus.READER_IDLE, waitForReadTime);
			session.setAttribute(TIMEOUTOCCURED, FALSE);
		}
		// retrieve the total bytes received till the last message
		// that value been maintained as an attribute in the session
		long sessionReadBytes = (Long) session.getAttribute(READBYTES);
		long totalReadBytes = session.getReadBytes() - sessionReadBytes;
		if (baos.size() < totalReadBytes) {
			if (TRUE.equals((String) session.getAttribute(TIMEOUTOCCURED))) {
				// In case of a Timeout, ignore all bytes received thus far
				baos.reset();
			}
			if(baos.size() == 4 && (bytes[1] == 0x06 || bytes[1] == 0x15)){ //MLLPV2 ack/nak will be of length 4.
			//	do nothing..continue to process the message		
				mLog.fine(I18n.msg("MLLPV2Decoder: MLLPV2 ACK/NAK received"));
			}else{
				return;
			}
		}
		session.setAttribute(SBCHAR_FOUND, FALSE);
		byte[] llpMessage = baos.toByteArray();
		// Resets the count field of this byte array output stream to zero,
		// so that all currently accumulated output in the output stream is
		// discarded.
		baos.reset();
		session.setAttribute(CURRENTINPUT, baos);
		session.setAttribute(READBYTES, session.getReadBytes());

		if (null == llpMessage || 3 > llpMessage.length) {
			mLog.log(Level.SEVERE, I18n.msg("E0236: MLLPDecoder.decode(): Invalid MLLP data [ {0} ]. Its length shouldn't be less than 3.",
					llpMessage.toString() ));
			logMsg = I18n.msg("E0236: MLLPDecoder.decode(): Invalid MLLP data [ {0} ]. Its length shouldn't be less than 3.",
					llpMessage.toString() );

			throw new ProtocolCodecException(logMsg);
		}
		
		//Check if the llp message also contains MLLPV2 Ack/nak at the beginning of data.
		//If it contains MLLPV2 ack/nak write ack/nak to ProtocolDecoderOutput, and 
		//remaining data to baos so that the remaining data will be available in the next read
		if (llpMessage.length > 4 && (getEndBlockChar() == (char) llpMessage[2]) 
				&& getEndDataChar() == llpMessage[3]) {
			mLog.fine(I18n.msg("MLLPV2Decoder:Found MLLPV2 ACK/NAK in the beginning of llpMessage. writing MLLPV2 ACK/NAK to protocoldecoderoutput"));
			byte[] mllpv2acknak = new byte[1];
			System.arraycopy(llpMessage,1,mllpv2acknak,0,1);
			out.write(new String(mllpv2acknak));
			
			
			byte[] remainingBytes = new byte[llpMessage.length -4];
			System.arraycopy(llpMessage,4,remainingBytes,0,remainingBytes.length);
			
			baos.reset();
			baos.write(remainingBytes,0,remainingBytes.length);
			return;
		}
		
		// EOB validation
		if (getEndBlockChar() != (char) llpMessage[llpMessage.length - 1]) {

			mLog.log(Level.SEVERE,
					I18n.msg("E0238: MLLPDecoder.decode(): Invalid MLLP data,.The configuration value of End Block Character is [{0}], it does not match with the actual data [{1}]. Full buffer contents: {2}",
					"" + (int)getEndBlockChar(),(int)(char)llpMessage[llpMessage.length - 1], new String(llpMessage) ));
			logMsg = I18n.msg("E0238: MLLPDecoder.decode(): Invalid MLLP data,.The configuration value of End Block Character is [{0}], it does not match with the actual data [{1}]. Full buffer contents: {2}",
					"" + (int)getEndBlockChar(),(int)(char)llpMessage[llpMessage.length - 1], new String(llpMessage) );

			throw new ProtocolCodecException(logMsg);
		}

		// EOD validation
		if (getEndDataChar() != (char) llpMessage[llpMessage.length - 2]) {

			mLog.log(Level.SEVERE,
					I18n.msg("E0239: MLLPDecoder.decode(): Invalid MLLP data,.The configuration value of End Data Character is [ {0}}] ,It does not match with the actual data.  [{1}}]",
					 "" + (int)getEndDataChar(),(int)(char)llpMessage[llpMessage.length - 2]));
			logMsg = I18n.msg(
					"E0239: MLLPDecoder.decode(): Invalid MLLP data,.The configuration value of End Data Character is [ {0}}] ,It does not match with the actual data.  [{1}}]",
					 "" + (int)getEndDataChar(),(int)(char)llpMessage[llpMessage.length - 2] );

			throw new ProtocolCodecException(logMsg);
		}
		byte[] hl7MessageBody = new byte[llpMessage.length - 3];
		System.arraycopy(llpMessage, 1, hl7MessageBody, 0,
				hl7MessageBody.length);
		byte[] hl7MsgBody = null;
		if (getEndBlockChar() == (char) hl7MessageBody[hl7MessageBody.length - 1]
				&& getEndBlockChar() == (char) hl7MessageBody[hl7MessageBody.length - 2]) {
			hl7MsgBody = new byte[hl7MessageBody.length - 1];

		} else {
			hl7MsgBody = new byte[hl7MessageBody.length];
		}
		System.arraycopy(hl7MessageBody, 0, hl7MsgBody, 0, hl7MsgBody.length);

		String hl7PayLoad = new String(hl7MsgBody);
		mLog.fine(I18n.msg("MLLPV2Decoder:Received HL7 Message : {0}",  hl7PayLoad));
		out.write(hl7PayLoad);

	}

}
