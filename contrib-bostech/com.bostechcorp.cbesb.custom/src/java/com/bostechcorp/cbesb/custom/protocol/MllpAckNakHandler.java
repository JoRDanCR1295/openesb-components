package com.bostechcorp.cbesb.custom.protocol;

import java.net.SocketTimeoutException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import com.bostechcorp.cbesb.runtime.ccsl.lib.DumpNormalizedMessage;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipContext;

public class MllpAckNakHandler extends MllpHandler {
	protected char fieldSep;
	protected String[] mshFields;

	public String getDescription() { return "TCPIP MLLP ACK/NAK protocol handler"; }

	
	public void init(ITcpipContext ctx) {
		super.init(ctx);
		ctx.setIsAsyncSend(true);
	}
	
	
	/*
	 * This handles received data in consumer mode.
	 * Create an exchange and send an ACK if the MSH is valid
	 */
	public int gotReceiveData(byte[] bytes) throws Exception {
		int bytesConsumed = receiveStateMachine(bytes);
		if (gotInMessage) {
			// pre-parse the message and make sure that we at least have a valid MSH segment
			String messageString = new String(inMessage);
			byte[] messageBytes = messageString.getBytes("ISO8859-1");
			logger.debug("got inbound message ("+inMessage.length()+" bytes)\n"+DumpNormalizedMessage.dumpBytesAsHex(messageBytes));
			if (hl7PreParse(messageString)) {
				// send a message exchange into the container.
				context.createInbound(messageBytes);
				
				// construct an ACK message
				String ackString = makeAck();
				byte[] ackBytes = ackString.getBytes("ISO8859-1");
				logger.debug("sending acknowledgement ("+ackString.length()+" bytes)\n"+DumpNormalizedMessage.dumpBytesAsHex(ackBytes));
				sendMessage(ackBytes);
			} else 
				logger.debug("not valid HL7");
			
			// reset the input state
			inMessage = new StringBuffer();
			inputState = INPUT_WANT_0B;
			gotInMessage = false;
			context.setSocketTimeout(0);
		}
		return bytesConsumed;
	}
	
	
	/*
	 * Send a message. Get the ack and resend if timeout or not "MSA|AA"
	 */
	public void processInOnlyBytes(byte[] bytes) throws Exception {
		int retries;
		for (retries=0; retries < 5; retries++) {
			try {
				byte[] receivedBytes;
				try {
					receivedBytes = super.processInOutBytes(bytes);
				}
				catch (SocketTimeoutException toe) {
					continue;
				}
				String receivedString = new String(receivedBytes, "ISO8859-1");
				int crPosition = receivedString.indexOf((char)13);
				if (crPosition > 0) {
					String fieldSep = receivedString.substring(3,4);
					String goodAck = "MSA" + fieldSep + "AA";
					if (receivedString.substring(crPosition+1, crPosition+1+goodAck.length()).equals(goodAck)) {
						logger.debug("got good ack\n"+DumpNormalizedMessage.dumpBytesAsHex(receivedBytes));
						break;
					}
				}
			}
			catch (SocketTimeoutException e) {
				logger.debug("ACKNACK send got "+e, e);
			}
		}
		if (retries == 5) {
			throw new Exception("no ACK received");
		}
	}
	

	/*
	 * in-out exchanges are not valid for this handler. We simply demote it to 
	 * in-only and return a canned success message.  
	 */
	public byte[] processInOutBytes(byte[] bytes) throws Exception {
		byte[] successBytes = "MllpAckNakHandler sent message successfully".getBytes("ISO8859-1");
		processInOnlyBytes(bytes);
		return successBytes;
	}
	
	
	/*
	 * Verify that the message begins with 'MSH' and contains a carriage return.
	 * Split the MSH segment into an array on the field separator
	 */
	protected boolean hl7PreParse(String messageString) {
		boolean isValid = false;
		int firstCr = messageString.indexOf(13);
		if (firstCr > 2 && messageString.startsWith("MSH")) {
			fieldSep = messageString.charAt(3);
			mshFields = splitFields(messageString.substring(0, firstCr), fieldSep);
			// we need at least 12 fields
			if (mshFields.length >= 12) isValid = true;
		}		
		return isValid;
	}

	protected String makeAck() {
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		long timeNow = System.currentTimeMillis();
		String timeStamp = sdf.format(new Date(timeNow));
		String eventString;
		if (mshFields[8].length() > 4) eventString = mshFields[8].substring(3);
		else eventString = "";
		String ack =
			"MSH"			+ fieldSep +
			mshFields[1]	+ fieldSep +	// encoding characters
			mshFields[4]	+ fieldSep +	// flip the sender and receiver
			mshFields[5]	+ fieldSep +
			mshFields[2]	+ fieldSep +
			mshFields[3]	+ fieldSep +
			timeStamp		+ fieldSep +	// current date & time
			""				+ fieldSep +	// not used
			"ACK" +							// message type
				eventString +
				fieldSep +
			timeNow			+ fieldSep +	// control number, use current time
			mshFields[10]	+ fieldSep +	// processing (T/P)
			mshFields[11]	+ (char)13 +
			"MSA"			+ fieldSep +
			"AA"			+ fieldSep +
			mshFields[9]	+ fieldSep;		// original control number
		return ack;
	}
	
	protected String[] splitFields(String mes, char sep) {
		Vector<String> fields = new Vector<String>();
		int startPosition = 0;
		int sepPosition;
		while((sepPosition = mes.indexOf(sep, startPosition)) >= 0) {
			fields.add(mes.substring(startPosition, sepPosition));
			startPosition = sepPosition+1;
		}
		if (startPosition < mes.length())
			fields.add(mes.substring(startPosition));
		return fields.toArray(new String[0]);
	}
}
