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
 * @(#)MLLPDecoderTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.llp;

import java.net.SocketAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.CloseFuture;
import org.apache.mina.common.IdleStatus;
import org.apache.mina.common.IoFilterChain;
import org.apache.mina.common.IoHandler;
import org.apache.mina.common.IoService;
import org.apache.mina.common.IoServiceConfig;
import org.apache.mina.common.IoSession;
import org.apache.mina.common.IoSessionConfig;
import org.apache.mina.common.TrafficMask;
import org.apache.mina.common.TransportType;
import org.apache.mina.common.WriteFuture;
import org.apache.mina.filter.codec.ProtocolCodecException;
import org.apache.mina.filter.codec.ProtocolDecoderOutput;
import org.jmock.Mock;

import com.sun.jbi.hl7bc.extservice.server.HL7EventHandler;

/**
 * Note, this test is based on the unit test for the Open eHealth MLLPDecoder implementation for
 * Apache Camel, which is available <a href="http://repo.openehealth.org/sites/ipf/reports/ipf-1.6.0/platform-camel/platform-camel-lbs/platform-camel-lbs-mina/xref-test/org/openehealth/ipf/platform/camel/lbs/mina/mllp/MllpDecoderTest.html"
 * >here</a> and is licensed under the Apache Software License
 * 
 * @author traghuna
 * @author James Agnew
 */
public class MLLPDecoderTest extends org.jmock.cglib.MockObjectTestCase {
	private MLLPDecoder instance;
	private IoSessionMock mIoSession;
	private ProtocolDecoderOutput mProtocolDecoderOutput;
	private Mock mProtocolDecoderOutputMock;

	private static final char START = '\u000b';
	private static final char END = '\u001c';
	private static final char CR = '\r';

	public MLLPDecoderTest(String testName) {
		super(testName);
	}

	protected void setUp() throws Exception {
		instance = new MLLPDecoder();
		instance.setStartBlockChar(START);
		instance.setEndBlockChar(CR);
		instance.setEndDataChar(END);

		mIoSession = new IoSessionMock();
		HL7EventHandler.initializeNewIoSession(mIoSession);

		mProtocolDecoderOutputMock = new Mock(ProtocolDecoderOutput.class);
		mProtocolDecoderOutput = (ProtocolDecoderOutput) mProtocolDecoderOutputMock.proxy();
	}

	protected void tearDown() throws Exception {
	}

	public static Test suite() {
		TestSuite suite = new TestSuite(MLLPDecoderTest.class);

		return suite;
	}

	/**
	 * Decode one message
	 */
	public void testDecodeSimpleMessage() throws ProtocolCodecException {
		String message = START + "Hello World" + END + CR;
		ByteBuffer in = toByteBuffer(message);

		mProtocolDecoderOutputMock.expects(once()).method("write").with(eq("Hello World"));
		instance.decode(mIoSession, in, mProtocolDecoderOutput);
	}

	public void testDecodeTwoMessages() throws Exception {
		String message = START + "Hello World" + END + CR + START + "Bye" + END + CR;
		ByteBuffer in = toByteBuffer(message);

		mProtocolDecoderOutputMock.expects(once()).method("write").with(eq("Hello World"));
		mProtocolDecoderOutputMock.expects(once()).method("write").with(eq("Bye"));
		instance.decode(mIoSession, in, mProtocolDecoderOutput);
		assertEquals(message.length(), in.position());
	}

	public void testDecodeMessageThatSpansMultipleBuffers() throws Exception {
		String messagePart1 = START + "This is only the beginning of the message. ";
		String messagePart2 = "It continues in the next buffer" + END + CR;
		ByteBuffer in1 = toByteBuffer(messagePart1);
		ByteBuffer in2 = toByteBuffer(messagePart2);

		mProtocolDecoderOutputMock.expects(once()).method("write").with(eq("This is only the beginning of the message. It continues in the next buffer"));
		instance.decode(mIoSession, in1, mProtocolDecoderOutput);
		instance.decode(mIoSession, in2, mProtocolDecoderOutput);
	}

	public void testDecodeMessageWithCRInNextBuffer() throws Exception {
		String messagePart1 = START + "This is nearly the whole message" + END;
		String messagePart2 = "" + CR;
		ByteBuffer in1 = toByteBuffer(messagePart1);
		ByteBuffer in2 = toByteBuffer(messagePart2);

		mProtocolDecoderOutputMock.expects(once()).method("write").with(eq("This is nearly the whole message"));
		instance.decode(mIoSession, in1, mProtocolDecoderOutput);
		instance.decode(mIoSession, in2, mProtocolDecoderOutput);

	}

	public void testDecodeMessageWithEndInNextBuffer() throws Exception {
		String messagePart1 = START + "This is nearly the whole message";
		String messagePart2 = "" + END + CR;
		ByteBuffer in1 = toByteBuffer(messagePart1);
		ByteBuffer in2 = toByteBuffer(messagePart2);

		mProtocolDecoderOutputMock.expects(once()).method("write").with(eq("This is nearly the whole message"));
		instance.decode(mIoSession, in1, mProtocolDecoderOutput);
		instance.decode(mIoSession, in2, mProtocolDecoderOutput);
	}

	public void testDecodeMessageWithStartAsLastChar() throws Exception {
		String messagePart1 = "" + START;
		String messagePart2 = "This is nearly the whole message" + END + CR;
		ByteBuffer in1 = toByteBuffer(messagePart1);
		ByteBuffer in2 = toByteBuffer(messagePart2);

		mProtocolDecoderOutputMock.expects(once()).method("write").with(eq("This is nearly the whole message"));
		instance.decode(mIoSession, in1, mProtocolDecoderOutput);
		instance.decode(mIoSession, in2, mProtocolDecoderOutput);
	}

	/**
	 * Test of setStartBlockChar and getStartBlockChar method.
	 */
	public void testSetGetStartBlockCharacter() {
		System.out.println("Testing setStartBlockCharacter and getStartBlockCharacter");
		try {
			char val = (char) 11;
			instance.setStartBlockChar(val);
			char result = instance.getStartBlockChar();
			assertEquals(val, result);
			System.out.println("Successfully tested setStartBlockCharacter and getStartBlockCharacter");
		} catch (Exception e) {
			System.out.println("Exception occred for setStartBlockCharacter and getStartBlockCharacter");
		}
	}

	/**
	 * Test of setEndDataChar and getEndDataChar method.
	 */
	public void testSetGetEndDataCharacter() {
		System.out.println("Testing setEndDataChar and getEndDataChar");
		try {
			char val = (char) 28;
			instance.setEndDataChar(val);
			char result = instance.getEndDataChar();
			assertEquals(val, result);
			System.out.println("Successfully tested setEndDataCharacter and getEndDataCharacter");
		} catch (Exception e) {
			System.out.println("Exception occred for setEndDataCharacter and getEndDataCharacter");
		}
	}

	/**
	 * Test of setEndBlockChar and getEndBlockChar method.
	 */
	public void testSetGetEndBlockCharacter() {
		System.out.println("Testing setStartBlockCharacter and getStartBlockCharacter");
		try {
			char val = (char) 13;
			instance.setEndBlockChar(val);
			char result = instance.getEndBlockChar();
			assertEquals(val, result);
			System.out.println("Successfully tested setEndBlockCharacter and getEndBlockCharacter");
		} catch (Exception e) {
			System.out.println("Exception occred for setEndBlockCharacter and getEndBlockCharacter");
		}
	}

	private class IoSessionMock implements IoSession {
		private Map<String, Object> mAttributes = new HashMap<String, Object>();
		private long mReadBytes = 0;

		public CloseFuture close() {
			throw new UnsupportedOperationException();
		}

		public boolean containsAttribute(String theKey) {
			return mAttributes.containsKey(theKey);
		}

		public Object getAttachment() {
			throw new UnsupportedOperationException();
		}

		public Object getAttribute(String theKey) {
			return mAttributes.get(theKey);
		}

		public Set<String> getAttributeKeys() {
			return mAttributes.keySet();
		}

		public CloseFuture getCloseFuture() {
			throw new UnsupportedOperationException();
		}

		public IoSessionConfig getConfig() {
			throw new UnsupportedOperationException();
		}

		public long getCreationTime() {
			throw new UnsupportedOperationException();
		}

		public IoFilterChain getFilterChain() {
			throw new UnsupportedOperationException();
		}

		public IoHandler getHandler() {
			throw new UnsupportedOperationException();
		}

		public int getIdleCount(IdleStatus theStatus) {
			throw new UnsupportedOperationException();
		}

		public int getIdleTime(IdleStatus theStatus) {
			throw new UnsupportedOperationException();
		}

		public long getIdleTimeInMillis(IdleStatus theStatus) {
			throw new UnsupportedOperationException();
		}

		public long getLastIdleTime(IdleStatus theStatus) {
			throw new UnsupportedOperationException();
		}

		public long getLastIoTime() {
			throw new UnsupportedOperationException();
		}

		public long getLastReadTime() {
			throw new UnsupportedOperationException();
		}

		public long getLastWriteTime() {
			throw new UnsupportedOperationException();
		}

		public SocketAddress getLocalAddress() {
			throw new UnsupportedOperationException();
		}

		public long getReadBytes() {
			return mReadBytes;
		}

		public long getReadMessages() {
			throw new UnsupportedOperationException();
		}

		public SocketAddress getRemoteAddress() {
			throw new UnsupportedOperationException();
		}

		public int getScheduledWriteBytes() {
			throw new UnsupportedOperationException();
		}

		public int getScheduledWriteRequests() {
			throw new UnsupportedOperationException();
		}

		public IoService getService() {
			throw new UnsupportedOperationException();
		}

		public SocketAddress getServiceAddress() {
			throw new UnsupportedOperationException();
		}

		public IoServiceConfig getServiceConfig() {
			throw new UnsupportedOperationException();
		}

		public TrafficMask getTrafficMask() {
			throw new UnsupportedOperationException();
		}

		public TransportType getTransportType() {
			throw new UnsupportedOperationException();
		}

		public int getWriteTimeout() {
			throw new UnsupportedOperationException();
		}

		public long getWriteTimeoutInMillis() {
			throw new UnsupportedOperationException();
		}

		public long getWrittenBytes() {
			throw new UnsupportedOperationException();
		}

		public long getWrittenMessages() {
			throw new UnsupportedOperationException();
		}

		public long getWrittenWriteRequests() {
			throw new UnsupportedOperationException();
		}

		public boolean isClosing() {
			throw new UnsupportedOperationException();
		}

		public boolean isConnected() {
			throw new UnsupportedOperationException();
		}

		public boolean isIdle(IdleStatus theStatus) {
			throw new UnsupportedOperationException();
		}

		public Object removeAttribute(String theKey) {
			return mAttributes.remove(theKey);
		}

		public void resumeRead() {
			throw new UnsupportedOperationException();
		}

		public void resumeWrite() {
			throw new UnsupportedOperationException();
		}

		public Object setAttachment(Object theAttachment) {
			throw new UnsupportedOperationException();
		}

		public Object setAttribute(String theKey) {
			return mAttributes.remove(theKey);
		}

		public Object setAttribute(String theKey, Object theValue) {
			return mAttributes.put(theKey, theValue);
		}

		public void setIdleTime(IdleStatus theStatus, int theIdleTime) {
			throw new UnsupportedOperationException();
		}

		public void setTrafficMask(TrafficMask theTrafficMask) {
			throw new UnsupportedOperationException();
		}

		public void setWriteTimeout(int theWriteTimeout) {
			throw new UnsupportedOperationException();
		}

		public void suspendRead() {
			throw new UnsupportedOperationException();
		}

		public void suspendWrite() {
			throw new UnsupportedOperationException();
		}

		public WriteFuture write(Object theMessage) {
			throw new UnsupportedOperationException();
		}

	}

	private ByteBuffer toByteBuffer(String message) {
		ByteBuffer in = ByteBuffer.allocate(1024);
		in.put(message.getBytes());
		in.flip();
		return in;
	}

}
