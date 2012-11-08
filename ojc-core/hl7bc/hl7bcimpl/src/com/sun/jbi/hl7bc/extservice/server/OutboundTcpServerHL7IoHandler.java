package com.sun.jbi.hl7bc.extservice.server;

import org.apache.mina.common.IdleStatus;
import org.apache.mina.common.IoHandler;
import org.apache.mina.common.IoSession;

/**
 * IoHandler for outbound message server connections
 * 
 * @author James Agnew
 */
public class OutboundTcpServerHL7IoHandler implements IoHandler {

	private OutboundTcpServerHL7Connector mConnector;

	/**
	 * Constructor
	 */
	public OutboundTcpServerHL7IoHandler(OutboundTcpServerHL7Connector theConnector) {
		mConnector = theConnector;
	}
	
	public void exceptionCaught(IoSession theSession, Throwable theCause) throws Exception {
		// TODO Auto-generated method stub

	}

	public void messageReceived(IoSession theSession, Object theMessage) throws Exception {
		// TODO Auto-generated method stub

	}

	public void messageSent(IoSession theSession, Object theMessage) throws Exception {
		// TODO Auto-generated method stub

	}

	public void sessionClosed(IoSession theSession) throws Exception {
		// TODO Auto-generated method stub

	}

	public void sessionCreated(IoSession theSession) throws Exception {
		// TODO Auto-generated method stub

	}

	public void sessionIdle(IoSession theSession, IdleStatus theStatus) throws Exception {
		// TODO Auto-generated method stub

	}

	public void sessionOpened(IoSession theSession) throws Exception {
		// TODO Auto-generated method stub

	}

}
