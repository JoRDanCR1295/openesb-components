package com.sun.jbi.hl7bc.extservice.server;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.I18n;
import com.sun.jbi.hl7bc.connection.Connection;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.extservice.client.HL7Connector;

/**
 * Implementation of HL7Connector which opens a TCP server port to send outgoing
 * messages (instead of the more common TCP client port).
 * 
 * @author James Agnew
 */
public class OutboundTcpServerHL7Connector implements HL7Connector {

	private static final Logger mLog = Logger.getLogger(OutboundTcpServerHL7Connector.class.getName());
	private final Lock lock = new ReentrantLock();
	private final Condition ackNotRecvd = lock.newCondition();
	private String hl7ResponseMsg;
	private Endpoint mEndpoint;
    private OutboundTcpServerHL7ConnectorPool mOutboundServerPool;
	private ProtocolInfo mProtocolInfo = null;

	public OutboundTcpServerHL7Connector(OutboundTcpServerHL7ConnectorPool theOutboundServerPool) {
		mLog.finer("Entering constructor");
		mOutboundServerPool = theOutboundServerPool;
	}

	public void connect(ProtocolInfo theProtocolInfo) throws Exception {
		mLog.finer("Entering connect(ProtocolInfo)");
		throw new UnsupportedOperationException();
	}

	public void connect(ProtocolInfo theProtocolInfo, Endpoint theEndpoint) throws Exception {
		mLog.finer("Entering connect(ProtocolInfo, Endpoint)");

		mEndpoint = theEndpoint;

		mOutboundServerPool.connect(theProtocolInfo, theEndpoint, this);
	}

	/**
	 * {@inheritDoc}
	 */
	public void discardConnection() throws Exception {
		mLog.finer("Entering discardConnection()");
		mOutboundServerPool.discard(mEndpoint, this);
	}

	public void disconnect() throws Exception {
		mLog.finer("Entering disconnect()");
		mOutboundServerPool.disconnect(mEndpoint, this);
	}

	/**
	 * Unsupported (this should probable be refactored out, as it is more for
	 * outbound pooled connections)
	 */
	public Connection getHL7Connection() throws Exception {
		throw new UnsupportedOperationException();
	}

	private String getHl7ResponseMsg() {
		return hl7ResponseMsg;
	}

    public ProtocolInfo getProtocolInfo() throws Exception {
       return  this.mProtocolInfo;
    }
    
    public void setProtocolInfo(ProtocolInfo pInfo) throws Exception {
        this.mProtocolInfo = pInfo;
     }

	public String recvHL7Message() throws Exception {
		lock.lock();
		try {
			while (getHl7ResponseMsg() == null) {
				ackNotRecvd.await(60, TimeUnit.SECONDS);
				if (!mOutboundServerPool.hasConnection(this)) {
					throw new IOException();
				}
			}
		} catch (Exception ex) {
			mLog.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
			String errMsg = I18n.msg("E0269: An exception occured in HL7 Connector");
			throw new Exception(errMsg, ex);
		} finally {
			lock.unlock();
		}
		String response = getHl7ResponseMsg();
		setHl7ResponseMsg(null); // reset
		return response;
	}

	public String recvHL7Message(long timeToWait) throws Exception {
		lock.lock();
		boolean timeOut = false;
		try {
			while (getHl7ResponseMsg() == null) {
				timeOut = ackNotRecvd.await(timeToWait, TimeUnit.SECONDS);
				if (!timeOut) {
					break;
				}
			}
		} catch (Exception ex) {
			mLog.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
			String errMsg = I18n.msg("E0269: An exception occured in HL7 Connector");
			throw new Exception(errMsg, ex);
		} finally {
			lock.unlock();
		}
		String response = getHl7ResponseMsg();
		setHl7ResponseMsg(null); // reset
		return response;
	}

	/**
	 * {@inheritDoc}
	 */
	public void sendHL7Message(String theHl7Msg) throws Exception {
		mOutboundServerPool.sendMessage(this, theHl7Msg);
	}

	/**
	 * Unsupported (this should probable be refactored out, as it is more for
	 * outbound pooled connections)
	 */
	public void setHL7Connection(Connection theConn) throws Exception {
		throw new UnsupportedOperationException();
	}

	public void setHl7ResponseMsg(String theHl7ResponseMsg) {
        lock.lock();
        this.hl7ResponseMsg = theHl7ResponseMsg;
        try {
            if (getHl7ResponseMsg() != null) {
                ackNotRecvd.signal();
            }
        } finally {
            lock.unlock();
        }
	}

	/**
	 * Unsupported (this should probable be refactored out, as it is more for
	 * outbound pooled connections)
	 */
	public void setIoSession(Connection theConn) throws Exception {
		throw new UnsupportedOperationException();
	}

}
