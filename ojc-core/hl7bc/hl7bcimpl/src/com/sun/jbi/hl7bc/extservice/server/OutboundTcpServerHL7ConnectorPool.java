package com.sun.jbi.hl7bc.extservice.server;

import static com.sun.jbi.hl7bc.extservice.llp.LLPProviderFactory.createLLPProvider;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;

import org.apache.mina.common.DefaultIoFilterChainBuilder;
import org.apache.mina.common.IoAcceptorConfig;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolCodecFilter;
import org.apache.mina.transport.socket.nio.SocketAcceptor;
import org.apache.mina.transport.socket.nio.SocketAcceptorConfig;

import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.I18n;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.extservice.llp.AbstractLLPProtocolProvider;
import com.sun.jbi.hl7bc.extservice.llp.LLPProviderFactory.LLPType;
import com.sun.jbi.hl7bc.extservice.llp.LLPProviderFactory.Util;

/**
 * Stores all socket acceptors for incoming connections relating to outbound
 * message exchanges. In other words, manages InOut and OutOnly connections with
 * a role of "Server".
 * 
 * @author James Agnew
 */
public class OutboundTcpServerHL7ConnectorPool extends IoHandlerAdapter {

//	/** Singleton instance */
//	public static final OutboundTcpServerHL7ConnectorPool INSTANCE = new OutboundTcpServerHL7ConnectorPool();

	private static final Logger mLog = Logger.getLogger(OutboundTcpServerHL7ConnectorPool.class.getName());

	private final ConcurrentHashMap<OutboundTcpServerHL7Connector, IoSession> mConnector2IoSession;
	private final ConcurrentHashMap<String, BlockingQueue<IoSession>> mEndpointUniqueName2FreeIoSessions;
	private final SocketAcceptor mIoAcceptor;
	private final ConcurrentHashMap<IoSession, OutboundTcpServerHL7Connector> mIoSession2Connector;

	private ComponentContext mComponentContext;

	/**
	 * Constructor
	 */
	OutboundTcpServerHL7ConnectorPool() {
		mIoAcceptor = new SocketAcceptor();
		mEndpointUniqueName2FreeIoSessions = new ConcurrentHashMap<String, BlockingQueue<IoSession>>();
		mIoSession2Connector = new ConcurrentHashMap<IoSession, OutboundTcpServerHL7Connector>();
		mConnector2IoSession = new ConcurrentHashMap<OutboundTcpServerHL7Connector, IoSession>();
	}

	private synchronized void addSession(Endpoint theEndpoint, IoSession theSession) {
		getFreeSessionsForEndpoint(theEndpoint).add(theSession);

		if (mLog.isLoggable(Level.FINE)) {
			mLog.fine("Endpoint " + theEndpoint.getEndpointName() + " now has " + getFreeSessionsForEndpoint(theEndpoint).size() + " connections");
		}
	}

	public void connect(ProtocolInfo theProtocolInfo, Endpoint theEndpoint, OutboundTcpServerHL7Connector theServerTCPIpHL7Connector) throws Exception {
		openConnection(theEndpoint);

		// TODO: make timeout configurable
		mLog.finer("Waiting for connection");
		
		IoSession session;
		do {

			session = getFreeSessionsForEndpoint(theEndpoint).poll(60, TimeUnit.SECONDS);
			if (session == null) {
				throw new IOException("Timed out waiting for connection");
			}

		} while (session.isConnected() == false);

		synchronized (this) {
			mIoSession2Connector.put(session, theServerTCPIpHL7Connector);
			mConnector2IoSession.put(theServerTCPIpHL7Connector, session);
		}
	}

	private InetSocketAddress createAddressForEndpoint(Endpoint theEndpoint) {
		int hl7ServerPort = theEndpoint.getHL7Address().getHL7ServerPort();
		InetSocketAddress socketAddr = new InetSocketAddress(hl7ServerPort);
		return socketAddr;
	}

	public synchronized void discard(Endpoint theEndpoint, OutboundTcpServerHL7Connector theConnector) {
		IoSession ioSession = mConnector2IoSession.remove(theConnector);
		mIoSession2Connector.remove(ioSession);
		ioSession.close();
	}

	public synchronized void disconnect(Endpoint theEndpoint, OutboundTcpServerHL7Connector theConnector) {
		IoSession ioSession = mConnector2IoSession.remove(theConnector);
		if (ioSession == null) {
			return;
		}
		mIoSession2Connector.remove(ioSession);
		getFreeSessionsForEndpoint(theEndpoint).add(ioSession);
	}

	private BlockingQueue<IoSession> getFreeSessionsForEndpoint(Endpoint theEndpoint) {
		return mEndpointUniqueName2FreeIoSessions.get(theEndpoint.getUniqueName());
	}

	public boolean hasConnection(OutboundTcpServerHL7Connector theServerTCPIpHL7Connector) {
		IoSession session = mConnector2IoSession.get(theServerTCPIpHL7Connector);
		if (session == null) {
			return false;
		}
		return session.isConnected();
	}

	private synchronized void openConnection(Endpoint theEndpoint) throws Exception, IOException {

        InetSocketAddress socketAddr = createAddressForEndpoint(theEndpoint);
	    if (!mIoAcceptor.getManagedServiceAddresses().contains(socketAddr) && mEndpointUniqueName2FreeIoSessions.containsKey(theEndpoint.getUniqueName())) {

	        if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("IOAcceptor reports that it is no longer managing socket for address: " + socketAddr);
            }
	        
	        mEndpointUniqueName2FreeIoSessions.remove(theEndpoint.getUniqueName());
	        
	    }
	    
	    // If we don't have a list of sessions for this endpoint, that means
	    // we haven't yet started a listener, so we start one now
	    if (!mEndpointUniqueName2FreeIoSessions.containsKey(theEndpoint.getUniqueName())) {
		    
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Binding server socket for address: " + socketAddr);
            }
	        
			String llpTypeStr = theEndpoint.getHL7ProtocolProperties().getLLPType();
			IoAcceptorConfig config = new SocketAcceptorConfig();
			DefaultIoFilterChainBuilder chain = config.getFilterChain();
			LLPType llpType = Util.stringToEnumValue(llpTypeStr);

			// create the mina - protocol provider
			AbstractLLPProtocolProvider llpProtocolProv = createLLPProvider(llpType);
			ProtocolInfo protocolInfo = Util.populateLLPInfo(theEndpoint.getHL7ProtocolProperties());
			llpProtocolProv.setProtocolEncoderDecoderProps(protocolInfo);

			// Add the protocol codec factory
			chain.addLast("protocolFilter", new ProtocolCodecFilter(llpProtocolProv.getCodecFactory()));
			
			// Bind
			mIoAcceptor.bind(socketAddr, new MyIoHandler(theEndpoint), config);

			mEndpointUniqueName2FreeIoSessions.put(theEndpoint.getUniqueName(), new LinkedBlockingQueue<IoSession>());
			
		} else {
		    
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer("Already have an IO acceptor for server socket address: " + socketAddr);
            }
		    
		}
	    
	}

	private synchronized void removeSession(Endpoint theEndpoint, IoSession theSession) {

		getFreeSessionsForEndpoint(theEndpoint).remove(theSession);

		OutboundTcpServerHL7Connector connector = mIoSession2Connector.remove(theSession);
		if (connector != null) {
			mConnector2IoSession.remove(connector);
		}

		if (mLog.isLoggable(Level.FINE)) {
			mLog.fine("Endpoint " + theEndpoint.getUniqueName() + " now has " + getFreeSessionsForEndpoint(theEndpoint).size() + " connections");
		}

	}

	/**
	 * Send a message
	 */
	public synchronized void sendMessage(OutboundTcpServerHL7Connector theServerTCPIpHL7Connector, String theMessage) throws IOException {
		IoSession session = mConnector2IoSession.get(theServerTCPIpHL7Connector);
		if (session == null) {
			throw new IOException("No active connection could be found");
		}
		session.write(theMessage);
	}

	public synchronized void shutdownSocketsAssociatedWithEndpoint(Endpoint theEndpoint) {
		if (getFreeSessionsForEndpoint(theEndpoint) == null) {
			return;
		}

		InetSocketAddress socketAddr = createAddressForEndpoint(theEndpoint);
		
		if (mLog.isLoggable(Level.FINE)) {
			mLog.fine("Unbinding server socket for address: " + socketAddr);
		}
		
		try {
			mIoAcceptor.unbind(socketAddr);
		} catch (IllegalArgumentException e) {
			// Socket was not bound in the first place
			mLog.fine("Attempted to unbind socket which was not bound: " + socketAddr);
		}
		
		mEndpointUniqueName2FreeIoSessions.remove(theEndpoint);
	}

	/**
	 * Attempt to start listening, fail gracefully if we can't
	 */
	public void tryToStartListenerForEndpoint(Endpoint theEndpoint) {
		if (mLog.isLoggable(Level.FINE)) {
			mLog.fine("Going to attempt to start endpoint: " + theEndpoint.getUniqueName());
		}
		
		try {
			openConnection(theEndpoint);
		} catch (Exception e) {
			// We don't treat this as fatal, as an attempt to bind will be mad again
			// when the sender actually tries to send a message. This way we don't
			// fail starting up the service unit if the address can't be bound at
			// that time
			mLog.log(Level.WARNING, I18n.msg("E0340: Failed to open socket on port {0} on startup, won't retry until first message exchange. Error: {1}", theEndpoint
					.getHL7Address().getHL7ServerPort(), e.getMessage()), e);
		}
	}

	private class MyIoHandler extends IoHandlerAdapter {
		private Endpoint mEndpoint;

		/**
		 * Constructor
		 */
		public MyIoHandler(Endpoint theEndpoint) {
			mEndpoint = theEndpoint;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void messageReceived(IoSession theSession, Object theMessage) {
			OutboundTcpServerHL7Connector hl7Connector = mIoSession2Connector.get(theSession);
			if (hl7Connector == null) {
				InetSocketAddress localAddress = (InetSocketAddress) theSession.getLocalAddress();
				InetSocketAddress remoteAddress = (InetSocketAddress) theSession.getRemoteAddress();
				mLog.log(Level.INFO, I18n.msg("E0339: Unexpected message arrived on port {0} to host {1}", localAddress.getPort(), remoteAddress.getAddress().getHostAddress()));
			} else {
				hl7Connector.setHl7ResponseMsg((String) theMessage);
			}
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void sessionClosed(IoSession theSession) throws Exception {
			InetSocketAddress localAddress = (InetSocketAddress) theSession.getLocalAddress();
			InetSocketAddress remoteAddress = (InetSocketAddress) theSession.getRemoteAddress();
			mLog.log(Level.INFO, I18n.msg("E0337: Incoming connection lost on port {0} to host {1}", localAddress.getPort(), remoteAddress.getAddress().getHostAddress()));

			removeSession(mEndpoint, theSession);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void sessionOpened(IoSession theSession) throws Exception {
			InetSocketAddress localAddress = (InetSocketAddress) theSession.getLocalAddress();
			InetSocketAddress remoteAddress = (InetSocketAddress) theSession.getRemoteAddress();
			mLog.log(Level.INFO, I18n.msg("E0338: Incoming connection received on port {0} to host {1}", localAddress.getPort(), remoteAddress.getAddress().getHostAddress()));

			theSession.setAttribute("currentInput", new ByteArrayOutputStream());
			theSession.setAttribute("readBytes", new Long(0));

			addSession(mEndpoint, theSession);
		}

	}

	/**
	 * Starts the pool
	 */
	public synchronized void startPool(ComponentContext theComponentContext) {
		mLog.log(Level.INFO, I18n.msg("E0341: Starting outbound server socket pool"));
		
		mComponentContext = theComponentContext;
	}
	
	/**
	 * Starts the pool
	 */
	public synchronized void stopPool(ComponentContext theComponentContext) {
		mLog.log(Level.INFO, I18n.msg("E0342: Stopping outbound server socket pool"));
		
		mIoAcceptor.unbindAll();
		mEndpointUniqueName2FreeIoSessions.clear();
	}
	
}
