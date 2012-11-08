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
 * $Id: TcpipSocketManager.java,v 1.1.1.1 2007/04/09 17:49:30 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip;

import java.io.FileInputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.security.KeyStore;

import javax.jbi.JBIException;
import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.common.util.EsbPathHelper;
import com.bostechcorp.cbesb.runtime.component.tcpip.wsdl.TcpipConfig;

/*
 * This class performs the job of connecting with remote systems. In server mode
 * it starts a server socket and waits for client connections. In client mode
 * it attempts to connect to the remote system.
 * In either case, it will retry in the event of failures.
 * When a connection is established, a new TcpipSocket thread is started to handle 
 * it and is registered with the endpoint. When the maximum connection
 * limit is reached this thread ends.
 */
public class TcpipSocketManager extends Thread {
	protected final transient Log logger = LogFactory.getLog(getClass());
	private TcpipEndpoint endpoint;
	private IManager manager;
	private int maxSockets;
	private boolean isRunning = true;

	
	public static TcpipSocketManager getInstance(TcpipEndpoint ep) throws JBIException {
		TcpipSocketManager mgr = new TcpipSocketManager(ep);
		mgr.start();
		return mgr;
	}

		
	// force stop the manager
	public void forceStop() {
		try {
			manager.stop();
		} catch (Exception e) {}
	}
	
	
	public int getMaxSockets() { return maxSockets; }

	
	protected TcpipSocketManager(TcpipEndpoint ep) throws JBIException {
		endpoint = ep;
		TcpipConfig config = endpoint.getConfig();
		
		try {
			if (config.connectionMode == TcpipConfig.CONNECTION_MODE_CLIENT) {
				maxSockets = 1;
				manager = new ClientManager(config);				
			}
			else {
				maxSockets = config.getMaxClients();
				manager = new ServerManager(config);
			}
		}
		catch (Exception e) {
			logger.error("can't create socket manager "+e, e);
			throw new JBIException(e);
		}
	}

	
	/*
	 * IManager abstracts the client and server connection logic into a common interface
	 */
	private interface IManager {
		void stop() throws Exception;
		TcpipSocket getSocket() throws Exception;
	}
	
	private class ServerManager implements IManager {
		InetAddress localBindAddress;
		int listenPort;
		ServerSocket listener;
		
		public ServerManager(TcpipConfig config) throws Exception{
			// basic socket configuration
			if (config.getLocalBindHost() != null)
					localBindAddress = InetAddress.getByName(config.getLocalBindHost());
			listenPort = config.getListenPort();
		}
		
		public void stop() throws Exception {
			isRunning = false;
			listener.close();
		}
		
		public TcpipSocket getSocket() throws Exception {
			TcpipSocket result = null;
			// start listening
			if (listener == null) {
				try {
					if (endpoint.getConfig().isUseSsl()) {
						// SSL Server
						SSLContext ctx = getSslContext();
						SSLServerSocketFactory ssocketFactory = ctx.getServerSocketFactory();
						SSLServerSocket ssocket = null;
						if (localBindAddress == null) 
							ssocket = (SSLServerSocket)ssocketFactory.createServerSocket(listenPort);
						else
							ssocket = (SSLServerSocket)ssocketFactory.createServerSocket(listenPort, 0, localBindAddress);
						if (endpoint.getConfig().isAllowAnonymous())
							ssocket.setEnabledCipherSuites(ssocket.getSupportedCipherSuites());
						ssocket.setNeedClientAuth(endpoint.getConfig().isAuthenticateClient());
						listener = ssocket;
					} else {
						// normal server
						if (localBindAddress == null) 
							listener = new ServerSocket(listenPort);
						else
							listener = new ServerSocket(listenPort, 0, localBindAddress);
					}
				}
				catch (Exception sse) {
					String listenAddress = "";
					if (localBindAddress != null)
						listenAddress = listenAddress.concat(localBindAddress+":");
					listenAddress = listenAddress.concat(""+listenPort);
					logger.error("ServerSocket Exception listening on "+listenAddress);
					throw sse;
				}
			}
			//accept a connection
			if (listener != null) {
				// get a client
				Socket client = listener.accept();
				logger.debug("accepting a client  "+client.getInetAddress()+":"+client.getPort());
				result = TcpipSocket.getInstance(endpoint, client);
			}
			return result;			
		}
	}
	
	
	private class ClientManager implements IManager {
		InetAddress localBindAddress;
		InetAddress remoteHost;
		int port;
		Socket connectedClient;
		SSLSocketFactory sslSocketFactory;
		
		public ClientManager(TcpipConfig config) throws Exception{
			// basic socket configuration
			if (config.getLocalBindHost() != null)
					localBindAddress = InetAddress.getByName(config.getLocalBindHost());
			remoteHost = InetAddress.getByName(config.getHost());
			port = config.getPort();
			// SSL configuration	
			if (endpoint.getConfig().isUseSsl()) {
				// SSL Client
				SSLContext ctx = getSslContext();
				sslSocketFactory = ctx.getSocketFactory();
			}
		}
		
		public void stop() throws Exception {
			isRunning = false;
		}
		
		public TcpipSocket getSocket() throws Exception {
			TcpipSocket result = null;
			if (sslSocketFactory != null) {
				SSLSocket sock = (SSLSocket)sslSocketFactory.createSocket();
				if (endpoint.getConfig().isAllowAnonymous())
					sock.setEnabledCipherSuites(sock.getSupportedCipherSuites());
				InetSocketAddress sockAddr = new InetSocketAddress(remoteHost, port);
				if (localBindAddress == null)
					sock.connect(sockAddr);
				else {
					InetSocketAddress bindAddr = new InetSocketAddress(localBindAddress, 0);
					sock.bind(bindAddr);
					sock.connect(sockAddr);
				}
				sock.setSoTimeout(10000);
				sock.startHandshake();
				connectedClient = sock;	
			} else {
				if (localBindAddress == null) 
					connectedClient = new Socket(remoteHost, port);
				else
					connectedClient = new Socket(remoteHost, port, localBindAddress, 0);
			}
			result = TcpipSocket.getInstance(endpoint, connectedClient);
			return result;
		}
	}
	
	/*
	 *  Thread main method
	 */
	public void run() {
		while (isRunning) {
			TcpipSocket newSocket = null;
			try {
				newSocket = manager.getSocket();
			}
			catch (Exception e) {
				logger.error("Exception getting a socket \n"+endpoint.getService()+endpoint.getEndpoint()+"\n"+e);
				if (isRunning) {
					logger.error("retry in 10 seconds");
					try {
						sleep(10000);
					} catch (InterruptedException ie) {}
				}
			}
			if (newSocket != null) {
				endpoint.addSocket(newSocket);
				if (endpoint.getSocketCount() >= maxSockets) {
					try {
						manager.stop();
					} catch (Exception e) {
						logger.debug("error stopping the manager"+e, e);
					}
					break;
				}
			}
		}
		endpoint.clearSocketManager();
	}
	
	private SSLContext getSslContext() {
		SSLContext ctx = null;
		try {
			ctx = SSLContext.getInstance(endpoint.getConfig().getSslProtocol());
			// get the private key
			KeyManager[] km = null;
			if (endpoint.getConfig().isUsePrivateKey()) {
				KeyStore ks = KeyStore.getInstance("JKS");
				KeyManagerFactory kmf = KeyManagerFactory.getInstance("SunX509");
				String keyStorePath = EsbPathHelper.getFullPathForDef(endpoint.getConfig().getKeyStoreFile());
				char[] keyStorePassword = endpoint.getConfig().getKeyStorePassword().toCharArray();
				FileInputStream kstr = new FileInputStream(keyStorePath);
				ks.load(kstr, keyStorePassword);
				kmf.init(ks, keyStorePassword);
				km = kmf.getKeyManagers();
			}
			// get the trust store
			TrustManager[] tm = null;
			if ((endpoint.getConfig().isAuthenticateServer() || endpoint.getConfig().isAuthenticateClient())) {
				if (!endpoint.getConfig().isUseDefaultTrustStore()) {
					TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
					KeyStore ks = KeyStore.getInstance("JKS");
					String trustStorePath = EsbPathHelper.getFullPathForDef(endpoint.getConfig().getTrustStoreFile());
					FileInputStream tstr = new FileInputStream(trustStorePath);
					ks.load(tstr, null);
					tmf.init(ks);
					tm = tmf.getTrustManagers();
				}
			} else {
				// trust anyone for unauthenticated
		        TrustManager[] trustAllCerts = new TrustManager[]{
		        		new X509TrustManager() {

		        			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
		        				return null;
		        			}

		        			public void checkClientTrusted(java.security.cert.X509Certificate[] certs, String authType) {
		        				//No need to implement.
		        			}

		        			public void checkServerTrusted(java.security.cert.X509Certificate[] certs, String authType) {
		        				//No need to implement.
		        			}
		        		}
		        };
		        tm = trustAllCerts;
			}
			// initialize the SSLContext
			ctx.init(km, tm, null);
		}
		catch (Exception e) {
			logger.error("error getting SSL context "+e, e);
			ctx = null;
		}		
		return ctx;
	}
}
