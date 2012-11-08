/*
 * ChainBuilder ESB
 * 		Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
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
 * $Id: TcpipEndpoint.java,v 1.4 2007/04/25 20:46:00 afung Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip;

import java.util.Vector;
import java.util.concurrent.SynchronousQueue;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;

import org.apache.servicemix.common.ExchangeProcessor;

import com.bostechcorp.cbesb.common.util.RuntimeClassLoader;
import com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging.BaseEndpoint;
import com.bostechcorp.cbesb.runtime.ccsl.jbi.messaging.IComponentProcessor;
import com.bostechcorp.cbesb.runtime.ccsl.lib.CcslUtil;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipHandler;
import com.bostechcorp.cbesb.runtime.component.tcpip.processors.ConsumerProcessor;
import com.bostechcorp.cbesb.runtime.component.tcpip.processors.ProviderProcessor;
import com.bostechcorp.cbesb.runtime.component.tcpip.wsdl.TcpipConfig;

public class TcpipEndpoint extends BaseEndpoint implements ExchangeProcessor {
	private TcpipConfig config;
	private Vector<TcpipSocket> sockets = new Vector<TcpipSocket>();
	private TcpipSocketManager socketManager;
	private int maxSockets;
	private Class handlerClass;
	private QName messageQName;
	private QName operationQName;
	private SynchronousQueue<MessageExchange> sendQueue = new SynchronousQueue<MessageExchange>(true);
	private SynchronousQueue<Object> returnQueue = new SynchronousQueue<Object>(true);
	
	public QName getMessageQName() { return messageQName; }
	
	public void setMessageQName(QName name) { messageQName = name; }
	
	public QName getOperationQName() { return operationQName; }
	
	public void setOperationQName(QName name) { operationQName = name; }
	
	public DeliveryChannel getDeliveryChannel() { return channel; }
	
	public SynchronousQueue<MessageExchange> getSendQueue() { return sendQueue; }

	public SynchronousQueue<Object> getReturnQueue() { return returnQueue; }
	
	public TcpipConfig getConfig() { return config; }
	
	public void setConfig(TcpipConfig arg0) { config = arg0; }
	
	public void addSocket(TcpipSocket sock) {
		sockets.add(sock);
		logger.debug("sockets="+sockets.size());
	}
	
	public void removeSocket(TcpipSocket sock) {
		sockets.remove(sock);
		logger.debug("sockets="+sockets.size());
		if (socketManager == null && sockets.size() < maxSockets)
			try {
				socketManager = TcpipSocketManager.getInstance(this);
			}
			catch (Exception e) {
				logger.error("error restarting socket manager "+e, e);
			}
	}
	
	public int getSocketCount() {
		return sockets.size();
	}

	public void clearSocketManager() {
		socketManager = null;
	}
	
	protected IComponentProcessor createProviderProcessor() {
		return new ProviderProcessor(this);
	}

	protected IComponentProcessor createConsumerProcessor() {
		return new ConsumerProcessor(this);
	}

	public void start() throws Exception {
		if (handlerClass == null) loadProtocolHandler();
		socketManager = TcpipSocketManager.getInstance(this);
		maxSockets = socketManager.getMaxSockets();
	}

	public void stop() {
		// Force stop the server if there is one
		if (socketManager != null) {
			socketManager.forceStop();
			socketManager=null;
		}
		// Force stop any active connections
		for (TcpipSocket socket : sockets) {
			socket.forceStop();
		}
		sockets.clear();
	}
	
	public ITcpipHandler getProtocolHandlerInstance() throws Exception { return (ITcpipHandler)handlerClass.newInstance(); }
	
	private void loadProtocolHandler() throws Exception {
		// load the handler class
		String suRootPath = getServiceUnit().getRootPath();
		//String serviceAssemblyName = CcslUtil.getAssemblyName(suRootPath);
		String handlerClassName = getConfig().getProtocolHandler();
		//ClassLoader cl = RuntimeClassLoader.getClassLoader(serviceAssemblyName, this);
		//handlerClass = Class.forName(handlerClassName, true, cl);
		handlerClass = Class.forName(handlerClassName);
		// verify the correct type here before we go too far
		Object temp=handlerClass.newInstance();
		try {
			ITcpipHandler temp1 = (ITcpipHandler)temp;
			logger.debug("TcpipEndpoint loaded protocol handler "+temp1);
		} 
		catch (ClassCastException e) {
			logger.error("protocol handler does not implement ITcpipHandler "+e);
		}
	}
}
