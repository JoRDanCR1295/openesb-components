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
 * $Id: TcpipSocket.java,v 1.5 2007/04/25 20:46:00 afung Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.URI;
import java.util.concurrent.TimeUnit;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.transform.Source;

import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.runtime.ccsl.lib.DumpNormalizedMessage;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ExternalInput;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipContext;
import com.bostechcorp.cbesb.runtime.ccsl.lib.ITcpipHandler;
import com.bostechcorp.cbesb.runtime.ccsl.lib.OutputWriter;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.NormalizedMessageHandler;
import com.bostechcorp.cbesb.runtime.component.util.wsdl.WsdlMepConstants;

public class TcpipSocket extends Thread implements ITcpipContext {
	protected final transient Log logger = LogFactory.getLog(getClass());
	private static final String SENDER_ENDPOINT_PROPERTY = "org.apache.servicemix.senderEndpoint";
	private Socket socket;
	private TcpipEndpoint endpoint;
	private ITcpipHandler handler;
	private InputStream sockin;
	private OutputStream sockout;
	private boolean isRunning = true;
	
	
	public static TcpipSocket getInstance(TcpipEndpoint ep, Socket sock) throws Exception {
		TcpipSocket inst = new TcpipSocket();
		inst.endpoint = ep;
		inst.socket = sock;
		inst.handler = ep.getProtocolHandlerInstance();
		inst.logger.debug("creating socket "+inst.socket+"   handler="+inst.handler.getDescription()+"   "+inst.handler);
		// start the thread running
		inst.start();
		return inst;
	}
	
	public void forceStop() {
		try {
			isRunning = false;
			socket.close();
		}
		catch (Exception e) {
			logger.error("error closing socket "+e, e);
		}
	}

	
	protected TcpipSocket() {}

	
	/*
	 * Thread main method
	 */
	public void run() {
		logger.debug("starting socket thread");
		try {
			socket.setSoTimeout(0);
			sockin = socket.getInputStream();
			sockout = socket.getOutputStream();
		}
		catch (IOException e) {
			logger.error("can't get socket streams "+e, e);
			isRunning = false;
		}
		handler.init(this);
		
		while (isRunning) {
			try {
				if (endpoint.getConfig().getRole() == MessageExchange.Role.CONSUMER) {
					doConsumer();
				} else {
					doProvider();
				}
			}
			catch (IOException ioe) {
				logger.debug("IOException in socket processor "+ioe, ioe);
				isRunning = false;;
			}
			catch (Exception e) {
				logger.debug("General Exception in socket processor "+e, e);
				isRunning = false;;
			}
		}
		logger.debug("ending socket thread for"+socket);
		endpoint.removeSocket(this);
		try {
			socket.close();
		} catch (IOException ioe) {}
	}

	
	/*
	 * Consumer processing - read data and pass to handler
	 */
	private void doConsumer() throws Exception {
		try {
			// get socket data
			byte[] bytes = new byte[2048];
			int l = sockin.read(bytes);
			if (l < 1) {
				throw new SocketException("socket disconnected");
			}
			byte[] result = null;
			if (l < 2048) {
				result = new byte[l];
				for (int i=0; i<l; i++) result[i] = bytes[i];
			} else
				result = bytes;
			
			// send it to the handler
			for (int bytesLeft = 0; (bytesLeft = result.length) > 0;) {
				int consumed = handler.gotReceiveData(result);
				if (consumed > 0) {
					byte[] newBytes = new byte[bytesLeft-consumed];
					for (int i=0; i<newBytes.length; i++) newBytes[i] = result[consumed++];
					result = newBytes;
				}
			}
		}
		catch (SocketTimeoutException toe) {
			// pass timeout exceptions to the handler
			handler.gotReceiveTimeout();
		}
		catch (Exception e) {
			throw e;
		}
	}
	
	
	/*
	 * Provider processing - wait for an exchange and call handler's provider method
	 */
	private void doProvider() throws Exception {
		MessageExchange me = null;
		try {
			me = endpoint.getSendQueue().poll(10, TimeUnit.SECONDS);
			if (me == null) return;
		}
		catch (InterruptedException e) {
			// don't worry about interrupted, just continue
			return;
		}

		try {
			URI mep = me.getPattern();
			
			if (isExchangeBasedProvider) {
				// call the MessageExchange based handler
				if (mep.compareTo(WsdlMepConstants.IN_ONLY) == 0) {
					handler.processInOnlyExchange(me);
				} else if (mep.compareTo(WsdlMepConstants.IN_OUT) == 0) {
					handler.processInOutExchange(me);
				} else 
					throw new Exception("trying to process unknown MEP \""+mep+"\"");
			} else {
// display the message content
System.out.println("\n\n\nTVOLLE -outbound message:\n" +
        com.bostechcorp.cbesb.runtime.ccsl.lib.DumpNormalizedMessage.dump(me.getMessage("in")) + "\n\n");

				// call the byte array based handler
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				NormalizedMessageHandler nmh = new NormalizedMessageHandler(me.getMessage("in"));
				Source src = nmh.getRecordAtIndex(0);
				OutputWriter.processOutputStream(src, baos, "raw", endpoint.getConfig().getCharset());
				if (mep.compareTo(WsdlMepConstants.IN_ONLY) == 0) {
					handler.processInOnlyBytes(baos.toByteArray());
				} else if (mep.compareTo(WsdlMepConstants.IN_OUT) == 0) {
					byte[] outBytes = handler.processInOutBytes(baos.toByteArray());
					if (outBytes != null) {
						ExternalInput ext = new ExternalInput(new ByteArrayInputStream(outBytes), endpoint.getConfig().getCharset(), "raw", "xml", 0);
				        NormalizedMessage msg = me.createMessage();
				        ext.populateMessage(msg, this.endpoint.getMessageQName());
System.out.println("\n\n\nTCPIPSocket reply bytes:\n"+DumpNormalizedMessage.dump(msg)+"\n\n");
				        me.setMessage(msg, "out");
					}
				} else 
					throw new Exception("trying to process unknown MEP \""+mep+"\"");
			}
			// Notify the waiting provider processor that the exchange processing ended normally
			endpoint.getReturnQueue().put(me);			
		}
		catch (Exception e) {
			// Notify the waiting provider processor that the exchange processing got an exception
			endpoint.getReturnQueue().put(e);
			this.forceStop();
		}
	}

	/*
	 * ITcpipContext Implementation
	 */
	private boolean isExchangeBasedProvider = false;
	private boolean isAsyncSend = false;
	
	public void sendSocket(byte[] bytes) throws Exception {
		sockin.available(); // this gets an exception if the peer disconnected.
		sockout.write(bytes);
		sockout.flush();
	}
	
	
	public byte[] receiveSocket() throws Exception {
		byte[] bytes = new byte[2048];
		int l;
		l = sockin.read(bytes);
		if (l < 1) {
			throw new SocketException("socket disconnected");
		}
		byte[] result = null;
		if (l < 2048) {
			result = new byte[l];
			for (int i=0; i<l; i++) result[i] = bytes[i];
		} else
			result = bytes;
		return result;
	}
	
	
	public void setSocketTimeout(int timeOutMillis) {
		try {
			socket.setSoTimeout(timeOutMillis);
		} catch (Exception e) {}
	}

	
	public byte[] createInbound(byte[] bytes) throws Exception {
		byte[] returnBytes = null;
		MessageExchange me = null;
		DeliveryChannel channel = endpoint.getDeliveryChannel();
		ComponentContext context = endpoint.getServiceUnit().getComponent().getComponentContext();
		
		// create a message exchange
		logger.debug("createInbound, DefaultMEP :" + endpoint.getConfig().getDefaultMep());
		URI defaultMep = endpoint.getConfig().getDefaultMep();
		if (defaultMep.compareTo(WsdlMepConstants.IN_ONLY) == 0) {
			me = channel.createExchangeFactory().createInOnlyExchange();
		} else if (defaultMep.compareTo(WsdlMepConstants.IN_OUT) == 0) {
			me = channel.createExchangeFactory().createInOutExchange();
		} else if (defaultMep.compareTo(WsdlMepConstants.ROBUST_IN_ONLY) == 0) {
			me = channel.createExchangeFactory().createRobustInOnlyExchange();
		} else 
			throw new Exception("trying to process unknown MEP \""+defaultMep+"\"");

		// populate the exchange and send it into the container
		me.setOperation(endpoint.getOperationQName());
        String endpointKey = "{"+endpoint.getService().getNamespaceURI()+"}"+endpoint.getService().getLocalPart()+":"+endpoint.getEndpoint();
        me.setProperty(SENDER_ENDPOINT_PROPERTY, endpointKey); 
        ExternalInput ext = new ExternalInput(new ByteArrayInputStream(bytes), endpoint.getConfig().getCharset(), "raw", endpoint.getConfig().getRecordType(), 0);
        NormalizedMessage msg = me.createMessage();
        ext.populateMessage(msg, this.endpoint.getMessageQName());
        me.setMessage(msg, "in");
        
        logger.debug("Consumer endpoint service="+endpoint.getService() + "  endpoint="+endpoint.getEndpoint());
        ServiceEndpoint linkedEndpoint = context.getEndpoint(endpoint.getService(), endpoint.getEndpoint());
        logger.debug("Got target endpoint "+linkedEndpoint+"   service="+linkedEndpoint.getServiceName()+"   endpoint="+linkedEndpoint.getEndpointName());
        me.setEndpoint(linkedEndpoint);
        me.setService(endpoint.getService());

        if (isAsyncSend) {
        	// do an asynchronous send, no return bytes
        	channel.send(me);
        } else {
        	// do synchronous send and check for return bytes
	        channel.sendSync(me);
	
			// populate the return message
	        if (ExchangeStatus.ACTIVE.equals(me.getStatus())) {
	        	msg = me.getMessage("out");
System.out.println("\n\n\nTVOLLE - return exchange\n" + 
        com.bostechcorp.cbesb.runtime.ccsl.lib.DumpMessageExchange.dump(me) + "\n\n");
	        	if (msg != null) {
	        		NormalizedMessageHandler nmh = new NormalizedMessageHandler(msg);
	        		if (nmh.getRecordCount() > 0) {
		        		Source src = nmh.getRecordAtIndex(0);
		        		ByteArrayOutputStream os = new ByteArrayOutputStream();
		        		OutputWriter.processOutputStream(src, os, "raw", endpoint.getConfig().getCharset());
		        		returnBytes = os.toByteArray();
	        		}
	        	}
	        }
        }	
		return returnBytes;
	}

	
	public void setExchangeBasedProvider(boolean isEnabled) {
		logger.debug("setting MessageExchange based provider");
		isExchangeBasedProvider = isEnabled;
	}
	
	
	public void setIsAsyncSend(boolean isAsync) {
		isAsyncSend = isAsync;
	}
}
