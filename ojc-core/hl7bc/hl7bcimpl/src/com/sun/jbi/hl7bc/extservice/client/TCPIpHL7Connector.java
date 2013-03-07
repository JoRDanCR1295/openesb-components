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
 * @(#)TCPIpHL7Connector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.client;

import java.io.ByteArrayOutputStream;
import java.net.InetSocketAddress;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.TimeUnit;

import org.apache.mina.common.CloseFuture;
import org.apache.mina.common.ConnectFuture;
import org.apache.mina.common.DefaultIoFilterChainBuilder;
import org.apache.mina.common.IoConnector;
import org.apache.mina.common.IoConnectorConfig;
import org.apache.mina.common.IoSession;
import org.apache.mina.transport.socket.nio.SocketConnector;
import org.apache.mina.transport.socket.nio.SocketConnectorConfig;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.filter.codec.ProtocolCodecFilter;

import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.connection.Connection;
import com.sun.jbi.hl7bc.connection.ConnectionInfo;
import com.sun.jbi.hl7bc.connection.HL7BCConnectionManager;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7CommunicationControl;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.extservice.llp.AbstractLLPProtocolProvider;
import com.sun.jbi.hl7bc.extservice.llp.LLPConstants;
import com.sun.jbi.hl7bc.I18n;

import java.io.IOException;
import static com.sun.jbi.hl7bc.extservice.llp.LLPProviderFactory.*;

/**
 * This class creates and manages connections to the HL7 External System.
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class TCPIpHL7Connector extends IoHandlerAdapter implements HL7Connector {

    private static final Logger log = Logger.getLogger(TCPIpHL7Connector.class.getName());

    protected IoSession mSession;

    private String hl7ResponseMsg = null;

    final Lock lock = new ReentrantLock();

    final Condition ackNotRecvd = lock.newCondition();

    private Connection mHl7Connection = null;
	private ProtocolInfo mProtocolInfo = null;
    boolean useConnectionPool = false;;
    public TCPIpHL7Connector() {
    }

    public void connect(ProtocolInfo protocolInfo) throws Exception {
        String hostName = protocolInfo.get(HL7Address.ATTR_HL7_SVR_LOCATION);
        int port = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
        String llpTypeStr = protocolInfo.get(LLPConstants.LLP_TYPE);
        LLPType llpType = Util.stringToEnumValue(llpTypeStr);
        // create the mina - protocol provider
        AbstractLLPProtocolProvider llpProtocolProv = createLLPProvider(llpType);
        llpProtocolProv.setProtocolEncoderDecoderProps(protocolInfo);
        IoConnectorConfig config = new SocketConnectorConfig();
        DefaultIoFilterChainBuilder chain = config.getFilterChain();
        // Add the protocolcodec factory
        chain.addLast("protocolFilter", new ProtocolCodecFilter(llpProtocolProv.getCodecFactory()));
        ConnectionHelper connHelper = new ConnectionHelper();
        ConnectFuture future = connHelper.getConnection(hostName,port,this,config,protocolInfo.get(HL7Constants.MAX_CONNECTION_RETRIES));
        future.join(); // Wait until the connection attempt is finished.
        mSession = future.getSession();
        mSession.setAttribute("currentInput", new ByteArrayOutputStream());
        mSession.setAttribute("readBytes", new Long(0));
    }

    public void connect(ProtocolInfo protocolInfo, Endpoint endpoint) throws Exception {
        String hostName = protocolInfo.get(HL7Address.ATTR_HL7_SVR_LOCATION);
        int port = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
        String llpTypeStr = protocolInfo.get(LLPConstants.LLP_TYPE);
        LLPType llpType = Util.stringToEnumValue(llpTypeStr);
        // create the mina - protocol provider
        AbstractLLPProtocolProvider llpProtocolProv = createLLPProvider(llpType);
        llpProtocolProv.setProtocolEncoderDecoderProps(protocolInfo);
        IoConnectorConfig config = new SocketConnectorConfig();
        DefaultIoFilterChainBuilder chain = config.getFilterChain();
        // Add the protocolcodec factory
        chain.addLast("protocolFilter", new ProtocolCodecFilter(llpProtocolProv.getCodecFactory()));
        ConnectionInfo connInfo = new ConnectionInfo();
        String endpointName = endpoint.getEndpointName() + endpoint.getUniqueName();
		this.useConnectionPool =  true;
        connInfo.setHost(hostName);
        connInfo.setPort(port);
        connInfo.setEndpointName(endpointName);
        connInfo.setIoHandler(this);
        connInfo.setIoServiceConfig(config);
        connInfo.setRetryLogicString(protocolInfo.get(HL7Constants.MAX_CONNECTION_RETRIES));
        connInfo.setMinPoolSize(Integer.parseInt(protocolInfo.get(HL7Constants.MIN_POOL_SIZE)));
        connInfo.setMaxPoolSize(Integer.parseInt(protocolInfo.get(HL7Constants.MAX_POOL_SIZE)));
        connInfo.setMaxIdleTimeout(Long.parseLong(protocolInfo.get(HL7Constants.MAX_IDLE_TIMEOUT)));
        ConnectFuture future = null;
        if(useConnectionPool){
        	mHl7Connection = HL7BCConnectionManager.getConnection(connInfo);
            this.setHL7Connection(mHl7Connection);
            mHl7Connection.setIoHandler(this);
        	future = (ConnectFuture)mHl7Connection.getClientObject();
            mSession = (IoSession)mHl7Connection.getIOSessionObject();
        }
    }
    public void sendHL7Message(String hl7Msg) throws Exception {
        mSession.write(hl7Msg);
    }
    public Connection getHL7Connection() throws Exception {
       return  this.mHl7Connection;
    }
    
    public void setHL7Connection(Connection conn) throws Exception {
        this.mHl7Connection = conn;
     }
    public ProtocolInfo getProtocolInfo() throws Exception {
       return  this.mProtocolInfo;
    }
    
    public void setProtocolInfo(ProtocolInfo pInfo) throws Exception {
        this.mProtocolInfo = pInfo;
     }
    
    public void setIoSession(Connection conn) throws Exception{
        this.mHl7Connection = conn;
        ConnectFuture future = null;
        ConnectionInfo connInfo = conn.getKey();
        future = (ConnectFuture)mHl7Connection.getClientObject();
        if(mHl7Connection.getIOSessionObject().isConnected()){
            mSession = (IoSession)mHl7Connection.getIOSessionObject();  
        }else{
            // if the connection taken from the pool is closed, creating new connection
           /* ConnectionHelper connHelper = new ConnectionHelper();
            future = connHelper.getConnection(connInfo.getHost(),connInfo.getPort(),mHl7Connection.getIoHandler(),connInfo.getIoServiceConfig(),connInfo.getRetryLogicString());
            future.join(); // Wait until the connection attempt is finished.
            mSession = future.getSession();
            mSession.setAttribute("currentInput", new ByteArrayOutputStream());
            mSession.setAttribute("readBytes", new Long(0));*/
        }
    }

    public String recvHL7Message() throws Exception {
        lock.lock();
		boolean timeOut = false;
        try {
            while (getHl7ResponseMsg() == null) {
                timeOut = ackNotRecvd.await(60, TimeUnit.SECONDS);
                if (!mSession.isConnected()) {
                    throw new IOException();
                }
				if (!timeOut) {
                    break;
                }
            }
        } catch (Exception ex) {
            log.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
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
            log.log(Level.SEVERE, I18n.msg("E0269: An exception occured in HL7 Connector"), ex);
            String errMsg = I18n.msg("E0269: An exception occured in HL7 Connector");
            throw new Exception(errMsg, ex);
        } finally {
            lock.unlock();
        }
        String response = getHl7ResponseMsg();
        setHl7ResponseMsg(null); // reset
        return response;
    }

    public void disconnect() throws Exception {
    	if(useConnectionPool){
    		if(this.mHl7Connection != null){
	    		HL7BCConnectionManager.returnConnection(this.mHl7Connection);
	    		this.mHl7Connection = null;
    		}
    	}else{
            if (mSession != null && mSession.isConnected()) {
                CloseFuture future = mSession.close();
            }
    	}
    }

    public void discardConnection() throws Exception {
    	if(useConnectionPool){
	    	if(this.mHl7Connection != null){
	    		this.mHl7Connection.discard();
	    		this.mHl7Connection = null;
	    	}
    	}else{
    		this.disconnect();
    	}
    }
    public void messageReceived(IoSession session, Object message) {
        lock.lock();
        setHl7ResponseMsg((String) message);
        try {
            if (getHl7ResponseMsg() != null) {
                ackNotRecvd.signal();
            }
        } finally {
            lock.unlock();
        }
    }

    public void messageSent(IoSession session, Object message) {
        if (log.isLoggable(Level.FINE)) {
            log.log(Level.FINE, I18n.msg("I0155: Sent HL7 request to HL7 External System :{0}", message.toString() ));
        }
    }

    public String getHl7ResponseMsg() {
        return hl7ResponseMsg;
    }

    public void setHl7ResponseMsg(String hl7ResponseMsg) {
        this.hl7ResponseMsg = hl7ResponseMsg;
    }

	public void exceptionCaught(IoSession session, Throwable cause) throws Exception {
		try{
			 if (log.isLoggable(Level.FINE)) {
				log.log(Level.FINE, I18n.msg("W9155: session exception caught :{0}", cause.getLocalizedMessage()));
			 }
			 // if the external is forcesibly diconnection the connection in middle of the process
			 // then cought this exception and should clean the pool.
			 if (session != null) {				 
                CloseFuture future = session.close();
				if(useConnectionPool){
					log.log(Level.INFO, I18n.msg("W9016: session exception caught :{0} , Hence cleaing up the pool", cause.getLocalizedMessage()));
					HL7BCConnectionManager.closeConnectionIfNotAvailable(this.mHl7Connection.getKey());
				}else{
					future = session.close();
				}
            }
		}catch(Exception e){} 
	
	}
}// end of class
