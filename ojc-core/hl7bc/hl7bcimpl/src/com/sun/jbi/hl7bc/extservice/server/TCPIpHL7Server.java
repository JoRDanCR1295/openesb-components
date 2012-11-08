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
 * @(#)TCPIpHL7Server.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.jbi.management.MBeanNames;
import javax.naming.InitialContext;

import org.apache.mina.common.DefaultIoFilterChainBuilder;
import org.apache.mina.common.IoAcceptor;
import org.apache.mina.common.IoAcceptorConfig;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolCodecFilter;
import org.apache.mina.transport.socket.nio.SocketAcceptor;
import org.apache.mina.transport.socket.nio.SocketAcceptorConfig;

import com.sun.jbi.hl7bc.ApplicationException;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.HL7BCPerformanceMeasurement;
import com.sun.jbi.hl7bc.HL7BindingComponent;
import com.sun.jbi.hl7bc.HL7RuntimeException;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.extservice.llp.AbstractLLPProtocolProvider;
import com.sun.jbi.hl7bc.extservice.llp.LLPConstants;
import com.sun.jbi.hl7bc.extservice.persist.MLLPV2PersistenceHandler;
import com.sun.jbi.hl7bc.extservice.persist.MLLPV1PersistenceHandler;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObjectFactory;
import com.sun.jbi.hl7bc.util.AlertsUtil;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.hl7bc.HL7Constants;

import com.sun.jbi.hl7bc.I18n;

import static com.sun.jbi.hl7bc.extservice.llp.LLPProviderFactory.*;

/**
 * Handles communication over TCP/IP transport protocol
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class TCPIpHL7Server implements HL7Server {

    private static final Logger mLog = Logger.getLogger(TCPIpHL7Server.class.getName());

    public static final int mDefaultPort = 4040;

    private Map<InetSocketAddress, HL7EventHandler> mServiceHandlerMapping;

    private IoAcceptor mIoAcceptor;
    
    private LinkedBlockingQueue<IoSession> mActiveSessions;

    // The runtime configuration bean
    private RuntimeConfiguration mRuntimeConfig;

    private DBObjectFactory mDBObjectFactory = null;

    private ComponentContext mComponentContext = null;

    private DBConnectionFactory mDBConnectionFactory = null;

    private Endpoint mEndpoint = null;

    private String mMBName;

    private ConcurrentHashMap<String, String> mExtSysConnStatus = null;

    private ConcurrentHashMap<String, String> mLastMessageReceivedMap = null;

    private ConcurrentHashMap<String, String> mLastACKMessageSentMap = null;

    private ConnMonitorMBean mConnMonitorMBean;

    private ConnMonitorMBeanHelper mConnMonitorMBeanHelper;

    public TCPIpHL7Server() {
        mServiceHandlerMapping = new HashMap<InetSocketAddress, HL7EventHandler>();
        mActiveSessions = new LinkedBlockingQueue<IoSession>();
        mIoAcceptor = new SocketAcceptor();
    }

    /**
     * creates HL7Service
     * 
     * @param hl7Listener The listener that receives the hl7 messages from transport protocol
     * @param protocolInfo Map of Information that is used during HL7 Service creation
     * @throws ApplicationException
     */
    public void createHL7Service(HL7Listener listener, ProtocolInfo protocolInfo) throws ApplicationException {
        InetSocketAddress socketAddr = null;
        int hl7ServerPort = -1;
        try {
			hl7ServerPort = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
            String llpTypeStr = protocolInfo.get(LLPConstants.LLP_TYPE);
            IoAcceptorConfig config = new SocketAcceptorConfig();
            socketAddr = new InetSocketAddress(hl7ServerPort);
            DefaultIoFilterChainBuilder chain = config.getFilterChain();
            LLPType llpType = Util.stringToEnumValue(llpTypeStr);
            // create the mina - protocol provider
            AbstractLLPProtocolProvider llpProtocolProv = createLLPProvider(llpType);
            llpProtocolProv.setProtocolEncoderDecoderProps(protocolInfo);
            mExtSysConnStatus = new ConcurrentHashMap<String, String>();
            mLastMessageReceivedMap = new ConcurrentHashMap<String, String>();
            mLastACKMessageSentMap = new ConcurrentHashMap<String, String>();
            // create the mina - protocol handler
            HL7EventHandler handler = createHL7EventHandler(listener, protocolInfo, llpType);
            // Add the protocolcodec factory
            chain.addLast("protocolFilter", new ProtocolCodecFilter(llpProtocolProv.getCodecFactory()));
            // Bind
            mIoAcceptor.bind(socketAddr, handler, config);
            initMBeans(socketAddr.toString());
            mServiceHandlerMapping.put(socketAddr, handler);
        } catch (IOException ioExc) {
            throw new ApplicationException(I18n.msg("E0278: Port {0} is already in use by some other application", hl7ServerPort),
                    ioExc.getCause());
        } catch (Exception exc) {
            if (socketAddr != null) {
                mIoAcceptor.unbind(socketAddr);
            }
            throw new ApplicationException(I18n.msg("E0279: An exception occured during creating the service"),
                    exc.getCause());
        }
    }

	private HL7EventHandler createHL7EventHandler(HL7Listener listener, ProtocolInfo protocolInfo, LLPType llpType) throws HL7RuntimeException {
		HL7EventHandler handler = null;
		if (llpType.equals(LLPType.MLLPv2)) {
		    int mllpv2RetriesCount = Integer.parseInt(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRIES_COUNT_ON_NAK));
		    long mllpv2RetryInterval = Long.parseLong(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRY_INTERVAL));
		    MLLPV2PersistenceHandler persistenceHandler = new MLLPV2PersistenceHandler(
		            this.getDBConnectionFactory(), this.getDBObjectFactory());
		    persistenceHandler.setApplicationId(this.mEndpoint.getEndpointName());
		    handler = new HL7MLLPV2EventHandler(mllpv2RetriesCount, mllpv2RetryInterval, persistenceHandler);
		} else {
		    boolean persistenceEnabled = Boolean.parseBoolean(protocolInfo.get(HL7Constants.PERSISTENCE_ENABLED));
		    if(persistenceEnabled){
		        MLLPV1PersistenceHandler persistenceHandler = new MLLPV1PersistenceHandler(
		                this.getDBConnectionFactory(), this.getDBObjectFactory());
		        persistenceHandler.setApplicationId(this.mEndpoint.getEndpointName());
		        handler = new HL7EventHandler(persistenceHandler);
		        
		    }else{
		        handler = new HL7EventHandler();
		    }
		}
		handler.addHL7Listener(listener);
		handler.addConnectionObserver(this);
		return handler;
	}

    /**
     * creates HL7Service
     * 
     * @param listeners Map of listener that receives the hl7 messages from transport protocol
     * @param protocolInfo Map of Information that is used during HL7 Service creation
     * @throws ApplicationException
     */
    public void createHL7Service(Map listeners, ProtocolInfo protocolInfo) throws ApplicationException {
        InetSocketAddress socketAddr = null;
        int hl7ServerPort = -1;
        try {
			hl7ServerPort = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
            String llpTypeStr = protocolInfo.get(LLPConstants.LLP_TYPE);
            IoAcceptorConfig config = new SocketAcceptorConfig();
            socketAddr = new InetSocketAddress(hl7ServerPort);
            DefaultIoFilterChainBuilder chain = config.getFilterChain();
            LLPType llpType = Util.stringToEnumValue(llpTypeStr);
            // create the mina - protocol provider
            AbstractLLPProtocolProvider llpProtocolProv = createLLPProvider(llpType);
            llpProtocolProv.setProtocolEncoderDecoderProps(protocolInfo);
            mExtSysConnStatus = new ConcurrentHashMap<String, String>();
            mLastMessageReceivedMap = new ConcurrentHashMap<String, String>();
            mLastACKMessageSentMap = new ConcurrentHashMap<String, String>();
            // create the mina - protocol handler
            HL7EventHandler handler = null;
            if (llpType.equals(LLPType.MLLPv2)) {
                int mllpv2RetriesCount = Integer.parseInt(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRIES_COUNT_ON_NAK));
                long mllpv2RetryInterval = Long.parseLong(protocolInfo.get(HL7ProtocolProperties.MLLPV2_RETRY_INTERVAL));
                MLLPV2PersistenceHandler persistenceHandler = new MLLPV2PersistenceHandler(
                        this.getDBConnectionFactory(), this.getDBObjectFactory());
                persistenceHandler.setApplicationId(this.mEndpoint.getEndpointName());
                handler = new HL7MLLPV2EventHandler(mllpv2RetriesCount, mllpv2RetryInterval, persistenceHandler);
            } else {
                boolean persistenceEnabled = Boolean.parseBoolean(protocolInfo.get(HL7Constants.PERSISTENCE_ENABLED));
                if(persistenceEnabled){
                    MLLPV1PersistenceHandler persistenceHandler = new MLLPV1PersistenceHandler(
                            this.getDBConnectionFactory(), this.getDBObjectFactory());
                    persistenceHandler.setApplicationId(this.mEndpoint.getEndpointName());
                    handler = new HL7EventHandler(persistenceHandler);
                    
                }else{
					handler = new HL7EventHandler();
                }
            }
            handler.addHL7Listeners(listeners);
            handler.addConnectionObserver(this);
            // Add the protocolcodec factory
            chain.addLast("protocolFilter", new ProtocolCodecFilter(llpProtocolProv.getCodecFactory()));
            // Bind
            mIoAcceptor.bind(socketAddr, handler, config);
            initMBeans(socketAddr.toString());
            mServiceHandlerMapping.put(socketAddr, handler);
        } catch (IOException ioExc) {
            throw new ApplicationException(I18n.msg("E0278: Port {0} is already in use by some other application", hl7ServerPort),
                    ioExc.getCause());
        } catch (Exception exc) {
            if (socketAddr != null) {
                mIoAcceptor.unbind(socketAddr);
            }
            throw new ApplicationException(I18n.msg("E0279: An exception occured during creating the service"),
                    exc);
        }
    }

    /*
     * destroys HL7Service @param transportProtoolInfo Map of Information that is used during HL7
     * Service creation @throws ApplicationException
     */
    public void destoryHL7Service(ProtocolInfo protocolInfo) throws ApplicationException {

        int hl7ServerPort = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
        try {
            InetSocketAddress socketAddr = new InetSocketAddress(hl7ServerPort);
            mIoAcceptor.unbind(socketAddr);
            HL7EventHandler eventHandler = mServiceHandlerMapping.get(socketAddr);
            for (HL7Listener listener : eventHandler.getHL7Listeners().values()) {
                listener.stopMsgReceiving();
            }
            shutdownMBeans(socketAddr.toString());
            mServiceHandlerMapping.remove(socketAddr);
        } catch (Exception exc) {
            throw new ApplicationException(I18n.msg("E0280: An exception occured during destroying the service"),
                    exc.getCause());
        }
    }

    /**
     * Suspends the service from message processing
     * 
     * @param transportProtocolInfo
     * @throws ApplicationException
     */
    public void suspendHL7Service(ProtocolInfo protocolInfo) throws ApplicationException {
        int hl7ServerPort = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
        try {
            InetSocketAddress socketAddr = new InetSocketAddress(hl7ServerPort);
            HL7EventHandler eventHandler = mServiceHandlerMapping.get(socketAddr);
            for (HL7Listener listener : eventHandler.getHL7Listeners().values()) {
                listener.suspendMsgProcessing();
            }

        } catch (Exception exc) {
            throw new ApplicationException(
                    I18n.msg("E0281: An exception occured during suspending the service from message processing"),
                    exc.getCause());
        }
    }

    /**
     * Resume the service to process the messages
     * 
     * @param transportProtocolInfo
     * @throws ApplicationException
     */
    public void resumeHL7Service(ProtocolInfo protocolInfo) throws ApplicationException {
        int hl7ServerPort = Integer.parseInt(protocolInfo.get(HL7Address.ATTR_HL7_SVR_PORT));
        try {
            InetSocketAddress socketAddr = new InetSocketAddress(hl7ServerPort);
            HL7EventHandler eventHandler = mServiceHandlerMapping.get(socketAddr);
            for (HL7Listener listener : eventHandler.getHL7Listeners().values()) {
                listener.resumeMsgProcessing();
            }
        } catch (Exception exc) {
            throw new ApplicationException(
                    I18n.msg("E0330: An exception occured during resuming the service to do message processing"),
                    exc.getCause());
        }
    }

    /**
     * Stops all the HL7 Services
     * 
     * @throws ApplicationException
     */
    public void stopAllServices() throws ApplicationException {
        try {
            mIoAcceptor.unbindAll();
            HL7EventHandler eventHandler = null;
            for (InetSocketAddress socketAdder : mServiceHandlerMapping.keySet()) {
                shutdownMBeans(socketAdder.toString());
                eventHandler = mServiceHandlerMapping.get(socketAdder);
                for (HL7Listener listener : eventHandler.getHL7Listeners().values()) {
                    listener.stopMsgReceiving();
                }
            }
            mServiceHandlerMapping = new HashMap<InetSocketAddress, HL7EventHandler>();
        } catch (Exception exc) {
            throw new ApplicationException(I18n.msg("E0282: An exception occured during stopping the services"),
                    exc.getCause());
        }
    }

    public void setRuntimeConfiguration(RuntimeConfiguration runtimeConfig) {
        this.mRuntimeConfig = runtimeConfig;
    }

    public void setComponentContext(ComponentContext ctx) {
        this.mComponentContext = ctx;
    }

    public void setEndpoint(Endpoint endpoint) {
        this.mEndpoint = endpoint;
    }

    /**
     * Set the MBeanName
     * 
     * @param mbname
     */
    public void setMonitorExtSysConnMBeanName(String mbname) {
        mMBName = mbname;
    }

    /**
     * Initializes the MBeans associated with this HL7Server. A Connection Monitor MBean is created
     * for use with the HL7Server:
     * <ul>
     * <li>ConnMonitorMBean - Provides status of the client connection</li>
     * </ul>
     * 
     * @exception Exception if any error occurs
     */
    private void initMBeans(String serverAddress) throws Exception {

        assert (mComponentContext != null);

        MBeanServer mbServer = mComponentContext.getMBeanServer();
        MBeanNames mbNames = mComponentContext.getMBeanNames();

        try {
            ObjectName monitorMBeanObjName = mbNames.createCustomComponentMBeanName(mMBName);
            mConnMonitorMBean = new ConnMonitor(serverAddress);
            mConnMonitorMBeanHelper = new ConnMonitorMBeanHelper(monitorMBeanObjName, mbServer);
            mConnMonitorMBeanHelper.registerMBean(mConnMonitorMBean);
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, I18n.msg("I0173: Registered Connection Monitor MBean for {0}", serverAddress));
            }
        } catch (Exception ex) {
            String msg = I18n.msg("E0325: Failed to register Connection Monitor MBean. An exception was raised :{0}",
                    ex.getLocalizedMessage());
            mLog.log(Level.SEVERE, msg, ex);
            AlertsUtil.getAlerter().critical(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0325");
            throw new Exception(msg, ex);
        }
    }

    /**
     * Shuts down the MBeans associated with this Binding Component.
     * 
     * @exception JBIException if any error occurs
     */
    private void shutdownMBeans(String serverAddress) throws Exception {
        try {
            mConnMonitorMBeanHelper.unregisterMBean();
        } catch (Exception ex) {
            String msg = I18n.msg(
                    "W0130: Failed to un-register Connection Monitor MBean for {0}. An exception was raised: {1}",
                    serverAddress, ex.getLocalizedMessage());
            mLog.log(Level.WARNING, msg, ex);
            AlertsUtil.getAlerter().warning(msg, HL7BindingComponent.SHORT_DISPLAY_NAME, null,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0130");
            throw new Exception(msg, ex);
        }
    }

    /**
     * notifies about new connection creation
     * 
     * @param connID
     */
    public void notifyNewConnetion(IoSession session) {
    	mActiveSessions.add(session);
    	String connID = session.getRemoteAddress().toString();
        mExtSysConnStatus.put(connID, UP);
        mConnMonitorMBean.setExternalSystemConnStatus(mExtSysConnStatus);
    }

    /**
     * notifies about a lost connection
     * 
     * @param connID
     */
    public void notifyLostConnection(IoSession session) {
    	mActiveSessions.remove(session);
    	String connID = session.getRemoteAddress().toString();
    	//mExtSysConnStatus.remove(connID);
        mExtSysConnStatus.replace(connID, UP, DOWN);
        //mLastMessageReceivedMap.remove(connID);
       // mLastACKMessageSentMap.remove(connID);
        mConnMonitorMBean.setExternalSystemConnStatus(mExtSysConnStatus);
    }

    /**
     * notifies about a Last message received timeStamp
     * 
     * @param connID
     */
    public void notifyLastMessageReceivedTimeStamp(String connID){
        long receivedMsgTimeStamp = System.currentTimeMillis(); 
        mLastMessageReceivedMap.put(connID, Long.toString(receivedMsgTimeStamp));
        mConnMonitorMBean.setMessageReceivedTimeStamp(mLastMessageReceivedMap);    
    }

    /**
     * notifies about a ACk message sent timeStamp
     * 
     * @param connID
     */
    public void notifyLastACKMessageSentTimeStamp(String connID){       
        long ACKMsgSentTimeStamp = System.currentTimeMillis(); 
        mLastACKMessageSentMap.put(connID, Long.toString(ACKMsgSentTimeStamp));
        mConnMonitorMBean.setACKMessageSentTimeStamp(mLastACKMessageSentMap);    
    }

    private DBConnectionFactory getDBConnectionFactory() throws HL7RuntimeException {
        if (this.mDBConnectionFactory == null) {
            // get the Initial Context
            InitialContext ic = mComponentContext.getNamingContext();
            // get the properties
            Properties props = mRuntimeConfig.getProperties();
            mDBConnectionFactory = new DBConnectionFactory(props, ic, mComponentContext.getInstallRoot());
            mDBObjectFactory = DBObjectFactory.getDBObjectFactory(mDBConnectionFactory.getType());

        }
        return this.mDBConnectionFactory;
    }

    private DBObjectFactory getDBObjectFactory() throws HL7RuntimeException {
        if (this.mDBObjectFactory == null) {
            mDBObjectFactory = DBObjectFactory.getDBObjectFactory(getDBConnectionFactory().getType());
        }
        return this.mDBObjectFactory;
    }

}// end of class
