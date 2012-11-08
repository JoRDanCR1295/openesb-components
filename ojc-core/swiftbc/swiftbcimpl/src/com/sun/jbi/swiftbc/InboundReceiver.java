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
 * @(#)InboundReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.xml.namespace.QName;

import com.sun.jbi.swiftbc.extensions.SwiftAddress;
import com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties;
import com.sun.jbi.swiftbc.extservice.server.SwiftServer;
import com.sun.jbi.swiftbc.extservice.ProtocolInfo;
import com.sun.jbi.internationalization.Messages;
import static com.sun.jbi.swiftbc.extservice.server.SwiftServerFactory.*;

/**
 * This class handles and starts the inbound message processors
 * 
 * @author Sriram Vedula, S. Nageswara Rao
 * @version
 */
public class InboundReceiver extends SAGConstants
{

	private static final Logger mLog = Messages
			.getLogger(InboundReceiver.class);

	private ComponentContext mContext;

	private DeliveryChannel mChannel;

	// private MessageStore mMessageStore;
	private Map<String, SwiftServer> mSwiftServers;

	private RuntimeConfiguration mRuntimeConfig;

	/**
	 * Constructor.
	 * 
	 * @param context
	 *            the component context associated with the Swift binding
	 *            component.
	 * @param dc
	 *            the delivery channel
	 * @param runtimeconfig
	 *            the runtime configuration bean
	 */
	public InboundReceiver(ComponentContext context, DeliveryChannel channel,
			RuntimeConfiguration runtimeconfig)
	{
		mContext = context;
		mChannel = channel;
		mRuntimeConfig = runtimeconfig;
		mSwiftServers = new HashMap<String, SwiftServer>();
	}

	public void stopReceiving()
	{
		try
		{
			SwiftServer swiftServer = null;
			for (String key : mSwiftServers.keySet())
			{
				swiftServer = mSwiftServers.get(key);
				swiftServer.stopAllServices();
			}
			mSwiftServers.clear();
		} catch (Exception ex)
		{
			mLog.log(Level.SEVERE,
					"an exception occured during stopping the servers", ex
							.getCause());
		}
	}

	/**
	 * Starts a new inbound message processor for each Swift address specified
	 * in the wsdl to listen to Swift messages
	 * 
	 * @param endpoint
	 *            A service end point.
	 */
	public void addInboundMessageProcessor(Endpoint endpoint) throws Exception
	{
		if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND)
		{
			String name = endpoint.getServiceName().toString()
					+ endpoint.getEndpointName();
			// Just select the first operation
			QName opname = lookupOperation(endpoint);
			String key = endpoint.getServiceName() + endpoint.getEndpointName()
					+ opname.toString();
			InboundMessageDelegator inMsgDelegator = new InboundMessageDelegator();
			inMsgDelegator.setComponentContext(mContext);
			inMsgDelegator.setDeliveryChannel(mChannel);
			inMsgDelegator.setEndPoint(endpoint);
			inMsgDelegator.setOperationName(opname);
			inMsgDelegator.setRuntimeConfiguration(mRuntimeConfig);
			inMsgDelegator.initialize();
			String transportProtocolName = endpoint.getSwiftAddress()
					.getTransportProtocolName();
			SwiftProtocolProperties swiftProtocolProperties = endpoint
					.getSwiftProtocolProperties();
			SwiftServer swiftServer = createSwiftServer(TransportProtocolType.TCPIP);
			ProtocolInfo protocolInfo = new ProtocolInfo();
			String svrPort = endpoint.getSwiftAddress().getSwiftServerPort()
					.toString();
			// Server port
			protocolInfo.put(SwiftAddress.ATTR_SWIFT_SVR_PORT, svrPort);
			protocolInfo.put("SERVICE_NAME", name);
			swiftServer.createSwiftService(inMsgDelegator, protocolInfo);
			mLog.log(Level.INFO, "InboundReceiver_SwiftSERVER_CREATED",
					new Object[]
					{ endpoint.getSwiftAddress().getSwiftServerLocation(),
							svrPort, transportProtocolName });
			mSwiftServers.put(key, swiftServer);
		}
	}

	private QName lookupOperation(Endpoint endpoint)
	{
		// Just select the first operation.
		QName[] operationNames = (QName[]) endpoint.getSwiftOperations()
				.keySet().toArray(new QName[0]);
		return operationNames[0];

	}

	/**
	 * Stops the Swiftserver and removes the inbound message processor
	 * 
	 * @param endpoint
	 *            A service end point.
	 */
	public void removeInboundMessageProcessor(Endpoint endpoint)
			throws Exception
	{
		if (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND)
		{
			String name = endpoint.getServiceName().toString()
					+ endpoint.getEndpointName();
			// Just select the first operation
			QName opname = lookupOperation(endpoint);
			String key = endpoint.getServiceName() + endpoint.getEndpointName()
					+ opname.toString();
			String transportProtocolName = endpoint.getSwiftAddress()
					.getTransportProtocolName();
			// right now only TCPIP transport protocol is supported
			SwiftServer SwiftServer = mSwiftServers.get(key);
			ProtocolInfo protocolInfo = new ProtocolInfo();
			String svrPort = endpoint.getSwiftAddress().getSwiftServerPort()
					.toString();
			// Server port
			protocolInfo.put(SwiftAddress.ATTR_SWIFT_SVR_PORT, svrPort);
			protocolInfo.put("SERVICE_NAME", name);
			SwiftServer.destorySwiftService(protocolInfo);
			mLog.log(Level.INFO, "InboundReceiver_SwiftSERVER_DESTROYED",
					new Object[]
					{ endpoint.getSwiftAddress().getSwiftServerLocation(),
							svrPort, transportProtocolName });
			// remove the server from Map
			mSwiftServers.remove(key);
		}
	}

	/**
	 * Package protected method. Used solely for JUnit test purposes
	 */
	Map getActivateInboundMsgProcs()
	{
		return mSwiftServers;
	}
}
