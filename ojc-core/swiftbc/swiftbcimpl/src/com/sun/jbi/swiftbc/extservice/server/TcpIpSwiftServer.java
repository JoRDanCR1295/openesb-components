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
 * @(#)TcpIpSwiftServer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extservice.server;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.mina.common.TransportType;
import org.apache.mina.registry.Service;
import org.apache.mina.registry.ServiceRegistry;
import org.apache.mina.registry.SimpleServiceRegistry;

import com.sun.jbi.swiftbc.ApplicationException;
import com.sun.jbi.swiftbc.extensions.SwiftAddress;
import com.sun.jbi.swiftbc.extservice.ProtocolInfo;


/**
 * Handles communication over TCPIP transport protocol
 * 
 * @author S. Nageswara Rao
 */
public class TcpIpSwiftServer implements SwiftServer {

    public static final int mDefaultPort = 4040;

    private ServiceRegistry mRegistry;

    private Map<Service, SwiftEventHandler> mServiceHandlerMapping;
    
    private static final String SERVICE_NAME = "name";

    public TcpIpSwiftServer() {
        mRegistry = new SimpleServiceRegistry();
        mServiceHandlerMapping = new HashMap<Service, SwiftEventHandler>();
    }

    /**
     * creates SwiftService
     * @param swiftListener The listener that receives the swift messages from transport protocol
     * @param protocolInfo Map of Information that is used during Swift Service creation
     * @throws ApplicationException
     */
    public void createSwiftService(SwiftListener listener, ProtocolInfo protocolInfo) throws ApplicationException {
	try {	
			String name = protocolInfo.get(SERVICE_NAME);
			int swiftServerPort = Integer.parseInt(protocolInfo.get(SwiftAddress.ATTR_SWIFT_SVR_PORT));
			//String llpTypeStr = protocolInfo.get(LLPConstants.LLP_TYPE);

			Iterator it = mServiceHandlerMapping.keySet().iterator();
			while (it.hasNext()) {
				Service service = (Service) it.next();
				TransportType type = service.getTransportType();
				int existingPort = ((InetSocketAddress) service.getAddress()).getPort();
				if (type.equals(TransportType.SOCKET) && existingPort == swiftServerPort && service.getName().equals(name)) {
					throw new ApplicationException("Already one Swift Listener created for this socket");
					// Is that true having only one listener per socket?
					/*
					 * HL7EventHandler handler = (HL7EventHandler)mServiceHandlerMapping.get(service);
					 * if (handler != null) { handler.addHL7Listener(listener); return; }
					 */
				}
			}

			Service service = new Service(name, TransportType.SOCKET, swiftServerPort);
			//LLPType llpType =  Util.stringToEnumValue(llpTypeStr);
			//create the mina - protocol provider 
			//AbstractLLPProtocolProvider llpProtocolProv =  createLLPProvider(llpType);
			//llpProtocolProv.setProtocolEncoderDecoderProps(protocolInfo);
			//create the mina - protocol handler
			SwiftEventHandler handler = new SwiftEventHandler();
			handler.addSwiftListener(listener);
			//associate the protocol event handler to protocol provider
			//llpProtocolProv.setHandler(handler);
			//mRegistry.bind(service, llpProtocolProv);
			mServiceHandlerMapping.put(service, handler);
		}catch(Exception exc){
		   throw new ApplicationException("An exception occured during creating the service", exc.getCause()); 
		}
    }

    /**
     * destroys SwiftService
     * @param transportProtoolInfo Map of Information that is used during HL7 Service creation
     * @throws ApplicationException
     */
    public void destorySwiftService(ProtocolInfo protocolInfo) throws ApplicationException {

        String name = protocolInfo.get(SERVICE_NAME);
        int swiftServerPort = Integer.parseInt(protocolInfo.get(SwiftAddress.ATTR_SWIFT_SVR_PORT));
		try {
			Iterator services = mRegistry.getServices(swiftServerPort).iterator();
			while (services.hasNext()) {
				Service service = (Service) services.next();
				if (service.getName().equals(name)) {
					mRegistry.unbind(service);
                    SwiftEventHandler eventHandler = mServiceHandlerMapping.get(service);
                    SwiftListener listener = eventHandler.getSwiftListener();
                    listener.stopMsgReceiving();
					mServiceHandlerMapping.remove(service);
				}
			}
		}catch(Exception exc) {
		    throw new ApplicationException("An exception occured during destroying the service", exc.getCause());  
		}
    }

   /**
    * Stops all the HL7 Services
    * @throws ApplicationException 
    */
    public void stopAllServices() throws ApplicationException {
        try { 
	        mRegistry.unbindAll();
            SwiftEventHandler eventHandler = null;
            SwiftListener listener = null;
            for (Service service: mServiceHandlerMapping.keySet()) {
                eventHandler = mServiceHandlerMapping.get(service);
                listener = eventHandler.getSwiftListener();
                listener.stopMsgReceiving();
             }
	        mServiceHandlerMapping = new HashMap<Service, SwiftEventHandler>();
        } catch(Exception exc) {
		    throw new ApplicationException("An exception occured during stopping the services", exc.getCause());  
		}
    }
 }// end of class
