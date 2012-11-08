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
 * $Id: TcpipExtension.java,v 1.1.1.1 2007/04/09 17:49:31 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip.wsdl;

import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;

public class TcpipExtension {

	public static final String NS_URI_TCPIP = "http://cbesb.bostechcorp.com/wsdl/tcpip/1.0";

	public static final String ELEM_CONFIG = "config";
	
	public static final QName Q_ELEM_TCPIP_ADDRESS = new QName(NS_URI_TCPIP,
			ELEM_CONFIG);

	public static final QName Q_ELEM_TCPIP_BINDING = new QName(NS_URI_TCPIP,
			Constants.ELEM_BINDING);
	
	public static final String PROTOCOL_HANDLER="protocolHandler";
	public static final String CONNECTION_MODE="connectionMode";
	
	public static final String HOST="host";
	public static final String PORT="port";
	public static final String LOCAL_BIND_HOST="localBindHost";
	public static final String USE_SSL="useSSL";
	
	public static final String SSL_PROTOCOL="sslProtocol";
	public static final String ALLOW_ANONYMOUS="allowAnonymous";
	public static final String USE_PRIVATE_KEY="usePrivateKey";
	public static final String KEY_STORE_FILE="keyStoreFile";
	public static final String KEY_STORE_PASSWORD="keyStorePassword";
	public static final String AUTHENTICATE_SERVER="authenticateServer";
	public static final String USE_DEFAULT_TRUST_STORE="useDefaultTrustStore";
	public static final String TRUST_STORE_FILE="trustStoreFile";
	public static final String LISTEN_PORT="listenPort";
	public static final String MAX_CLIENTS="maxClients";
	public static final String AUTHENTICATE_CLIENT="authenticateClient";
	public static final String RECORD_TYPE="recordType";
	public static final String CHARSET="charset";

	
	public static void register(ExtensionRegistry registry) {
		
		registry.registerDeserializer(javax.wsdl.Port.class,
				Q_ELEM_TCPIP_ADDRESS, new TcpipConfigDeserializer());
		registry.mapExtensionTypes(javax.wsdl.Port.class, Q_ELEM_TCPIP_ADDRESS,
				TcpipConfig.class);
		registry.registerDeserializer(javax.wsdl.Binding.class,
				Q_ELEM_TCPIP_BINDING, new TcpipBindingDeserializer());
		registry.mapExtensionTypes(javax.wsdl.Binding.class,
				Q_ELEM_TCPIP_BINDING, TcpipBinding.class);
	}
}