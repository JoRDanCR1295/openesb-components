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
 * $Id: TcpipConfigDeserializer.java,v 1.1.1.1 2007/04/09 17:49:31 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip.wsdl;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.bostechcorp.cbesb.runtime.component.util.wsdl.BaseCommonAttributeDeserializer;
import com.ibm.wsdl.util.xml.DOMUtils;

public class TcpipConfigDeserializer extends BaseCommonAttributeDeserializer implements ExtensionDeserializer {

	public ExtensibilityElement unmarshall(Class parentType, QName elementType,
			Element el, Definition def, ExtensionRegistry extReg)
			throws WSDLException {
				
		TcpipConfig tcpipConfig = (TcpipConfig) super.unmarshall(parentType, elementType, el,
                def, extReg);
 
		tcpipConfig.setProtocolHandler(DOMUtils.getAttribute(el, TcpipExtension.PROTOCOL_HANDLER));

		String strConnectionMode = DOMUtils.getAttribute(el, TcpipExtension.CONNECTION_MODE);
		short connectionMode = -1;
		if (strConnectionMode != null) {
			if (strConnectionMode.equalsIgnoreCase("client")) connectionMode = TcpipConfig.CONNECTION_MODE_CLIENT;
			else if (strConnectionMode.equalsIgnoreCase("server")) connectionMode = TcpipConfig.CONNECTION_MODE_SERVER;
		}
		tcpipConfig.setConnectionMode(connectionMode);
				
		tcpipConfig.setHost(DOMUtils.getAttribute(el, TcpipExtension.HOST));
		
		String portStr = DOMUtils.getAttribute(el, TcpipExtension.PORT);
		if (portStr != null)
			tcpipConfig.setPort(Integer.parseInt(portStr));
		
		tcpipConfig.setLocalBindHost(DOMUtils.getAttribute(el, TcpipExtension.LOCAL_BIND_HOST));
		tcpipConfig.setUseSsl(Boolean.parseBoolean(DOMUtils.getAttribute(el, TcpipExtension.USE_SSL)));
		tcpipConfig.setSslProtocol(DOMUtils.getAttribute(el, TcpipExtension.SSL_PROTOCOL));
		tcpipConfig.setAllowAnonymous(Boolean.parseBoolean(DOMUtils.getAttribute(el, TcpipExtension.ALLOW_ANONYMOUS)));
		tcpipConfig.setUsePrivateKey(Boolean.parseBoolean(DOMUtils.getAttribute(el, TcpipExtension.USE_PRIVATE_KEY)));
		tcpipConfig.setKeyStoreFile(DOMUtils.getAttribute(el, TcpipExtension.KEY_STORE_FILE));
		tcpipConfig.setKeyStorePassword(DOMUtils.getAttribute(el, TcpipExtension.KEY_STORE_PASSWORD));
		tcpipConfig.setAuthenticateServer(Boolean.parseBoolean(DOMUtils.getAttribute(el, TcpipExtension.AUTHENTICATE_SERVER)));
		tcpipConfig.setUseDefaultTrustStore(Boolean.parseBoolean(DOMUtils.getAttribute(el, TcpipExtension.USE_DEFAULT_TRUST_STORE)));
		tcpipConfig.setTrustStoreFile(DOMUtils.getAttribute(el, TcpipExtension.TRUST_STORE_FILE));

		String listenPortStr = DOMUtils.getAttribute(el, TcpipExtension.LISTEN_PORT);
		if (listenPortStr != null)
			tcpipConfig.setListenPort(Integer.parseInt(listenPortStr));

		String maxClientStr = DOMUtils.getAttribute(el, TcpipExtension.MAX_CLIENTS);
		if (maxClientStr != null)
			tcpipConfig.setMaxClients(Integer.parseInt(maxClientStr));

		tcpipConfig.setAuthenticateClient(Boolean.parseBoolean(DOMUtils.getAttribute(el, TcpipExtension.AUTHENTICATE_CLIENT)));
		tcpipConfig.setRecordType(DOMUtils.getAttribute(el, TcpipExtension.RECORD_TYPE));
		tcpipConfig.setCharset(DOMUtils.getAttribute(el, TcpipExtension.CHARSET));
		return tcpipConfig;
	}
}
