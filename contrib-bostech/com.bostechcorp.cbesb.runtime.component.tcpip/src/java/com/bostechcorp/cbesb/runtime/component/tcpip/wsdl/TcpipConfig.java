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
 * $Id: TcpipConfig.java,v 1.1.1.1 2007/04/09 17:49:31 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.tcpip.wsdl;

import com.bostechcorp.cbesb.runtime.component.util.wsdl.BaseCommonAttribute;

/**
 * A JMS extensibily element used to specify the parameters needed to retrieve
 * the JMS ConnectionFactory and Destination to use.
 * 
 */
public class TcpipConfig extends BaseCommonAttribute /*implements ExtensibilityElement, Serializable*/ {
	public static short CONNECTION_MODE_SERVER = 0;
	public static short CONNECTION_MODE_CLIENT = 1;
	
	/**
	 * Generated serial version UID
	 */
	private static final long serialVersionUID = 1L;

	public String protocolHandler;
	public short connectionMode;
	
	public String host;
	public int port;
	public String localBindHost;
	public boolean useSsl;
	
	public String sslProtocol;
	public boolean allowAnonymous;
	public boolean usePrivateKey;
	public String keyStoreFile;
	public String keyStorePassword;
	public boolean authenticateServer;
	public boolean useDefaultTrustStore;
	public String trustStoreFile;
	public int listenPort;
	public int maxClients;
	public boolean authenticateClient;
	public String recordType;
	public String charset;


	
	public boolean isAllowAnonymous() {
		return allowAnonymous;
	}



	public void setAllowAnonymous(boolean allowAnonymous) {
		this.allowAnonymous = allowAnonymous;
	}



	public boolean isAuthenticateClient() {
		return authenticateClient;
	}



	public void setAuthenticateClient(boolean authenticateClient) {
		this.authenticateClient = authenticateClient;
	}



	public boolean isAuthenticateServer() {
		return authenticateServer;
	}



	public void setAuthenticateServer(boolean authenticateServer) {
		this.authenticateServer = authenticateServer;
	}



	public String getCharset() {
		return charset;
	}



	public void setCharset(String charset) {
		this.charset = charset;
	}



	public short getConnectionMode() {
		return connectionMode;
	}



	public void setConnectionMode(short connectionMode) {
		this.connectionMode = connectionMode;
	}



	public String getHost() {
		return host;
	}



	public void setHost(String host) {
		this.host = host;
	}



	public String getKeyStoreFile() {
		return keyStoreFile;
	}



	public void setKeyStoreFile(String keyStoreFile) {
		this.keyStoreFile = keyStoreFile;
	}



	public String getKeyStorePassword() {
		return keyStorePassword;
	}



	public void setKeyStorePassword(String keyStorePassword) {
		this.keyStorePassword = keyStorePassword;
	}



	public int getListenPort() {
		return listenPort;
	}



	public void setListenPort(int listenPort) {
		this.listenPort = listenPort;
	}



	public String getLocalBindHost() {
		return localBindHost;
	}



	public void setLocalBindHost(String localBindHost) {
		this.localBindHost = localBindHost;
	}



	public int getMaxClients() {
		return maxClients;
	}



	public void setMaxClients(int maxClients) {
		this.maxClients = maxClients;
	}



	public int getPort() {
		return port;
	}



	public void setPort(int port) {
		this.port = port;
	}



	public String getProtocolHandler() {
		return protocolHandler;
	}



	public void setProtocolHandler(String protocolHandler) {
		this.protocolHandler = protocolHandler;
	}



	public String getRecordType() {
		return recordType;
	}



	public void setRecordType(String recordType) {
		this.recordType = recordType;
	}



	public String getSslProtocol() {
		return sslProtocol;
	}



	public void setSslProtocol(String sslProtocol) {
		this.sslProtocol = sslProtocol;
	}



	public String getTrustStoreFile() {
		return trustStoreFile;
	}



	public void setTrustStoreFile(String trustStoreFile) {
		this.trustStoreFile = trustStoreFile;
	}



	public boolean isUseDefaultTrustStore() {
		return useDefaultTrustStore;
	}



	public void setUseDefaultTrustStore(boolean useDefaultTrustStore) {
		this.useDefaultTrustStore = useDefaultTrustStore;
	}



	public boolean isUsePrivateKey() {
		return usePrivateKey;
	}



	public void setUsePrivateKey(boolean usePrivateKey) {
		this.usePrivateKey = usePrivateKey;
	}



	public boolean isUseSsl() {
		return useSsl;
	}



	public void setUseSsl(boolean useSsl) {
		this.useSsl = useSsl;
	}



	public String toString() {
		return "TcpipConfig[" 
		  + super.toString()  + ", " 
		  + "protocolHandler=" + protocolHandler + ", " 
		  + "connectionMode=" + connectionMode + ", " 
		  + "host=" + host + ", "
		  + "port=" + port + ", " 
		  + "localBindHost=" + localBindHost + ", " 
		  + "listenPort=" + listenPort + ", "
		  + "maxClients=" + maxClients + ", "
		  + "useSsl=" + useSsl + ", "
		  + "sslProtocol=" + sslProtocol + ", " 
		  + "allowAnonymous=" + allowAnonymous + ","
		  + "usePrivateKey=" + usePrivateKey + ", "
		  + "keyStoreFile=" + keyStoreFile + ", "
		  + "keyStorePassword=" + keyStorePassword + ", "
		  + "authenticateServer=" + authenticateServer + ", "
		  + "authenticateClient=" + authenticateClient + ", "
		  + "useDefaultTrustStore=" + useDefaultTrustStore + ", "
		  + "trustStoreFile=" + trustStoreFile + ", "
		  + "recordType=" + recordType + ", "
		  + "charset=" + charset + "]";
	}
}