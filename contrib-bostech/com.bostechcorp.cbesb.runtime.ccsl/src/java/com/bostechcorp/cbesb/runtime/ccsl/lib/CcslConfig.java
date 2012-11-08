/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
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
 * $Id: CcslConfig.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.logging.Log;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.org.apache.xpath.internal.XPathAPI;

public class CcslConfig {
	private static final String CCSL_CONFIG_FILE_NAME = "META-INF/ccsl.xml";

	private String installRoot;
	private String componentClassName;
	private boolean defaultSaveErrors;
	private boolean defaultAddRecord;
	private boolean defaultStripRecord;
	private boolean useSendMessage;
	Hashtable<String, EndpointConfig> endpoints = new Hashtable<String, EndpointConfig>();
	Hashtable<String, LinkedList<EndpointConfig>> suEndpoints = new Hashtable<String, LinkedList<EndpointConfig>>();
	private Log log;
	
	
	public CcslConfig(Log log, String installRoot) {
		this.log = log;
		this.installRoot = installRoot;
		try {
			//	 Read the component level XML file
			DocumentBuilderFactory factory =
				DocumentBuilderFactory.newInstance(); 
			DocumentBuilder builder =
				factory.newDocumentBuilder(); 
			Document doc = builder.parse( new File( installRoot, CCSL_CONFIG_FILE_NAME ) );
			
			// Get the real component class name
			Node componentNode = XPathAPI.selectSingleNode(doc, "/ccslComponentConfig/componentClassName");
			componentClassName = componentNode.getTextContent();

			// Get the component default settings
			Element defaultSettingsElement = (Element)XPathAPI.selectSingleNode(doc, "/ccslComponentConfig/defaultSettings");
			defaultSaveErrors = Boolean.parseBoolean(defaultSettingsElement.getAttribute("saveErrors"));
			defaultAddRecord = Boolean.parseBoolean(defaultSettingsElement.getAttribute("addRecord"));
			defaultStripRecord = Boolean.parseBoolean(defaultSettingsElement.getAttribute("stripRecord"));
			useSendMessage = Boolean.parseBoolean(defaultSettingsElement.getAttribute("useSendMessage"));
		}
		catch (Exception e) {
			log.error("\n\nerror reading component level ccsl.xml "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n");
		}
	}

	public void loadSuSettings(String suName, String suRoot) {
		log.debug("suName is [" + suName + "] and suRoot is [" + suRoot + "].");
		String rootDir = suRoot;
		LinkedList<EndpointConfig> theseEndpoints = new LinkedList<EndpointConfig>();
		try {
			//	 Read the SU level XML file
			DocumentBuilderFactory factory =
				DocumentBuilderFactory.newInstance(); 
			DocumentBuilder builder =
				factory.newDocumentBuilder(); 
			Document doc = builder.parse( new File( suRoot, CCSL_CONFIG_FILE_NAME ) );
		
			// load the list of endpoints
			NodeList endpointNodes =
				XPathAPI.selectNodeList( doc, "/ccslSuConfig/endpoints/endpoint");
			for( int nodeIndex = 0; nodeIndex < endpointNodes.getLength(); nodeIndex++ ) { 
				Element endpointElement = (Element)endpointNodes.item( nodeIndex );
				// get the endpoint level configuration
				EndpointConfig endpoint = new EndpointConfig();
				endpoint.setSuRoot(suRoot);
				QName endpointService = new QName(endpointElement.getAttribute("serviceNS"), endpointElement.getAttribute("serviceLocal"));
				String endpointName = endpointElement.getAttribute("name");
				String endpointKey = endpointService+":"+endpointName;
				log.debug("found endpointkey: " + endpointKey);
				endpoint.setSaveErrors(endpointElement.getAttribute("saveErrors"), defaultSaveErrors);
				endpoint.setAddRecord(endpointElement.getAttribute("addRecord"), defaultAddRecord);
				log.debug("addRecord is " + endpointElement.getAttribute("addRecord"));
				endpoint.setStripRecord(endpointElement.getAttribute("stripRecord"), defaultStripRecord);
				endpoint.setSendMessage(endpointElement.getAttribute("useSendMessage"), useSendMessage);
				log.debug("useSendMessage is " + endpointElement.getAttribute("useSendMessage"));
				String scriptRootDir=CcslUtil.getScriptPathPath(suRoot);
				
				// get the UPOCs
				NodeList upocNodes =
					XPathAPI.selectNodeList(endpointElement, "upoc");
				for (int upocIndex = 0; upocIndex < upocNodes.getLength(); upocIndex++) {
					Element upocElement = (Element)upocNodes.item(upocIndex);
					UpocConfig upoc = new UpocConfig();
					String context = upocElement.getAttribute("context");
					if (context.equals("start")) endpoint.setNeedToRunStart(true);
					else if (context.equals("stop")) endpoint.setNeedToRunStop(true);
					upoc.setType(upocElement.getAttribute("type"));
					upoc.setClassName(upocElement.getAttribute("class"));
					upoc.setMethod(upocElement.getAttribute("method"));
					String rootAttribute = upocElement.getAttribute("root");
					if (rootAttribute != null && rootAttribute.length() > 0) rootDir = rootAttribute;
					else rootDir = scriptRootDir;
					upoc.setRootDir(rootDir);
					endpoint.putUpoc(context, upoc);
				}
				log.debug("endpoints put -> key = " + endpointKey);
				endpoints.put(endpointKey, endpoint);
				theseEndpoints.add(endpoint);
			}
		}
		catch (Exception e) {
			// The SU level ccsl.xml is not required so we can ignore file not found
			if (!(e instanceof FileNotFoundException)) {
				System.out.println("\n\nerror reading SU level ccsl.xml "+e+"\n"+ExceptionUtil.stackTraceString(e));
			}
		}
		suEndpoints.put(suName, theseEndpoints);
	}
	
	public String getComponentClassName() {
		return componentClassName;
	}
	
	public EndpointConfig getEndpointConfig(String endpointKey) {
		return (EndpointConfig)endpoints.get(endpointKey);
	}
	
	public String getInstallRoot() {
		return installRoot;
	}
	
	public boolean getDefaultSaveErrors() {
		return defaultSaveErrors;
	}
	
	public boolean getDafaultAddRecord() {
		return defaultAddRecord;
	}
	
	public boolean getDefaultStripRecord() {
		return defaultStripRecord;
	}
	
	public boolean getUseSendMessage() {
		return useSendMessage;
	}
	
	public Log getLog() {
		return log;
	}
	
	public void  stopSuUpocs(String suName) {
		LinkedList<EndpointConfig> endpoints = suEndpoints.get(suName);
		if (endpoints == null) return;
		for (Iterator<EndpointConfig> i=endpoints.iterator(); i.hasNext();) {
			EndpointConfig endpoint = i.next(); 
			if (endpoint.getNeedToRunStop()) {
				UpocConfig stopUpoc = endpoint.getUpoc("stop");
				if (stopUpoc != null) {
					try {
						CcslUpoc.runScript(log, stopUpoc.getRootDir(), "stop", stopUpoc.getType(), stopUpoc.getClassName(), stopUpoc.getMethod(), null, null, null,null, this);
					}
					catch (Exception e) {
						log.error("stop UPOC error: "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n\n\n");
					}
					endpoint.setNeedToRunStart(true);
					endpoint.setNeedToRunStop(false);
				}

			}
		}
	}
}
