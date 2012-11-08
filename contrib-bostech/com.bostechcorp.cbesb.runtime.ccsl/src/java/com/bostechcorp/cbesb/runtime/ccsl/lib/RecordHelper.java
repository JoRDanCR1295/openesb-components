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
 * $Id: RecordHelper.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.util.Set;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.ByteArraySource;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.DataRecord;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.NormalizedMessageHandler;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringSource;
import com.bostechcorp.cbesb.common.util.Dom;

import com.sun.org.apache.xpath.internal.XPathAPI;

public class RecordHelper {

	protected static final transient Log log = LogFactory.getLog(RecordHelper.class);
	private static final String SENDMESSAGE_NAMESPACE_URL = "http://cbesb.bostechcorp.com/soap/sendmessage/1.0";

	public static synchronized void addXmlRecord(NormalizedMessage mess) {
		log.debug("In addXmlRecord method.");
		Source content = mess.getContent();
		Node doc = null;
		DOMSource copiedContent = null;
		if (content instanceof DOMSource) doc = ((DOMSource)content).getNode();
		else {
			//convert into a DOMSource
			try {
				DOMResult dr = new DOMResult();
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.transform(content, dr);
		        doc = dr.getNode();
			}
			catch (Exception e) {
				System.err.println("Exception converting content to DOMSource: "+e.getMessage()+"\n");
				e.printStackTrace();
			}
		}
		try {
			copiedContent = new DOMSource(doc);
			mess.setContent(null);
			NormalizedMessageHandler handler = new NormalizedMessageHandler(mess);
			handler.addRecord(copiedContent);
			handler.generateMessageContent();
		}
		catch (Exception e) {
			e.printStackTrace();
			log.error("exception adding dataEnvelope: "+e);
		}
	}

	public static synchronized void stripXmlRecord(NormalizedMessage mess) throws MessagingException {
		log.debug("In stripXmlRecord method.");
		NormalizedMessageHandler h = new NormalizedMessageHandler(mess);
		mess.setContent(h.getRecordAtIndex(0));
	}

	public static synchronized void addSendMessage(NormalizedMessage mess, MessageExchange me, boolean isResponse) throws MessagingException {
		log.debug("In addSendMessage method.");
		try {
			// create the content document
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder parser = factory.newDocumentBuilder();
		    Document doc = parser.newDocument();
			Element root = null;
			if (isResponse)
				root = doc.createElement("sendMessageResponse");
			else
				root = doc.createElement("sendMessage");
			root.setAttribute("xmlns", SENDMESSAGE_NAMESPACE_URL);
		    doc.appendChild(root);

		    // loop through the message records
			NormalizedMessageHandler nmh = new NormalizedMessageHandler(mess);
		    for (int i=0; i<nmh.getRecordCount(); i++) {
		    	Source rec = nmh.getRecordAtIndex(i);

				if (rec instanceof StringSource) {
			    	// string content
					String textValue = ((StringSource)rec).getText();
					Element cur = doc.createElement("messageType");
					cur.setTextContent("STRING");
					root.appendChild(cur);
					cur = doc.createElement("message");
					cur.setTextContent(textValue);
					root.appendChild(cur);
				}else if (rec instanceof ByteArraySource) {
					// byte (base64) content
					byte[] rawBytes = ((ByteArraySource)rec).getBytes();
					byte[] encodedBytes = Base64.encodeBase64(rawBytes);
					String encodedString = new String(encodedBytes, "utf-8");
					Element cur = doc.createElement("messageType");
					cur.setTextContent("BASE64");
					root.appendChild(cur);
					cur = doc.createElement("message");
					cur.setTextContent(encodedString);
					root.appendChild(cur);					
				} else {
					// convert anything else to DOM
					DOMResult dr = new DOMResult();
					TransformerFactory tf = TransformerFactory.newInstance();
					Transformer t = tf.newTransformer();
					t.transform(rec, dr);
					Document resultDoc = (Document)dr.getNode();
					Node resultNode = resultDoc.getDocumentElement();
					Node newNode = doc.importNode(resultNode, true);
					
					Element cur = doc.createElement("messageType");
					cur.setTextContent("XML");
					root.appendChild(cur);
					cur = doc.createElement("message");
					cur.appendChild(newNode);
					root.appendChild(cur);
				}
		    }
		    mess.setContent(new DOMSource(doc));
		    
		    // now add the properties
		    Set<String> properties = mess.getPropertyNames();
		    for (String propertyName : properties) {
				Object value = mess.getProperty(propertyName);
				Element cur=doc.createElement("property");
				Element e = doc.createElement("name");
				e.setTextContent(propertyName);
				cur.appendChild(e);
				e = doc.createElement("value");
				e.setTextContent(value.toString());
				cur.appendChild(e);
					
				root.appendChild(cur);
		    }
		    
		    // delete any attachments from the message
		    Set<String> attachments = mess.getAttachmentNames();
		    for (String attachment : attachments) {
		    	mess.removeAttachment(attachment);
		    }
		}
		catch (Exception e) {
			log.error("exception "+e+"\n\n"+ExceptionUtil.stackTraceString(e));
			throw new MessagingException(e);
		}
	}

	
	
	public static synchronized void stripSendMessage(NormalizedMessage mess, MessageExchange exchange, boolean isResponse) throws MessagingException {
		log.debug("In stripSendMessage method.");
		NormalizedMessage newMessage = exchange.createMessage();
		NormalizedMessageHandler nmh = new NormalizedMessageHandler(newMessage);
		
		// Get a DOM document from the content
		Document doc = null;
		Source content = mess.getContent();
		if (content instanceof DOMSource) {
			doc = (Document)((DOMSource)content).getNode();
		} else {
			//convert into a DOMSource
			try {
				DOMResult dr = new DOMResult();
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.transform(content, dr);
		        doc = (Document)dr.getNode();
		        content = new DOMSource(doc);
		        mess.setContent(content);
			} catch (Exception e) {
				log.error("Exception converting content to DOMSource: "+e.getMessage()+"\n");
				e.printStackTrace();
				throw new MessagingException(e);
			}
		}

		try {
			Element root = (Element)doc.getFirstChild();
						
			// get the main element
			String mainElementTag=null;
			if (isResponse) mainElementTag = "sendMessageResponse";
			else mainElementTag = "sendMessage";

			// Only the sendMessage element needs to be namespace qualified
			// Check that it matches the correct namespace
			// But only check the local name for all sub elements
			
			if (!root.getNamespaceURI().equals(SENDMESSAGE_NAMESPACE_URL) ||
					!root.getLocalName().equals(mainElementTag))
			{
				log.error("stripSendMessage: {"+SENDMESSAGE_NAMESPACE_URL+"}"+mainElementTag+" tag not found in message, leaving original message");
				return;
			}

			Element childElement = getFirstChildElement(root);
			
			while (childElement != null) {
				if (childElement.getLocalName().equals("messageType")) {
					String dataType = childElement.getTextContent();
					childElement = getNextSiblingElement(childElement);
					if (childElement != null && childElement.getLocalName().equals("message")) {
						if (dataType.equalsIgnoreCase("STRING")) {
							// make a string record
							StringSource ss = new StringSource(childElement.getTextContent());
							nmh.addRecord(ss);
						} else if (dataType.equalsIgnoreCase("BASE64")) {
							// make a binary record
							byte[] binaryBytes = Base64.decodeBase64(childElement.getTextContent().getBytes("utf-8"));
							ByteArraySource bas = new ByteArraySource(binaryBytes);
							nmh.addRecord(bas);
						} else {
							// make an XML record
							try {
								DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
								dbf.setNamespaceAware(true);
								DocumentBuilder db = dbf.newDocumentBuilder();
								Document newDoc = db.newDocument();
								Node recordNode = childElement.getFirstChild();
								while (recordNode != null && !(recordNode instanceof Element))
								{
									recordNode = recordNode.getNextSibling();
								}
								if (recordNode != null)
								{
									Element newRecord = (Element)recordNode;
									newDoc.adoptNode(newRecord);
									newDoc.appendChild(newRecord);
							        DOMSource ds = new DOMSource(newDoc);
							        nmh.addRecord(ds);
								}
								
							}
							catch (ParserConfigurationException e)
							{
								log.error("stripSendMessage - Exception creating DOM Document: "+e+"\n");
							}
						}
					}
					
				}

				if (childElement.getLocalName().equals("properties")) {
					Node propertyNode = childElement.getFirstChild();
					Element propertyElement = getFirstChildElement(childElement);
					while (propertyElement != null) {
						if (propertyElement.getLocalName().equals("property")) {
							Element nameElement = getFirstChildElement(propertyElement);
							if (nameElement != null && nameElement.getLocalName().equals("name")) {
								String name = nameElement.getTextContent();
								Element valueElement = getNextSiblingElement(nameElement);
								if (valueElement != null && valueElement.getLocalName().equals("value"))
								{
									String value = valueElement.getTextContent();
									mess.setProperty(name, value);
								}
							}
						}						
					}
				}
				
				childElement = getNextSiblingElement(childElement);
			}
						
			// copy the new message over the original
			nmh.generateMessageContent();
			mess.setContent(newMessage.getContent());
			Set<String> attachments = newMessage.getAttachmentNames();
			for (String name : attachments) {
				mess.addAttachment(name, newMessage.getAttachment(name));
			}
				
		} catch (Exception e) {
			throw new MessagingException("Exception stripping sendMessage: "+e.getMessage()+"\n"+ExceptionUtil.stackTraceString(e));
		}
	}	
		
	private static Element getFirstChildElement(Element node)
	{
		Node child = node.getFirstChild();
		while (child != null && !(child instanceof Element))
		{
			child = child.getNextSibling();
		}
		return (Element)child;
	}
	
	private static Element getNextSiblingElement(Element node)
	{
		Node child = node.getNextSibling();
		while (child != null && !(child instanceof Element))
		{
			child = child.getNextSibling();
		}
		return (Element)child;
	}
}