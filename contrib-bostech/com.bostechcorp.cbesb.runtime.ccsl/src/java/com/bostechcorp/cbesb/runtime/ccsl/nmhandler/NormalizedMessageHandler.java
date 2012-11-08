/*
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
 * $Id: NormalizedMessageHandler.java,v 1.4 2007/04/25 20:46:00 afung Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.nmhandler;

import java.util.Vector;

import javax.activation.DataHandler;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * A utility class to add and retrieve data from a NormalizedMessage.
 *
 */
public class NormalizedMessageHandler {

    private static final Log log = LogFactory.getLog(NormalizedMessageHandler.class);
    protected static final String NAMESPACE="http://bostech.com/envelope/1.0";
	protected static final String DATA_ENVELOPE = "DataEnvelope";
	protected static final String XML_RECORD = "XMLRecord";
	protected static final String BINARY_RECORD = "BinaryRecord";
	protected static final String STRING_RECORD = "StringRecord";
	protected static final String USERDATA = "Userdata";
	protected static final String ATTACHMENTID_PREFIX = "attachment";
	protected static final String WSI_AP_SWAREF_PREFIX = "cid:";
	
	protected NormalizedMessage msg;
	protected Document msgContent;
	protected Vector<DataRecord> records;
	protected Element userdata;
	private int maxAttachmentId;
	private boolean modified;
	private QName messageQName;
	
	/**
	 * Constructor requires an instance of a NormalizedMessage.  The message
	 * may be empty or already have data that was added using DataMessageUtil.
	 * @param msg a NormalizedMessage instance
	 */
	public NormalizedMessageHandler(NormalizedMessage msg) {
		this(msg, null);
	}
	
	public NormalizedMessageHandler(NormalizedMessage msg, QName msgqn)
	{
		messageQName = msgqn;
		this.msg = msg;
		records = new Vector<DataRecord>();
		maxAttachmentId = 0;
		modified = false;
		Source content = msg.getContent();
		
		if (content != null && !(content instanceof DOMSource)) {
			//convert StreamSource into a DOMSource
			try {
				DOMResult dr = new DOMResult();
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.transform(content, dr);
		        content = new DOMSource(dr.getNode());
			}
			catch (Exception e) {
				log.error("Exception converting content to DOMSource: ", e);
			}
		}
		
		if (content instanceof DOMSource) {
			try {
				//Try to get the DataEnvelope components
				
				parseContent((DOMSource)content);
			}
			catch (Exception e) {
				log.error("Exception reading message content: ", e);
			}
		} 
		else
		{
			//Assume this is a new message
			//Force new content to be created.
			modified = true;
		}
	}
	
	
	/**
     * Returns the NormalizedMessage instance with modified content and possibly
     * attachments.
	 * @return The instance of NormalizedMessage passed into the Constructor.
	 */
	public NormalizedMessage generateMessageContent()
	{
		if (modified)
		{
			createNewContent();
		}
		try {
			//set the content to a new instance of a Source
			//since some Sources may only be accessed once.
			msg.setContent(new DOMSource(msgContent));
		}
		catch (MessagingException e)
		{
			log.error("Exception creating message content: ", e);
		}

		return msg;
	}
	
	/**
	 * Re-builds the XML body of the normalized message to
	 * include any records that were added.
	 */
	private void createNewContent()
	{
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
			DocumentBuilder db = dbf.newDocumentBuilder();
			msgContent = db.newDocument();
			Element jbiMessage = msgContent.createElementNS("http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper", "jbi:message");
			jbiMessage.setAttribute("xmlns:msgns", messageQName.getNamespaceURI());
			jbiMessage.setAttribute("type", "msgns:"+messageQName.getLocalPart());
			jbiMessage.setAttribute("version", "1.0");
			Element jbiPart = msgContent.createElementNS("http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper", "jbi:part");
			Element dataEnvelope = msgContent.createElementNS(NAMESPACE, DATA_ENVELOPE);
			msgContent.appendChild(jbiMessage);
			jbiMessage.appendChild(jbiPart);
			jbiPart.appendChild(dataEnvelope);
			for (int i=0; i < records.size(); i++)
			{
				DataRecord dr = records.get(i);
				if (dr.getType() == DataRecord.TYPE_XML)
				{
					Element recordRoot = dr.getDocument().getDocumentElement();
					Element recordCopy = (Element)recordRoot.cloneNode(true);
					Element xmlRecord = msgContent.createElementNS(NAMESPACE, XML_RECORD);
					msgContent.adoptNode(recordCopy);
					xmlRecord.appendChild(recordCopy);
					dataEnvelope.appendChild(xmlRecord);
				}
				else if (dr.getType() == DataRecord.TYPE_STRING)
				{
				    Element stringRecord = msgContent.createElementNS(NAMESPACE, STRING_RECORD);
					stringRecord.setTextContent(WSI_AP_SWAREF_PREFIX + dr.getAttachmentId());
					dataEnvelope.appendChild(stringRecord);
				}
				else if (dr.getType() == DataRecord.TYPE_BINARY)
				{
				    Element binaryRecord = msgContent.createElementNS(NAMESPACE, BINARY_RECORD);
					binaryRecord.setTextContent(WSI_AP_SWAREF_PREFIX + dr.getAttachmentId());
					dataEnvelope.appendChild(binaryRecord);					
				}
			}
			if (userdata != null)
			{
				
			}


			
		}
		catch (ParserConfigurationException e)
		{
			log.error("Exception creating DOM Document: ", e);
		}
		modified = false;

	}
	
	/**
	 * Parses the NormalizedMessage XML Content to retrieve the individual records.
	 * @param source
	 */
	private void parseContent(DOMSource source)
	{
		Node node = source.getNode();
		if (node instanceof Document)
		{
			msgContent = (Document)node;
			node = msgContent.getDocumentElement();
		}
		if (node instanceof Element && node.getLocalName().equals("message")) node = node.getFirstChild();
		if (node instanceof Element && node.getLocalName().equals("part")) node = node.getFirstChild();
		
		if (node instanceof Element)
		{
			Element root = (Element)node;
			if (root.getLocalName().equals(DATA_ENVELOPE)) 
			{
				Node child = root.getFirstChild();
				while (child != null)
				{
					if (child instanceof Element)
					{
                                            if (child.getLocalName().equals(XML_RECORD))
						{
							processXmlRecord((Element)child);
						}
                                            else if (child.getLocalName().equals(BINARY_RECORD))
						{
							processBinaryRecord((Element)child);
						}
                                            else if (child.getLocalName().equals(STRING_RECORD))
						{
							processStringRecord((Element)child);
						}
                                            else if (child.getLocalName().equals(USERDATA))
						{
							userdata = (Element)child;
						}
					}
					child = child.getNextSibling();
				}
			}
			else
			{
                            log.error("Message content is not a " + DATA_ENVELOPE + "; name is " + root.getLocalName());
			}
		}
	}
	
	/**
	 * Processes an xml record in the NormalizedMessage
	 * @param xmlRecord
	 */
	private void processXmlRecord(Element xmlRecord)
	{
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document newDoc = db.newDocument();
			Node recordNode = xmlRecord.getFirstChild();
			while (recordNode != null && !(recordNode instanceof Element))
			{
				recordNode = recordNode.getNextSibling();
			}
			if (recordNode != null)
			{
				Element newRecord = (Element)recordNode.cloneNode(true);
				newDoc.adoptNode(newRecord);
				newDoc.appendChild(newRecord);
				DataRecord dr = new DataRecord(newDoc);
				records.add(dr);
			}
		}
		catch (ParserConfigurationException e)
		{
			log.error("Exception creating DOM Document: ", e);
		}
	}
	
	/**
	 * Processes a binary record in the NormalizedMessage
	 * @param binaryRecord
	 */
	private void processBinaryRecord(Element binaryRecord)
	{
		String attachmentId = getAttachmentId(binaryRecord);
		if (attachmentId != null)
		{
			DataHandler dh = msg.getAttachment(attachmentId);
			if (dh != null)
			{
				ByteArrayDataSource dataSource = (ByteArrayDataSource)dh.getDataSource();
				DataRecord dr = new DataRecord(dataSource.getData());
				dr.setAttachmentId(attachmentId);
				records.add(dr);
			}
			else
			{
				log.error("Could not locate attachment: " + attachmentId);
			}
		}
	}
	
	/**
	 * Processes a String record in the NormalizedMessage
	 * @param stringRecord
	 */
	private void processStringRecord(Element stringRecord)
	{
		String attachmentId = getAttachmentId(stringRecord);
		if (attachmentId != null)
		{
			DataHandler dh = msg.getAttachment(attachmentId);
			if (dh != null)
			{
				StringDataSource dataSource = (StringDataSource)dh.getDataSource();
				DataRecord dr = new DataRecord(dataSource.getData());
				dr.setAttachmentId(attachmentId);
				records.add(dr);
			}
			else
			{
				log.error("Could not locate attachment: " + attachmentId);
			}
		}
	}
	
	/**
	 * Retrieves the attachment ID from an element using 
	 * the WS-I AP notation.
	 * @param record
	 * @return
	 */
	private String getAttachmentId(Element record)
	{
		String attachmentId;
		String value = record.getTextContent();
		if (value != null && value.startsWith(WSI_AP_SWAREF_PREFIX))
		{
			attachmentId = value.substring(WSI_AP_SWAREF_PREFIX.length());
			//Reset maxAttachmentId if this Id is larger.
			if (attachmentId.startsWith(ATTACHMENTID_PREFIX))
			{
				String intStr = attachmentId.substring(ATTACHMENTID_PREFIX.length());
				int intValue = Integer.parseInt(intStr);
				if (intValue > maxAttachmentId)
				{
					maxAttachmentId = intValue;
				}
			}
		}
		else
		{
			log.error("Not a valid WS-I AP content ID specification: " + value);
			attachmentId = null;
		}
		return attachmentId;
	}
	
	/**
	 * Returns the number of individual data records are in the message.
	 * @return
	 */
	public int getRecordCount()
	{
		return records.size();
	}
	
	/**
	 * Appends a new record to the NormalizedMessage.  Based on the Source
	 * implementation, the record will be a StringRecord, BinaryRecord or XmlRecord.
	 * @param src
	 */
	public void addRecord(Source src)
	{
		DataRecord dr = new DataRecord(src);
		if (dr.getType() == DataRecord.TYPE_STRING)
		{
			//Add the attachment for non-xml records
			String attachmentId = getNextAttachmentId();
			dr.setAttachmentId(attachmentId);
			try {
				StringDataSource dataSource = new StringDataSource(dr.getText());
				msg.addAttachment(attachmentId, new DataHandler(dataSource));
			}
			catch (MessagingException e)
			{
				log.error("Caught exception while adding attachment to message: ", e);
			}
		}
		else if (dr.getType() == DataRecord.TYPE_BINARY)
		{
			//Add the attachment for non-xml records
			String attachmentId = getNextAttachmentId();
			dr.setAttachmentId(attachmentId);
			try {
				ByteArrayDataSource dataSource = new ByteArrayDataSource(dr.getData());
				msg.addAttachment(attachmentId, new DataHandler(dataSource));
			}
			catch (MessagingException e)
			{
				log.error("Caught exception while adding attachment to message: ", e);
			}
		}
		records.add(dr);
		modified = true;
	}
	
	/**
	 * Retrieves the next automatically generated attachment ID
	 * @return
	 */
	private String getNextAttachmentId()
	{
		maxAttachmentId++;
		return ATTACHMENTID_PREFIX + Integer.toString(maxAttachmentId);
	}
	
	/**
	 * Returns a Source instance for the record at the specified index.
	 * A StringRecord will always return a StringSource.
	 * A BinaryRecord will always return a ByteArraySource.
	 * An XMLRecord will always return a DOMSource.
	 * @param index
	 * @return
	 */
	public Source getRecordAtIndex(int index)
	{	
		try {
			DataRecord dr = records.get(index);
			return dr.getSource();
		}
		catch (IndexOutOfBoundsException e)
		{
			log.error("Invalid record index '" + index + "'.  Valid values are: 0 <= index < " + records.size(), e);
			return null;
		}
	}
	
	/**
	 * Debug method to print the contents of the XML body to stdout.
	 *
	 */
	public void debugContents()
	{
		log.debug("DataMessageUtil");
		createNewContent();
		
		log.debug("Message Body:");
		try {
			TransformerFactory tFactory = TransformerFactory.newInstance();
			Transformer transformer = tFactory.newTransformer();
			DOMSource source = new DOMSource(msgContent);
			StreamResult result = new StreamResult(System.out);
			transformer.transform(source, result);
		} catch (TransformerException te) {
			log.error("Error Writing Output", te);
		}
	}
}
