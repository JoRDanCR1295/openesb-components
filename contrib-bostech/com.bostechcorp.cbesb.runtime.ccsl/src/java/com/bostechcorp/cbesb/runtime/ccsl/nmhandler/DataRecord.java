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
 * $Id: DataRecord.java,v 1.1.1.1 2007/04/09 17:49:30 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.nmhandler;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;

/**
 * Class used by DataMessageUtil to represent a single record of data.
 *
 */
public class DataRecord {

    private static final Log log = LogFactory.getLog(DataRecord.class);
	public static final byte TYPE_XML = 0;
	public static final byte TYPE_STRING = 1;
	public static final byte TYPE_BINARY = 2;
	
	protected byte type;
	protected Document document;
	protected String text;
	protected byte[] data;
	protected String attachmentId;
	
	public DataRecord(String text)
	{
		type = TYPE_STRING;
		document = null;
		this.text = text;
		data = null;
		attachmentId = null;
	}

	public DataRecord(byte[] data)
	{
		type = TYPE_BINARY;
		document = null;
		text = null;
		this.data = data;
		attachmentId = null;
	}
	
	public DataRecord(Document document)
	{
		type = TYPE_XML;
		this.document = document;
		text = null;
		data = null;
		attachmentId = null;
	}

	public DataRecord(Source src)
	{
		if (src instanceof StringSource)
		{
			type = TYPE_STRING;
			text = ((StringSource)src).getText();
			document = null;
			data = null;
			attachmentId = null;
		}
		else if (src instanceof ByteArraySource)
		{
			type = TYPE_BINARY;
			data = ((ByteArraySource)src).getBytes();
			document = null;
			text = null;
			attachmentId = null;
		}
		else if (src instanceof DOMSource)
		{
			type = TYPE_XML;
			document = (Document)((DOMSource)src).getNode();
			data = null;
			text = null;
			attachmentId = null;
		}
		else
		{
			type = TYPE_XML;
			//Assume anything else is XML and can be transformed into a DOMResult
			try {
				DOMResult dr = new DOMResult();
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.transform(src, dr);
		        document = (Document)dr.getNode();
			}
			catch (Exception e) {
				System.err.println("Exception converting content to DOMSource: "+e+"\n");
			}
			data = null;
			text = null;
			attachmentId = null;
		}
	}
	
	public Source getSource()
	{
		if (type == TYPE_XML)
		{
			return new DOMSource(document);
		}
		else if (type == TYPE_STRING)
		{
			return new StringSource(text);
		}
		else if (type == TYPE_BINARY)
		{
			return new ByteArraySource(data);
		}
		else
		{
			log.error("Invalid DataRecord type: " + type);
			return null;
		}
	}
	
	public String getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(String attachmentId) {
		this.attachmentId = attachmentId;
	}

	public byte[] getData() {
		return data;
	}

	public void setData(byte[] data) {
		this.data = data;
	}

	public Document getDocument() {
		return document;
	}

	public void setDocument(Document document) {
		this.document = document;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public byte getType() {
		return type;
	}

	public void setType(byte type) {
		this.type = type;
	}
	
}
