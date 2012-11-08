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
 * $Id: DumpNormalizedMessage.java,v 1.1.1.1 2007/04/09 17:49:28 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.ByteArrayOutputStream;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.Set;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringDataSource;

public class DumpNormalizedMessage {

	
	
	public static String dump(NormalizedMessage m) { return dumpProcessor(m, false); }
	public static String dumpUnicode(NormalizedMessage m) { return dumpProcessor(m, true); }

	
	public static String dumpProcessor(NormalizedMessage m, boolean showUnicode) {
		String s="";
		
		if (m == null) return "NormalizedMessage is null";
		
		/*
		 * try to make the content readable
		 */
		Source content = m.getContent();
		if (content == null)
			s=s.concat("NormalizedMessage content is null\n");
		else {
			s=s.concat("CONTENT="+content.toString()+"\n");
		
			/*
			 * If the content is StreamSource then we convert it to DOMSource to have 
			 * an in-memory copy
			 */
			if (!(content instanceof DOMSource)) {
				try {
					DOMResult dr = new DOMResult();
			        TransformerFactory tf = TransformerFactory.newInstance();
			        Transformer t = tf.newTransformer();
			        t.transform(content, dr);
			        DOMSource ds = new DOMSource(dr.getNode());
			        m.setContent(ds);
			        content=m.getContent();
				}
				catch (Exception e) {
					s=s.concat("Exception converting content to DOMSource: "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n");
				}
			}
			
			if (content instanceof DOMSource) {
				try {
					StringWriter w = new StringWriter();
					StreamResult result = new StreamResult(w);
			        TransformerFactory tf = TransformerFactory.newInstance();
			        Transformer t = tf.newTransformer();
			        t.setOutputProperty("indent", "yes");
			        t.transform(content, result);
			        if (showUnicode) 
			        	s=s.concat(dumpStringAsUnicode(w.toString()+"\n"));
			        else
			        	s=s.concat(w.toString()+"\n");
				}
				catch (Exception e) {
					s=s.concat("Exception printing content: "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n");
				}
			} else {
				s=s.concat("ERROR - content is not valid\n");
			}
		}

		s=s.concat("\nATTACHMENTS:\n");
		Set attachmentNames = m.getAttachmentNames();
		for (Iterator i = attachmentNames.iterator(); i.hasNext();) {
			try {
				String name = (String)i.next();
				DataHandler dh = m.getAttachment(name);
				DataSource ds = dh.getDataSource();
				s=s.concat("	ATT["+name+"]: "+ds+"\n");
				if (ds instanceof StringDataSource) {
					String sdata = ((StringDataSource)ds).getData();
					s=s.concat(dumpStringAsUnicode(sdata));
				} else {
					ByteArrayOutputStream baos = new ByteArrayOutputStream();
					dh.writeTo(baos);
					s=s.concat(dumpBytesAsHex(baos.toByteArray())+"\n");
				}
			}
			catch (Exception e) {
				s=s.concat("\n\nException writing attachment: "+e+"\n"+ExceptionUtil.stackTraceString(e)+"\n");
			}
		}

		s=s.concat("\nPROPERTIES:\n");
		Set propertyNames = m.getPropertyNames();
		for (Iterator i = propertyNames.iterator(); i.hasNext();) {
			String name = (String)i.next();
			s=s.concat("	["+name+"]"+m.getProperty(name)+"\n");
		}
		
		s=s.concat("getSecuritySubject="+m.getSecuritySubject());
		return s;
	}
	

	public static String dumpStringAsUnicode(String s) {
		int charsPerLine= 16;
		StringBuffer out = new StringBuffer();
		StringBuffer left = null;
		StringBuffer right = null;
		int position=0;
		for (char ch : s.toCharArray()) {
			if (left == null) left = new StringBuffer();
			if (right == null) right = new StringBuffer();
			left.append((ch < 32) ? '.' : ch);
			right.append(String.format("%4X ", (int)ch));
			if (++position == charsPerLine) {
				position=0;
				out.append("	"+left+" - "+right+"\n");
				left = null;
				right = null;
			}
		}
		if (left != null) {
			while (left.length() < charsPerLine) left.append(" ");
			out.append("	"+left+" - "+right+"\n");
		}
		return new String(out);
	}
	
	public static String dumpBytesAsHex(byte[] bytes) {
		int charsPerLine= 16;
		StringBuffer out = new StringBuffer();
		StringBuffer left = null;
		StringBuffer right = null;
		int position=0;
		for (byte b : bytes) {
			if (left == null) left = new StringBuffer();
			if (right == null) right = new StringBuffer();
			left.append((b < 32) ? '.' : (char)b);
			right.append(String.format("%2X ", b));
			if (++position == charsPerLine) {
				position=0;
				out.append("	"+left+" - "+right+"\n");
				left = null;
				right = null;
			}
		}
		if (left != null) {
			while (left.length() < charsPerLine) left.append(" ");
			out.append("	"+left+" - "+right+"\n");
		}
		return new String(out);
	}


}
