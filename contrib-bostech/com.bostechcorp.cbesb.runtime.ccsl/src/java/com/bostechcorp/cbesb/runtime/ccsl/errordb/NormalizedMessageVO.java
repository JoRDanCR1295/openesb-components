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
 * $Id: NormalizedMessageVO.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.errordb;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;

import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import com.bostechcorp.cbesb.runtime.ccsl.lib.ExceptionUtil;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringSource;

public class NormalizedMessageVO extends AbstractVO {
	private long exchangeId;
	private String type;
	private NormalizedMessage message;

	public NormalizedMessageVO(long id, String type, NormalizedMessage mess) {
		exchangeId = id;
		this.type = type;
		message = mess;
	}
	
	public long getExchangeId() {
		return exchangeId;
	}
	
	public String getType() {
		return type;
	}

	public String getContent() {
		Source content = message.getContent();
		String s = "";
		/*
		 * If the content is StreamSource then we convert it to StringSource to have 
		 * an in-memory copy
		 */
		if (content instanceof StreamSource) {
			try {
				StringBuffer sb = new StringBuffer();
				InputStream is = ((StreamSource)content).getInputStream();
				for (int ch=0; (ch=is.read()) >= 0;) {
					sb.append((char)ch);
				}
				is.close();
				StringSource ss = new StringSource(new String(sb));
				message.setContent(ss);
				s=s.concat(ss.getText());
			}
			catch (Exception e) {
				s=s.concat("Exception converting content to DOMSource: "+e+"\n");
			}
		/*
		 * If it's a StringSource then we just grab the content
		 */
		} else if (content instanceof StringSource) {
			s=s.concat(((StringSource)content).getText());
		/*
		 * If it's anything else then try to make it DOMSource
		 */			
		} else {
			try {
				DOMResult dr = new DOMResult();
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.transform(content, dr);
		        content = new DOMSource(dr.getNode());
		        message.setContent(content);
			}
			catch (Exception e) {
				s=s.concat("Exception converting content to DOMSource: "+e.getMessage()+"\n"+ExceptionUtil.stackTraceString(e));
			}
		}
		if (content instanceof DOMSource) {
			try {
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				StreamResult result = new StreamResult(baos);
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.setOutputProperty("indent", "yes");
		        t.transform(content, result);
		        s=s.concat(baos.toString("utf-8")+"\n");
			}
			catch (Exception e) {
				s=s.concat("Exception printing content: "+e+"\n");
			}
		} else {
			s=s.concat("ERROR - content is not valid\n");
		}
		return s;
	}
}
