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
 * $Id: SourceHelper.java,v 1.1.1.1 2007/04/09 17:49:30 mpreston Exp $
 */

package com.bostechcorp.cbesb.runtime.ccsl.nmhandler;

import java.io.ByteArrayInputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;

import com.bostechcorp.cbesb.common.util.Dom;

public final class SourceHelper {

    private static final String START_TAG = "<content>";

    private static final String END_TAG = "</content>";

    public static Source createContentSource(String msg) {
        return createSource(START_TAG + msg + END_TAG);
    }

    private SourceHelper() {
    }
    
    /**
     * Creates an XML fault with the stacktrace, message and the specified code
     * 
     * @param e
     *            exception, used to provide faultstring and detail
     * @param code
     *            code of the fault
     * @return
     */
    public static String createSoapFault(Throwable e, String code) {
        StringBuffer s = new StringBuffer();
        /*
         * s.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"); s.append("<s:Envelope
         * xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"");
         * s.append("xmlns:wsmd=\"http://www.openuri.org/2003/02/soap/messagedata/\">");
         * s.append("<s:Header>");
         * 
         * s.append("<wsmd:MessageData><wsmd:RefToMessageId>"); if(id != null)
         * s.append(id); s.append("</wsmd:RefToMessageId></wsmd:MessageData>");
         * 
         * s.append("</s:Header>");
         * 
         * s.append("<s:Body>");
         */
        s
                .append("<s:Fault xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\"><s:faultcode>");
        if (code != null) {
            s.append(code);
        }
        s.append("</s:faultcode><s:faultstring>");
        s.append(e.getMessage());
        s.append("</s:faultstring><s:detail>");

        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        s.append(sw);

        s.append("</s:detail></s:Fault>");
        /*
         s.append("</s:Body></s:Envelope>");
         */
        return s.toString();
    }

    public static Source createSource(String msg) {
        StreamSource source = new StreamSource();

        byte[] msgByte = null;
        try {
        	msgByte = msg.getBytes("utf-8");
        } catch (Exception e) {
        	e.printStackTrace();
        	System.err.println("getBytes() error "+e);
        }

        ByteArrayInputStream in = new ByteArrayInputStream(msgByte);

        source.setInputStream(in);

        return source;
    }

    public static String createString(Source s) throws Exception {
        if (s == null) {
            throw new Exception("source is null");
        } else if (s instanceof DOMSource) {
        	return Dom.createStringFromDOMDocument((Document)(((DOMSource)s).getNode()), true);
        } else if (s instanceof StringSource) {
        	return ((StringSource)s).getText();
        } else if (s instanceof ByteArraySource) {
        	return ((ByteArraySource)s).toString();
        } else if (s instanceof StreamSource) {
	        ByteArrayInputStream bio = (ByteArrayInputStream) ((StreamSource) s)
	                .getInputStream();
	        byte[] buff = new byte[bio.available()];
	        bio.read(buff);
	        String str = new String(buff,"utf-8");
	        return str;
        } else {
        	throw new Exception("data source ("+s+") is not a supported type");
        }
    }

    public static String createStringContent(Source s) throws Exception {
        String content = createString(s);
        int index = content.indexOf(START_TAG);
        String result = content.substring(index + START_TAG.length());
        return result.substring(0, result.indexOf(END_TAG));
    }

}
