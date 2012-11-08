package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.InputStream;
import java.io.StringWriter;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.NormalizedMessageHandler;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringSource;

public class NormalizedMessageUtil {

	//
	// Get the message content as a DOM Node
	//
	public static Node convertToNode(Source content) throws TransformerException {
		if (content == null) return null;
	
		/*
		 * If the content is StreamSource or StringSource then we convert it to DOM 
		 * and get the node.
		 */
		if (content instanceof StreamSource || content instanceof StringSource) {
				DOMResult dr = new DOMResult();
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.transform(content, dr);
		        return dr.getNode();
		} else if (content instanceof DOMSource) {
DOMSource ds = (DOMSource)content;
Document doc = (Document)ds.getNode();
			return doc.getDocumentElement();
		} else {
			throw new TransformerException("Content "+content+" can not be converted to Node");
		}
	}
	

	//
	// Get the message content as a DOM Node
	//
	public static Node getContentAsNode(NormalizedMessage nm) throws TransformerException, MessagingException {
		Source content = nm.getContent();
		if (content == null) return null;
	
		/*
		 * If the content is StreamSource or StringSource then we convert it to DOM 
		 * and get the node.
		 */
		if (content instanceof StreamSource || content instanceof StringSource) {
				DOMResult dr = new DOMResult();
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.transform(content, dr);
		        // If the content is streamsource then replace it with the DOM since the
		        // stream can only be read once.
		        if (content instanceof StreamSource) {
			        DOMSource ds = new DOMSource(dr.getNode());
			        nm.setContent(ds);
		        }
		        return dr.getNode();
		} else if (content instanceof DOMSource) {
DOMSource ds = (DOMSource)content;
Document doc = (Document)ds.getNode();
			return doc.getDocumentElement();
		} else {
			throw new TransformerException("Content "+content+" can not be converted to Node");
		}
	}
	
	public static String getContentAsString(NormalizedMessage nm) {
		Source content = nm.getContent();
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
				nm.setContent(ss);
				s=s.concat(ss.getText());
			}
			catch (Exception e) {
				s=s.concat("Exception converting content to DOMSource: "+e+"\n");
			}
		}
		if (content instanceof DOMSource) {
			try {
				StringWriter sw = new StringWriter();
				StreamResult result = new StreamResult(sw);
		        TransformerFactory tf = TransformerFactory.newInstance();
		        Transformer t = tf.newTransformer();
		        t.setOutputProperty("indent", "yes");
		        t.transform(content, result);
		        s=s.concat(sw.toString()+"\n");
			}
			catch (Exception e) {
				s=s.concat("Exception printing content: "+e+"\n");
			}
		} else {
			s=s.concat("ERROR - content is not valid\n");
		}
		return s;
	}
	
	public static void setContent(NormalizedMessage nm, String s) throws MessagingException {
		StringSource ss = new StringSource(s);
		nm.setContent(ss);
	}
	
	public static String getTextContent(NormalizedMessage nm) {
		String s = "";
		Node n = null;
		try {
			n = getContentAsNode(nm);
			s = n.getTextContent();
		}
		catch (Exception e) {
			s = "Exception getting text content "+e;
		}
		return s;
	}
	
	public static void addXmlRecord(NormalizedMessage nm, String xml) throws TransformerConfigurationException, TransformerException {
		NormalizedMessageHandler nmh = new NormalizedMessageHandler(nm);
		StringSource ss = new StringSource(xml);
		DOMResult dr = new DOMResult();
        TransformerFactory tf = TransformerFactory.newInstance();
        Transformer t = tf.newTransformer();
        t.transform(ss, dr);
        DOMSource ds = new DOMSource(dr.getNode());
		nmh.addRecord(ds);
		
		nmh.generateMessageContent();
	}
}
