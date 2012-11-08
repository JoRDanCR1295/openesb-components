/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)XmlUtil.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.xml;

import java.io.InputStream;
import java.io.StringWriter;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.crl.service.WSMessage;
import com.sun.jbi.crl.service.impl.JbiMessage;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.WrapperUtil;

/**
 * XML utility methods for CRL.
 * @author Kevan Simpson
 */
public class XmlUtil {
	private static Logger mLogger = Logger.getLogger(XmlUtil.class.getName());
	private static TransformerFactory mTransFact = newTransformerFactory();
    private static ErrorListener mErrorListener =
            new ErrorListener(){
                public void warning(TransformerException exception) throws TransformerException {
                	mLogger.log(Level.WARNING, "WARNING", exception);
                }
                public void fatalError(TransformerException exception) throws TransformerException {
                	mLogger.log(Level.SEVERE, "FATAL", exception);
                }
                public void error(TransformerException exception) throws TransformerException {
                	mLogger.log(Level.SEVERE, "ERROR", exception);
                }
            };
    private static XmlResourcePool mPool = new XmlResourcePoolImpl(10);
    
    private XmlUtil() {}
    
    /**
     * Creates a {@link DocumentFragment} containing all element nodes in the specified list.
     * 
     * @param nodes A list of nodes to add to fragment
     * @return a <code>DocumentFragment</code> or <code>null</code>.
     */
    public static DocumentFragment createDocumentFragment(NodeList nodes) {
    	if (nodes != null) {
    		DocumentFragment frag = null;
    		for (int i = 0, n = nodes.getLength(); i < n; i++) {
    			Node node = nodes.item(i);
    			if (node instanceof Element) {
    				if (frag == null) { // must be owner doc, new doc fails
    					frag = node.getOwnerDocument().createDocumentFragment();
    				}
    				frag.appendChild(node);
    			}
    		}

    		return frag;
    	}

    	return null;
    }
    
    /**
     * Creates a {@link WSMessage} from the specified XML and WSDL message definition.
     * 
     * @param src The XML content of the message.
     * @param model The WSDL message definition.
     * @return a service message.
     * @throws Exception if an error occurs creating message.
     */
    public static WSMessage createWSMessage(Source src, Message model) throws Exception {
    	DOMSource dom = toDOMSource(src);
		Document doc = (Document) dom.getNode();
		JbiMessage msg = new JbiMessage(model);
		
		// the document element is expected to be a jbi:message element or empty doc
		if (WrapperUtil.isMessageWrapped(doc)) {
			List parts = model.getOrderedParts(null);
			for (int i = 0, n = parts.size(); i < n; i++) {
				msg.setPart(((Part) parts.get(i)).getName(), 
							WrapperUtil.getPartElement(doc, i));
			}
		}
		
		return msg;
    }

    /**
     * Return CRL's singleton {@link XmlResourcePool}.
     * @return a pool of XML resources.
     */
    public static XmlResourcePool getXmlResourcePool() {
    	return mPool;
    }
    
    /**
     * Creates a new DOM Document from a pool of builders.
     * @return a new <code>Document</code>.
     */
    public static Document newDocument() {
    	return newDocBuilder().newDocument();
    }

    /**
     * Creates a new DOM DocumentBuilder from a pool.
     * @return a new <code>DocumentBuilder</code>.
     */
    public static DocumentBuilder newDocBuilder() {
    	XmlResource xml = null;
    	try {
    		xml = getXmlResourcePool().acquireXmlResource();
    		return xml.getDocumentBuilder();
    	}
    	finally {
    		getXmlResourcePool().releaseXmlResource(xml);
    		xml = null;
    	}
    }

    public static TransformerFactory newTransformerFactory() {
    	// TODO make this configurable?
    	return TransformerFactory.newInstance();
//    	return new org.apache.xalan.processor.TransformerFactoryImpl();
    }
    
    /**
     * Formats the specified xml source and returns an xml string.
     * 
     * @param src The xml source object.
     * @return Formatted xml or an empty string if formatting fails.
     */
    public static String print(Source src) {
        try {
            Transformer transformer = null;
            synchronized(mTransFact) {
                transformer = mTransFact.newTransformer();
                transformer.setErrorListener(mErrorListener);
            }
            
            StringWriter writer = new StringWriter();
            StreamResult dest = new StreamResult(writer);
            transformer.transform(XmlUtil.toDOMSource(src), dest);
            return writer.toString();
        } 
        catch (Exception e) {
        	if (mLogger.isLoggable(Level.FINE)) {
        		mLogger.fine("CRL-3019: Generation of xml string failed: "+ 
        					 e.getMessage());
        	}
            return "";
        }
    }

    /**
     * Converts the specified stream into a <code>Document</code>.
     * @param stream An input stream containing xml data.
     * @return A DOM Document.
     * @throws Exception if an error occurs reading stream or parsing xml.
     */
	public static Document readXml(InputStream stream) throws Exception {
		Document expectedDoc = null;
		XmlResource xml = null;
		try {
			xml = XmlUtil.getXmlResourcePool().acquireXmlResource();
			expectedDoc = xml.getDocumentBuilder().parse(stream);
		}
		finally {
			XmlUtil.getXmlResourcePool().releaseXmlResource(xml);
			xml = null;
		}

		return expectedDoc;
	}

    public static DOMSource toDOMSource(Source src) throws Exception {
        try {
        	if (src instanceof DOMSource) {
        		return (DOMSource) src;
        	}
        	
            Transformer transformer = null;
            synchronized(mTransFact) {
                transformer = mTransFact.newTransformer();
                transformer.setErrorListener(mErrorListener);
            }
            DOMResult result = new DOMResult(newDocument());
            transformer.transform(src, result);   
            return new DOMSource((Document) result.getNode());
        } 
        catch (Exception e) {
        	String msg = I18n.loc("CRL-6010: Failed to convert to DOMSource: {0}", e.getMessage());
        	mLogger.log(Level.WARNING, msg);
        	if (mLogger.isLoggable(Level.FINE)) {
        		// log exception at fine
        		mLogger.log(Level.FINE, "CRL-3020: DOMSource convert failure stacktrace is below", e);
        	}
            throw e;
        }
    }

//    public static ServiceExchange toServiceExchange(MessageExchange me, 
//    												WSMessage... msgs) throws Exception {
//    	JbiExchange jbi = new JbiExchange(me);
//    	if (msgs != null && msgs.length > 0) {
//    		jbi.setInMessage(msgs[0]);
//    		if (msgs.length > 1) {
//    			jbi.setOutMessage(msgs[1]);
//    			// TODO fault is 3rd msg?
//    		}
//    	}
//    	
//    	return jbi;
//    }
}
