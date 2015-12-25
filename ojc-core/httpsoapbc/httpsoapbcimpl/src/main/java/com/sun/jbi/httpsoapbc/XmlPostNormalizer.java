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
 * @(#)XmlPostNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.httpsoapbc.util.StringUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.activation.DataSource;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.ws.handler.MessageContext;
import javax.wsdl.Message;
import javax.wsdl.Part;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 *
 */
public class XmlPostNormalizer {

    private static final int INITIAL_STREAM_SIZE = 131072; // 128 KB
    private static final String SOAP_HEADERS_PROPERTY = "com.sun.jbi.headers.soap";
    private static final String HTTP_HEADERS_PROPERTY = "com.sun.jbi.headers.http";
    
    private static Messages mMessages = Messages.getMessages(XmlPostNormalizer.class);
    private static Logger mLog = Messages.getLogger(XmlPostNormalizer.class);
    private static TransformerFactory cTransformerFactory = TransformerFactory.newInstance();
    
    private boolean mIsHeaderCopyEnabled = false; // Feature disabled because BPELSE does not yet support it
    
    DocumentBuilder mBuilder = null;    
    
    WrapperBuilder wrapperBuilder;

    /** Creates a new instance of XmlNormalizer */
    public XmlPostNormalizer() throws MessagingException {
        try {
            wrapperBuilder = HelperFactory.createBuilder();        
        } catch (WrapperProcessingException ex) {
            throw new MessagingException("Failed to create wrapper builder", ex);
        }

        try {        
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            throw new MessagingException("Failed to create document builder", ex);
        }
        
    }
    
    /**
     * Note that it is not safe to call this method concurrently on the same instance of XmlNormalizer,
     * so only one thread should work on a specific instance.
     *
     * To prevent a regression, it should be noted that this method was not safe as a static as neither the 
     * documentbuilder or builder instance are guaranteed to be thread safe.
     */
    private Document newDocument() throws ParserConfigurationException {
        Document doc = null;
        // As long as each instance is only be used by a single thread at the same time we do not want to synchronize
        // synchronized(mBuilder) {
            doc = mBuilder.newDocument();
        // }
            
        return doc;
    }
    
    public NormalizedMessage normalize(Object message, MessageExchange exchange, OperationMetaData meta, boolean inMsg, MessageContext context) throws MessagingException {
        NormalizedMessage normalMsg = exchange.createMessage();
        try {
            Transformer transformer = cTransformerFactory.newTransformer();
            DataSource sourceMsg = (DataSource) message;
            Document normalDoc = newDocument();
            Element normalRoot = null;
            
            QName type = null;
            String name = null;
            Message msg = null;
            if (inMsg) {
                name = meta.getInMsgName();
                msg = meta.getInputMessage();
            } else {
                name = meta.getOutMsgName();
                msg = meta.getOutputMessage();
            }

            wrapperBuilder.initialize(normalDoc, msg, name);
            Document xmlDoc = null;
            
            // Extract
            Collection parts = msg.getParts().values();
            if (!parts.isEmpty()) {
                for (Iterator partsIter = parts.iterator(); partsIter.hasNext();) {
                    Part part = (Part) partsIter.next();
                    QName elementName = part.getElementName();
                    if (elementName == null) {
                        String value = StringUtil.streamToString(sourceMsg.getInputStream(), "UTF-8");
                        Node node = normalDoc.createTextNode(value == null ? "" : value);
                        NodeListImpl nodeList = new NodeListImpl();
                        nodeList.addItem(node);
                        wrapperBuilder.addPart(part.getName(), nodeList);
                    } else {
                        String localname = elementName.getLocalPart();
                        String uri = elementName.getNamespaceURI();
                        if (xmlDoc == null) {
                            xmlDoc = mBuilder.parse(sourceMsg.getInputStream());
                        }
                        NodeList nodeList = xmlDoc.getElementsByTagNameNS(uri, localname);
                        if (nodeList.getLength() > 0) {
                            wrapperBuilder.addPart(part.getName(), nodeList);
                        }
                    }
                }
            }
                

            normalDoc = wrapperBuilder.getResult();
            
            if (mIsHeaderCopyEnabled && context != null && inMsg) {
                processHTTPRequestHeaders(normalMsg, context);
            }
            
            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "Normalized message", normalDoc);
            }
            
            normalMsg.setContent(new DOMSource(normalDoc));
        } catch (TransformerConfigurationException ex) {
            throw new MessagingException("TransformerFactory creation error", ex);
        } catch (TransformerException ex) {
            throw new MessagingException("message transformation failed (source to result)", ex);
        } catch (SAXException ex) {
            String msg = mMessages.getString("HTTPBC-E00798.Normalize_fail");
            throw new MessagingException(msg, ex);
        } catch (IOException ex) {
            throw new MessagingException("parsing failed due to I/O error", ex);
        } catch (ParserConfigurationException tex) {
            throw new MessagingException("unable to obtain normalized message as a DOM document", tex);
        } catch (WrapperProcessingException ex) {
            String msg = mMessages.getString("HTTPBC-E00798.Normalize_fail");
            throw new MessagingException(msg, ex);
        } catch (ClassCastException ex) {
            String msg = mMessages.getString("HTTPBC-E00797.Normalize_fail_nonsource", message.getClass().toString());
            throw new MessagingException(msg, ex);
        }
        return normalMsg;
    }
    
    // Copies HTTP request headers from the context, into the normalized
    // message.
    private void processHTTPRequestHeaders(NormalizedMessage normalMsg, MessageContext context) {
        Map httpHeadersProperty = (Map) normalMsg.getProperty(HTTP_HEADERS_PROPERTY);
        if (httpHeadersProperty == null) {
            httpHeadersProperty = new HashMap<String, String>();
            normalMsg.setProperty(HTTP_HEADERS_PROPERTY, httpHeadersProperty);
        }
        Map requestHeaders = (Map) context.get(MessageContext.HTTP_REQUEST_HEADERS);
        for (Iterator iter = requestHeaders.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry) iter.next();
            String key = (String) entry.getKey();
            // JAX-WS Packet implementation populates each header's value
            // as a single value contained in a singleton list.
            String value = (String) ((List) entry.getValue()).get(0);
            httpHeadersProperty.put(key, value);
        }
    }
}
