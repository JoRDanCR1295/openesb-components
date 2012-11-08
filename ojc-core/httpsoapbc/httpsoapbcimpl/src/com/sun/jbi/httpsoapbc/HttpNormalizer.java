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
 * @(#)HttpNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.httpsoapbc.util.WSDLUtilities;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataSource;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 * HttpNormalizer.java
 *
 */
public class HttpNormalizer {

    private static Messages mMessages = Messages.getMessages(HttpNormalizer.class);
    private static Logger mLog = Messages.getLogger(HttpNormalizer.class);
    private static DocumentBuilder docBuilder;

    static {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try{
            docBuilder = dbf.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            mLog.log(Level.SEVERE, mMessages.getString("HttpNormalizer.Could_not_create_HttpNormalizer_failed_to_create_document_builder", ex.getMessage()), ex);
        }
    }


    

    
    
    /** Creates a new instance of HttpNormalizer */
    public HttpNormalizer() throws MessagingException {
        
    }

    public NormalizedMessage normalize(Source msgContent, MessageExchange exchange, OperationMetaData meta, boolean inMsg) throws MessagingException {
        NormalizedMessage normalMsg = exchange.createMessage();
        try {
            Document normalDoc = docBuilder.newDocument();

            String name = null;
            Message msg = null;

            if (inMsg) {
                name = meta.getInMsgName();
                msg = meta.getInputMessage();
            } else {
                name = meta.getOutMsgName();
                msg = meta.getOutputMessage();
            }

            Element jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(normalDoc, msg.getQName(), name);
            normalDoc.appendChild(jbiMessageWrapper);

            Node part;
            if (msgContent instanceof StreamSource && isSimpleMessageType(msg)) {
                part = createRootElement(normalDoc, msgContent);
            } else {
                part = getRootElement(msgContent);
            }
            Element jbiMessagePart = WrapperUtil.importJBIWrappedPart(normalDoc, part);
            jbiMessageWrapper.appendChild(jbiMessagePart);

            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "Normalized message", normalDoc);
            }

            normalMsg.setContent(new DOMSource(normalDoc));
        } catch (Exception ex) {
            throw new MessagingException(ex);
        }
        return normalMsg;
    }

    public NormalizedMessage normalize(DataSource msgContent,
            MessageExchange exchange,
            OperationMetaData meta, boolean inMsg)
            throws MessagingException {
        NormalizedMessage normalMsg = exchange.createMessage();
        try {
            Document normalDoc = docBuilder.newDocument();
            Element normalRoot = null;

            String name = null;
            Message msg = null;

            if (inMsg) {
                name = meta.getInMsgName();
                msg = meta.getInputMessage();
            } else {
                name = meta.getOutMsgName();
                msg = meta.getOutputMessage();
            }

            Element jbiMessageWrapper = WrapperUtil.createJBIMessageWrapper(normalDoc, msg.getQName(), name);
            normalDoc.appendChild(jbiMessageWrapper);

            Node part = createRootElement(normalDoc, msgContent);
            Element jbiMessagePart = WrapperUtil.importJBIWrappedPart(normalDoc, part);
            jbiMessageWrapper.appendChild(jbiMessagePart);

            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "Normalized message", normalDoc);
            }

            normalMsg.setContent(new DOMSource(normalDoc));
        } catch (Exception ex) {
            throw new MessagingException(ex);
        }
        return normalMsg;
    }

    private Element getRootElement(Source source) throws Exception {
        Element root = null;
        if (source instanceof DOMSource) {
            Node sourceNode = ((DOMSource) source).getNode();
            if (sourceNode instanceof Element) {
                root = (Element) sourceNode;
            } else if (sourceNode instanceof Document) {
                root = ((Document) sourceNode).getDocumentElement();
            }
        } else {
            // convert Source to DOMResult
            try {
                DOMResult result = new DOMResult();

                Transformer trans = TransformerFactory.newInstance().newTransformer();
                trans.transform(source, result);
                root = ((Document) result.getNode()).getDocumentElement();
            } catch (Exception e) {
                throw new Exception(mMessages.getString("HTTPBC-E00798.Normalize_fail"), e);
            }
        }

        return root;
    }

    private Node createRootElement(Document normalDoc, Source msgContent) throws IOException {
        Node root = null;

        final int CHARBUF_SIZE = 8192;

        StreamSource streamSource = (StreamSource) msgContent;

        // TODO: UTF-8 encoding is an assumption. How to eliminate
        // this uncertainty?  Encoding information is not available
        // from JAX-WS; all I get is a Source object.
        InputStreamReader streamReader =
                new InputStreamReader(streamSource.getInputStream(), "UTF-8");

        char[] charBuf = new char[CHARBUF_SIZE];
        StringBuffer dataBuf = new StringBuffer();
        int readcnt = 0;
        while ((readcnt = streamReader.read(charBuf)) != -1) {
            dataBuf.append(charBuf, 0, readcnt);
        }
        String content = dataBuf.toString();
        if ("".equals(content)) {
            throw new IOException(mMessages.getString("HTTPBC-E00784.Blank_message"));
        }
        return normalDoc.createTextNode(content);
    }

    private Node createRootElement(Document normalDoc, DataSource msgContent) throws IOException {
        Node root = null;

        final int CHARBUF_SIZE = 8192;

        // TODO: UTF-8 encoding is an assumption. How to eliminate
        // this uncertainty?  Encoding information is not available
        // from JAX-WS; all I get is a DataSource object.
        InputStreamReader streamReader =
                new InputStreamReader(msgContent.getInputStream(), "UTF-8");

        char[] charBuf = new char[CHARBUF_SIZE];
        StringBuffer dataBuf = new StringBuffer();
        int readcnt = 0;
        while ((readcnt = streamReader.read(charBuf)) != -1) {
            dataBuf.append(charBuf, 0, readcnt);
        }
        root = normalDoc.createTextNode(dataBuf.toString());
        return root;
    }

    private boolean isSimpleMessageType(Message msgDef) {
        boolean isSimple = true;
        List<Part> partsList = msgDef.getOrderedParts(null);
        ListIterator<Part> iter = partsList.listIterator();
        do {
            Part p = iter.next();
            isSimple = p.getElementName() == null && WSDLUtilities.isBuiltInType(p.getTypeName().getNamespaceURI());
        } while (isSimple && iter.hasNext());

        return isSimple;
    }
}
