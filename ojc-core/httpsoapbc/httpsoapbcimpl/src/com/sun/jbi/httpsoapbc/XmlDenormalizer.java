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
 * @(#)XmlDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.util.WSDLUtilities;
import com.sun.jbi.httpsoapbc.util.Util;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import java.util.List;
import java.util.ListIterator;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.activation.DataSource;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.Fault;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.TransformerException;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Part;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;

/**
 *
 */
public class XmlDenormalizer {

    private static Messages mMessages = Messages.getMessages(XmlDenormalizer.class);
    private static Logger mLog = Messages.getLogger(XmlDenormalizer.class);
    private DocumentBuilderFactory mBuilderFactory;
    private DocumentBuilder builder;
    
    /** Creates a new instance */
    public XmlDenormalizer() {
        try {
            mBuilderFactory = DocumentBuilderFactory.newInstance();
            builder = mBuilderFactory.newDocumentBuilder();
        } catch (Exception e) {
            mLog.log(Level.SEVERE, e.getMessage(), e);
        }
    }
    
    /**
     * Only a single thread should invoke this method on the same instance at the same time
     * as it uses the builder instance member variable without guard against concurrency
     */
    public DataSource denormalize(NormalizedMessage normalizedMessage, MessageExchange exchange, OperationMetaData meta, boolean inMsg)
        throws MessagingException {
        
        try {
            Node node = Util.messageAsDom(normalizedMessage);
            Document normalDoc = null;
            if (node != null) {
                if (node instanceof Document) {
                    normalDoc = (Document) node;
                } else {
                    normalDoc = ((Element) node).getOwnerDocument();
                }
            }
            
            WrapperParser wrapperParser = HelperFactory.createParser();
            if (Fault.class.isInstance(normalizedMessage)) {
                wrapperParser.parse(normalDoc, meta.getFullDefinition());
            } else {
                Message msgDef = null;
                if (inMsg) {
                    msgDef = meta.getInputMessage();
                } else {
                    msgDef = meta.getOutputMessage();
                }
                wrapperParser.parse(normalDoc, msgDef);
            }
            return processMessage(wrapperParser, meta.getFullDefinition());
        } catch (TransformerException tex) {
            throw new MessagingException("Unable to obtain normalized message as DOM", tex);
        } catch (WrapperProcessingException ex) {
            String msg = mMessages.getString("HTTPBC-E00799.Denormalize_fail");
            throw new MessagingException(msg, ex);
        }
    }
    
    private DataSource processMessage(WrapperParser wrapperParser,
                                  Definition wsDef)
        throws MessagingException, WrapperProcessingException {
        
        DataSource result =  null;
        
        Message msgDef = wsDef.getMessage(wrapperParser.getMessageType());
        
        if (isSimpleMessageType(msgDef)) {
            String[] partNames = wrapperParser.getPartNames();
            StringBuffer contentBuffer = new StringBuffer();
            for (int i = 0; i < partNames.length; ++i) {
                NodeList nodeList = wrapperParser.getPartNodes(partNames[i]);
                for (int n = 0, N = nodeList.getLength(); n < N; ++n) {
                    Node node = nodeList.item(n);
                    contentBuffer.append(node.getTextContent());
                }
            }
            result = new StringDataSourceImpl(msgDef.getQName(), contentBuffer.toString());
        } else {
            Document document = builder.newDocument();
            String[] partNames = wrapperParser.getPartNames();
            for (int i = 0; i < partNames.length; ++i) {
                NodeList nodeList = wrapperParser.getPartNodes(partNames[i]);
                for (int n = 0, N = nodeList.getLength(); n < N; ++n) {
                    Node node = nodeList.item(n);
                    if (node != null) {
                        copyNode(document, node);
                    }
                }
            }
            result = new DOMDataSourceImpl(msgDef.getQName(), document);
        }
        
        return result;
    }
    
    private boolean isSimpleMessageType(Message msgDef) {
        boolean isSimple = true;
        List<Part> partsList = msgDef.getOrderedParts(null);
        ListIterator<Part> iter = partsList.listIterator();
        do {
            Part p = iter.next();
            isSimple = p.getElementName() == null
                    && WSDLUtilities.isBuiltInType(p.getTypeName().getNamespaceURI());
        } while (isSimple && iter.hasNext());
        
        return isSimple;
    }
    
    private void copyNode(Document document, Node node) {
        node = document.importNode(node, true);
        document.appendChild(node);
        /*
        NodeList nl = src.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node n = nl.item(i);
            n = dest.getOwnerDocument().importNode(n, true);
            dest.appendChild(n);
        }
         */
        
        NamedNodeMap attrs = node.getAttributes();
        NamedNodeMap destAttrs = document.getAttributes();
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                Node attr = attrs.item(i);
                Node n = document.importNode(attr, true);
                if (destAttrs != null) {
                    destAttrs.setNamedItemNS(n);
                }
            }
        }
    }

}
