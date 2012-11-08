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
 * @(#)ExecNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import com.sun.encoder.Encoder;
import com.sun.jbi.execbc.Endpoint.EndpointMessageType;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.util.WSDLUtilities;
import com.sun.jbi.execbc.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

/**
 * This is the class that normalized a File BC input message.
 *
 */
public class ExecNormalizer {
    private static final Messages mMessages = Messages.getMessages(ExecNormalizer.class);
    
    private Transformer mTrans = null;
    private WrapperBuilder wrapperBuilder;
    private DocumentBuilder mBuilder = null;
    
    public ExecNormalizer() throws Exception {
        try {
            wrapperBuilder = HelperFactory.createBuilder();
        } catch (WrapperProcessingException ex) {
            throw new Exception(mMessages.getString("FNMR_Create_wrapper_builder_exception", ex.getMessage()), ex);
        }
        
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            throw new Exception(mMessages.getString("FNMR_Create_doc_builder_exception", ex.getMessage()), ex);
        }
        
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(mMessages.getString("FNMR_Create_transformer_exception", ex.getMessage()), ex);
        }
    }
    
    public NormalizedMessage normalize(MessageExchange exchange,
            QName operationName,
            Endpoint endpoint,
            ExecMessage fileMessage,
            byte[] data) throws Exception {
        Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        Map operationMeps = endpoint.getOperationMsgExchangePattern();
        QName fqOperationName = new QName(portType.getQName().getNamespaceURI(),
                operationName.getLocalPart());
        String mep = (String) operationMeps.get(fqOperationName);
        boolean outin = mep.equals(EndpointMessageType.OUT_IN);
        
        /**
         * Locate the operation we are interested in.
         * There may be multiple operations by the same name (operation overloading)
         * and the WSDL spec does not allow it. The uniqueness
         * should be guaranteed by the examination of input and/or output names.
         * The WSDL validation should've been enforced the uniqueness at design time.
         * For the time being, we will assume that we don't have operation overloading.
         */
        Message wsdlMessage = null;
        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation)it.next();
            if (op.getName().equals(operationName.toString()) ||
                    op.getName().equals(operationName.getLocalPart())) {
                if (outin) {
                    wsdlMessage = op.getOutput().getMessage();
                } else {
                    wsdlMessage = op.getInput().getMessage();
                }
                break;
            }
        }
        
        if (wsdlMessage == null) {
            throw new Exception(mMessages.getString("FNMR_Invalid_message", operationName));
        }
        
        WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
        wrapperBuilder.initialize(null,
                wsdlMessage,
                null);
        
        return buildMessagePayload(exchange, wsdlMessage, partMappings, wrapperBuilder, fileMessage, data);
    }
    
    private NormalizedMessage buildMessagePayload(MessageExchange exchange,
            Message wsdlMessage,
            Map partMappings,
            WrapperBuilder wrapperBuilder,
            ExecMessage execMessage,
            byte[] data)
            throws SAXException, IOException, Exception {
        NormalizedMessage normalizedMessage = exchange.createMessage();
        
        /**
         * File BC basically handles data from the file system and
         * so we are potentially dealing with any file format.
         * However, an important assumption has to be made, i.e.
         * we deal with one message part for each file operation only.
         * Otherwise, the "part" attribute for file:read element is required.
         * This assumption will be enforced as part of WSDL validation at design time.
         *
         */
        Document document = null;
        Part aPart = null;
        QName messageQName = wsdlMessage.getQName();
        String use = execMessage.getExecUseType();
        //For Exec BC, partName is always null, so only one part is assumed
        //String partName = execMessage.getPart();
        String partName = null;
        
        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
            if (aPart == null) {
                throw new Exception(mMessages.getString("FNMR_Invalid_no_part", partName));
            }
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[])parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }
        
        // It is safe to assume all WSDL validation are done by now.
        if (use.equals(ExecMessage.EXEC_USE_TYPE_ENCODED)) {
            Encoder encoder = (Encoder) partMappings.get(messageQName + partName);
            if (encoder == null) {
                throw new Exception(mMessages.getString("FNMR_Invalid_encodingStyle"));
            }
            
            // Decode raw data and add message part
            Source source = encoder.decodeFromBytes(data);
            Element element = getRootElement(source);
            wrapperBuilder.addPart(partName, element);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                document = XmlUtil.createDocumentFromXML(true, new String(data));
                Element element = document.getDocumentElement();
                wrapperBuilder.addPart(partName, element);
            } else { // must be "type" otherwise there would've been a WSDL validation error
                // We may still be dealing with XML node
                QName typename = aPart.getTypeName();
                if (!WSDLUtilities.isBuiltInType(typename.getNamespaceURI())) {
                    document = XmlUtil.createDocumentFromXML(true, new String(data));
                    Element element = document.getDocumentElement();
                    wrapperBuilder.addPart(partName, element);
                } else {
                    // treat it as Text node
                    document = mBuilder.newDocument();
                    // Note that we need to treat text based and binary data differently
                    // For now, we support text based data only
                    Text textNode = document.createTextNode(new String(data));
                    wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                }
            }
        }
        normalizedMessage.setContent(new DOMSource(wrapperBuilder.getResult()));
        
        return normalizedMessage;
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
                DOMResult result = XmlUtil.transformToDOMResult(mTrans, source);
                root = ((Document) result.getNode()).getDocumentElement();
            } catch (Exception e) {
                throw new Exception(mMessages.getString("FNMR_Failed_ConvertToDOM"));
            }
        }
        
        return root;
    }
}
