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
 * @(#)FileNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.sun.encoder.Encoder;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.util.FileStreamHandler;
import com.sun.jbi.filebc.util.WSDLUtilities;
import com.sun.jbi.filebc.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;

/**
 * This is the class that normalized a File BC input message.
 *
 */
public class FileNormalizer {

    private static final Messages mMessages = Messages.getMessages(FileNormalizer.class);
    private static Logger mLogger = Messages.getLogger(FileNormalizer.class);
    private static Transformer transformer = null;
    private WrapperBuilder wrapperBuilder;
    private static DocumentBuilder docBuilder = null;
    private static final long TWO_MB = 2 * 1024 * 1024;

    static {
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            docBuilder = dbf.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00708.FNMR_Create_doc_builder_exception", ex.getMessage()), ex);
        }

        try {
            TransformerFactory tf = TransformerFactory.newInstance();
            transformer = tf.newTransformer();
        } catch (TransformerConfigurationException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00709.FNMR_Create_transformer_exception", ex.getMessage()), ex);
        }        
    }

    public FileNormalizer() throws Exception {
        try {
            wrapperBuilder = HelperFactory.createBuilder();
        } catch (WrapperProcessingException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00707.FNMR_Create_wrapper_builder_exception", ex.getMessage()), ex);
        }
    }

    public NormalizedMessage normalize(MessageExchange exchange,
            QName operationName,
            Endpoint endpoint,
            FileMessage fileMessage,
            FileStreamHandler fileStreamHandler,
            boolean loadWholeMessage) throws Exception {
        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Map partMappings = endpoint.getMessagePartEncoderMapping();

        FileOperation fileOp = (FileOperation) endpoint.getFileOperations().get(
                new QName(portType.getQName().getNamespaceURI(), operationName.getLocalPart()));
        String verb = fileOp.getVerb();

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
            Operation op = (Operation) it.next();
            if (op.getName().equals(operationName.toString()) ||
                    op.getName().equals(operationName.getLocalPart())) {
                /**
                 * For Solicited Read, message type will be specified by
                 * Operation Output. For all other types of WSDLs, it should 
                 * be specified in Operation input.
                 */
                if (FileOperation.VERB_READ.equals(verb)) {
                    wsdlMessage = op.getOutput().getMessage();
                } else {
                    wsdlMessage = op.getInput().getMessage();
                }
                break;
            }
        }

        if (wsdlMessage == null) {
            throw new Exception(mMessages.getString("FILEBC-E00710.FNMR_Invalid_message", operationName));
        }

        wrapperBuilder.initialize(null,
                wsdlMessage,
                null);

        return buildMessagePayload(exchange, wsdlMessage, partMappings, wrapperBuilder, fileMessage, fileStreamHandler, loadWholeMessage);
    }

    private NormalizedMessage buildMessagePayload(MessageExchange exchange,
            Message wsdlMessage,
            Map partMappings,
            WrapperBuilder wrapperBuilder,
            FileMessage fileMessage,
            FileStreamHandler fileStreamHandler,
            boolean loadWholeMessage)
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
        String use = fileMessage.getFileUseType();
        String partName = fileMessage.getPart();

        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
            if (aPart == null) {
                throw new Exception(mMessages.getString("FILEBC-E00711.FNMR_Invalid_no_part", partName));
            }
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        //Before doing anything check if file message is tagged to forward as attachment or
        //if it is binary then we have to send it as attachment
        boolean isBinary = false;
        if (aPart.getTypeName() != null) {
            String uri = aPart.getTypeName().getNamespaceURI();
            if (uri != null) {
                isBinary = (uri.equals("http://www.w3.org/2001/XMLSchema") || uri.equals("http://www.w3.org/1999/XMLSchema")) && "base64Binary".equals(aPart.getTypeName().getLocalPart());
            }
        }

        byte[] data = null;
        if (fileStreamHandler != null) {
            if (fileStreamHandler.isMultipleRecords() && !isBinary) {
                if (fileStreamHandler.hasMoreRecords()) {
                    data = fileStreamHandler.readNextRecord();
                }
                if (data == null) {
                    return null; //no more data to process
                }
            }

            FileBCDataSource ds = null;
            boolean donotCloseStream = false;
            if (isBinary) {
                if (!fileStreamHandler.hasMoreRecords()) {
                    return null;
                }
                //Binary data can only be send as an attachment. Multiple records for binary data is not supported
                //Check the size of the file. If file is very large send the InputStream as an attachment if file
                //is less then 2 MB (This value is hard coded for now. Latter we can make is configurable property)
                if (!loadWholeMessage && fileStreamHandler.getSize() > TWO_MB) {
                    donotCloseStream = true;
                    ds = new FileBCDataSource(fileStreamHandler.getInpuStream(), partName);
                    mLogger.log(Level.WARNING, mMessages.getString("FILEBC-W00710.INPUT_BINARY_FILE_IS_VERY_SENT_INPUT_STREAM_AS_ATTACHMENT"));
                } else {
                    data = fileStreamHandler.getAllContentsAsBytes();
                    ds = new FileBCDataSource(new ByteArrayInputStream(data), partName);
                }
                String cid = wrapperBuilder.addPartWithAttachment(partName);
                normalizedMessage.addAttachment(cid, new DataHandler(ds));
            } else if (fileMessage.isForwardAsAttachment()) {
                //If it is text data then load the data and send it as attachment
                if (data == null) {
                    data = fileStreamHandler.getAllContentsAsBytes();
                }
                ds = new FileBCDataSource(new ByteArrayInputStream(data), partName);
                String cid = wrapperBuilder.addPartWithAttachment(partName);
                normalizedMessage.addAttachment(cid, new DataHandler(ds));
            } else {
                if (data == null) {
                    // open esb issue #2244: data can be null when the input file is 0 length
                    data = fileStreamHandler.getAllContentsAsBytes();
                }
                // It is safe to assume all WSDL validation are done by now.
                if (use.equals(FileMessage.FILE_USE_TYPE_ENCODED)) {
                    Encoder encoder = (Encoder) partMappings.get(messageQName + partName);
                    if (encoder == null) {
                        throw new Exception(mMessages.getString("FILEBC-E00712.FNMR_Invalid_encodingStyle"));
                    }
                    if (data == null) {
                        throw new Exception(mMessages.getString("FILEBC-E00780.FNMR_Empty_Msg_Payload_4_Encoded_Msg",
                                new Object[]{messageQName, aPart.getName(), fileMessage.getFileEncodingStyle(), aPart.getTypeName(), aPart.getElementName()}));
                    }
                    // Decode raw data and add message part
                    Source source = encoder.decodeFromBytes(data);
                    Element element = getRootElement(source);
                    wrapperBuilder.addPart(partName, element);
                } else {
                    // XML element node vs. simple Text node?
                    if (aPart.getElementName() != null) {
                        // May want to add basic xml validation later
                        // when data is null - it is not a valid XML - throw exception
                        if (data == null) {
                            throw new Exception(mMessages.getString("FILEBC-E00781.FNMR_Empty_Msg_Payload_4_Msg_of_XML_Elem",
                                    new Object[]{messageQName, aPart.getName(), aPart.getElementName()}));
                        }
                        document = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(data));
                        Element element = document.getDocumentElement();
                        wrapperBuilder.addPart(partName, element);
                    } else {
                        // must be "type" otherwise there would've been a WSDL validation error
                        // We may still be dealing with XML node
                        String charset = fileMessage.getCharacterEncoding();
                        QName typename = aPart.getTypeName();
                        if (WSDLUtilities.isXsdAnyType(typename)) {
                            // special treatment for xsd:any type
                            // we will try to load an an XML payload first
                            if (data != null) {
                                try {
                                    document = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(data));
                                    wrapperBuilder.addPart(partName, document.getDocumentElement());
                                } catch (SAXException e) {
                                    // well, this is not a complex type
                                    // treat it as a built-in type now
                                    wrapperBuilder.addPart(partName,
                                            new NodeListImpl(docBuilder.newDocument().createTextNode(
                                            (charset != null && charset.length() > 0) ? new String(data, charset) : new String(data))));
                                }
                            } else {
                                // text node with empty string text
                                wrapperBuilder.addPart(partName, new NodeListImpl(docBuilder.newDocument().createTextNode("")));
                            }
                        } else if (!WSDLUtilities.isBuiltInType(typename)) {
                            if (data == null) {
                                throw new Exception(mMessages.getString("FILEBC-E00782.FNMR_Empty_Msg_Payload_4_Msg_of_XML_Type",
                                        new Object[]{messageQName, aPart.getName(), aPart.getTypeName()}));
                            }
                            document = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(data));
                            wrapperBuilder.addPart(partName, document.getDocumentElement());
                        } else {
                            // treat it as Text node
                            // Note that we need to treat text based and binary data differently
                            // For now, we support text based data only
                            // open esb issue #2244: when it is a text node - it can be empty string
                            if (data != null) {
                                wrapperBuilder.addPart(partName,
                                        new NodeListImpl(docBuilder.newDocument().createTextNode(
                                        (charset != null && charset.length() > 0) ? new String(data, charset) : new String(data))));
                            } else {
                                // for empty string - no matter what charset is
                                wrapperBuilder.addPart(partName,
                                        new NodeListImpl(docBuilder.newDocument().createTextNode("")));
                            }
                        }
                    }
                }
            }
            normalizedMessage.setContent(new DOMSource(wrapperBuilder.getResult()));
            fileStreamHandler.setDonotClose(donotCloseStream);
        } else {
            normalizedMessage.setContent(new DOMSource(wrapperBuilder.getResult()));
        }

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
                DOMResult result = XmlUtil.transformToDOMResult(transformer, source);
                root = ((Document) result.getNode()).getDocumentElement();
            } catch (Exception e) {
                throw new Exception(mMessages.getString("FILEBC-E00713.FNMR_Failed_ConvertToDOM"));
            }
        }

        return root;
    }
}
