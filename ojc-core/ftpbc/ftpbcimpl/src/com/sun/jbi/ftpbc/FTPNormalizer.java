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
 * @(#)FTPNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderException;
import com.sun.jbi.ftpbc.extensions.FTPConstants;
import com.sun.jbi.ftpbc.extensions.FTPTransferExtension;
import com.sun.jbi.ftpbc.util.FTPInputStreamWrapper;
import com.sun.jbi.ftpbc.util.WSDLUtilities;
import com.sun.jbi.ftpbc.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import javax.xml.namespace.QName;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.activation.DataHandler;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

/**
 * This is the class that normalized a FTP BC input message.
 * @author jim.fu@sun.com
 */
public class FTPNormalizer {

    private static final Messages mMessages = Messages.getMessages(FTPNormalizer.class);
    private Transformer mTrans = null;
    private WrapperBuilder wrapperBuilder;
    private DocumentBuilder mBuilder = null;

    public FTPNormalizer() throws NormalizerException {
        try {
            wrapperBuilder = HelperFactory.createBuilder();
        } catch (WrapperProcessingException ex) {
            throw new NormalizerException(mMessages.getString("FTPBC-E005005.FNMR_Create_wrapper_builder_exception", ex.getLocalizedMessage()), ex);
        }

        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            throw new NormalizerException(mMessages.getString("FTPBC-E005006.FNMR_Create_doc_builder_exception", ex.getLocalizedMessage()), ex);
        }

        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new NormalizerException(mMessages.getString("FTPBC-E005007.FNMR_Create_transformer_exception", ex.getLocalizedMessage()), ex);
        } catch (TransformerConfigurationException ex2) {
            throw new NormalizerException(mMessages.getString("FTPBC-E005007.FNMR_Create_transformer_exception", ex2.getLocalizedMessage()), ex2);
        }
    }

    public NormalizedMessage normalize(MessageExchange exchange,
            QName operationName,
            Endpoint endpoint,
            FTPTransferExtension extElem,
            FTPInputStreamWrapper inStream,
            boolean loadWholeContent,
            boolean useInput) throws IOException, EncoderException, MessagingException, NormalizationException, EncoderNotFoundException {
        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Map partMappings = endpoint.getMessagePartEncoderMapping();

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
                wsdlMessage = useInput ? op.getInput().getMessage() : op.getOutput().getMessage();
                break;
            }
        }

        if (wsdlMessage == null) {
            throw new NormalizationException(mMessages.getString("FTPBC-E005008.FNMR_Invalid_message", operationName));
        }

        try {
            wrapperBuilder.initialize(null,
                    wsdlMessage,
                    null);
        } catch (WrapperProcessingException wpe) {
            throw new NormalizationException("Exception when building DOM for the message", wpe);
        }

        NormalizedMessage nmsg = exchange.createMessage();

        Map<String, DataHandler> attachments = new HashMap<String, DataHandler>();
        Source src = buildMessagePayload(wsdlMessage,
                partMappings,
                wrapperBuilder,
                extElem,
                inStream,
                loadWholeContent,
                attachments);

        nmsg.setContent(src);

        if (attachments.size() > 0) {
            Set<Entry<String, DataHandler>> es = attachments.entrySet();
            Iterator it = es.iterator();
            while (it.hasNext()) {
                Entry<String, DataHandler> entry = (Entry<String, DataHandler>) it.next();
                nmsg.addAttachment(entry.getKey(), entry.getValue());
            }
        }
        return nmsg;
    }

    public Source normalize(QName operationName,
            Endpoint endpoint,
            FTPTransferExtension extElem,
            FTPInputStreamWrapper inStream,
            boolean loadWholeContent,
            Map<String, DataHandler> attachments) throws NormalizationException, EncoderException, IOException, EncoderNotFoundException {
        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Map partMappings = endpoint.getMessagePartEncoderMapping();

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
                wsdlMessage = op.getInput().getMessage();
                break;
            }
        }

        if (wsdlMessage == null) {
            throw new NormalizationException(mMessages.getString("FTPBC-E005008.FNMR_Invalid_message", operationName));
        }

        try {
            wrapperBuilder.initialize(null, wsdlMessage, null);
        } catch (WrapperProcessingException ex) {
            throw new NormalizationException("Exception when building DOM for the message", ex);
        }

        return buildMessagePayload(wsdlMessage, partMappings, wrapperBuilder, extElem, inStream, loadWholeContent, attachments);
    }

    private Source buildMessagePayload(Message wsdlMessage,
            Map partMappings,
            WrapperBuilder wrapperBuilder,
            FTPTransferExtension extElem,
            FTPInputStreamWrapper inStream,
            boolean loadWholeContent,
            Map attachments)
            throws IOException, EncoderException, NormalizationException, EncoderNotFoundException {

        /**
         * Assumption: one message part for each ftp operation only.
         * Otherwise, the "part" attribute for ftp:transfer (or ftp:message, etc) element is required.
         * This assumption will be enforced as part of WSDL validation at design time.
         */
        Document document = null;
        Part aPart = null;
        QName messageQName = wsdlMessage.getQName();

        String encoding = extElem.getCharacterEncoding();

        if (encoding != null) {
            encoding = encoding.trim();
        }
        if (encoding != null && encoding.length() == 0) {
            encoding = null;
        }
        String use = ((FTPTransferExtension) extElem).getUse();
        String encodingStyle = ((FTPTransferExtension) extElem).getEncodingStyle();
        String partName = ((FTPTransferExtension) extElem).getPart();

        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        // processing attachment
        boolean isBinary = false;
        if (aPart.getTypeName() != null) {
            String uri = aPart.getTypeName().getNamespaceURI();
            if (uri != null) {
                isBinary = (uri.equals("http://www.w3.org/2001/XMLSchema") || uri.equals("http://www.w3.org/1999/XMLSchema")) && "base64Binary".equals(aPart.getTypeName().getLocalPart());
            }
        }

        DOMSource src = null;

        // It is safe to assume all WSDL validation are done by now.
        try {

            byte[] data = null;

            if (inStream != null) {
                FTPBCDataSource ds = null;
                boolean donotCloseStream = false;
                if (isBinary) {
                    if (loadWholeContent) {
                        data = inStream.getAllContentsAsBytes();
                        ds = new FTPBCDataSource(new ByteArrayInputStream(data), partName);
                    } else {
                        donotCloseStream = true;
                        ds = new FTPBCDataSource(inStream.getInpuStream(), partName);
                    }
                    String cid = wrapperBuilder.addPartWithAttachment(partName);
                    attachments.put(cid, new DataHandler(ds));
                } else {
                    data = inStream.getAllContentsAsBytes();
                    if (extElem.getForwardAsAttachment()) {
                        ds = new FTPBCDataSource(new ByteArrayInputStream(data), partName);
                        String cid = wrapperBuilder.addPartWithAttachment(partName);
                        attachments.put(cid, new DataHandler(ds));
                    } else {
                        if (use.equals(FTPConstants.EXT_ELEM_ATTR_USE_ENCODED)) {
                            Encoder encoder = (Encoder) partMappings.get(messageQName + partName);
                            if (encoder == null) {
                                throw new EncoderNotFoundException(mMessages.getString("FTPBC-E005010.FNMR_Invalid_encodingStyle",
                                        new Object[]{
                                            messageQName + partName,
                                            use,
                                            encodingStyle
                                        }));
                            }
                            Source source = null;
                            // Decode raw data and add message part
                            // use explicit encoding if set by application
                            if (encoding != null && encoding.length() > 0) {
                                source = encoder.decodeFromString(new String(data, encoding));
                            } else {
                                source = encoder.decodeFromBytes(data);
                            }
                            Element element = null;
                            try {
                                element = getRootElement(source);
                            } catch (TransformerException ex) {
                                throw new NormalizationException(mMessages.getString("FTPBC-E005011.FNMR_Failed_ConvertToDOM"), ex);
                            }
                            wrapperBuilder.addPart(partName, element);
                        } else {
                            // adopt some patches from file BC
                            // XML element node vs. simple Text node?
                            if (aPart.getElementName() != null) {
                                // May want to add basic xml validation later
                                if (encoding == null) {
                                    document = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(data));
                                } else {
                                    document = XmlUtil.createDocumentFromXMLStreamReader(true, new StringReader(new String(data, encoding)));
                                }
                                Element element = document.getDocumentElement();
                                wrapperBuilder.addPart(partName, element);
                            } else { // must be "type" otherwise there would've been a WSDL validation error
                                // We may still be dealing with XML node
                                QName typename = aPart.getTypeName();
                                if (WSDLUtilities.isXsdAnyType(typename)) {
                                    // special treatment for xsd:any type
                                    // we will try to load an an XML payload first
                                    try {
                                        if (encoding == null) {
                                            document = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(data));
                                        } else {
                                            document = XmlUtil.createDocumentFromXMLStreamReader(true, new StringReader(new String(data, encoding)));
                                        }
                                        Element element = document.getDocumentElement();
                                        wrapperBuilder.addPart(partName, element);
                                    } catch (SAXException e) {
                                        // fallback:
                                        // treat it as a built-in type now
                                        document = mBuilder.newDocument();
                                        Text textNode = document.createTextNode(encoding != null ? new String(data, encoding) : new String(data));  // use default encoding for the locale
                                        wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                                    }
                                } else if (!WSDLUtilities.isBuiltInType(typename)) {
                                    if (encoding == null) {
                                        document = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(data));
                                    } else {
                                        document = XmlUtil.createDocumentFromXMLStreamReader(true, new StringReader(new String(data, encoding)));
                                    }
                                    Element element = document.getDocumentElement();
                                    wrapperBuilder.addPart(partName, element);
                                } else {
                                    // treat it as Text node
                                    document = mBuilder.newDocument();
                                    // Note that we need to treat text based and binary data differently
                                    // For now, we support text based data only
                                    Text textNode = document.createTextNode(encoding != null ? new String(data, encoding) : new String(data));  // use default encoding for the locale
                                    wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                                }
                            }
                        }
                    }
                }
                inStream.setDonotClose(donotCloseStream);
            }
            src = new DOMSource(wrapperBuilder.getResult());
        } catch (WrapperProcessingException wpe) {
            throw new NormalizationException("Exception when normalizing message", wpe);
        } catch (SAXException sax) {
            throw new NormalizationException("Exception when normalizing message", sax);
        } catch (ParserConfigurationException pce) {
            throw new NormalizationException("Exception when normalizing message", pce);
        }

        return src;
    }

    private Element getRootElement(Source source) throws TransformerException {
        Element root = null;
        if (source instanceof DOMSource) {
            DOMSource domSource = (DOMSource) source;
            Node srcNode = domSource.getNode();
            if (srcNode instanceof Element) {
                root = (Element) srcNode;
            } else if (srcNode instanceof Document) {
                root = ((Document) srcNode).getDocumentElement();
            }
        } else {
            // convert Source to DOMResult
//            try {
            DOMResult result = XmlUtil.transformToDOMResult(mTrans, source);
            root = ((Document) result.getNode()).getDocumentElement();
//            } catch (Exception e) {
//                throw new Exception(mMessages.getString("FTPBC-E005011.FNMR_Failed_ConvertToDOM", new Object[]{e.getLocalizedMessage()}));
//            }
        }
        return root;
    }
}
