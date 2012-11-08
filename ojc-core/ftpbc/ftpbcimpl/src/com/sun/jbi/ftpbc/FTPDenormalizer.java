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
 * @(#)FTPDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderException;
import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.ftpbc.Endpoint.EndpointMessageType;
import com.sun.jbi.ftpbc.extensions.FTPConstants;
import com.sun.jbi.ftpbc.extensions.FTPTransferExtension;
import com.sun.jbi.ftpbc.extensions.FTPOperation;
import com.sun.jbi.ftpbc.util.WSDLUtilities;
import com.sun.jbi.ftpbc.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.activation.DataHandler;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * This is the class that denormalizes a Normalized Message
 * for FTP BC.
 * @author jim.fu@sun.com
 */
public class FTPDenormalizer {

    private static final Messages mMessages = Messages.getMessages(FTPDenormalizer.class);

    public InputStream denormalize(NormalizedMessage normalizedMessage,
            QName operationName,
            Endpoint endpoint,
            FTPTransferExtension extElem) throws IOException, EncoderNotFoundException, EncoderException, NormalizationException, TransformerException {

        DOMResult result = new DOMResult();
        Source src = normalizedMessage.getContent();
        if (src != null) {
            TransformerFactory fact = TransformerFactory.newInstance();
            Transformer transformer = fact.newTransformer();
            transformer.transform(src, result);
        }

        return getMessagePartPayload(normalizedMessage, endpoint, operationName, extElem, result);
    }

    private InputStream getMessagePartPayload(NormalizedMessage normalizedMessage,
            Endpoint endpoint,
            QName operationName,
            FTPTransferExtension extElem,
            DOMResult domResult) throws IOException, EncoderNotFoundException, EncoderException, NormalizationException {
        byte[] result = null;

        Node node = domResult.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }

        WrapperParser wrapperParser = null;

        try {
            wrapperParser = HelperFactory.createParser();
            wrapperParser.parse(normalizedDoc, endpoint.getDefinition());
        } catch (WrapperProcessingException ex) {
            throw new NormalizationException("", ex);
        }

        // locate the WSDL message
        Message wsdlMessage = null;
        Map operations = endpoint.getOperations();
        Map operationMeps = endpoint.getOperationMsgExchangePattern();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        FTPOperation operation = null;
        Part aPart = null;

        boolean inout = false;
        String mep = null;
        String use = ((FTPTransferExtension) extElem).getUse();
        String encodingStyle = ((FTPTransferExtension) extElem).getEncodingStyle();
        String partName = ((FTPTransferExtension) extElem).getPart();

        QName fqn = new QName(endpoint.getServiceName().getNamespaceURI(), operationName.getLocalPart());

        if (operations.get(fqn) == null) {
            throw new NormalizationException(mMessages.getString("FTPBC-E005001.FDNMR_Invalid_opname", operationName));
        }

        mep = (String) operationMeps.get(fqn);

        if (mep == null || mep.equals(EndpointMessageType.UNSUPPORTED)) {
            throw new NormalizationException(mMessages.getString("FTPBC-E005002.FDNMR_Invalid_mep", operationName));
        }

        if (mep.equals(EndpointMessageType.IN_OUT)) {
            inout = true;
        }

        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();

        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation) it.next();
            if (op.getName().equals(operationName.toString()) ||
                    op.getName().equals(operationName.getLocalPart())) {
                if (inout) {
                    wsdlMessage = op.getOutput().getMessage();
                } else {
                    wsdlMessage = op.getInput().getMessage();
                }
                break;
            }
        }

        if (wsdlMessage == null) {
            throw new NormalizationException(mMessages.getString("FTPBC-E005003.FDNMR_Invalid_message", operationName));
        }

        /**
         * Assumption: one message part for each ftp operation only.
         * Otherwise, the "part" attribute for ftp:transfer (or ftp:message, etc) element is required.
         * This assumption will be enforced as part of WSDL validation at design time.
         */        // get the message part
        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        try {
            NodeList parts = wrapperParser.getPartNodes(partName);
            Node aNode = (Node) parts.item(0);

            boolean isBinary = false;
            if (aPart.getTypeName() != null) {
                String uri = aPart.getTypeName().getNamespaceURI();
                if (uri != null) {
                    isBinary = (uri.equals("http://www.w3.org/2001/XMLSchema") || uri.equals("http://www.w3.org/1999/XMLSchema")) && "base64Binary".equals(aPart.getTypeName().getLocalPart());
                }
            }

            if (isBinary) {
                if (WrapperUtil.isNodeXopInclude(aNode)) {
                    String contentId = WrapperUtil.getXopContentId(aNode);
                    DataHandler dataHandler = normalizedMessage.getAttachment(contentId);
                    InputStream in = dataHandler.getInputStream();
                    return in;
                }
                result = Base64Utils.base64DecodeToByte(aNode.getNodeValue());
            } else {
                String encoding = extElem.getCharacterEncoding();
                if (encoding != null) {
                    encoding = encoding.trim();
                }
                if (encoding != null && encoding.trim().length() == 0) {
                    encoding = null;
                }
                if (use.equals(FTPConstants.EXT_ELEM_ATTR_USE_ENCODED)) {
                    // Locate the encoder
                    Encoder encoder = (Encoder) partMappings.get(wsdlMessage.getQName() + partName);
                    if (encoder == null) {
                        throw new EncoderNotFoundException(mMessages.getString("FTPBC-E005004.FDNMR_Invalid_encodingStyle",
                                new Object[]{
                                    wsdlMessage.getQName() + partName,
                                    use,
                                    encodingStyle
                                }));
                    }
                    // Encode DOM source to raw data format
                    byte[] bytes = getPartIfAvailableAsAttachment(aNode, normalizedMessage);
                    Source source;
                    if (bytes != null) {
                        Document doc = null;
                        if (encoding != null) {
                            doc = XmlUtil.createDocumentFromXMLStreamReader(true, new StringReader(new String(bytes, encoding)));
                        } else {
                            doc = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(bytes));
                        }
                        source = new DOMSource(doc);
                    } else {
                        source = new DOMSource(aNode);
                    }
                    result = encoder.encodeToBytes(source);
                } else {
                    // XML element node vs. simple Text node?
                    result = getPartIfAvailableAsAttachment(aNode, normalizedMessage);
                    if (result == null) {
                        // XML element node vs. simple Text node?
                        if (aPart.getElementName() != null) {
                            // May want to add basic xml validation later
                            result = XmlUtil.transformToBytes(aNode, encoding != null ? encoding : "UTF-8", false, "xml");
                        } else { // must be "type" otherwise there would've been a WSDL validation error
                            // We may still be dealing with XML node
                            QName typename = aPart.getTypeName();
                            if (!WSDLUtilities.isBuiltInType(typename) || WSDLUtilities.isXsdAnyType(typename)) {
                                result = XmlUtil.transformToBytes(aNode, encoding != null ? encoding : "UTF-8", false, "xml");
                            } else {
                                // treat it as Text node
                                result = XmlUtil.transformToBytes(aNode, encoding != null ? encoding : "UTF-8", false, "text");
                            }
                        }
                    }
                }
            }
        } catch (SAXException sax) {
            throw new NormalizationException("Exception when de-normalizing message", sax);
        } catch (TransformerConfigurationException tce) {
            throw new NormalizationException("Exception when de-normalizing message", tce);
        } catch (TransformerException te) {
            throw new NormalizationException("Exception when de-normalizing message", te);
        } catch (WrapperProcessingException wpe) {
            throw new NormalizationException("Exception when de-normalizing message", wpe);
        } catch (ParserConfigurationException pce) {
            throw new NormalizationException("Exception when de-normalizing message", pce);
        }

        return new ByteArrayInputStream(result);
    }

    private byte[] getPartIfAvailableAsAttachment(Node aNode, NormalizedMessage nm) throws IOException {
        if (!WrapperUtil.isNodeXopInclude(aNode)) {
            return null;
        }
        String contentId = WrapperUtil.getXopContentId(aNode);
        DataHandler dataHandler = nm.getAttachment(contentId);
        InputStream in = dataHandler.getInputStream();
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        byte[] bytes = new byte[128];
        int len;
        while ((len = in.read(bytes)) != -1) {
            bout.write(bytes, 0, len);
        }
        //reset inputstream if somewants to read again
        in.reset();
        return bout.toByteArray();
    }
}
