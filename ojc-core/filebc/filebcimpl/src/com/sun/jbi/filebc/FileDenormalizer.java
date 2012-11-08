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
 * @(#)FileDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.encoder.Encoder;
import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.filebc.Endpoint.EndpointMessageType;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.filebc.util.WSDLUtilities;
import com.sun.jbi.filebc.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
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
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This is the class that denormalizes a Normalized Message
 * for File BC.
 *
 */
public class FileDenormalizer {

    private static final Messages mMessages = Messages.getMessages(FileDenormalizer.class);

    public InputStream denormalize(NormalizedMessage normalizedMessage,
            QName operationName,
            Endpoint endpoint,
            FileMessage fileMessage) throws Exception {
        DOMResult result = new DOMResult();
        Source src = normalizedMessage.getContent();
        if (src != null) {
            TransformerFactory fact = TransformerFactory.newInstance();
            Transformer transformer = fact.newTransformer();
            transformer.transform(src, result);
        }
        Node node = result.getNode();
        if (node == null) {
            throw new Exception(mMessages.getString("FILEBC-E00701.FDNMR_Invalid_NM_content_null"));
        }
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }

        WrapperParser wrapperParser = HelperFactory.createParser();
        wrapperParser.parse(normalizedDoc, endpoint.getDefinition());

        return getMessagePartPayload(normalizedMessage, endpoint, operationName, fileMessage, wrapperParser);
    }

    private InputStream getMessagePartPayload(NormalizedMessage normalizedMessage,
            Endpoint endpoint,
            QName operationName,
            FileMessage fileMessage,
            WrapperParser wrapperParser) throws Exception {
        byte[] result = null;

        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();

        // locate the WSDL message
        Message wsdlMessage = null;
        Map operations = endpoint.getFileOperations();
        Map operationMeps = endpoint.getOperationMsgExchangePattern();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        Part aPart = null;

        boolean inout = false;
        String mep = null;
        String use = fileMessage.getFileUseType();
        String partName = fileMessage.getPart();
        QName fqOperationName = new QName(portType.getQName().getNamespaceURI(),
                operationName.getLocalPart());
        if (operations.get(fqOperationName) == null) {
            throw new Exception(mMessages.getString("FILEBC-E00702.FDNMR_Invalid_opname", operationName));
        }
        mep = (String) operationMeps.get(fqOperationName);
        if (mep == null || mep.equals(EndpointMessageType.UNSUPPORTED)) {
            throw new Exception(mMessages.getString("FILEBC-E00703.FDNMR_Invalid_mep", operationName));
        }

        if (mep.equals(EndpointMessageType.IN_OUT)) {
            inout = true;
        }

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
            throw new Exception(mMessages.getString("FILEBC-E00704.FDNMR_Invalid_message", operationName));
        }

        /**
         * File BC basically handles data from the file system and
         * so we are potentially dealing with any file format.
         * However, an important assumption has to be made, i.e.
         * we deal with one message part for each file operation only.
         * Otherwise, the "part" attribute for file:read element is required.
         * This assumption will be enforced as part of WSDL validation at design time.
         *
         */
        // get the message part
        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
            if (aPart == null) {
                throw new Exception(mMessages.getString("FILEBC-E00705.FDNMR_Invalid_no_part", partName));
            }
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        NodeList parts = wrapperParser.getPartNodes(partName);
        Node aNode = (Node) parts.item(0);

        //Here first check if message type is binary
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
            if (use.equals(FileMessage.FILE_USE_TYPE_ENCODED)) {
                // Locate the encoder
                Encoder encoder = (Encoder) partMappings.get(wsdlMessage.getQName() + partName);
                if (encoder == null) {
                    throw new Exception(mMessages.getString("FILEBC-E00706.FDNMR_Invalid_encodingStyle"));
                }
                // Encode DOM source to raw data format
                byte[] bytes = getPartIfAvailableAsAttachment(aNode, normalizedMessage);
                Source source;
                if (bytes != null) {
                    Document doc = XmlUtil.createDocumentFromXMLStream(true, new ByteArrayInputStream(bytes));
                    source = new DOMSource(doc);
                } else {
                    source = new DOMSource(aNode);
                }
                result = encoder.encodeToBytes(source);
            } else {
                result = getPartIfAvailableAsAttachment(aNode, normalizedMessage);
                if (result == null) {
                    // XML element node vs. simple Text node?
                    if (aPart.getElementName() != null) {
                        // May want to add basic xml validation later
                        result = XmlUtil.transformToBytes(aNode, fileMessage.getCharacterEncoding(), false, "xml");
                    } else {
                        // must be "type" otherwise there would've been a WSDL validation error
                        // We may still be dealing with XML node
                        QName typename = aPart.getTypeName();
                        if (!WSDLUtilities.isBuiltInType(typename) || WSDLUtilities.isXsdAnyType(typename)) {
                            result = XmlUtil.transformToBytes(aNode, fileMessage.getCharacterEncoding(), false, "xml");
                        } else {
                            // treat it as Text node
                            result = XmlUtil.transformToBytes(aNode, fileMessage.getCharacterEncoding(), false, "text");
                        }
                    }
                }
            }
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
