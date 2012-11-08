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
 * @(#)SwiftDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

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
import javax.xml.transform.TransformerFactoryConfigurationError;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


import com.sun.encoder.Encoder;
import com.sun.jbi.swiftbc.Endpoint.EndpointMessageType;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import com.sun.jbi.swiftbc.util.WSDLUtilities;
import static com.sun.jbi.swiftbc.util.XmlUtil.*;



/**
 * This is the class that denormalizes a Normalized Message for Swift BC.
 * 
 * @author S. Nageswara Rao
 */
public class SwiftDenormalizer {
    private static final Messages mMessages = Messages.getMessages(SwiftDenormalizer.class);

    private Transformer mTrans;

    private WrapperParser wrapperParser;

    private static SwiftDenormalizer singleton = null;

    private SwiftDenormalizer() throws Exception {
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(mMessages.getString("SwiftDNMR_Create_transformer_exception", ex.getMessage()), ex);
        }

        try {
            wrapperParser = HelperFactory.createParser();
        } catch (WrapperProcessingException ex) {
            throw new Exception(mMessages.getString("SwiftDNMR_Create_wrapper_parser_exception", ex.getMessage()), ex);
        }
    }

    public static SwiftDenormalizer getInstance() throws Exception {
        if (singleton == null) {
            singleton = new SwiftDenormalizer();
        }
        return singleton;
    }

    /**
     * Given a NormalizedMessage and SwiftMessage extensibility element information, this method
     * converts the NormalizedMessage to the Swift message/payload using the Encoder configured for
     * the SwiftMessage part
     * 
     * @param normalizedMessage The NormalizedMessage from the MessageExchange.
     * @param operationName The Swift Operation Name
     * @param endpoint The Endpoint instance containing the wsdl "metadata".
     * @param SwiftMessage The SwiftMessage extensibility element
     * @throws Exception upon error.
     */

    public String denormalize(NormalizedMessage normalizedMessage,
                              QName operationName,
                              Endpoint endpoint,
                              SwiftMessage SwiftMessage) throws Exception {
        DOMResult result = null;
        Source src = normalizedMessage.getContent();
        if (src != null) {
            result = transformToDOMResult(src);
        }
        Node node = result.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }

        wrapperParser.parse(normalizedDoc, endpoint.getDefinition());

        return getMessagePartPayload(normalizedMessage, endpoint, operationName, SwiftMessage, wrapperParser);
    }

    private String getMessagePartPayload(NormalizedMessage normalizedMessage,
                                         Endpoint endpoint,
                                         QName operationName,
                                         SwiftMessage SwiftMessage,
                                         WrapperParser wrapperParser) throws Exception {
        String result = null;

        // locate the WSDL message
        Message wsdlMessage = null;
        Map operations = endpoint.getSwiftOperations();
        Map operationMeps = endpoint.getOperationMsgExchangePattern();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        SwiftOperation operation = null;
        Part aPart = null;

        boolean inout = false;
        String mep = null;
        String use = SwiftMessage.getUseType();
        String partName = null;//SwiftMessage.getPart();

        if (operations.get(operationName) == null) {
            throw new Exception(mMessages.getString("SwiftDNMR_Invalid_opname", operationName));
        }
        mep = (String) operationMeps.get(operationName);
        if (mep == null || mep.equals(EndpointMessageType.UNSUPPORTED)) {
            throw new Exception(mMessages.getString("SwiftDNMR_Invalid_mep", operationName));
        }

        if (mep.equals(EndpointMessageType.IN_OUT)) {
            inout = true;
        }

        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();

        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation) it.next();
            if (op.getName().equals(operationName.toString()) || op.getName().equals(operationName.getLocalPart())) {
                if (inout) {
                    wsdlMessage = op.getOutput().getMessage();
                } else {
                    wsdlMessage = op.getInput().getMessage();
                }
                break;
            }
        }

        if (wsdlMessage == null) {
            throw new Exception(mMessages.getString("SwiftDNMR_Invalid_message", operationName));
        }
        // get the message part
        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        NodeList parts = wrapperParser.getPartNodes(partName);
        Node aNode = (Node) parts.item(0);

        if (use.equals(SwiftMessage.USE_TYPE_ENCODED)) {
            // Locate the encoder
            Encoder encoder = (Encoder) partMappings.get(wsdlMessage.getQName() + partName);
            if (encoder == null) {
                throw new Exception(mMessages.getString("SwiftDNMR_Invalid_encodingStyle"));
            }
            // Encode DOM source to raw data format
            Source source = new DOMSource(aNode);
            result = encoder.encodeToString(source);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                result = transformToString(aNode, "UTF-8", true, "xml");
            } else { 
                // must be "type" otherwise there would've been a WSDL validation error //
                //We may still be dealing with XML node 
                QName typename = aPart.getTypeName();
                byte[] byteResult;
                if (!WSDLUtilities.isBuiltInType(typename)) {
                    byteResult = transformToBytes(aNode, "UTF-8", true, "xml");
                    result = new String(byteResult, "UTF-8");
                } else { // treat it as Text node
                    byteResult = transformToBytes(aNode, "UTF-8", true, "text");
                    result = new String(byteResult, "UTF-8");
                }
            }
             
        }

        return result;
    }
}
