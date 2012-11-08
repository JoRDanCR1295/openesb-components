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
 * @(#)MSMQDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc;

import java.io.ByteArrayOutputStream;
import java.io.StringWriter;
import java.text.MessageFormat;
import java.util.Iterator;
import java.util.Map;
import java.util.Collection;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Part;
import javax.wsdl.Service;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Message;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;

import com.sun.jbi.msmqbc.extensions.MSMQOperation;
import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQMessage;

import com.sun.jbi.msmqbc.msmq.Channel;

import com.sun.encoder.Encoder;

import com.sun.xsd.model.FastSchemaFactory;
import com.sun.xsd.model.FastSchema;

import com.sun.jbi.msmqbc.util.XmlUtil;
import com.sun.jbi.msmqbc.util.WSDLUtilities;

/**
 * MSMQ message denormalizer class. Converts an NMR message to a MSMQ message using the WSDL defined
 * MSMQ extensibility elements and message parts as mapping instructions.
 *
 * @author Sun Microsystems
 */
public class MSMQDenormalizer {

    private static final Messages mMessages = Messages.getMessages(MSMQDenormalizer.class);

    private static final Logger mLogger = Messages.getLogger(MSMQDenormalizer.class);

    private static MSMQDenormalizer singleton;

    protected MSMQDenormalizer() {

    }

    // ////
    // public methods
    // ////

    public static MSMQDenormalizer getInstance() {
        if (singleton == null) {
            singleton = new MSMQDenormalizer();
        }
        return singleton;
    }

    /**
     * Produces a MSMQ Message given an NMR (NormalizedMessage) message.
     *
     * @param msmqChannel The MSMQ Channel used to create the MSMQ message.
     * @param normalizedMessage The NormalizedMessage from the MessageExchange.
     * @param operationName The service operation.
     * @param endpoint The service endpoint containing the service operation.
     * @throws Exception upon error.
     */
    public MSMQPayLoad denormalize(Channel msmqChannel,
                                   NormalizedMessage normalizedMessage,
                                   QName operationName,
                                   MSMQMessage msmqMessage) throws Exception {

        Endpoint endpoint = msmqChannel.getEndpoint();
        // Convert the NMR message into a DOM object
        DOMResult result = new DOMResult();
        Source src = normalizedMessage.getContent();
        if (src != null) {
            TransformerFactory fact = TransformerFactory.newInstance();
            Transformer transformer = fact.newTransformer();
            transformer.transform(src, result);
        }
        Node node = result.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }

        // Use the WrapperParser to help in parsing out the Parts
        WrapperParser wrapperParser = HelperFactory.createParser();
        wrapperParser.parse(normalizedDoc, endpoint.getDefinition());

        // Construct the MSMQ message payload - refer to the message parts
        // per encoding rules
        return getMessagePartPayload(endpoint, operationName, msmqMessage, wrapperParser);
    }

    private MSMQPayLoad getMessagePartPayload(Endpoint endpoint,
                                              QName operationName,
                                              MSMQMessage msmqMessage,
                                              WrapperParser wrapperParser) throws Exception {
        // locate the WSDL message
        Message wsdlMessage = null;
        byte[] data = null;
        MSMQPayLoad payload = new MSMQPayLoad();
        Map operations = endpoint.getMSMQOperations();
        Map operationMeps = endpoint.getOperationMsgExchangePatterns();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        MSMQOperation operation = null;
        Part aPart = null;

        boolean inout = false;
        String mep = null;
        String use = msmqMessage.getUseType();
        String partName = msmqMessage.getMessagePart();

        if (operations.get(operationName) == null) {
            throw new Exception(mMessages.getString("MSMQDENORMALIZER_INVALID_OPNAME", operationName));
        }
        mep = (String) operationMeps.get(operationName);
        if (mep == null || mep.equals(Endpoint.EndpointMessageType.UNSUPPORTED)) {
            throw new Exception(mMessages.getString("MSMQDENORMALIZER_INVALID_MEP", operationName));
        }

        if (mep.equals(Endpoint.EndpointMessageType.IN_OUT)) {
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
            throw new Exception(mMessages.getString("MSMQDENORMALIZER_INVALID_MESSAGE", operationName));
        }

        /**
         * MSMQ BC basically handles data with any file format. However, an important assumption has
         * to be made, i.e. we deal with one message part for each msmq operation only. Otherwise,
         * the "part" attribute for msmq:recieve element is required. This assumption will be
         * enforced as part of WSDL validation at design time.
         */

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

        if (use.equals(msmqMessage.USE_TYPE_ENCODED)) {
            // Locate the encoder
            Encoder encoder = (Encoder) partMappings.get(wsdlMessage.getQName() + partName);
            if (encoder == null) {
                throw new Exception(mMessages.getString("MSMQDenormalizer_INVALID_ENCODINGSTYLE"));
            }
            // Encode DOM source to raw data format
            Source source = new DOMSource(aNode);
            data = encoder.encodeToBytes(source);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                data = XmlUtil.transformToBytes(aNode, "UTF-8", true, "xml");
            } else { // must be "type" otherwise there would've been a WSDL validation error
                // We may still be dealing with XML node
                QName typename = aPart.getTypeName();
                if (!WSDLUtilities.isBuiltInType(typename)) {
                    data = XmlUtil.transformToBytes(aNode, "UTF-8", true, "xml");
                } else {
                    // treat it as Text node
                    data = XmlUtil.transformToBytes(aNode, "UTF-8", true, "text");
                }
            }
        }
        payload.setPayLoad(data);
        return payload;
    }
}
