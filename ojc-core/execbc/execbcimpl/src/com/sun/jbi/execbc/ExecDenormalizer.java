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
 * @(#)ExecDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import com.sun.encoder.Encoder;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.util.WSDLUtilities;
import com.sun.jbi.execbc.util.XmlUtil;
import com.sun.jbi.execbc.Endpoint.EndpointMessageType;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;

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

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This is the class that denormalizes a Normalized Message
 * for File BC.
 *
 */
public class ExecDenormalizer {
    private static final Messages mMessages = Messages.getMessages(ExecDenormalizer.class);
    
    public byte[] denormalize(NormalizedMessage normalizedMessage,
            QName operationName,
            Endpoint endpoint,
            ExecMessage execMessage) throws Exception {
        DOMResult result = new DOMResult();
        Source src = normalizedMessage.getContent();
        if (src != null) {
            TransformerFactory fact = TransformerFactory.newInstance();
            Transformer transformer = fact.newTransformer();
            transformer.transform(src, result);
        }
        Node node = result.getNode();
        if (node == null) {
            throw new Exception(mMessages.getString("FDNMR_Invalid_NM_content_null"));
        }
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }
        
        WrapperParser wrapperParser = HelperFactory.createParser();
        wrapperParser.parse(normalizedDoc, endpoint.getDefinition());
        
        return getMessagePartPayload(normalizedMessage, endpoint, operationName, execMessage, wrapperParser);
    }
    
    private byte[] getMessagePartPayload(NormalizedMessage normalizedMessage,
            Endpoint endpoint,
            QName operationName,
            ExecMessage execMessage,
            WrapperParser wrapperParser) throws Exception {
        byte[] result = null;
        
        Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        
        // locate the WSDL message
        Message wsdlMessage = null;
        Map operations = endpoint.getExecOperations();
        Map operationMeps = endpoint.getOperationMsgExchangePattern();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        ExecOperation operation = null;
        Part aPart = null;
        
        boolean inout = false;
        String mep = null;
        String use = execMessage.getExecUseType();
        //For Exec BC, partName is always null, so only one part is assumed
        //String partName = execMessage.getPart();
        String partName = null;
        QName fqOperationName = new QName(portType.getQName().getNamespaceURI(),
                operationName.getLocalPart());
        if (operations.get(fqOperationName) == null) {
            throw new Exception(mMessages.getString("FDNMR_Invalid_opname", operationName));
        }
        mep = (String) operationMeps.get(fqOperationName);
        if (mep == null || mep.equals(EndpointMessageType.UNSUPPORTED)) {
            throw new Exception(mMessages.getString("FDNMR_Invalid_mep", operationName));
        }
        
        if (mep.equals(EndpointMessageType.IN_OUT)) {
            inout = true;
        }
        
        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation)it.next();
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
            throw new Exception(mMessages.getString("FDNMR_Invalid_message", operationName));
        }
        
        /**
         * Exec BC is either used for invoking a command actively or for
         * continuously invoking a command and receiving result passively.
         * When Denormalizer is involved, usually it is invoking a command and
         * all parts of the WSDL message are parameters to that command.
         */
        
        // get the message part
        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
            if (aPart == null) {
                throw new Exception(mMessages.getString("FDNMR_Invalid_no_part", partName));
            }
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[])parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }
        
        NodeList parts = wrapperParser.getPartNodes(partName);
        Node aNode = (Node)parts.item(0);
        
        if (use.equals(ExecMessage.EXEC_USE_TYPE_ENCODED)) {
            // Locate the encoder
            Encoder encoder = (Encoder)partMappings.get(wsdlMessage.getQName() + partName);
            if (encoder == null) {
                throw new Exception(mMessages.getString("FDNMR_Invalid_encodingStyle"));
            }
            // Encode DOM source to raw data format
            Source source = new DOMSource(aNode);
            result = encoder.encodeToBytes(source);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                result = XmlUtil.transformToBytes(aNode, "UTF-8", true, "xml");
            } else { // must be "type" otherwise there would've been a WSDL validation error
                // We may still be dealing with XML node
                QName typename = aPart.getTypeName();
                if (!WSDLUtilities.isBuiltInType(typename.getNamespaceURI())) {
                    result = XmlUtil.transformToBytes(aNode, "UTF-8", true, "xml");
                } else {
                    // treat it as Text node
                    result = XmlUtil.transformToBytes(aNode, "UTF-8", true, "text");
                }
            }
        }
        
        return result;
    }
}
