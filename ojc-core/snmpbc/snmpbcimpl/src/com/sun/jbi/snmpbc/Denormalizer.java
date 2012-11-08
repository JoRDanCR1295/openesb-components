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
 * @(#)Denormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.snmpbc.extensions.SNMPMessage;
import com.sun.jbi.snmpbc.extensions.SNMPOperation;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import java.util.Iterator;
import java.util.logging.Logger;

/**
 * SNMP message denormalizer class.  Converts an NMR message to a SNMP trap
 *
 * @author echou
 */
public class Denormalizer {

    private static final Messages mMessages =
        Messages.getMessages(Denormalizer.class);
    private static final Logger mLogger =
        Logger.getLogger(Denormalizer.class.getName());

    private WrapperParser wrapperParser = null;
    private Transformer mTrans = null;
    
    public Denormalizer() throws Exception {
        
        try {
            wrapperParser = HelperFactory.createParser();
        } catch (WrapperProcessingException ex) {
            throw new Exception(mMessages.getString("SNMPBC_E00713.FAILED_TO_CREATE_WRAPPER_PARSER"),
                                ex);
        }
        
        try {        
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(mMessages.getString("SNMPBC_E00711.TRANSFORMER_CREATE_FAILED"),
                                ex);
        }                        
  
    }
    
    public Source denormalize(NormalizedMessage normalizedMessage,
            GenericOperation genericOp,
            boolean isOperationInput) throws Exception {
        
        return denormalize(normalizedMessage, 
                genericOp.getEndpoint(),
                genericOp.getSNMPOperation(),
                genericOp.getSNMPOutput().getSNMPMessage(),
                isOperationInput);
    }
    
    /**
     * Given a NormalizedMessage and a SNMP Message, this method converts the
     * NormalizedMessage to the SNMP Message using the mapping rules in the
     * provided extensibility element.
     * 
     * @param normalizedMessage The NormalizedMessage from the MessageExchange.
     * @param endpoint The Endpoint instance containing the wsdl "metadata".
     * @param snmpOp 
     * @param snmpMessage 
     * @param isOperationInput If snmpMessage is on the operation's input then true, otherwise false.
     *
     * @return 
     * @throws Exception upon error.
     */
    public Source denormalize(NormalizedMessage normalizedMessage, 
            Endpoint endpoint,
            SNMPOperation snmpOp, 
            SNMPMessage snmpMessage, 
            boolean isOperationInput) throws Exception {
              
        QName operationName = new QName(snmpOp.getBindingOperation()
                                             .getName());
        Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        
        // Grab the operation that matches the operationName.  There actually may
        // be more than one operation with the same name (but different input/output)
        // names.  We need to fix this so that we uniquely identify which operation we're
        // going after
        Iterator it = portType.getOperations().iterator();
        javax.wsdl.Message wsdlMessage = null;
        while (it.hasNext()) {
            Operation op = (Operation)it.next();
            if (op.getName().equals(operationName.toString()) ||
                op.getName().equals(operationName.getLocalPart())) {
                if (isOperationInput) {
                    wsdlMessage = op.getInput().getMessage();
                } else { // operation output
                    wsdlMessage = op.getOutput().getMessage();
                } 
            }
        }
        
        // use helper class to parse wrapped msg
        Document normalizedDoc = null;
        Source source = normalizedMessage.getContent();
        Node node = null;
        if (source instanceof DOMSource) {
            // saves a transformation
            node = ((DOMSource) source).getNode();
        } else {
            DOMResult domResult = new DOMResult();
            mTrans.transform(source, domResult);
            node = domResult.getNode();
        }
        if (node instanceof Document) {
            wrapperParser.parse((Document) node, wsdlMessage);
        } else {
            wrapperParser.parse(node.getOwnerDocument(), wsdlMessage);
        }
        
        String partName = snmpMessage.getTrapPart();
        NodeList nodes = wrapperParser.getPartNodes(partName);
        if (nodes == null || nodes.getLength() == 0) {
            throw new Exception(mMessages.getString("SNMPBC_E00712.UNABLE_TO_FIND_VALID_PART"));
        }
        
        // return the first node
        return new DOMSource(nodes.item(0));
    }


}

