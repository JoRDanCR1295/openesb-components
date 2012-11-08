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
 * @(#)Normalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.snmpbc.extensions.SNMPMessage;
import com.sun.jbi.snmpbc.extensions.SNMPOperation;
import java.io.StringWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

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


/**
 * SNMP normalizer class (convert snmp to nmr message)
 *
 * @author echou
 */
public class Normalizer {

    private static final Messages mMessages =
        Messages.getMessages(Normalizer.class);

    private Transformer mTrans = null;
    
    private WrapperBuilder wrapperBuilder;
    
    public Normalizer() throws Exception {
        
        try {
            wrapperBuilder = HelperFactory.createBuilder();        
        } catch (WrapperProcessingException ex) {
            throw new Exception(mMessages.getString("SNMPBC_E00714.WRAPPER_BUILDER_CREATE_FAILED", ex.getMessage()), ex);
        }
        
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(mMessages.getString("SNMPBC_E))715.TRANSFORMER_CREATE_FAILED", ex.getMessage()), ex);
        }                
    }
    
    public void normalize (Source xmlSource,
            NormalizedMessage normalizedMsg,
            GenericOperation genericOp,
            boolean isOperationInput) throws Exception {
            
        normalize(xmlSource, 
                normalizedMsg, 
                genericOp.getEndpoint(), 
                genericOp.getSNMPOperation(),
                genericOp.getSNMPInput().getSNMPMessage(),
                isOperationInput);
    }
    
    public void normalize(Source xmlSource,
            NormalizedMessage normalizedMsg,
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

        wrapperBuilder.initialize(null,
                                  wsdlMessage,
                                  null);

        // From the SNMP message, build the message parts for the body
        buildMessagePayload (xmlSource, snmpMessage);
        
        Document doc = wrapperBuilder.getResult();
        DOMSource domSource = new DOMSource(doc);
        //printXML(domSource);
        normalizedMsg.setContent(domSource);
    }
    
    private void buildMessagePayload(Source xmlSource,
            SNMPMessage mapDef) throws Exception {
        
        // add trap part to normalized msg
        String partName = mapDef.getTrapPart();
        Node node = null;
        if (xmlSource instanceof DOMSource) {
            // saves a transformation
            node = ((DOMSource) xmlSource).getNode();
        } else {
            DOMResult domResult = new DOMResult();
            mTrans.transform(xmlSource, domResult);
            node = domResult.getNode();
        }
        
        if (node instanceof Document) {
            wrapperBuilder.addPart(partName, ((Document) node).getDocumentElement());
        } else if (node instanceof Element) {
            wrapperBuilder.addPart(partName, (Element) node);
        } else {
            throw new Exception(mMessages.getString("SNMPBC_E00716.INVALID_RESULT_FROM_XML_TRANSFORM", node.getClass()));
        }
    }
    
    private static void printXML(Source source) throws Exception {
        TransformerFactory factory = TransformerFactory.newInstance();
        Transformer trans = factory.newTransformer();
        trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        trans.setOutputProperty(OutputKeys.INDENT, "yes");
        trans.setOutputProperty(OutputKeys.METHOD, "xml");
        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        StringWriter out = new StringWriter();
        StreamResult result = new StreamResult(out);
        trans.transform(source, result);
        out.flush();
        out.close();
        System.out.println("result = " + out.toString());
        
    }
}

