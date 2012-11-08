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
 * @(#)MessageDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.WSDLMessage;
import java.util.ArrayList;
import java.util.List;

import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.WSDLException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This is the class that denormalizes a Normalized Message
 * for SAP BC.
 *
 */
class MessageDenormalizer {
    
    /**
     * Denormalizes the message received from the JBI NMR and returns a list of Nodes
     * for each jbi:part associated with the jbi:message.
     *
     * @param normalizedMessage Message from NMR
     * @param msgdef The WSDL as a WSDLDefinitions object
     * @param endpointName Name of the binding
     *
     */
    public List<Node> denormalize(
            NormalizedMessage normalizedMessage,
            WSDLMessage wsdlMsg,
            String endpointName)
            
            throws MessageProcessingException {
        
        // Make a DOM tree out of the message content.
        // No denormalization is taking place yet, just transforming
        // representations of the normalized message.
        final Document normalizedDoc;
        final DOMResult result = new DOMResult();
        {
            Source src = normalizedMessage.getContent();
            if (src != null) {
                TransformerFactory fact = TransformerFactory.newInstance();
                Transformer transformer;
                try {
                    transformer = fact.newTransformer();
                    transformer.transform(src, result);
                } catch (TransformerConfigurationException ex) {
                    throw new MessageProcessingException(
                            mMessages.getString(
                            "MessageDenormalizer.Failed_acquire_transform",
                            new Object[] {
                        endpointName,
                    }),
                            ex
                            );
                } catch (TransformerException ex) {
                    throw new MessageProcessingException(
                            mMessages.getString(
                            "MessageDenormalizer.Failed_transform_normmsg",
                            new Object[] {
                        endpointName,
                    }),
                            ex
                            );
                }
            }
        }
        normalizedDoc = result.getNode() instanceof Document
                ? (Document) result.getNode()
                : result.getNode().getOwnerDocument();
        
        SAPWSDLUtilities.doctostring(normalizedDoc);
        
/*
        DOC XML CONTENT Example
        <?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>
        <jbi:message type="msgns:FlightGetDetail" version="1.0"
            xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper" xmlns:msgns="urn:sap-com:document:sap:soap:functions:mc-style">
            <jbi:part>
                <someNS:FlightGetDetail xmlns:someNS="urn:sap-com:document:sap:soap:functions:mc-style">
                    <AirlineID>LH</AirlineID>
                    <ConnectionID>0400</ConnectionID>
                    <ExtensionIn>
                        <item>
                            <Valuepart1>Val1</Valuepart1>
                            <Valuepart2>val2</Valuepart2>
                        </item>
                    </ExtensionIn>
                    <FlightDate>1995-02-28</FlightDate>
                </someNS:FlightGetDetail>
            </jbi:part>
        </jbi:message>
 */
        
        // Denormalization starts here.
        return unwrapMessage(normalizedDoc, wsdlMsg, endpointName);
    }
    
    private List<Node> unwrapMessage(
            final Document normalizedMessage,
            final WSDLMessage wsdlMsg,
            final String endpointName)
            
            throws MessageProcessingException  {
        
        // Parse the message content. At this stage the actual SAP BC-specific
        // content is wrapped in JBI normalized.
        final WrapperParser wrapperParser;
        try {
            wrapperParser = HelperFactory.createParser();
            //wrapperParser.parse(normalizedMessage, msgdef);
            wrapperParser.parse(normalizedMessage, SAPWSDLUtilities.getMessage(wsdlMsg));
        } catch (WrapperProcessingException ex) {
            throw new MessageProcessingException(
                    mMessages.getString(
                    "MessageDenormalizer.Failed_unwrap",
                    new Object[] {
                endpointName,
            }),
                    ex
                    );
        } catch (WSDLException ex) {
            throw new MessageProcessingException(
                    mMessages.getString(
                    "MessageDenormalizer.wsdlexception",
                    new Object[] {
                endpointName,
            }),
                    ex
                    );
        }
        
        // Unwrap the content: build a list of DOM Nodes of it.
        // Ostensibly, SAP message definitions contain only one part,
        // but it is within the realm of possibility that SAP NetWeaver may
        // generate definitions with multiple parts.  Multiple parts mean
        // multiple JBI Wrapper parts, so maintain a list.
        final String[] partNames = wrapperParser.getPartNames();
        final List<Node> nodes = new ArrayList<Node>(partNames.length);
        for (String partName: partNames) {
            NodeList nodeList;
            try {
                nodeList = wrapperParser.getPartNodes(partName);
            } catch (WrapperProcessingException ex) {
                throw new MessageProcessingException(
                        mMessages.getString(
                        "MessageDenormalizer.Failed_unwrap_part",
                        new Object[] {
                    endpointName,
                    partName
                }),
                        ex
                        );
            }
            for (int i = 0; i < nodeList.getLength(); ++i) {
                nodes.add(nodeList.item(i));
                
                SAPWSDLUtilities.nodetostring(nodeList.item(i));
/*
                NODE XML CONTENT Example
                <?xml version="1.0" encoding="UTF-16"?>
                <someNS:FlightGetDetail xmlns:someNS="urn:sap-com:document:sap:soap:functions:mc-style">
                  <AirlineID>LH</AirlineID>
                  <ConnectionID>0400</ConnectionID>
                  <ExtensionIn>
                    <item>
                      <Valuepart1>Val1</Valuepart1>
                      <Valuepart2>val2</Valuepart2>
                    </item>
                  </ExtensionIn>
                  <FlightDate>1995-02-28</FlightDate>
                </someNS:FlightGetDetail>
 */
                
            }
        }
        
        return nodes;
    }
    
    private static final Messages mMessages =
            Messages.getMessages(MessageDenormalizer.class);
}
