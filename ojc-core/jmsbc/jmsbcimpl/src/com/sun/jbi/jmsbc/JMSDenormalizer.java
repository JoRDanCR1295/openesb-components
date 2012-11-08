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
 * @(#)JMSDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.activation.DataHandler;
import javax.jms.BytesMessage;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.TextMessage;

import javax.jbi.messaging.NormalizedMessage;

import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSProperty;
import com.sun.jbi.jmsbc.extensions.JMSProperties;
import com.sun.jbi.jmsbc.extensions.JMSMapMessage;
import com.sun.jbi.jmsbc.extensions.JMSMapMessagePart;
import com.sun.jbi.jmsbc.extensions.JMSMessage;
import com.sun.jbi.jmsbc.jms.Util;
import com.sun.jbi.jmsbc.util.AlertsUtil;

import com.sun.encoder.Encoder;

/**
 * JMS message denormalizer class.  Converts an NMR message to a JMS message 
 * using the WSDL defined JMS extensibility elements and message parts as
 * mapping instructions.
 *
 */
public class JMSDenormalizer {

    private static final Messages mMessages =
        Messages.getMessages(JMSDenormalizer.class);
    private static final Logger mLogger =
        Messages.getLogger(JMSDenormalizer.class);

    private Transformer mTrans = null;
    private DocumentBuilder mBuilder = null;    
    
    public JMSDenormalizer() throws Exception {
        try {        
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(mMessages.getString("JMSBC-E0736.TransformerCreateFailed"), ex);
        }                        
        
        try {        
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            throw new Exception(mMessages.getString("JMSBC-E0737.DocumentBuilderCreateFailed"), ex);
        }        
    }

    
    /**
     * Given a NormalizedMessage and a JMS Message, this method converts the
     * NormalizedMessage to the JMS Message using the mapping rules in the
     * provided JMSSend extensibility element.
     * 
     * @param normalizedMessage The NormalizedMessage from the MessageExchange.
     * @param jmsMessage The JMS Message to denormalize.
     * @param endpoint The Endpoint instance containing the wsdl "metadata".
     * @param jmsSend The JMSSend instance containing the mapping information.
     * @param isOperationInput If JMSSend is on the operation's input then true, otherwise false.
     *
     * @throws Exception upon error.
     */
    public void denormalize(NormalizedMessage normalizedMessage,
                            Message jmsMessage,
                            Endpoint endpoint,
                            JMSOperation jmsOp,
                            JMSMessage mappingInfo,
                            boolean isOperationInput) 
        throws Exception {
                
        Definition serviceDef = endpoint.getDefinition();
        
        QName operationName = new QName(jmsOp.getBindingOperation()
                                             .getName());
        Service service  = endpoint.getDefinition()
                                   .getService(endpoint.getServiceName());
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

        DOMResult result = null;
        Source src = normalizedMessage.getContent();
        if (src instanceof StreamSource) {
            StreamSource stream = (StreamSource)src;
            InputStream inputStream = stream.getInputStream();
            if (inputStream != null) {
                inputStream.reset();
            }
            Reader reader = stream.getReader();
            if (reader != null) {
                reader.reset();
            }
        }
        if (src != null) {
            result = XmlUtil.transformToDOMResult(mTrans, src, mBuilder.newDocument());
        }
        Node node = result.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }
        if (src instanceof StreamSource) {
            StreamSource stream = (StreamSource)src;
            InputStream inputStream = stream.getInputStream();
            if (inputStream != null) {
                inputStream.reset();
            }
            Reader reader = stream.getReader();
            if (reader != null) {
                reader.reset();
            }
        }            

        // Transform to stream and then create document with resuable doc builder is better way!
        // Straight transform to DOMResult is very expensive. Every call to Transformer.transform(Source, DOMResult) 
        // results in DocumentBuilderFactory.newInstance() to be called every single time.
        /*
        Source src = normalizedMessage.getContent();
        String str = null;
        if (src != null) {
            if (src instanceof StreamSource) {
                StreamSource stream = (StreamSource)src;
                InputStream inputStream = stream.getInputStream();
                if (inputStream != null) {
                    inputStream.reset();
                }
                Reader reader = stream.getReader();
                if (reader != null) {
                    reader.reset();
                }
            }
            str= XmlUtil.transformToString(mTrans,src,"UTF-8",true);
            if (src instanceof StreamSource) {
                StreamSource stream = (StreamSource)src;
                InputStream inputStream = stream.getInputStream();
                if (inputStream != null) {
                    inputStream.reset();
                }
                Reader reader = stream.getReader();
                if (reader != null) {
                    reader.reset();
                }
            }            
        }        
        Document normalizedDoc = XmlUtil.createDocumentFromXML(mBuilder, str);
        */

        /** 
         * Transform only necessary.. straight transform to DOMResult is very expensive.
         * Every call to Transformer.transform(Source, DOMResult) results in 
         * DocumentBuilderFactory.newInstance() to be called!
         *
        Source src = normalizedMessage.getContent();
        Document normalizedDoc = null;       
        if (src instanceof DOMSource) {
            Node srcNode = ((DOMSource)src).getNode();
            if (srcNode instanceof Document) {
                normalizedDoc = (Document)srcNode;
            } else {
                normalizedDoc = ((Element)srcNode).getOwnerDocument();                
            }
        } else if (src instanceof StreamSource) {
            // Alot faster to transform to StreamResult first instead of transforming to DOMResult
            StringWriter out = new StringWriter();
            StreamResult result = new StreamResult(out);
            if (src != null) {
                XmlUtil.transformToStreamResult(mTrans, src, result);
            }        
            normalizedDoc = XmlUtil.createDocumentFromXML(mBuilder, out.toString());        
        } else {  // the old fashion way
            DOMResult result = XmlUtil.transformToDOMResult(mTrans, src);
            Node node = result.getNode();
            if (node instanceof Document) {
                normalizedDoc = (Document) node;
            } else {
                normalizedDoc = ((Element) node).getOwnerDocument();
            }            
        }
         */
        
        // Use the WrapperParser to help in parsing out the Parts
        WrapperParser wrapperParser = HelperFactory.createParser();
        wrapperParser.parse(normalizedDoc, serviceDef);

        // From the NMR message, map the parts to the JMS message "payload"
        buildJMSMessagePayload (endpoint,
                                wrapperParser, 
                                jmsMessage, 
                                wsdlMessage, 
                                mappingInfo,
                                normalizedMessage);
        
        // From the NMR message, map the parts to the JMS headers using
        // the mapping instructions in the jms operations input/output attributes
        mapPartsToJMSHeaders (wrapperParser, 
                              jmsMessage, 
                              wsdlMessage, 
                              mappingInfo,
                              normalizedMessage);
        
        // From the NMR message, map the parts to the JMS properties using
        // the mapping instructions in the jms properties element and
        // property elements
        mapPartsToJMSProperties (wrapperParser, 
                                 jmsMessage, 
                                 wsdlMessage, 
                                 mappingInfo,
                                 normalizedMessage);        
    }
    

    private void buildJMSMessagePayload (Endpoint endpoint,
                                         WrapperParser wrapperParser, 
                                         Message jmsMsg,
                                         javax.wsdl.Message wsdlMessage,
                                         JMSMessage mapDef,
                                         NormalizedMessage nm) 
    throws Exception {
        if (jmsMsg instanceof TextMessage) {
            TextMessage jmsTxtMsg = (TextMessage)jmsMsg;
            buildJMSTextMessagePayload (endpoint,
                                        wrapperParser, 
                                        jmsTxtMsg, 
                                        wsdlMessage,
                                        mapDef,
                                        nm);
        } else if (jmsMsg instanceof MapMessage) {
            MapMessage jmsMapMsg = (MapMessage)jmsMsg;
            buildJMSMapMessagePayload (wrapperParser, 
                                       jmsMapMsg, 
                                       wsdlMessage,
                                       mapDef,
                                       nm);
        } else if (jmsMsg instanceof BytesMessage) {
        	BytesMessage jmsBytesMsg = (BytesMessage)jmsMsg;
        	buildJMSBytesMessagePayload (wrapperParser, 
            						   jmsBytesMsg, 
                                       wsdlMessage,
                                       mapDef,
                                       nm);
        }
    }

    private void buildJMSBytesMessagePayload (WrapperParser wrapperParser,
    		BytesMessage jmsBytesMsg,
            javax.wsdl.Message wsdlMessage,
            JMSMessage mapDef,
            NormalizedMessage nm) throws Exception{
        String bytesPart = mapDef.getBytesPart();
        Part aPart = WSDLUtilities.findWSDLMessagePart(bytesPart, wsdlMessage);
        if (aPart != null) {
            NodeList nodes = wrapperParser.getPartNodes(bytesPart); 
            if (nodes == null || nodes.getLength() == 0) {
            	String msg = mMessages.getString(
						"JMSBC-W0706.NullMessagePartContent", new Object[] {
								bytesPart, wrapperParser.getMessageName() });
                mLogger.log(Level.WARNING, msg);
                AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0706.NullMessagePartContent",
                            new Object[]{bytesPart, 
                                         wrapperParser.getMessageName()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0706");                         

                throw new Exception(msg);
            } else {
            	Node aNode = nodes.item(0);
                byte[] bytes = getPartIfAvailableAsAttachment(aNode, nm);
                if(bytes == null){
    	            bytes = Base64Utils.base64DecodeToByte(aNode.getNodeValue());
                }
                jmsBytesMsg.writeBytes(bytes);
            }
        }else{
        	String msg = mMessages.getString(
					"JMSBC-W0706.NullMessagePartContent", new Object[] {
							bytesPart, wrapperParser.getMessageName() });
            mLogger.log(Level.WARNING, msg);
            throw new Exception(msg);
        }
    	
    }
    
    
    private void buildJMSTextMessagePayload (Endpoint endpoint,
                                             WrapperParser wrapperParser,
                                             TextMessage jmsTxtMsg,
                                             javax.wsdl.Message wsdlMessage,
                                             JMSMessage mapDef,
                                             NormalizedMessage nm) 
        throws Exception {
        Map partMappings = endpoint.getMessagePartEncoderMapping();        
        String textPart = mapDef.getTextPart();
        String partContent = getPartValue(textPart, 
                                          wsdlMessage,
                                          wrapperParser,
                                          nm);
        if (partContent != null) {
            String useAttrVal = mapDef.getUse();
            if (useAttrVal == null ){
                throw new Exception(mMessages.getString("JMSBC-E0733.UnspecifiedUseAttribute")); 
            } else if (useAttrVal.equals(JMSMessage.ATTR_USE_TYPE_LITERAL)) {

                jmsTxtMsg.setText(partContent);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSDenormalizer_PARTS_ADD_AS_JMS_TEXT",
                                new Object[] {textPart, 
                                              partContent});
                }                

            } else if (useAttrVal.equals(JMSMessage.ATTR_USE_TYPE_ENCODED)) {
                NodeList parts = wrapperParser.getPartNodes(textPart);
                Node aNode = (Node)parts.item(0);

                // Locate the encoder                
                Encoder encoder = (Encoder)partMappings.get(wsdlMessage.getQName() + textPart);
                if (encoder == null) {
                    throw new Exception(mMessages.getString("JMSDenormalizer_Invalid_encodingStyle"));
                }
//                //First check in part is available is attachment
//                byte[] bytes = getPartIfAvailableAsAttachment(aNode, nm);
//                Source source;
//                if(bytes != null){
//                	String xml = new String(bytes);
//                	Document doc = XmlUtil.createDocumentFromXML(mBuilder, xml);
//                    source = new DOMSource(doc);
//                }else{
//                    source = new DOMSource(aNode);
//                }
                
                
                // Encode DOM source to raw data format
            	Document doc = XmlUtil.createDocumentFromXML(mBuilder, partContent);
            	Source source = new DOMSource(doc);
                String result = encoder.encodeToString(source);
                jmsTxtMsg.setText(result);

                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSDenormalizer_PARTS_ADD_AS_JMS_TEXT",
                                new Object[] {textPart, 
                                              result});
                }                

            } else {
                throw new Exception(mMessages.getString("JMSBC-E0732.InvalidUseAttributeValue",
                                        new Object [] {useAttrVal}));
            }
        } else {
        	String msg = mMessages.getString("JMSBC-W0709.MessagePartForTextMessageTextMissing",
                        new Object[] {textPart});
            mLogger.log(Level.WARNING, msg);
          AlertsUtil.getAlerter().warning(msg, 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(), 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-W0709");  
            throw new Exception(msg);
        }
    }
    
    private void buildJMSMapMessagePayload (WrapperParser wrapperParser,
                                            MapMessage jmsMapMsg,
                                            javax.wsdl.Message wsdlMessage,
                                            JMSMessage mapDef,
                                            NormalizedMessage nm)
        throws Exception {
        JMSMapMessage mapMsgParts = mapDef.getMapMessage();
        if (mapMsgParts == null) {  // no map message parts element is defined
            mLogger.log(Level.WARNING,
                        "JMSBC-W0707.MessagePartsAndMapMessageMappingUndefined");
          AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0707.MessagePartsAndMapMessageMappingUndefined"), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-W0707");            
        } else {
            Map mapMsgPartColl = mapMsgParts.getMapMessageParts();
            if (mapMsgPartColl == null || mapMsgPartColl.size()==0) {
                mLogger.log(Level.WARNING,
                            "JMSBC-W0707.MessagePartsAndMapMessageMappingUndefined");
                AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0707.MessagePartsAndMapMessageMappingUndefined"), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0707");                             
            } else {  // build the map message payload
                Iterator mapPartsIter = mapMsgPartColl.keySet().iterator();                
                while (mapPartsIter.hasNext()) {
                    JMSMapMessagePart mapMsgPart = 
                            (JMSMapMessagePart)mapMsgPartColl.get(mapPartsIter.next());
                    String mapMsgValPart = mapMsgPart.getPart();
                    String mapMsgValName = mapMsgPart.getName();
                    String mapMsgValType = mapMsgPart.getType();
                    
                    String mapMsgValValue = getPartValue(mapMsgValPart,
                                                         wsdlMessage,
                                                         wrapperParser,
                                                         nm);
                    
                    if (mapMsgValValue != null) {
                        // Found the message part that has the map message value
                        addMapMessageValue (jmsMapMsg,
                                            mapMsgValName, 
                                            mapMsgValType,
                                            mapMsgValValue);
                        
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                        "JMSDenormalizer_PART_ADD_AS_MAPMESSAGE_VAL",
                                        new Object[] {mapMsgValPart, 
                                                      mapMsgValName, 
                                                      mapMsgValType,
                                                      mapMsgValValue});
                        }
                    } else {
                        mLogger.log(Level.WARNING,
                                    "JMSBC-W0708.MessagePartForMappMessagePropertyMissing",
                                    new Object[] {mapMsgValPart,
                                                  mapMsgValName,
                                                  mapMsgValType});
                        AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0708.MessagePartForMappMessagePropertyMissing"), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0708");                            
                    }
                }
            }
        }
    }
    
    private void mapPartsToJMSHeaders (WrapperParser wrapperParser, 
                                       Message jmsMsg,
                                       javax.wsdl.Message wsdlMessage,
                                       JMSMessage mapDef,
                                       NormalizedMessage nm)
        throws Exception {        
        // Get correlationId
        String correlationIdPart = mapDef.getCorrelationIdPart();
        if (isNonEmpty(correlationIdPart)) { // part reference
            String correlationId = getPartValue(correlationIdPart,
                                                wsdlMessage,
                                                wrapperParser, nm);
            if (correlationId != null) {
                jmsMsg.setJMSCorrelationID(correlationId);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSDenormalizer_PART_ADD_AS_JMS_HEADER",
                                new Object[]{correlationIdPart, 
                                             "JMSCorrelationID",
                                             correlationId});
                }
            }
        }
        
        /* Todo: need to base64 decode?
        // Get correlationIdAsBytes        
        String correlationIdAsBytesPart = opParam.getCorrelationIdAsBytes();
        if (isNonEmpty(correlationIdAsBytesPart)) {
            String correlationIdAsBytes = getPartValue(correlationIdAsBytesPart,
                                                       wrapperParser);                        
            jmsMsg.setJMSCorrelationIDAsBytes(correlationIdAsBytes.getBytes());
            mLogger.log(Level.INFO, "JMSDenormalizer_JMS_PART_ADD_AS_HEADER",
                        new Object[]{correlationIdAsBytesPart, "JMSCorrelationIDAsBytes"});
        }
        */
        
        // Get deliveryMode
        String deliveryModePart = mapDef.getDeliveryModePart();
        if (isNonEmpty(deliveryModePart)) { // part reference
            String deliveryMode = getPartValue(deliveryModePart,
                                               wsdlMessage,
                                               wrapperParser, nm);
            if (deliveryMode != null) {
                jmsMsg.setJMSDeliveryMode(Util.toIntDeliveryMode(deliveryMode));
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSDenormalizer_PART_ADD_AS_JMS_HEADER",
                                new Object[]{deliveryModePart, 
                                             "JMSDeliveryMode",
                                             deliveryMode});
                }
            }
        }
                
        // Get priority 
        String priorityPart = mapDef.getPriorityPart();
        if (isNonEmpty(priorityPart)) {  // part reference
            String priorityStr = getPartValue(priorityPart,
                                              wsdlMessage,
                                              wrapperParser, nm);
            if (priorityStr != null) {
                int iPriority = Integer.parseInt(priorityStr);
                jmsMsg.setJMSPriority(iPriority);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSDenormalizer_PART_ADD_AS_JMS_HEADER",
                                new Object[]{priorityPart, 
                                             "JMSPriority",
                                             priorityStr});
                }
            }
        }
                    
        // Get type
        String typePart = mapDef.getTypePart();
        if (isNonEmpty(typePart)) {  // part reference
            String type = getPartValue(typePart,
                                       wsdlMessage,
                                       wrapperParser, nm);
            if (type != null) {
                jmsMsg.setJMSType(type);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSDenormalizer_PART_ADD_AS_JMS_HEADER",
                                new Object[]{typePart, 
                                             "JMSType",
                                             type});
                }
            }
        }
    }
    
    private void mapPartsToJMSProperties (WrapperParser wrapperParser, 
                                          Message jmsMsg,
                                          javax.wsdl.Message wsdlMessage,
                                          JMSMessage mapDef,
                                          NormalizedMessage nm) 
        throws Exception {
        JMSProperties jmsProperties = mapDef.getProperties();
        
        // If properties element is defined in the jms operation input/output 
        // extensibility elements, then traverse each property element under properties
        // element and add a JMS property to the JMS message
        if (jmsProperties != null) {
            Map propParts = jmsProperties.getProperties();
            if (propParts != null && propParts.size() > 0) {
                Iterator propsIter = propParts.keySet().iterator();
                while (propsIter.hasNext()) {
                    JMSProperty jmsProp = (JMSProperty)propParts.get(propsIter.next());
                    String propName = jmsProp.getName();
                    String propType = jmsProp.getType();
                    String partReference = jmsProp.getPart();
                    String propValue = getPartValue(partReference,
                                                    wsdlMessage,
                                                    wrapperParser, nm);
                    if (propValue != null) {
                        setJMSProperty (jmsMsg, 
                                        propName, 
                                        propType, 
                                        propValue);

                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                        "JMSDenormalizer_PART_ADD_AS_JMS_PROPERTY",
                                        new Object[] {partReference, 
                                                      propName, 
                                                      propType,
                                                      propValue});
                        }
                    } else {
                        mLogger.log(Level.WARNING,
                                    "JMSBC-W0710.MessagePartForJMSPropertyMissing",
                                    new Object[] {partReference,
                                                  propName,
                                                  propType});
                        AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0710.MessagePartForJMSPropertyMissing",
                                    new Object[] {partReference,
                                                  propName,
                                                  propType}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0710");                                                      
                    }
                }
            }
        }
    }
    
    private String getPartValue (String msgPart,
                                 javax.wsdl.Message wsdlMessage,
                                 WrapperParser wrapperParser,
                                 NormalizedMessage normalizedMessage) throws Exception {
        String value = null;
        Part aPart = WSDLUtilities.findWSDLMessagePart(msgPart, wsdlMessage);
        if (aPart != null) {
            NodeList nodes = wrapperParser.getPartNodes(msgPart); 
            if (nodes == null || nodes.getLength() == 0) {
                mLogger.log(Level.WARNING, 
                            "JMSBC-W0706.NullMessagePartContent",
                            new Object[]{msgPart, 
                                         wrapperParser.getMessageName()});
                AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0706.NullMessagePartContent",
                            new Object[]{msgPart, 
                                         wrapperParser.getMessageName()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0706");                         
                return null;
            } else {

            	Node aNode = nodes.item(0);
                byte[] bytes = getPartIfAvailableAsAttachment(aNode, normalizedMessage);
                if(bytes != null){
                	return new String(bytes);
                }
                
                // XML element node vs. simple Text node?
                if (aPart.getElementName() != null) {
                    // May want to add basic xml validation later
                    value = XmlUtil.transformToString(mTrans,
                                                      aNode,
                                                      "UTF-8", 
                                                      true, 
                                                      "xml");
                } else { 
                    // must be "type" otherwise there would've been a WSDL
                    // validation error
                    // We may still be dealing with XML node
                    if (WSDLUtilities.isXsdAnyType(aPart.getTypeName())) {
                        value = XmlUtil.transformToString(mTrans,
                                aNode, 
                                "UTF-8", 
                                 true, 
                                 "xml");
                    }else if (!WSDLUtilities.isBuiltInType(aPart.getTypeName())) {
                        // type is complex type
                        // create doc with root element using part name as element's name
                        Document wrapdoc = mBuilder.newDocument();                        
                        Element srcPart = wrapperParser.getWrappedPart(msgPart);
                        Element partElem = wrapdoc.createElement(msgPart);
                        copyNode(srcPart, partElem);                        
                        value = XmlUtil.transformToString(mTrans, 
                                                          partElem, 
                                                          "UTF-8", 
                                                          true, 
                                                          "xml");
                    } else {
                        value = XmlUtil.transformToString(mTrans,
                                                          aNode, 
                                                          "UTF-8", 
                                                           true, 
                                                           "text");
                    }
                }
            }
        }

        return value;
    }
        
    private void addMapMessageValue (MapMessage jmsMapMsg,
                                     String mapMsgValName, 
                                     String mapMsgValType,
                                     String mapMsgValValue)
        throws Exception {
        if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_BOOLEAN)) {
            jmsMapMsg.setBoolean(mapMsgValName, 
                                 Boolean.valueOf(mapMsgValValue).booleanValue());
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_BYTE)) {
            throw new Exception(mMessages.getString(
                       "JMSBC-E0734.MapMessagePropertyTypeUnsupported",
                        new Object[] { mapMsgValName, mapMsgValType}));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_BYTE)) {
            throw new Exception(mMessages.getString(
                       "JMSBC-E0734.MapMessagePropertyTypeUnsupported",
                        new Object[] { mapMsgValName, mapMsgValType}));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_CHAR)) {
            jmsMapMsg.setChar(mapMsgValName,
                              mapMsgValValue.charAt(0));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_DOUBLE)) {
            jmsMapMsg.setDouble(mapMsgValName,
                                Double.parseDouble(mapMsgValValue));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_FLOAT)) {
            jmsMapMsg.setFloat(mapMsgValName, 
                               Float.parseFloat(mapMsgValValue));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_INT)) {
            jmsMapMsg.setInt(mapMsgValName,
                             Integer.parseInt(mapMsgValValue));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_LONG)) {
            jmsMapMsg.setLong(mapMsgValName,
                              Long.parseLong(mapMsgValValue));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_SHORT)) {
            jmsMapMsg.setShort(mapMsgValName,
                               Short.parseShort(mapMsgValValue));
        } else if (mapMsgValType.equals(JMSMapMessagePart.MAPMESSAGE_TYPE_STRING)) {
            jmsMapMsg.setString(mapMsgValName,
                                mapMsgValValue);
        } else {
            throw new Exception(mMessages.getString(
                       "JMSBC-E0735.MapMessagePropertyTypeUnknown",
                            new Object[] {mapMsgValName, mapMsgValType}));            
        }
        
    }
    
    private void setJMSProperty (Message jmsMsg,
                                 String propName,
                                 String propType,
                                 String propValue) 
        throws Exception {
        if (propType.equals(JMSProperty.PROPERTY_TYPE_BOOLEAN)) {
            jmsMsg.setBooleanProperty(propName, 
                                  Boolean.valueOf(propValue).booleanValue());
        } else if (propType.equals(JMSProperty.PROPERTY_TYPE_BYTE)) {
            throw new Exception(mMessages.getString(
                        "JMSBC-E0730.JMSPropertyTypeUnsupported",
                        new Object[] {propName,propType}));            
        } else if (propType.equals(JMSProperty.PROPERTY_TYPE_DOUBLE)) {
            jmsMsg.setDoubleProperty(propName,
                                  Double.parseDouble(propValue));
        } else if (propType.equals(JMSProperty.PROPERTY_TYPE_FLOAT)) {
            jmsMsg.setFloatProperty(propName, 
                                  Float.parseFloat(propValue));
        } else if (propType.equals(JMSProperty.PROPERTY_TYPE_INT)) {
            jmsMsg.setIntProperty(propName,
                                  Integer.parseInt(propValue));
        } else if (propType.equals(JMSProperty.PROPERTY_TYPE_LONG)) {
            jmsMsg.setLongProperty(propName,
                                  Long.parseLong(propValue));
        } else if (propType.equals(JMSProperty.PROPERTY_TYPE_SHORT)) {
            jmsMsg.setShortProperty(propName,
                                  Short.parseShort(propValue));
        } else if (propType.equals(JMSProperty.PROPERTY_TYPE_STRING)) {
            jmsMsg.setStringProperty(propName,
                                  propValue);
        } else {
            throw new Exception(mMessages.getString(
                        "JMSBC-E0731.JMSPropertyTypeUnknown",
                        new Object[] {propName, propType}));            
        }
    }
    
    
    private boolean isNonEmpty (Object obj) {
        boolean isNonEmpty = false;
        if (obj != null) {
            if (obj instanceof String) {
                if ( ((String)obj).length() > 0) {
                    isNonEmpty = true;
                }
            } else {
                isNonEmpty = true;
            }
        }
        
        return isNonEmpty;
    }
    
    private boolean isNonEmpty (Integer i) {
        return i != null;
    }
    
    private boolean isNonEmpty (Long l) {
        return l != null;
    }
    
    private void copyNode(Node src, Node dest) {
        NodeList nl = src.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node n = nl.item(i);
            n = dest.getOwnerDocument().importNode(n, true);
            dest.appendChild(n);
        }
        
        NamedNodeMap attrs = src.getAttributes();
        NamedNodeMap destAttrs = dest.getAttributes();
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                Node attr = attrs.item(i);
                Node n = dest.getOwnerDocument().importNode(attr, true);
                if (destAttrs != null) {
                    destAttrs.setNamedItemNS(n);
                }
            }
        }
    }    
    
    private byte[] getPartIfAvailableAsAttachment(Node aNode, NormalizedMessage nm) throws IOException{
        if (!WrapperUtil.isNodeXopInclude(aNode)) {
        	return null;
        }
        String contentId = WrapperUtil.getXopContentId(aNode);
        DataHandler dataHandler = nm.getAttachment(contentId);
        InputStream in = dataHandler.getInputStream();
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        byte[] bytes = new byte[128];
        int len;
        while((len = in.read(bytes)) != -1){
        	bout.write(bytes, 0, len);
        }
        //reset inputstream if somewants to read again
        in.reset();
        return bout.toByteArray();
    }
}
