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
 * @(#)JMSNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.jbi.messaging.NormalizedMessage;
import javax.jms.BytesMessage;
import javax.jms.MapMessage;
import javax.jms.TextMessage;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.sun.encoder.Encoder;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.extensions.JMSMapMessage;
import com.sun.jbi.jmsbc.extensions.JMSMapMessagePart;
import com.sun.jbi.jmsbc.extensions.JMSMessage;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSProperties;
import com.sun.jbi.jmsbc.extensions.JMSProperty;
import com.sun.jbi.jmsbc.jms.Channel;
import com.sun.jbi.jmsbc.jms.Util;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;

/**
 * JMS message normalizer class (jms message to nmr message)
 *
 */
public class JMSNormalizer {

    private static final Messages mMessages =
        Messages.getMessages(JMSNormalizer.class);
    private static final Logger mLogger =
        Messages.getLogger(JMSNormalizer.class);

    private DocumentBuilder mBuilder = null;    
    private Transformer mTrans = null;
    
    private WrapperBuilder wrapperBuilder;
    
    public JMSNormalizer() throws Exception {
        try {
            wrapperBuilder = HelperFactory.createBuilder();        
        } catch (WrapperProcessingException ex) {
            throw new Exception(mMessages.getString("JMSBC-E0741.WrapperBuilderCreateFailed"), ex);
        }

        try {        
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            throw new Exception(mMessages.getString("JMSBC-E0737.DocumentBuilderCreateFailed"), ex);
        }        
        
        try {        
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(mMessages.getString("JMSBC-E0736.TransformerCreateFailed"), ex);
        }                
    }
    
    /**
     * Note that it is not safe to call this method concurrently on the same instance of JMSNormalizer,
     * so only one thread should work on a specific instance.
     *
     * To prevent a regression, it should be noted that this method was not safe as a static as neither the 
     * documentbuilder or builder instance are guaranteed to be thread safe.
     */
    private Document newDocument() throws ParserConfigurationException {
        Document doc = null;
        // As long as each instance is only be used by a single thread at the same time we do not want to synchronize
        // synchronized(mBuilder) {
            doc = mBuilder.newDocument();
        // }
            
        return doc;
    }
    
    public void normalize (javax.jms.Message jmsMsg,
                           NormalizedMessage normalizedMsg,
                           Endpoint endpoint,
                           JMSOperation jmsOp,
                           JMSMessage mappingInfo,
                           boolean isOperationInput) 
        throws Exception {
        
    	
         javax.wsdl.Message wsdlMessage = getwsdlMessage(endpoint, jmsOp, isOperationInput);
    	
        wrapperBuilder.initialize(null,
                                  wsdlMessage,
                                  null);

        Map partMappings = endpoint.getMessagePartEncoderMapping();
        //Check if the message is to be forwarded as attachment
        if ( (mappingInfo.forwardAsAttachment()
				&& Util.toIntMessageType(mappingInfo.getMessageType()) == Channel.MESSAGE_TYPE_TEXT) 
				) {
        	byte[] bytes = null;
        	if(jmsMsg instanceof TextMessage){
        		bytes = ((TextMessage)jmsMsg).getText().getBytes("UTF8");
        	}else{
                throw new Exception(mMessages.getString(
						"JMSBC-E0769.UnexpectedJMSMessageTypeReceived",
						new Object[] { jmsMsg.getClass().getName() }));                
        	}
        	JMSBCDataSource ds = new JMSBCDataSource(new ByteArrayInputStream(bytes), mappingInfo.getTextPart()); 
        	String cid = wrapperBuilder.addPartWithAttachment( mappingInfo.getTextPart());
        	normalizedMsg.addAttachment(cid, new DataHandler(ds));
        }else if (Util.toIntMessageType(mappingInfo.getMessageType()) == Channel.MESSAGE_TYPE_BYTES){
        	byte[] bytes = null;
        	String bytesPart = mappingInfo.getBytesPart();
        	if(bytesPart == null){
                throw new Exception(mMessages.getString(
						"JMSBC-E0770.BytesPartIsAbsentForBytesMessageType"));                
        	}
            Part aPart = WSDLUtilities.findWSDLMessagePart(bytesPart, wsdlMessage);
            if(!WSDLUtilities.isBuiltInBinaryType(aPart.getTypeName())){
                throw new Exception(mMessages.getString(
				"JMSBC-E0771.BytesPartIsNotBinary"));                
            }
        	if(jmsMsg instanceof BytesMessage){
        		BytesMessage bytesMessage = (BytesMessage)jmsMsg;
        		try{
        			int len = (int)bytesMessage.getBodyLength();
        			bytes = new byte[len];
        			bytesMessage.readBytes(bytes);
        		} catch (AbstractMethodError amerr) { //JMS 1.0 provider
        			byte[] bytesbuff = new byte[2048];
        			byte[] totbyte = null ; 
        			int len = 0,totlen = 0;
        			do{
        				len = bytesMessage.readBytes(bytesbuff);
        				totlen+=len;
        				if(totbyte == null) {
        					totbyte = new byte[totlen];
        					System.arraycopy(bytesbuff, 0, totbyte, 0, len);
        				} else {
        					byte tempbarr[] = new byte[totlen];
        					System.arraycopy(totbyte, 0, tempbarr, 0, totbyte.length);
        					System.arraycopy(bytesbuff, 0, tempbarr, totbyte.length, len);
        					totbyte = tempbarr;
        				}
        			} while(len == 2048);
        			bytes = new byte[totlen];
        			System.arraycopy(totbyte, 0, bytes, 0, totlen);
        		}
        		
        	}else{
                throw new Exception(mMessages.getString(
						"JMSBC-E0769.UnexpectedJMSMessageTypeReceived",
						new Object[] { jmsMsg.getClass().getName() }));                
        	}
        	if(mappingInfo.forwardAsAttachment()){
            	JMSBCDataSource ds = new JMSBCDataSource(new ByteArrayInputStream(bytes), bytesPart); 
            	String cid = wrapperBuilder.addPartWithAttachment(bytesPart);
            	normalizedMsg.addAttachment(cid, new DataHandler(ds));
        	}else{
                Document document = newDocument();
                String base64Encoded = Base64Utils.byteToBase64String(bytes);
                Text textNode = document.createTextNode(new String(base64Encoded));
                wrapperBuilder.addPart(bytesPart, new NodeListImpl(textNode));
        	}
        }else{
        // From the JMS message, build the message parts for the body
        buildMessagePayload (jmsMsg, 
                             wsdlMessage,
                             partMappings,
                             wrapperBuilder, 
                             mappingInfo);
        
        }

        
        // From the JMS message, map the headers to the NMR message parts
        // using the mapping instructions in the jms operations parameter elelement
        mapJMSHeadersToParts (jmsMsg, 
                              wsdlMessage, 
                              wrapperBuilder, 
                              mappingInfo);
        
        // From the JMS message, map the properties to the NMR message parts
        // using the mapping instructions in the jms properties element and
        // property elements
        JMSProperties jmsProperties = mappingInfo.getProperties();
        mapJMSPropertiesToParts (jmsMsg, 
                                 wsdlMessage, 
                                 wrapperBuilder,
                                 jmsProperties);
        
        Document doc = wrapperBuilder.getResult();
        normalizedMsg.setContent(new DOMSource(doc));
    }
    
    public void generateEmptyMessage(NormalizedMessage normalizedMsg,
            Endpoint endpoint,
            JMSOperation jmsOp,
            JMSMessage mappingInfo,
            boolean isOperationInput) throws Exception{
    	
        javax.wsdl.Message wsdlMessage = getwsdlMessage(endpoint, jmsOp, isOperationInput);
        wrapperBuilder.initialize(null,
                                  wsdlMessage,
                                  null);
        Document doc = wrapperBuilder.getResult();
        normalizedMsg.setContent(new DOMSource(doc));
    }
    

	private javax.wsdl.Message getwsdlMessage(Endpoint endpoint,
			JMSOperation jmsOp, boolean isOperationInput) {
		javax.wsdl.Message wsdlMessage = null;
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
		return wsdlMessage;
	}
    
    private void buildMessagePayload (javax.jms.Message jmsMsg,
                                      javax.wsdl.Message wsdlMessage,
                                      Map partMappings,
                                      WrapperBuilder wrapperBuilder,
                                      JMSMessage mapDef) 
    throws Exception {
        if (jmsMsg instanceof TextMessage) {
            if (Util.toIntMessageType(mapDef.getMessageType()) != Channel.MESSAGE_TYPE_TEXT) {
                throw new Exception(
                        mMessages.getString("JMSBC-E0738.UnexpectedJMSMessageTypeReceived", 
                            new Object[] {"javax.jms.TextMessage", 
                                          mapDef.getMessageType()}));                
            }
            
            buildMessagePayloadFromTextMessage ((TextMessage)jmsMsg,
                                                wsdlMessage,
                                                partMappings,
                                                wrapperBuilder,
                                                mapDef);
            
        } else if (jmsMsg instanceof MapMessage) {
            if (Util.toIntMessageType(mapDef.getMessageType()) != Channel.MESSAGE_TYPE_MAP) {
                throw new Exception(
                        mMessages.getString("JMSBC-E0738.UnexpectedJMSMessageTypeReceived", 
                            new Object[] {"javax.jms.MapMessage", 
                                          mapDef.getMessageType()}));                
            }
            
            buildMessagePayloadFromMapMessage ((MapMessage)jmsMsg, 
                                               wsdlMessage,
                                               wrapperBuilder,
                                               mapDef);
        }
    }
    
    private void buildMessagePayloadFromTextMessage (TextMessage jmsTxtMsg,
                                                     javax.wsdl.Message wsdlMessage,
                                                     Map partMappings,
                                                     WrapperBuilder wrapperBuilder,
                                                     JMSMessage mapDef)
    throws Exception {
        
        String theTextPayload = jmsTxtMsg.getText();
        // The part where payload goes
        String partPayload = mapDef.getTextPart();
        
        if (!isNonEmpty(partPayload)) {
            mLogger.log(Level.WARNING,
                        "JMSBC-W0710.TextMessageTextToMessagePartMissing",
                        new Object [] {JMSMessage.ATTR_TEXTPART});
            AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0710.TextMessageTextToMessagePartMissing",
                        new Object [] {JMSMessage.ATTR_TEXTPART}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0710");                        
        }
        
        if (theTextPayload == null) {
            mLogger.log(Level.WARNING,
                        "JMSBC-W0711.TextMessageTextIsNull");
        } else {
        
            /* Check if encoder is used
            */        
            Encoder encoder = null;
            String useAttrVal = mapDef.getUse();
            if (useAttrVal == null) {
                throw new Exception(mMessages.getString(
                            "JMSBC-E0733.UnspecifiedUseAttribute"));
            } else if (useAttrVal.equals(mapDef.ATTR_USE_TYPE_ENCODED)) {
                QName messageQName = wsdlMessage.getQName();
                encoder = (Encoder) partMappings.get(messageQName + partPayload);
                if (encoder == null) {
                    throw new Exception(mMessages.getString("JMSNormalizer_Invalid_encodingStyle"));
                }
            }
        
            addPart(partPayload, theTextPayload, wsdlMessage, wrapperBuilder, encoder);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG, 
                            "JMSNormalizer_JMS_TEXT_ADD_AS_PART",
                            new Object [] {partPayload, theTextPayload});
            }        
        }
    }
    
    private void buildMessagePayloadFromMapMessage (MapMessage mapMsg,
                                                    javax.wsdl.Message wsdlMessage,            
                                                    WrapperBuilder wrapperBuilder,
                                                    JMSMessage mapDef) 
    throws Exception {
        JMSMapMessage jmsMapMsg = mapDef.getMapMessage();        
        Enumeration mapNamesEnum = mapMsg.getMapNames();
        if (mapNamesEnum != null) {
            while (mapNamesEnum.hasMoreElements()) {
                String mapName = (String)mapNamesEnum.nextElement();
                Object mapValObj = mapMsg.getObject(mapName);
                String mapValue = null;
                if (mapValObj != null) {
                    if (mapValObj instanceof Boolean) {
                        mapValue = ((Boolean)mapValObj).toString();
                    } else if (mapValObj instanceof Byte) {
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                       "JMSNormalizer_JMS_UNSUPPORTED_MAPMESSAGE_TYPE",
                                        new Object[] {mapName,  mapValObj.getClass().getName()});
                        }
                    }  else if (mapValObj instanceof byte[]) {
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                       "JMSNormalizer_JMS_UNSUPPORTED_MAPMESSAGE_TYPE",
                                        new Object[] {mapName, mapValObj.getClass().getName()});
                        }
                    }  else if (mapValObj instanceof Double) {
                        mapValue = ((Double)mapValObj).toString();
                    } else if (mapValObj instanceof Float) {
                        mapValue = ((Float)mapValObj).toString();
                    } else if (mapValObj instanceof Integer) {
                        mapValue = ((Integer)mapValObj).toString();
                    } else if (mapValObj instanceof Long) {
                        mapValue = ((Long)mapValObj).toString();
                    } else if (mapValObj instanceof Short) {
                        mapValue = ((Short)mapValObj).toString();
                    } else if (mapValObj instanceof String) {
                        mapValue = (String)mapValObj;
                    } else if (mapValObj instanceof Character) {
                        mapValue = ((Character)mapValObj).toString();
                    } else {
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                       "JMSNormalizer_JMS_UNKNOWN_MAPMESSAGE_TYPE",
                                    new Object[] {mapName, mapValObj.getClass().getName()});
                        }
                   }
                }

                if (mapValue != null) {
                    String partName = getMessagePartFromJMSMapMessageParts (jmsMapMsg,
                                                                            mapName);
                    if (partName != null) { // the jms property has a valid mapping to a message part
                        addPart (partName, mapValue, wsdlMessage, wrapperBuilder, null);
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                        "JMSNormalizer_JMS_MAPMESSAGE_ENTRY_ADD_AS_PART",
                                        new Object[]{mapName,
                                                     partName,
                                                     mapValue});
                        }
                    } else {
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                        "JMSNormalizer_JMS_MAPMESSAGE_ENTRY_SKIPPING",
                                        new Object[]{mapName, mapValue});
                        }
                    }
                }                
            } // mapNamesEnum
        }
    }
   
    private void mapJMSHeadersToParts (javax.jms.Message jmsMsg,
                                       javax.wsdl.Message wsdlMessage,            
                                       WrapperBuilder wrapperBuilder,
                                       JMSMessage mapDef) 
    throws Exception {
        // Get correlationId 
       
        String correlationId = jmsMsg.getJMSCorrelationID();
        if (isNonEmpty(correlationId)) {
            String correlationIdPart = mapDef.getCorrelationIdPart();
            if (isNonEmpty(correlationIdPart)) {  // add to message part
                addPart (correlationIdPart, correlationId, wsdlMessage, wrapperBuilder, null);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSNormalizer_JMS_HEADER_ADD_AS_PART",
                                new Object[]{"JMSCorrelationID", 
                                             correlationIdPart,
                                             correlationId});
                }
            }
        }
        
        /* Todo: Need to base64 encode?
        // Get correlationIdAsBytes
        String correlationIdAsBytesPart = opInput.getCorrelationIdAsBytes();
        if (isNonEmpty(correlationIdAsBytesPart) {
            byte [] correlationIdAsBytes = jmsMsg.getJMSCorrelationIDAsBytes();
            if (isNonEmpty(correlationIdAsBytes)) {
                Document document =
                        DocumentBuilderFactory.newInstance()
                                              .newDocumentBuilder()
                                              .newDocument();
                String correlationIdAsByteStr = new String(correlationIdAsBytes);
                Text textNode = document.createTextNode(correlationIdAsByteStr);
                mLogger.log(Level.INFO, 
                            "JMSNormalizer_JMS_HEADER_ADD_AS_PART_TEXTNODE",
                            new Object[]{"JMSCorrelationIDAsBytes", correlationIdAsBytesPart});
                wrapperBuilder.addPart(correlationIdAsBytesPart, 
                                       new NodeListImpl(textNode));
            }
        }
         */

        // Get deliveryMode     
        String deliveryModePart = mapDef.getDeliveryModePart();
        if (isNonEmpty(deliveryModePart)) {
            int deliveryMode = jmsMsg.getJMSDeliveryMode();
            String deliveryModeStr = Util.toStringDeliveryMode(deliveryMode);
            addPart (deliveryModePart, deliveryModeStr, wsdlMessage, wrapperBuilder, null);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG, 
                            "JMSNormalizer_JMS_HEADER_ADD_AS_PART",
                            new Object[]{"JMSDeliveryMode", 
                                         deliveryModePart,
                                         deliveryModeStr});
            }
        }
        
        // Get priority 
        String priorityPart = mapDef.getPriorityPart();
        if (isNonEmpty(priorityPart)) {
            int priority = jmsMsg.getJMSPriority();
            String priorityStr = Integer.toString(priority);
            addPart (priorityPart, priorityStr, wsdlMessage, wrapperBuilder, null);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG, 
                            "JMSNormalizer_JMS_HEADER_ADD_AS_PART",
                            new Object[]{"JMSPriority", 
                                         priorityPart,
                                         priorityStr});
            }
        }
        
        // Get type
        String type= jmsMsg.getJMSType();
        if (isNonEmpty(type)) {
            String typePart = mapDef.getTypePart();
            if (isNonEmpty(typePart)) {
                addPart (typePart, type, wsdlMessage, wrapperBuilder, null);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSNormalizer_JMS_HEADER_ADD_AS_PART",
                                new Object[]{"JMSType", 
                                             typePart,
                                             type});
                }
            }
        }
        
        // Get messageID   
        String messageID = jmsMsg.getJMSMessageID();
        if (isNonEmpty(messageID)) {
            String messageIDPart = mapDef.getMessageIDPart();
            if (isNonEmpty(messageIDPart)) {
                addPart (messageIDPart, messageID, wsdlMessage, wrapperBuilder, null);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG, 
                                "JMSNormalizer_JMS_HEADER_ADD_AS_PART",
                                new Object[]{"JMSMessageID", 
                                             messageIDPart,
                                             messageID});
                }
            }
        }

        // Get redelivered   
        String redeliveredPart = mapDef.getRedeliveredPart();
        if (isNonEmpty(redeliveredPart)) {
            boolean redelivered = jmsMsg.getJMSRedelivered();
            String redeliveredStr = Boolean.toString(redelivered);
            addPart (redeliveredPart, redeliveredStr, wsdlMessage, wrapperBuilder, null);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG, 
                            "JMSNormalizer_JMS_HEADER_ADD_AS_PART",
                            new Object[]{"JMSRedelivered", 
                                         redeliveredPart,
                                         redeliveredStr});
            }            
        }
        
        // Get timestamp   
        String timestampPart = mapDef.getTimestampPart();
        if (isNonEmpty(timestampPart)) {
            long timestamp = jmsMsg.getJMSTimestamp();
            String timestampStr = Long.toString(timestamp);
            addPart (timestampPart, timestampStr, wsdlMessage, wrapperBuilder, null);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG, 
                            "JMSNormalizer_JMS_HEADER_ADD_AS_PART",
                            new Object[]{"JMSTimestamp", 
                                         timestampPart,
                                         timestampStr});
            }
        }
            
    }
    
    private void mapJMSPropertiesToParts (javax.jms.Message jmsMsg,
                                          javax.wsdl.Message wsdlMessage,            
                                          WrapperBuilder wrapperBuilder,            
                                          JMSProperties jmsProperties) 
        throws Exception {
        Enumeration propertyNames = jmsMsg.getPropertyNames();
        if (propertyNames != null && jmsProperties != null) {
            while (propertyNames.hasMoreElements()) {
                String propName = (String)propertyNames.nextElement();
                Object propObjValue = jmsMsg.getObjectProperty(propName);
                String propValue = null;
                if (propObjValue instanceof Boolean) {
                    propValue = ((Boolean)propObjValue).toString();
                } else if (propObjValue instanceof Byte) {
                    // ToDo: Need to base64 encode byte data?
                    mLogger.log(Level.WARNING, 
                               "JMSBC-E0730.JMSPropertyTypeUnsupported",
                                new Object[] {propName, propObjValue.getClass().getName()});
                    AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-E0730.JMSPropertyTypeUnsupported",
                                new Object[] {propName, propObjValue.getClass().getName()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-E0730");                                    
                } else if (propObjValue instanceof Double) {
                    propValue = ((Double)propObjValue).toString();
                } else if (propObjValue instanceof Float) {
                    propValue = ((Float)propObjValue).toString();
                } else if (propObjValue instanceof Integer) {
                    propValue = ((Integer)propObjValue).toString();
                } else if (propObjValue instanceof Long) {
                    propValue = ((Long)propObjValue).toString();
                } else if (propObjValue instanceof Short) {
                    propValue = ((Short)propObjValue).toString();
                } else if (propObjValue instanceof String) {
                    propValue = (String)propObjValue;
                } else if(propObjValue != null){
                    mLogger.log(Level.WARNING, 
                               "JMSBC-E0731.JMSPropertyTypeUnknown",
                                new Object[] {propName, propObjValue.getClass().getName()});
                    AlertsUtil.getAlerter().warning(mMessages.getString( "JMSBC-E0731.JMSPropertyTypeUnknown",
                                new Object[] {propName, propObjValue.getClass().getName()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-E0731");                                  
                }

                if (propValue != null) {
                    String partName = getMessagePartFromJMSProperty (jmsProperties,
                                                                     propName);
                    if (partName != null) { // the jms property has a valid mapping to a message part
                        addPart (partName, propValue, wsdlMessage, wrapperBuilder, null);
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                        "JMSNormalizer_JMS_PROPERTY_ADD_AS_PART",
                                        new Object[]{propName,
                                                     propObjValue.getClass().getName(),
                                                     partName,
                                                     propValue});
                        }
                    } else {
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG, 
                                        "JMSNormalizer_JMS_PROPERTY_SKIPPING",
                                        new Object[]{propName,
                                                     propObjValue.getClass().getName(),
                                                     propValue});
                        }
                    }
                }
            }
        }
    }

    private void addPart (String partName,            
                          String partValue,
                          javax.wsdl.Message wsdlMessage,            
                          WrapperBuilder wrapperBuilder,
                          Encoder encoder)
    throws Exception {

        Part aPart = WSDLUtilities.findWSDLMessagePart(partName, wsdlMessage);
        Document document = null;        
        if (aPart != null) {
            if (encoder != null) {                
                // It is safe to assume all WSDL validation are done by now.            
                // Decode raw data and add message part
                Source source = encoder.decodeFromString(partValue);
                Element element = getRootElement(source);
                wrapperBuilder.addPart(partName, element);
            } else {
                // XML element node vs. simple Text node?
                if (aPart.getElementName() != null) {
                    // May want to add basic xml validation later
                    document = XmlUtil.createDocumentFromXML(mBuilder, new String(partValue));
                    Element element = document.getDocumentElement();
                    wrapperBuilder.addPart(partName, element);
                } else { // must be "type" otherwise there would've been a WSDL validation error
                    // We may still be dealing with XML node
                    if (WSDLUtilities.isXsdAnyType(aPart.getTypeName())) {
                        try {
                        	document = XmlUtil.createDocumentFromXML(mBuilder, new String(partValue));
                            Element element = document.getDocumentElement();
                            wrapperBuilder.addPart(partName, element);
                        } catch (SAXException e) {
                            // well, this is not a complex type
                            // treat it as a built-in type now
                            document = mBuilder.newDocument();
                            Text textNode = document.createTextNode(new String(partValue));  // use default encoding for the locale
                            wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                        }
                    }else if (!WSDLUtilities.isBuiltInType(aPart.getTypeName())) {
                        // complex type, need to remove the part element wrapper
                        document = XmlUtil.createDocumentFromXML(mBuilder, new String(partValue));
                        Element element = document.getDocumentElement();
                        // first check that the element name (localpart) matches part name
                        if (element.getLocalName().equals(partName)) {
                            wrapperBuilder.addPart(partName, element.getChildNodes());
                        } else {
                            throw new Exception(mMessages.getString(
                                        "JMSBC-E0742.DocElementNameMessagePartNameMismatch",
                                        new Object[] {element.getLocalName(),
                                        			  aPart.getTypeName().toString(),
                                                      partName}));
                        }
                    } else {
                        // treat it as Text node
                        document = newDocument();
                        // Note that we need to treat text based and binary data differently
                        // For now, we support text based data only
                        Text textNode = document.createTextNode(new String(partValue));
                        wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                    }
                }
            }
        } else {
            throw new Exception(mMessages.getString(
                        "JMSBC-E0739.PartNotDefinedInWSDLMessage", 
                        new Object[] {partName,
                                      wsdlMessage.getQName()}));
        }        
    }
        
    // Find the part name for the jms property from the mapping as defined in the wsdl
    private String getMessagePartFromJMSProperty (JMSProperties jmsProperties,
                                                  String jmsPropName) {
        Map propsMap = jmsProperties.getProperties();
        String partName = null;
        if (propsMap != null && propsMap.keySet().contains(jmsPropName)) {
            partName = ((JMSProperty)propsMap.get(jmsPropName)).getPart();
        }
        return partName;
    }
    
    // Find the part name for the jms mapmessage entry from the mapping as defined in the wsdl
    private String getMessagePartFromJMSMapMessageParts (JMSMapMessage jmsMapMsg,
                                                         String jmsMapName) {
        String partName = null;
        Map jmsMapMsgMap = jmsMapMsg.getMapMessageParts();
        if (jmsMapMsgMap != null && jmsMapMsgMap.keySet().contains(jmsMapName)) {
            partName = ((JMSMapMessagePart)jmsMapMsgMap.get(jmsMapName)).getPart();
        }                        
        return partName;
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
    
    private Element getRootElement(Source source) throws Exception {
    	Element root = null;
        if (source instanceof DOMSource) {
            Node sourceNode = ((DOMSource) source).getNode();
            if (sourceNode instanceof Element) {
               root = (Element) sourceNode;
            } else if (sourceNode instanceof Document) {
                root = ((Document) sourceNode).getDocumentElement();
            }
        } else {
	    // convert Source to DOMResult
	    try {
	        DOMResult result = XmlUtil.transformToDOMResult(mTrans, source);
	        root = ((Document) result.getNode()).getDocumentElement();
	    } catch (Exception e) {
	        throw new Exception(mMessages.getString("JMSBC-E0740.SourceToDOMTransformFailed"), e);
	    }
	}
        
        return root;
    }    
}
