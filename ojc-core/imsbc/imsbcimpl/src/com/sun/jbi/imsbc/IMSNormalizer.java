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

package com.sun.jbi.imsbc;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Collection;

import java.io.IOException;

import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSMessage;

import com.sun.jbi.imsbc.util.XmlUtil;
import com.sun.jbi.imsbc.util.WSDLUtilities;
import com.sun.jbi.imsbc.util.AlertsUtil;

import com.sun.encoder.Encoder;

/**
 * IMS message normalizer class (ims message to nmr message)
 *
 * @author Sun Microsystems
 */
public class IMSNormalizer {

    private static final Messages mMessages = Messages.getMessages(IMSNormalizer.class);

    private static final Logger mLogger = Messages.getLogger(IMSNormalizer.class);

    private static IMSNormalizer singleton;

	private Transformer mTrans;

    private WrapperBuilder wrapperBuilder;
	    
	private DocumentBuilder mBuilder;    
    

    public IMSNormalizer() throws Exception {
        try {
            wrapperBuilder = HelperFactory.createBuilder();        
        } catch (WrapperProcessingException ex) {
            throw new Exception(mMessages.getString("IMSBC-E00508.INMR_WrapperBuilder_Create_Failed"), ex);
        }

        try {        
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            throw new Exception(mMessages.getString("IMSBC-E00509.INMR_DocumentBuilder_Create_Failed"), ex);
        }        
        
        try {        
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();
        } catch (TransformerFactoryConfigurationError ex) {
            throw new Exception(mMessages.getString("IMSBC-E00510.INMR_TransformerCreate_Failed"), ex);
        }                
    }


    public static IMSNormalizer getInstance() throws Exception {
        if (singleton == null) {
            singleton = new IMSNormalizer();
        }
        return singleton;
    }

    public NormalizedMessage normalize(MessageExchange exchange,
                                       QName operationName,
                                       IMSMessage imsMessage,
                                       boolean isInput,
                                       Endpoint endpoint,
                                       String payLoad) throws Exception {

        Service service = endpoint.getDefinition().getService(endpoint.getServiceName());
        Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Map partMappings = endpoint.getMessagePartEncoderMapping();

        /**
         * Locate the operation we are interested in. There may be multiple operations by the same
         * name (operation overloading) and the WSDL spec does not allow it. The uniqueness should
         * be guaranteed by the examination of input and/or output names. The WSDL validation
         * should've been enforced the uniqueness at design time. For the time being, we will assume
         * that we don't have operation overloading.
         */
        Iterator it = portType.getOperations().iterator();
        Message wsdlMessage = null;
        while (it.hasNext()) {
            Operation op = (Operation) it.next();
            if (op.getName().equals(operationName.toString()) || op.getName().equals(operationName.getLocalPart())) {
                /**
                 * Since we are not handling Solicit-response or Notification type of IMS BC
                 * operations, it is safe to assume that the message type for ims message is always
                 * associated with the Operation Input.
                 */

                if (isInput) {
                    wsdlMessage = op.getInput().getMessage();
                    break;
                } else {
                    wsdlMessage = op.getOutput().getMessage();
                    break;

                }
            }
        }

        if (wsdlMessage == null) {
			if (mLogger.isLoggable(Level.INFO)) 
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-E00505.INMR_Invalid_Message", operationName ));
            String errMsg = mMessages.getString("IMSBC-E00505.INMR_Invalid_Message", operationName);
            AlertsUtil.getAlerter().critical(mMessages.getString("IMSBC-E00505.INMR_Invalid_Message",
											operationName), 
											IMSBindingComponent.SHORT_DISPLAY_NAME, 
											null, 
											AlertsUtil.getServerType(),
											AlertsUtil.COMPONENT_TYPE_BINDING,
											NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											NotificationEvent.EVENT_TYPE_ALERT,
											"IMSBC-E00505");  
            throw new Exception(errMsg);
        }

        wrapperBuilder.initialize(null, wsdlMessage, null);

        // From the IMS message, build the body from message parts
        return buildMessagePayload(exchange, wsdlMessage, partMappings, wrapperBuilder, imsMessage, payLoad);
    }

    // ////
    // private methods
    // ////

    private NormalizedMessage buildMessagePayload(MessageExchange exchange,
                                                  Message wsdlMessage,
                                                  Map partMappings,
                                                  WrapperBuilder wrapperBuilder,
                                                  IMSMessage imsMessage,
                                                  String data) throws SAXException, IOException, Exception {
        NormalizedMessage normalizedMessage = exchange.createMessage();

        /**
         * IMS BC basically handles data format. However, an important assumption has to be made,
         * i.e. we deal with one message part for each ims operation only. Otherwise, the "part"
         * attribute for ims:element element is required. This assumption will be enforced as part
         * of WSDL validation at design time.
         */
        Document document = null;
        Part aPart = null;
        QName messageQName = wsdlMessage.getQName();
        String use = imsMessage.getUseType();
        String partName = imsMessage.getMessagePart();

        if (partName != null && !partName.equals("")) {
            aPart = wsdlMessage.getPart(partName);
        } else {
            // there is only one part
            Collection parts = wsdlMessage.getParts().values();
            Part[] partArray = (Part[]) parts.toArray(new Part[0]);
            aPart = (Part) partArray[0];
            partName = aPart.getName();
        }

        // It is safe to assume all WSDL validation are done by now.
        if (use.equals(IMSMessage.USE_TYPE_ENCODED)) {
            Encoder encoder = (Encoder) partMappings.get(messageQName + partName);
            if (encoder == null) {
				if (mLogger.isLoggable(Level.INFO)) 
					mLogger.log(Level.INFO, mMessages.getString("IMSBC-E00506.INMR_Invalid_Encodingstyle"));
                throw new Exception(mMessages.getString("IMSBC-E00506.INMR_Invalid_Encodingstyle"));
            }

            // Decode raw data and add message part
            Source source = encoder.decodeFromString(data);
            //Element element = document.getDocumentElement();
            Element element = getRootElement(source);
            wrapperBuilder.addPart(partName, element);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                document = XmlUtil.createDocumentFromXML(true, new String(data));
                Element element = document.getDocumentElement();
                wrapperBuilder.addPart(partName, element);
            } else { // must be "type" otherwise there would've been a WSDL validation error
                // We may still be dealing with XML node
                QName typename = aPart.getTypeName();
                if (!WSDLUtilities.isBuiltInType(typename)) {
                    document = XmlUtil.createDocumentFromXML(true, new String(data));
                    Element element = document.getDocumentElement();
                    wrapperBuilder.addPart(partName, element);
                } else {
                    // treat it as Text node
                    document = mBuilder.newDocument();
                    // Note that we need to treat text based and binary data differently
                    // For now, we support text based data only
                    Text textNode = document.createTextNode(new String(data));
                    wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                }
            }
        }
        normalizedMessage.setContent(new DOMSource(wrapperBuilder.getResult()));

        return normalizedMessage;

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
                DOMResult result = XmlUtil.transformToDOMResult(source);
                root = ((Document) result.getNode()).getDocumentElement();
            } catch (Exception e) {
				if (mLogger.isLoggable(Level.INFO)) 
					mLogger.log(Level.INFO, mMessages.getString("IMSBC-E00507.INMR_Failed_Converttodom"));
                String errMsg = mMessages.getString("IMSBC-E00507.INMR_Failed_Converttodom");
                throw new Exception(errMsg);
            }
        }
        return root;
    }
}
