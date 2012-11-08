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
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.extensions.IMSOperation;
import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.ims.Channel;
import com.sun.jbi.imsbc.util.XmlUtil;
import com.sun.jbi.imsbc.util.WSDLUtilities;
import com.sun.jbi.imsbc.util.AlertsUtil;

import com.sun.encoder.Encoder;
import com.sun.xsd.model.FastSchemaFactory;
import com.sun.xsd.model.FastSchema;


/**
 * IMS message denormalizer class. Converts an NMR message to a IMS message using the WSDL defined
 * IMS extensibility elements and message parts as mapping instructions.
 *
 * @author Sun Microsystems
 */
public class IMSDenormalizer {

    private static final Messages mMessages = Messages.getMessages(IMSDenormalizer.class);

    private static final Logger mLogger = Messages.getLogger(IMSDenormalizer.class);

    private static IMSDenormalizer singleton;

    protected IMSDenormalizer() {

    }

    // ////
    // public methods
    // ////

    public static IMSDenormalizer getInstance() {
        if (singleton == null) {
            singleton = new IMSDenormalizer();
        }
        return singleton;
    }

    /**
     * Produces a IMS Message given an NMR (NormalizedMessage) message.
     *
     * @param imsChannel The IMS Channel used to create the IMS message.
     * @param normalizedMessage The NormalizedMessage from the MessageExchange.
     * @param operationName The service operation.
     * @param endpoint The service endpoint containing the service operation.
     * @throws Exception upon error.
     */
    public String denormalize(Channel imsChannel,
                                   NormalizedMessage normalizedMessage,
                                   QName operationName,
                                   IMSMessage imsMessage) throws Exception {

        Endpoint endpoint = imsChannel.getEndpoint();
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

        // Construct the IMS message payload - refer to the message parts
        // per encoding rules
        return getMessagePartPayload(endpoint, operationName, imsMessage, wrapperParser);
    }

    private String getMessagePartPayload(Endpoint endpoint,
                                              QName operationName,
                                              IMSMessage imsMessage,
                                              WrapperParser wrapperParser) throws Exception {
        // locate the WSDL message
        Message wsdlMessage = null;
        String data = null;
        Map operations = endpoint.getIMSOperations();
        Map operationMeps = endpoint.getOperationMsgExchangePatterns();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        IMSOperation operation = null;
        Part aPart = null;

        boolean inout = false;
        String mep = null;
        String use = imsMessage.getUseType();
        String partName = imsMessage.getMessagePart();
        
        IMSOperation imsOperation = null;
        Iterator imsItr = operations.entrySet().iterator();
        while(imsItr.hasNext())
        {
        	Map.Entry entry = (Map.Entry) imsItr.next();
        	if(entry.getKey().toString().equalsIgnoreCase(operationName.getLocalPart())){
        		imsOperation = (IMSOperation) entry.getValue();
        	}
        }
        
        if (imsOperation == null) {
			if (mLogger.isLoggable(Level.INFO)) 
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-E00501.IDNMR_Invalid_Opname", operationName ));
            AlertsUtil.getAlerter().critical(mMessages.getString("IMSBC-E00501.IDNMR_Invalid_Opname",
											operationName), 
											IMSBindingComponent.SHORT_DISPLAY_NAME, 
											null, 
											AlertsUtil.getServerType(),
											AlertsUtil.COMPONENT_TYPE_BINDING,
											NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											NotificationEvent.EVENT_TYPE_ALERT,
											"IMSBC-E00501");
            throw new Exception(mMessages.getString("IMSBC-E00501.IDNMR_Invalid_Opname", operationName));
        }
        Iterator mepItr = operationMeps.entrySet().iterator();
        while(mepItr.hasNext())
        {
        	Map.Entry entry = (Map.Entry) mepItr.next();
        	if(entry.getKey().toString().equalsIgnoreCase(operationName.getLocalPart())){
        		mep = entry.getValue().toString();
        	}
        }        
        if (mep == null || mep.equals(Endpoint.EndpointMessageType.UNSUPPORTED)) {
            throw new Exception(mMessages.getString("IMSBC-E00502.IDNMR_Invalid_Mep", operationName));
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
            throw new Exception(mMessages.getString("IMSBC-E00503.IDNMR_Invalid_Message", operationName));
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

        if (use.equals(imsMessage.USE_TYPE_ENCODED)) {
            // Locate the encoder
            Encoder encoder = (Encoder) partMappings.get(wsdlMessage.getQName() + partName);
            if (encoder == null) {
                throw new Exception(mMessages.getString("IMSBC-E00504.IDNMR_Invalid_Encodingstyle"));
            }
            // Encode DOM source to raw data format
            Source source = new DOMSource(aNode);
            data = encoder.encodeToString(source);
        } else {
            // XML element node vs. simple Text node?
            if (aPart.getElementName() != null) {
                // May want to add basic xml validation later
                data =XmlUtil.transformToString(aNode, "UTF-8", true, "xml");
            } else { // must be "type" otherwise there would've been a WSDL validation error
                // We may still be dealing with XML node
                QName typename = aPart.getTypeName();
                if (!WSDLUtilities.isBuiltInType(typename)) {
                    data = XmlUtil.transformToString(aNode, "UTF-8", true, "xml");
                } else {
                    // treat it as Text node
                    data = XmlUtil.transformToString(aNode, "UTF-8", true, "text");
                }
            }
        }
        return data;
    }
}
