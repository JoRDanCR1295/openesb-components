/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc;

import static com.sun.jbi.dcombc.util.XmlUtil.transformToString;

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

import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMMessage;

import com.sun.jbi.dcombc.dcom.Channel;

import com.sun.encoder.Encoder;

import com.sun.xsd.model.FastSchemaFactory;
import com.sun.xsd.model.FastSchema;

import com.sun.jbi.dcombc.util.XmlUtil;
import com.sun.jbi.dcombc.util.WSDLUtilities;

/**
 * DCOM message denormalizer class. Converts an NMR message to a DCOM message using the WSDL defined
 * DCOM extensibility elements and message parts as mapping instructions.
 *
 * @author Chandrakanth Belde
 */
public class DCOMDenormalizer {
	/**
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(DCOMDenormalizer.class);

    private static final Logger mLogger = Messages.getLogger(DCOMDenormalizer.class);

    private static DCOMDenormalizer singleton;

    protected DCOMDenormalizer() {
		//
    }

    // ////
    // public methods
    // ////

    public static DCOMDenormalizer getInstance() {
        if (singleton == null) {
            singleton = new DCOMDenormalizer();
        }
        return singleton;
    }

    /**
     * Produces a DCOM Message given an NMR (NormalizedMessage) message.
     *
     * @param dcomChannel The DCOM Channel used to create the DCOM message.
     * @param normalizedMessage The NormalizedMessage from the MessageExchange.
     * @param operationName The service operation.
     * @param endpoint The service endpoint containing the service operation.
     * @throws Exception upon error.
     */
    public String denormalize(Channel dcomChannel,
    						  NormalizedMessage normalizedMessage,
    						  QName operationName) throws Exception {

        Endpoint endpoint = dcomChannel.getEndpoint();
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

        // Construct the DCOM message payload - refer to the message parts
        // per encoding rules
        return getMessagePartPayload(endpoint, operationName, wrapperParser);
    }
    
    /**
     * 
     * @param endpoint
     * @param operationName
     * @param dcomMessage
     * @param wrapperParser
     * 
     * @return
     * @throws Exception
     */
    private String getMessagePartPayload(Endpoint endpoint,
    									 QName operationName,
    									 WrapperParser wrapperParser) 
    									 throws Exception {
        // locate the WSDL message
        Message wsdlMessage = null;
        String data = null;
        Map operations = endpoint.getDCOMOperations();
        Map operationMeps = endpoint.getOperationMsgExchangePatterns();
        Map partMappings = endpoint.getMessagePartEncoderMapping();
        DCOMOperation operation = null;
        Part aPart = null;

        boolean inout = false;
        String mep = null;

        if (operations.get(operationName) == null) {
            throw new Exception(mMessages.getString("DCOMDenormalizer.INVALID_OPNAME", operationName));
        }
        mep = (String) operationMeps.get(operationName);
        if (mep == null || mep.equals(Endpoint.EndpointMessageType.UNSUPPORTED)) {
            throw new Exception(mMessages.getString("DCOMDenormalizer.INVALID_MEP", operationName));
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
            throw new Exception(mMessages.getString("DCOMDenormalizer.INVALID_MESSAGE", operationName));
        }

        Collection wsdlParts = wsdlMessage.getParts().values();
        Part[] partArray = (Part[]) wsdlParts.toArray(new Part[0]);
        aPart = (Part) partArray[0];
        String partName = aPart.getName();

        NodeList parts = wrapperParser.getPartNodes(partName);
        Node aNode = (Node) parts.item(0);

        // XML element node vs. simple Text node?
        /*if (aPart.getElementName() != null) {
            // May want to add basic xml validation later
            data = transformToString(aNode, "UTF-8", true, "xml");
        } */
        data = transformToString(aNode, "UTF-8", true, "xml");
        return data;
    }
}
