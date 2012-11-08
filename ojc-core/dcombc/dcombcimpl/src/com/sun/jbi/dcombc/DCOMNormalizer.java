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
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.dom.DOMResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;

import com.sun.jbi.dcombc.extensions.DCOMMessage;

import com.sun.jbi.dcombc.util.XmlUtil;
import com.sun.jbi.dcombc.util.WSDLUtilities;


/**
 * DCOM message normalizer class (dcom message to nmr message)
 *
 * @author Chandrakanth Belde
 */
public class DCOMNormalizer {
	/** 
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(DCOMNormalizer.class);

    private static final Logger mLogger = Messages.getLogger(DCOMNormalizer.class);

    private static DCOMNormalizer singleton;

    protected DCOMNormalizer() {
		//
    }

    // ////
    // public methods
    // ////

    public static DCOMNormalizer getInstance() {
        if (singleton == null) {
            singleton = new DCOMNormalizer();
        }
        return singleton;
    }

    public NormalizedMessage normalize(MessageExchange exchange,
                                       QName operationName,
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
                 * Since we are not handling Solicit-response or Notification type of DCOM BC
                 * operations, it is safe to assume that the message type for dcom message is always
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
            mLogger.log(Level.INFO, "DCOMNormalizer.INVALID_MESSAGE", new Object[] { operationName });
            String errMsg = mMessages.getString("DCOMNormalizer.INVALID_MESSAGE", operationName);
            throw new Exception(errMsg);
        }

        WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
        wrapperBuilder.initialize(null, wsdlMessage, null);

        // From the DCOM message, build the body from message parts
        return buildMessagePayload(exchange, wsdlMessage, partMappings, wrapperBuilder, payLoad);
    }

    // ////
    // private methods
    // ////

    private NormalizedMessage buildMessagePayload(MessageExchange exchange,
                                                  Message wsdlMessage,
                                                  Map partMappings,
                                                  WrapperBuilder wrapperBuilder,
                                                  String data) throws SAXException, IOException, Exception {
        NormalizedMessage normalizedMessage = exchange.createMessage();

        /**
         * DCOM BC basically handles data format. However, an important assumption has to be made,
         * i.e. we deal with one message part for each dcom operation only. Otherwise, the "part"
         * attribute for dcom:element element is required. This assumption will be enforced as part
         * of WSDL validation at design time.
         */
        Document document = null;
        Part aPart = null;
        QName messageQName = wsdlMessage.getQName();
        String partName = null;

        // there is only one part
        Collection parts = wsdlMessage.getParts().values();
        Part[] partArray = (Part[]) parts.toArray(new Part[0]);
        aPart = (Part) partArray[0];
        partName = aPart.getName();

        // XML element node vs. simple Text node?
        if (aPart.getElementName() != null) {
            // May want to add basic xml validation later
            document = XmlUtil.createDocumentFromXML(true, data);
            Element element = document.getDocumentElement();
            wrapperBuilder.addPart(partName, element);
        } else { // must be "type" otherwise there would've been a WSDL validation error
            // We may still be dealing with XML node
            QName typename = aPart.getTypeName();
            if (!WSDLUtilities.isBuiltInType(typename)) {
                document = XmlUtil.createDocumentFromXML(true, data);
                Element element = document.getDocumentElement();
                wrapperBuilder.addPart(partName, element);
            } else {
                // treat it as Text node
                document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
                // Note that we need to treat text based and binary data differently
                // For now, we support text based data only
                Text textNode = document.createTextNode(data);
                wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
            }
        }
        normalizedMessage.setContent(new DOMSource(wrapperBuilder.getResult()));

        return normalizedMessage;
    }

    /**
     * @param source
     * 
     * @return
     * @throws Exception
     */
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
                mLogger.log(Level.INFO, "DCOMNormalizer.FAILED_CONVERTTODOM");
                String errMsg = mMessages.getString("DCOMNormalizer.FAILED_CONVERTTODOM");
                throw new Exception(errMsg);
            }
        }
        return root;
    }
}
