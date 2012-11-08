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
 * @(#)ClientOperationsHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jbi.messaging.MessagingException;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.engine.workflow.clientapi.ClientApiException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryDocument;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;

/**
 * 
 * @author radval
 */
public class ClientOperationsHelper {

    private static final String TASK_ID = "taskId";

    private static final String OUTPUT_MSG = "outputMsg";

    private static final String GROUP_PART = "group";

    // Used
    private static final String USER_PART = "user";

    private static final String USER_ID_PART = "userId";
    
    private static final String START_INDEX = "start";
    
    private static final String PAGE_SIZE = "pageSize";

    // queryString
    private static final String Query_PART = "queryString";

    private static final String Query_El_Name = "query";
    
    private static final String Query_El_NS = "http://jbi.com.sun/wfse/wsdl/TaskCommon";

    private static final String EXCLUDED_GROUP_PART = "excludedGroup";

    private static final String EXCLUDED_USER_PART = "excludedUser";

    private static String RESULT_PART_NAME = "resultCode";

    private static String TASK_OUTPUT_WRAPPER = "taskOutputWrapper";

    private static DocumentBuilder mBuilder = null;

    /** Creates a new instance of ClientOperationsHelper */
    public ClientOperationsHelper() {
    }

    public static QueryType extractQuery(Element source, Operation operation)
            throws ClientApiException {
        QueryType query = null;
        try {
            Document normalDoc = source.getOwnerDocument();

            Element normalRoot = normalDoc.getDocumentElement();
            Input input = operation.getInput();
            Message inputMessage = input.getMessage();

            WrapperParser wrapperParser = HelperFactory.createParser();

            wrapperParser.parse(normalDoc, inputMessage);

            Map parts = inputMessage.getParts();
            Iterator it = parts.values().iterator();
            Element queryWrapper = null;
            while (it.hasNext()) {
                Part part = (Part) it.next();                
                if (part.getName().equals((Query_PART))) {
                    if (wrapperParser.hasPart(Query_PART)) {
                        QName typeQName = part.getTypeName();
                        if (typeQName != null) {
                            
                            NodeList unwrappedList = wrapperParser
                                    .getPartNodes(Query_PART);   
                            //Since rpc stripped off the outter element, we need to create such wrapper
                            if (unwrappedList != null
                                    && unwrappedList.getLength() > 0) {
                                queryWrapper = normalDoc.createElementNS(Query_El_NS, Query_El_Name);
                                NamedNodeMap atts = normalRoot.getAttributes();
                                for (int index = 0; index < atts.getLength(); index ++) {
                                    Attr att = (Attr) atts.item(index);
                                    queryWrapper. setAttribute(att.getName(), att.getValue());
                               }
                                List<Node> childrenNodes = new ArrayList<Node> ();
                                for (int j = 0; j < unwrappedList.getLength(); j++) {
                                    
                                    Node unwrapped = (Node) unwrappedList
                                            .item(j);
                                    childrenNodes.add(unwrapped);
                                }
                                for (int j = 0; j < childrenNodes.size(); j ++) {
                                    Node unwrapped = (Node) childrenNodes
                                    .get(j);
                                    if (unwrapped.getNodeType() == Node.ELEMENT_NODE) {      
                                        Element element = null;
                                        element = (Element) unwrapped;
                                        queryWrapper.appendChild(unwrapped.cloneNode(true));
                                    }                                   
                                }
                                if (queryWrapper != null) {
                                    QueryDocument queryDoc = QueryDocument.Factory
                                            .parse(queryWrapper);
                                    if (queryDoc != null) {
                                        query = queryDoc.getQuery();
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            String msg = I18n.loc(
                    "WLM-6111: Failed to denormalize message part {0}",
                    Query_PART);
            throw new ClientApiException("WLM-6111", msg, e);
        }
        return query;
    }
    
    public static Long extractPartNumber (String partName, Element source, Operation operation) throws ClientApiException {
        Long returnNo = new Long(-1);
        try {

            Document normalDoc = source.getOwnerDocument();

            Element normalRoot = normalDoc.getDocumentElement();
            Input input = operation.getInput();
            Message inputMessage = input.getMessage();

            WrapperParser wrapperParser = HelperFactory.createParser();

            wrapperParser.parse(normalDoc, inputMessage);

            Map parts = inputMessage.getParts();
            Iterator it = parts.values().iterator();
            while (it.hasNext()) {
                Part part = (Part) it.next();
                if (part.getName().equals((partName))) {
                    if (wrapperParser.hasPart(partName)) {
                        QName elementQName = part.getElementName();
                        QName typeQName = part.getTypeName();

                        if (elementQName == null && typeQName == null) {
                            String msgEx = I18n
                                    .loc(
                                            "WLM-6024: Failed to denormalize message. part {0} should have element or type attribute defined.",
                                            partName);
                            throw new MessagingException(msgEx);
                        }
                        Element element = null;
                        NodeList unwrappedList = wrapperParser
                                .getPartNodes(partName);
                        if (elementQName != null) {
                            for (int j = 0; j < unwrappedList.getLength(); j++) {
                                Node unwrapped = (Node) unwrappedList.item(j);
                                if (unwrapped.getNodeType() == Node.ELEMENT_NODE
                                        && unwrapped.getLocalName() != null
                                        && unwrapped.getLocalName().equals(
                                                elementQName.getLocalPart())) {
                                    element = (Element) unwrapped;

                                    if (element != null) {
                                        String value = DOMUtils
                                                .getChildCharacterData(element);
                                        if (value != null
                                                && !value.trim().equals("")) {
                                            returnNo = Long.valueOf(value.trim());
                                        }
                                    }
                                    break;
                                }
                            }
                        } else if (typeQName != null) {
                            if (unwrappedList.getLength() > 0) {
                                Node unwrapped = (Node) unwrappedList.item(0);
                                if (unwrapped.getNodeType() == Node.TEXT_NODE) {
                                    Text text = (Text) unwrapped;
                                    returnNo = Long.valueOf(text.getNodeValue()
                                            .trim());
                                    break;
                                }

                            }

                        }

                    } else {
                        String msgEx = I18n.loc(
                                "WLM-6025: Part {0} is missing in the message",
                                partName);
                        throw new MessagingException(msgEx);
                    }

                }
            }

        } catch (Throwable th) {
            String msg = I18n.loc("WLM-6026: Unable to normalize message");
            throw new ClientApiException(
                    "ClientOperationsHelper.FailedToNormalize", msg, th);
        }
        return returnNo;
    }

    public static Long extractTaskId(Element source, Operation operation)
            throws ClientApiException {
        return extractPartNumber (TASK_ID, source, operation);
    }


    /**
     * Extract the content of the output message to set, wrapp the content
     * within an element "content"
     * 
     * @param source
     * @param operation
     * @return
     * @throws ClientApiException
     */
    public static Element extractTaskOutputFromClient(Element source,
            Operation operation) throws ClientApiException {
        Element outputElement = null;
        Document normalDoc = source.getOwnerDocument();

        try {
            Document newDoc = XmlUtil.createDocument(true);

            Element normalRoot = normalDoc.getDocumentElement();
            Input input = operation.getInput();
            Message inputMessage = input.getMessage();

            WrapperParser wrapperParser = HelperFactory.createParser();

            wrapperParser.parse(normalDoc, inputMessage);

            Map parts = inputMessage.getParts();
            Iterator it = parts.values().iterator();
            while (it.hasNext()) {
                Part part = (Part) it.next();
                if (part.getName().equals(OUTPUT_MSG)) {
                    if (wrapperParser.hasPart(OUTPUT_MSG)) {
                        QName elementQName = part.getElementName();
                        QName typeQName = part.getTypeName();

                        if (elementQName == null && typeQName == null) {
                            String msgEx = I18n
                                    .loc(
                                            "WLM-6024: Failed to denormalize message. part {0} should have element or type attribute defined.",
                                            OUTPUT_MSG);
                            throw new MessagingException(msgEx);
                        }
                        NodeList unwrappedList = wrapperParser
                                .getPartNodes(OUTPUT_MSG);
                        if (elementQName != null) {
                            for (int j = 0; j < unwrappedList.getLength(); j++) {
                                Node unwrapped = (Node) unwrappedList.item(j);
                                if (unwrapped.getNodeType() == Node.ELEMENT_NODE
                                        && unwrapped.getLocalName() != null
                                        && unwrapped.getLocalName().equals(
                                                elementQName.getLocalPart())) {
                                    Element element = (Element) unwrapped;
                                    outputElement = (Element) newDoc
                                            .importNode(element, true);
                                    break;
                                }
                            }
                        } else if (typeQName != null) {
                            outputElement = newDoc
                                    .createElement(TASK_OUTPUT_WRAPPER);

                            if (unwrappedList.getLength() > 0) {
                                for (int j = 0; j < unwrappedList.getLength(); j++) {
                                    Node unwrapped = (Node) unwrappedList
                                            .item(j);
                                    Node newNode = newDoc.importNode(unwrapped,
                                            true);
                                    outputElement.appendChild(newNode);

                                }

                            }

                        }

                    } else {
                        String msgEx = I18n.loc(
                                "WLM-6025: Part {0} is missing in the message",
                                OUTPUT_MSG);
                        throw new MessagingException(msgEx);
                    }

                }
            }

            newDoc.appendChild(outputElement);
            //       
        } catch (Throwable th) {
            String msg = I18n.loc("WLM-6026: Unable to normalize message");
            throw new ClientApiException(
                    "ClientOperationsHelper.FailedToNormalize", msg, th);
        }

        return outputElement;
    }

    public static Element converStatus(
            StaticTaskManagementService.OperationStatus status,
            Operation operation) throws ClientApiException {
        Element result = null;

        String statusElementName = "";
        try {
            Document normalDoc = newDocument();

            String operationOutputName = null;
            Message msg = null;
            Output output = operation.getOutput();

            String resultCode = null;

            if (status
                    .equals(StaticTaskManagementService.OperationStatus.SUCCESS)) {
                resultCode = "SUCCESS"; // NOT I18N
            } else {
                resultCode = "FAILED";// NOT I18N
            }

            if (output != null) {
                operationOutputName = output.getName();
                msg = output.getMessage();
                if (msg != null) {
                    WrapperBuilder wrapperBuilder = newWrapperBuilder();
                    wrapperBuilder.initialize(normalDoc, msg,
                            operationOutputName);
                    Part part = msg.getPart(RESULT_PART_NAME);
                    if (part != null) {
                        Text text = normalDoc.createTextNode("" + resultCode);
                        QName type = part.getTypeName();
                        if (type != null) {
                            NodeList list = new NodeListImpl(text);
                            wrapperBuilder.addPart(part.getName(), list);
                            normalDoc = wrapperBuilder.getResult();
                        } else {
                            QName element = part.getElementName();
                            Element elementRoot = normalDoc
                                    .createElement(element.getLocalPart());

                            elementRoot.setAttribute(
                                    XMLConstants.XMLNS_ATTRIBUTE, element
                                            .getNamespaceURI());
                            elementRoot.appendChild(text);
                            wrapperBuilder.addPart(part.getName(), elementRoot);
                            normalDoc = wrapperBuilder.getResult();

                        }
                    } else {
                        String msgEx = I18n
                                .loc(
                                        "WLM-6027: Failed to normalize message. missing part with name {0} in message {1}",
                                        RESULT_PART_NAME, msg.getQName()
                                                .toString());
                        throw new ClientApiException(
                                "ClientOperationsHelper.MissingPart", msgEx);
                    }
                } else {
                    String msgEx = I18n
                            .loc(
                                    "WLM-6028: Failed to normalize message. wsdl operation {0} is missing message in its <output>.",
                                    operation.getName());
                    throw new ClientApiException(
                            "ClientOperationsHelper.MissingMessage", msgEx);
                }
            } else {
                String msgEx = I18n
                        .loc(
                                "WLM-6029: Failed to normalize message. wsdl operation {0} is missing <output>.",
                                operation.getName());
                throw new ClientApiException(
                        "ClientOperationsHelper.MissingMessage", msgEx);
            }

            result = normalDoc.getDocumentElement();

        } catch (Throwable th) {
            throw new ClientApiException(th);
        }
        return result;
    }



    public static String extractUser(Element source, Operation operation)
            throws ClientApiException {
        String userNode = extractSimplePart(USER_PART, source, operation);
        return userNode;

    }

    public static String extractUserID(Element source, Operation operation)
            throws ClientApiException {
        String userNode = extractSimplePart(USER_ID_PART, source, operation);
        return userNode;

    }
    
    public static String extractExcludedGroup(Element input, Operation operation) {
        // TODO Auto-generated method stub
        String excludedGroup = extractSimplePart(EXCLUDED_GROUP_PART, input, operation);
        return excludedGroup;
    }

    public static String extractExcludedUser(Element input, Operation operation) {
        // TODO Auto-generated method stub
        String excludedUser = extractSimplePart(EXCLUDED_USER_PART, input, operation);
        return excludedUser;
    }    
    
    public static String extractRole(Element source, Operation operation)
            throws ClientApiException {
        String group = extractSimplePart(GROUP_PART, source, operation);
        return group;
    }

    public static String extractSimplePart(String partName, Element source,
            Operation operation) throws ClientApiException {
        Node returnNode = null;

        String user = null;

        Document normalDoc = source.getOwnerDocument();

        try {
            Document newDoc = XmlUtil.createDocument(true);

            Element normalRoot = normalDoc.getDocumentElement();
            Input input = operation.getInput();
            Message inputMessage = input.getMessage();

            WrapperParser wrapperParser = HelperFactory.createParser();

            wrapperParser.parse(normalDoc, inputMessage);

            Map parts = inputMessage.getParts();
            Iterator it = parts.values().iterator();
            while (it.hasNext()) {
                Part part = (Part) it.next();
                if (part.getName().equals(partName)) {
                    if (wrapperParser.hasPart(partName)) {
                        QName elementQName = part.getElementName();
                        QName typeQName = part.getTypeName();

                        if (elementQName == null && typeQName == null) {
                            String msgEx = I18n
                                    .loc(
                                            "WLM-6024: Failed to denormalize message. part {0} should have element or type attribute defined.",
                                            partName);
                            throw new MessagingException(msgEx);
                        }
                        NodeList unwrappedList = wrapperParser
                                .getPartNodes(partName);
                        if (elementQName != null) {
                            for (int j = 0; j < unwrappedList.getLength(); j++) {
                                Node unwrapped = (Node) unwrappedList.item(j);
                                if (unwrapped.getNodeType() == Node.ELEMENT_NODE
                                        && unwrapped.getLocalName() != null
                                        && unwrapped.getLocalName().equals(
                                                elementQName.getLocalPart())) {
                                    Element element = (Element) unwrapped;
                                    returnNode = newDoc.importNode(element,
                                            true);
                                    break;
                                }
                            }
                        } else if (typeQName != null) {
                            if (unwrappedList.getLength() > 0) {
                                returnNode = newDoc.createTextNode("");
                                String textvalue = "";
                                for (int j = 0; j < unwrappedList.getLength(); j++) {
                                    Node unwrapped = (Node) unwrappedList
                                            .item(j);
                                    Node newNode = newDoc.importNode(unwrapped,
                                            true);
                                    if (newNode.getNodeType() == Node.TEXT_NODE) {
                                        textvalue = textvalue + newNode.getNodeValue();
                                    }

                                }
                                returnNode.setTextContent(textvalue);
                            }

                        }

                    } else {
                        String msgEx = I18n.loc(
                                "WLM-6025: Part {0} is missing in the message",
                                partName);
                        throw new MessagingException(msgEx);
                    }

                }
            }

            // newDoc.appendChild(userNode);
            //       
        } catch (Throwable th) {
            String msg = I18n.loc("WLM-6026: Unable to normalize message");
            throw new ClientApiException(
                    "ClientOperationsHelper.FailedToNormalize", msg, th);
        }

        if (returnNode != null) {
            user = XmlUtil.getText(returnNode);
        }

        return user;
    }

    public static WrapperBuilder newWrapperBuilder() throws ClientApiException {
        WrapperBuilder wrapperBuilder = null;
        try {
            wrapperBuilder = HelperFactory.createBuilder();
        } catch (WrapperProcessingException ex) {
            throw new ClientApiException(
                    "ClientOperationsHelper.FailToCreateWrapperBuilder",
                    I18n.loc("WLM-6030: Failed to create wrapper builder {0}.",
                            ex.getMessage()), ex);
        }

        return wrapperBuilder;
    }

    private static Document newDocument() throws ParserConfigurationException {
        if (mBuilder == null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory
                    .newInstance();
            mBuilder = factory.newDocumentBuilder();
        }
        return mBuilder.newDocument();
    }

    public static int extractStartIndex(Element input, Operation operation) throws ClientApiException {
        // TODO Auto-generated method stub
        return extractPartNumber (START_INDEX, input, operation).intValue();
    }

    public static int  extractPageSize(Element input, Operation operation) throws ClientApiException {
        // TODO Auto-generated method stub
        return extractPartNumber (PAGE_SIZE, input, operation).intValue();
    }



}
