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
 * @(#)LDAPNormalizer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ldapbc;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.add.LDAPAddResponse;
import com.sun.jbi.ldapbc.delete.LDAPDeleteResponse;
import com.sun.jbi.ldapbc.extensions.LDAPConstants;
import com.sun.jbi.ldapbc.extensions.LDAPOperation;
import com.sun.jbi.ldapbc.extensions.LDAPOperationOutput;
import com.sun.jbi.ldapbc.extensions.LDAPResponseAbstract;
import com.sun.jbi.ldapbc.update.LDAPUpdateResponse;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.SearchResult;
import org.w3c.dom.Node;

/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LDAPNormalizer {

    private static final Messages mMessages = Messages.getMessages(LDAPNormalizer.class);
    private static final Logger mLogger = Messages.getLogger(LDAPNormalizer.class);
    private static DocumentBuilder mBuilder = null;
    private WrapperBuilder wrapperBuilder;

    /** Creates a new instance of SoapNormalizer */
    public LDAPNormalizer() throws MessagingException {
        try {
            wrapperBuilder = HelperFactory.createBuilder();
        } catch (final WrapperProcessingException ex) {
            throw new MessagingException(LDAPNormalizer.mMessages.getString(
                    "LDAPBC-E00505.LDAPN_Failed_Create") + ex.getMessage(), ex);
        }
    }

    /**
     *
     * @param rowsUpdated
     * @param exchange
     * @param meta
     * @return
     * @throws MessagingException
     */
    public NormalizedMessage normalize(LDAPResponseAbstract result,
            final MessageExchange exchange, final OperationMetaData meta,
            final MessageExchange mExchange)
            throws MessagingException, NamingException {
        final NormalizedMessage normalMsg = exchange.createMessage();

        try {
            Document normalDoc = LDAPNormalizer.newDocument();
            String partName = null;
            final LDAPOperationOutput ldapOpOutput = meta.getLDAPOperationOutput();
            String opType = meta.getLDAPOperation().getOperationType();

            if (ldapOpOutput != null) {
                partName = ldapOpOutput.getReturnPartName();
                if (partName == null) {
                    final String msgEx = LDAPNormalizer.mMessages.getString("LDAPBC-E00506.LDAPN_Failed_NM") +
                            "missing " + LDAPOperationOutput.ATTR_RETURN_PART_NAME +
                            " attribute in " +
                            LDAPConstants.QNAME_OPERATION_OUTPUT;
                    throw new MessagingException(msgEx);
                }
            } else {
                final String msgEx = LDAPNormalizer.mMessages.getString("LDAPBC-E00506.LDAPN_Failed_NM") +
                        "missing " + LDAPConstants.QNAME_OPERATION_OUTPUT;
                throw new MessagingException(msgEx);
            }

            if (HelperFactory.WRAPPER_ENABLED) {
                String operationOutputName = null;
                Message msg = null;
                final Output output = meta.getBindingOperation().getOperation().getOutput();

                if (output != null) {
                    operationOutputName = output.getName();
                    msg = output.getMessage();

                    if (msg != null) {
                        wrapperBuilder.initialize(normalDoc, msg,
                                operationOutputName);

                        final Part part = msg.getPart(partName);

                        if (part != null) {
                            final Element returnPartElement = normalDoc.createElement(partName);
                            final QName type = part.getTypeName();
                            if (type != null) {
                                processResult(returnPartElement, result, ldapOpOutput, opType, meta);
                                wrapperBuilder.addPart(part.getName(),
                                        returnPartElement);
                                normalDoc = wrapperBuilder.getResult();
                            } else {
                                final QName element = part.getElementName();
                                final Element elementRoot = normalDoc.createElementNS(element.getNamespaceURI(),
                                        element.getLocalPart());

                                returnPartElement.appendChild(elementRoot);
                                processResult(elementRoot, result, ldapOpOutput, opType, meta);
                                wrapperBuilder.addPart(part.getName(),
                                        elementRoot);
                                normalDoc = wrapperBuilder.getResult();
                            }
                        } else {
                            final String msgEx = LDAPNormalizer.mMessages.getString(
                                    "LDAPBC-E00507.LDAPN_Failed_NM_Part") + partName +
                                    "in message " + msg.getQName();
                            throw new MessagingException(msgEx);
                        }
                    } else {
                        final String msgEx = LDAPNormalizer.mMessages.getString(
                                "LDAPBC-E00508.LDAPN_Failed_NM_WS_OPER") +
                                meta.getBindingOperation().getName() +
                                " is missing message in its <output>";
                        throw new MessagingException(msgEx);
                    }
                } else {
                    final String msgEx = LDAPNormalizer.mMessages.getString(
                            "LDAPBC-E00508.LDAPN_Failed_NM_WS_OPER") +
                            meta.getBindingOperation().getName() +
                            " is missing <output> ";
                    throw new MessagingException(msgEx);
                }
            } else {
                final String operationName = meta.getBindingOperation().getName();
                final Element normalRoot = normalDoc.createElement(operationName);
                normalDoc.appendChild(normalRoot);

                if (partName != null) {
                    final Element returnElement = normalDoc.createElement(partName);
                    processResult(returnElement, result, ldapOpOutput, opType, meta);
                    normalRoot.appendChild(returnElement);
                }
            }

            if (LDAPNormalizer.mLogger.isLoggable(Level.INFO)) {
//                LDAPNormalizer.mLogger.log(Level.INFO, "normalized message", normalDoc);
            }

            normalMsg.setContent(new DOMSource(normalDoc));
        } catch (final ParserConfigurationException tex) {
            final String msg = LDAPNormalizer.mMessages.getString("LDAPBC-E00509.LDAPN_Failed_NM_DOM");
            throw new MessagingException(msg, tex);
        } catch (final WrapperProcessingException ex) {
            final String exMsg = LDAPNormalizer.mMessages.getString("LDAPBC-E00506.LDAPN_Failed_NM", new Object[]{ex.getMessage()});
            throw new MessagingException(exMsg, ex);
        }
        
        if (LDAPNormalizer.mLogger.isLoggable(Level.FINEST)) {
            LDAPNormalizer.mLogger.log(Level.FINEST, mMessages.getString("LDAPBC_ACFG0249_LDAP_Normalize_OMP_Accept_msg", new Object[]{normalMsg.toString(), mExchange.getExchangeId()}));
        }          

        return normalMsg;
      
    }

    /**
     *
     * @return
     * @throws ParserConfigurationException
     */
    private static Document newDocument() throws ParserConfigurationException {
        if (LDAPNormalizer.mBuilder == null) {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            LDAPNormalizer.mBuilder = factory.newDocumentBuilder();
        }

        return LDAPNormalizer.mBuilder.newDocument();
    }

    private void processResult(Element root, LDAPResponseAbstract res, final LDAPOperationOutput ldapOpOutput,
            String opType, OperationMetaData meta) throws NamingException {
        if (opType.equals(LDAPOperation.OPERATION_TYPE_SEARCH)) {
            processResultSearch(root, (LDAPResponse) res, ldapOpOutput);
        } else if (opType.equals(LDAPOperation.OPERATION_TYPE_UPDATE)) {
            processResultUpdate(root, (LDAPUpdateResponse) res, ldapOpOutput);
        } else if (opType.equals(LDAPOperation.OPERATION_TYPE_ADD)) {
            processResultAdd(root, (LDAPAddResponse) res, meta);
        } else if (opType.equals(LDAPOperation.OPERATION_TYPE_DELETE)) {
            processResultDelete(root, (LDAPDeleteResponse) res);
        } else {
        }
    }

    private void processResultAdd(Element root, LDAPAddResponse result, OperationMetaData meta) throws NamingException {
//        LDAPNormalizer.mLogger.log(Level.SEVERE,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0245_LDAP_Normalize_Add_Start"));
        Element responsePropertyType = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "property");
        Element code = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "code");
        Node codeValue = root.getOwnerDocument().createTextNode(result.getCode());
        code.appendChild(codeValue);
        responsePropertyType.appendChild(code);

        Element requestId = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "requestId");
        Node requestIdValue = root.getOwnerDocument().createTextNode(result.getRequestId());
        requestId.appendChild(requestIdValue);
        responsePropertyType.appendChild(requestId);
        root.appendChild(responsePropertyType);

        Element opResult = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "OperationResult");
        Node opResultValue = null;

        Element responseEntry = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "AddedEntry");
        if (result.getOpResult()) {
            opResultValue = root.getOwnerDocument().createTextNode("Success");
            opResult.appendChild(opResultValue);
            root.appendChild(opResult);

            List<String> addAttrs = meta.getAddedReturnAttrs();
            DirContext responseContext = result.getResponseDirContext();
            Attributes attrs = responseContext.getAttributes("");
            for (int i = 0; i < addAttrs.size(); i++) {
                String attrName = addAttrs.get(i);
                Element anElem = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), attrName);
                Node vElem;
                if ("dn".equals(attrName)) {
                    vElem = root.getOwnerDocument().createTextNode(responseContext.getNameInNamespace());
                } else if ("objectClass".equals(attrName)) {
                    String value = "";
                    Attribute attr = attrs.get(attrName);
                    value = getAttributeValues(attr);
                    vElem = root.getOwnerDocument().createTextNode(value);
                } else {
                    String contextAttrName = getAttrNameInContext(attrName);
                    String value = "";
                    Attribute attr = attrs.get(contextAttrName);
                    value = getAttributeValues(attr);
                    vElem = root.getOwnerDocument().createTextNode(value);
                }
                anElem.appendChild(vElem);
                responseEntry.appendChild(anElem);
            }
        } else {
            opResultValue = root.getOwnerDocument().createTextNode("Fail");
            opResult.appendChild(opResultValue);
            root.appendChild(opResult);

            List<String> addAttrs = meta.getAddedReturnAttrs();
            for (int i = 0; i < addAttrs.size(); i++) {
                String attrName = addAttrs.get(i);
                Element anElem = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), attrName);
                Node vElem = root.getOwnerDocument().createTextNode("null");
                anElem.appendChild(vElem);
                responseEntry.appendChild(anElem);
            }
        }
        root.appendChild(responseEntry);
//        LDAPNormalizer.mLogger.log(Level.SEVERE,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0246_LDAP_Normalize_Add_Finish"));
    }

    private void processResultDelete(Element root, LDAPDeleteResponse result) throws NamingException {
//        LDAPNormalizer.mLogger.log(Level.SEVERE,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0241_LDAP_Normalize_Delete_Start"));
        Element responsePropertyType = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "property");
        Element code = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "code");
        Node codeValue = root.getOwnerDocument().createTextNode(result.getCode());
        code.appendChild(codeValue);
        responsePropertyType.appendChild(code);

        Element requestId = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "requestId");
        Node requestIdValue = root.getOwnerDocument().createTextNode(result.getRequestId());
        requestId.appendChild(requestIdValue);
        responsePropertyType.appendChild(requestId);
        root.appendChild(responsePropertyType);

        Element opResult = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "OperationResult");
        Node opResultValue = null;
        if (result.getOpResult()) {
            opResultValue = root.getOwnerDocument().createTextNode("Success");
        } else {
            opResultValue = root.getOwnerDocument().createTextNode("Fail");
        }
        opResult.appendChild(opResultValue);
        root.appendChild(opResult);
//        LDAPNormalizer.mLogger.log(Level.SEVERE,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0242_LDAP_Normalize_Delete_Finish"));
    }

    private void processResultSearch(Element root, LDAPResponse result, final LDAPOperationOutput ldapOpOutput) throws NamingException {
//        LDAPNormalizer.mLogger.log(Level.INFO,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0243_LDAP_Normalize_Query_Start"));
        List results = result.getEntries();
        List returnAttrs = ldapOpOutput.getReturnAttrs();

        Element responsePropertyType = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "property");
        Element code = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "code");
        Node codeValue = root.getOwnerDocument().createTextNode(result.getCode());
        code.appendChild(codeValue);
        responsePropertyType.appendChild(code);

        Element requestId = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "requestId");
        Node requestIdValue = root.getOwnerDocument().createTextNode(result.getRequestId());
        requestId.appendChild(requestIdValue);
        responsePropertyType.appendChild(requestId);

        Element itemCount = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "EntryCount");
        Node itemCountValue = root.getOwnerDocument().createTextNode(String.valueOf(results.size()));
        itemCount.appendChild(itemCountValue);
        responsePropertyType.appendChild(itemCount);
        root.appendChild(responsePropertyType);

        if (results != null && results.size() > 0) {
            for (int i = 0; i < results.size(); i++) {
                Element entriesElem = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "ResponseEntries");
                LDAPResponse.Entry entry = (LDAPResponse.Entry) results.get(i);
                SearchResult sr = entry.getResult();
                Attributes as = sr.getAttributes();
                Element itemIndex = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "EntryIndex");
                Node itemIndexValue = root.getOwnerDocument().createTextNode(String.valueOf(i));
                itemIndex.appendChild(itemIndexValue);
                entriesElem.appendChild(itemIndex);
                for (int j = 0; j < returnAttrs.size(); j++) {
                    String attName = (String) returnAttrs.get(j);
                    Attribute att = as.get(changeToAtt(attName));
                    if (att == null && !"EntryIndex".equals(attName)) {
                        Element anElem = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), attName);
                        Node vElem = root.getOwnerDocument().createTextNode("null");
                        anElem.appendChild(vElem);
                        entriesElem.appendChild(anElem);
                    }

                    if (att != null && att.size() > 0) {
                        for (int k = 0; k < att.size(); k++) {
                            Element anElem = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), attName);
                            Node vElem = root.getOwnerDocument().createTextNode(att.get(k).toString());
                            anElem.appendChild(vElem);
                            entriesElem.appendChild(anElem);
                        }
                    }

                    att = null;
                }
                root.appendChild(entriesElem);
            }
        }
//        LDAPNormalizer.mLogger.log(Level.INFO,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0244_LDAP_Normalize_Query_Finish"));
    }

    private void processResultUpdate(Element root, LDAPUpdateResponse result, final LDAPOperationOutput ldapOpOutput) throws NamingException {
//        LDAPNormalizer.mLogger.log(Level.SEVERE,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0247_LDAP_Normalize_Update_Start"));
        Element responsePropertyType = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "property");
        Element code = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "code");
        Node codeValue = root.getOwnerDocument().createTextNode(result.getCode());
        code.appendChild(codeValue);
        responsePropertyType.appendChild(code);

        Element requestId = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "requestId");
        Node requestIdValue = root.getOwnerDocument().createTextNode(result.getRequestId());
        requestId.appendChild(requestIdValue);
        responsePropertyType.appendChild(requestId);
        root.appendChild(responsePropertyType);

        Element opResult = root.getOwnerDocument().createElementNS(root.getNamespaceURI(), "OperationResult");
        String resultStr = "";
        if (result.getOpResult()) {
            resultStr = "Success";
        } else {
            resultStr = "Fail";
        }
        Node opResultValue = root.getOwnerDocument().createTextNode(resultStr);
        opResult.appendChild(opResultValue);
        root.appendChild(opResult);

//        LDAPNormalizer.mLogger.log(Level.SEVERE,
//                LDAPNormalizer.mMessages.getString("LDAPBC_ACFG0248_LDAP_Normalize_Update_Finish"));
    }

    private String changeToAtt(String str) {
        int index = str.indexOf(".");
        if (index < 1) {
            return str;
        }
        return str.substring(index + 1);
    }

    private String getAttrNameInContext(String objectClassAndAttr) {
        int index = objectClassAndAttr.indexOf(".");
        return objectClassAndAttr.substring(index + 1);
    }

    private String getAttributeValues(Attribute attr) {
        if (attr == null) {
            return "";
        }
        String value = "";
        try {
            value = attr.get(0).toString();

            if (attr.size() > 1) {
                for (int k = 1; k < attr.size(); k++) {
                    value += "," + attr.get(k).toString();
                }
            }
        } catch (NamingException ex) {
            Logger.getLogger(LDAPNormalizer.class.getName()).log(Level.SEVERE, null, ex);
        }
        return value;
    }
}
