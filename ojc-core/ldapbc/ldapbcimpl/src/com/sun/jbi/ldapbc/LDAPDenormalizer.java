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
 * @(#)LDAPDenormalizer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ldapbc;

import com.sun.jbi.ldapbc.add.LDAPAdd;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamResult;
import java.io.InputStream;
import java.io.Reader;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.delete.LDAPDelete;
import com.sun.jbi.ldapbc.extensions.LDAPOperation;
import com.sun.jbi.ldapbc.extensions.LDAPOperationAbstract;
import com.sun.jbi.ldapbc.update.LDAPUpdate;
import com.sun.jbi.ldapbc.update.UpdateBean;
import com.sun.jbi.ldapbc.update.UpdateBeanAdd;
import com.sun.jbi.ldapbc.update.UpdateBeanRemove;
import com.sun.jbi.ldapbc.update.UpdateBeanRemoveAll;
import com.sun.jbi.ldapbc.update.UpdateBeanReplace;
import com.sun.jbi.ldapbc.util.LdapConnection;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import java.util.HashMap;
import java.util.Hashtable;
import javax.jbi.messaging.MessageExchange;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttribute;
import javax.naming.directory.BasicAttributes;
import javax.naming.directory.SearchControls;

/**
 *
 * LDAPDenormalizer
 *
 */
public class LDAPDenormalizer {

    private static final Messages mMessages = Messages.getMessages(LDAPDenormalizer.class);
    private static final Logger mLogger = Messages.getLogger(LDAPDenormalizer.class);
    private Map connections = null;

    public LDAPOperationAbstract denormalizeOutbound(final NormalizedMessage normalizedMessage,
            final OperationMetaData opMetaData, final MessageExchange mExchange)
            throws MessagingException, Exception {
        if (LDAPDenormalizer.mLogger.isLoggable(Level.FINEST)) {
            LDAPDenormalizer.mLogger.log(Level.FINEST, mMessages.getString("LDAPBC_ACFG0239_LDAP_Denormalize_OMP_Accept_msg", mExchange.getExchangeId()));
        }
        LDAPOperationAbstract res = null;
        if (opMetaData != null) {
            final String operationType = opMetaData.getLDAPOperation().getOperationType();

//            if (operationType.equals(LDAPOperation.OPERATION_TYPE_SEARCH)) {
            try {
                final TransformerFactory tFactory = TransformerFactory.newInstance();
                final Transformer trans = tFactory.newTransformer();
                final Source source = normalizedMessage.getContent();
                final DOMResult result = new DOMResult();
                trans.transform(source, result);

				if (source instanceof StreamSource) {
	                StreamSource stream = (StreamSource) source;
	                InputStream inputStream = stream.getInputStream();
	                if (inputStream != null) {
	                    inputStream.reset();
	                }
	                Reader reader = stream.getReader();
	                if (reader != null) {
	                    reader.reset();
	                }
				}

                final Node node = result.getNode();
                final StringWriter strWriter = new StringWriter();
                final StreamResult sResult = new StreamResult(strWriter);                
                trans.transform(source, sResult);

				if (source instanceof StreamSource) {
	                StreamSource stream = (StreamSource) source;
	                InputStream inputStream = stream.getInputStream();
	                if (inputStream != null) {
	                    inputStream.reset();
	                }
	                Reader reader = stream.getReader();
	                if (reader != null) {
	                    reader.reset();
	                }
				}

                if (node != null) {
                    Document normalDoc = null;

                    if (node instanceof Document) {
                        normalDoc = (Document) node;
                    } else {
                        normalDoc = ((Element) node).getOwnerDocument();
                    }

                    final Element normalRoot = normalDoc.getDocumentElement();

                    final Operation operation = opMetaData.getBindingOperation().getOperation();

                    final Input input = operation.getInput();
                    final Message inputMessage = input.getMessage();

                    if (HelperFactory.WRAPPER_ENABLED) {
                        final WrapperParser wrapperParser = HelperFactory.createParser();
                        wrapperParser.parse(normalDoc, inputMessage);

                        final Map parts = inputMessage.getParts();
                        final Iterator it = parts.values().iterator();

                        while (it.hasNext()) {
                            final Part part = (Part) it.next();

                            if (wrapperParser.hasPart(part.getName())) {
                                final QName elementQName = part.getElementName();

                                if (elementQName == null) {
                                    final String msgEx = LDAPDenormalizer.mMessages.getString(
                                            "LDAPBC-E00501.LDAPDN_Failed_Denormalize") +
                                            part.getName() +
                                            "should have element attribute defined.";
                                    throw new MessagingException(msgEx);
                                }

                                Element element = null;
                                final NodeList unwrappedList = wrapperParser.getPartNodes(part.getName());

                                for (int j = 0;
                                        j < unwrappedList.getLength();
                                        j++) {
                                    final Node unwrapped = unwrappedList.item(j);

                                    if ((unwrapped.getNodeType() == Node.ELEMENT_NODE) &&
                                            (unwrapped.getLocalName() != null) &&
                                            unwrapped.getLocalName().equals(elementQName.getLocalPart())) {
                                        element = (Element) unwrapped;
                                        break;
                                    }
                                }

                                if (element != null) {
                                    res = populateResult(operationType, element, opMetaData);
                                } else {
                                    final String msgEx = LDAPDenormalizer.mMessages.getString(
                                            "LDAPBC-E00502.LDAPDN_Failed_Finding_Node") +
                                            elementQName.getLocalPart() +
                                            ", in the part wrapper";
                                    LDAPDenormalizer.mLogger.log(Level.WARNING, msgEx);
                                    throw new MessagingException(msgEx);
                                }
                            }
                        }
                    } else {
                        final Element messageElement = normalRoot;

                        if (messageElement != null) {
                            final Map parts = inputMessage.getParts();
                            final Iterator it = parts.values().iterator();

                            while (it.hasNext()) {
                                final Part part = (Part) it.next();
                                final Element partElement = findPart(messageElement,
                                        part.getName());

                                if (partElement != null) {
                                    final QName elementQName = part.getElementName();

                                    if (elementQName != null) {
                                        final Element element = findChildElement(partElement,
                                                elementQName);

                                        if (element != null) {
                                            res = populateResult(operationType, element, opMetaData);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (final TransformerConfigurationException ex) {
                final String msg = LDAPDenormalizer.mMessages.getString("LDAP_Failed_Convert_NM");
                throw new MessagingException(msg, ex);
            } catch (final TransformerException ex) {
                final String msg = LDAPDenormalizer.mMessages.getString("LDAPBC-E00503.LDAPDN_Failed_Convert_NM");
                throw new MessagingException(msg, ex);
            } catch (final Throwable th) {
                final String msg = LDAPDenormalizer.mMessages.getString("LDAPBC-E00504.LDAPDN_Failed_PS");
                throw new MessagingException(msg, th);
            }
//            }
        }
        return res;
    }

    private LDAPOperationAbstract populateResult(String operationType, Element element, OperationMetaData opMetaData) throws MessagingException, Exception {
        if (operationType.equals(LDAPOperation.OPERATION_TYPE_SEARCH)) {
            return populateSearch(element, opMetaData);
        } else if (operationType.equals(LDAPOperation.OPERATION_TYPE_UPDATE)) {
            return populateUpdate(element, opMetaData);
        } else if (operationType.equals(LDAPOperation.OPERATION_TYPE_ADD)) {
            return populateAdd(element, opMetaData);
        } else if (operationType.equals(LDAPOperation.OPERATION_TYPE_DELETE)) {
            return populateDelete(element, opMetaData);
        } else {
            return null;
        }

    }

    public LDAPDenormalizer() {
        connections = new HashMap();
    }

    private LDAPDelete populateDelete(final Element element, final OperationMetaData opMetaData) throws MessagingException, Exception {
//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0231_LDAP_Denormalize_Delete_Start"));
        LDAPDelete ret = new LDAPDelete();
        LDAPSearch ldapSearch = new LDAPSearch();
        ldapSearch.setDN(opMetaData.getSearchDN());
        SearchControls controls = opMetaData.getSearchControls();
        NodeList nodes = element.getChildNodes();

        for (int i = 0; i < nodes.getLength(); i++) {
            Node n = nodes.item(i);

            if (n.getLocalName().equals("property")) {
                NodeList nl = n.getChildNodes();
                for (int j = 0; j < nl.getLength(); j++) {
                    Node np = nl.item(j);
                    if (np.getLocalName().equals("dn")) {
                        ldapSearch.setDN(np.getTextContent());
                    }
                    if (np.getLocalName().equals("scope")) {
                        controls.setSearchScope(Integer.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("size")) {
                        controls.setCountLimit(Long.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("timeout")) {
                        controls.setTimeLimit(Integer.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("deref")) {
                        controls.setDerefLinkFlag(Boolean.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("referral")) {
                        opMetaData.setReferral(np.getTextContent());
                        opMetaData.setIsConnectionRecreate(true);
                    }
                }
            }
            if (n.getLocalName().equals("attributes")) {
                NodeList nl = n.getChildNodes();
                for (int j = 0; j < nl.getLength(); j++) {
                    Node node = nl.item(j);
                    String queryName = node.getLocalName();
                    int index = queryName.indexOf(".");
                    String objName = queryName.substring(0, index);
                    String attrName = queryName.substring(index + 1);
                    NodeList values = node.getChildNodes();
                    Node v = values.item(0);
                    String value = v.getTextContent();
                    Map queryAttrs = opMetaData.getMQueryAttributes();
                    QueryAttributeBean attrBean = (QueryAttributeBean) queryAttrs.get(queryName);
                    if (attrBean == null) {
                        final String msg = LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0203_LDAP_Failed_Convert_NM");
//                        String msg = "LDAP failed to get query element's attribute";
                        throw new MessagingException(msg);
                    }
                    int positionIndex = attrBean.getPositionIndex();
                    int bracketDepth = attrBean.getBracketDepth();
                    int bracketBeginDepth = attrBean.getBracketBeginDepth();
                    int bracketEndDepth = attrBean.getBracketEndDepth();
                    String logicOp = attrBean.getLogicOp();
                    String compareOp = attrBean.getCompareOp();
                    ldapSearch.addFilter(positionIndex, objName, logicOp, attrName, compareOp, bracketDepth, bracketBeginDepth, bracketEndDepth, value);
                }
            }

         /*   if (n.getLocalName().equals("connection")) {
                NodeList nl = n.getChildNodes();
                String url = null;
                LdapConnection poolconn = new LdapConnection();
                boolean found = false;
                for (int j = 0; j < nl.getLength(); j++) {
                    opMetaData.setIsConnectionRecreate(true);
                    Node connNode = nl.item(j);
                    String text = connNode.getTextContent();
                    if (connNode.getLocalName().equals("location")) {
                        url = text;
                        LdapConnection tmp = (LdapConnection) connections.get(url);
                        if (null != tmp) {
                            found = true;
                            poolconn = tmp;
                            break;
                        }
                        poolconn.setLocation(text);
                    }
                    if (poolconn.isProperty(connNode.getLocalName())) {
                        poolconn.setProperty(connNode.getLocalName(), text);
                    }
                }

                if (null != url && !found) {
//                    poolconn.getConnection(opMetaData);
                    connections.put(url, poolconn);
                }
                ldapSearch.setLdapConnection(poolconn);
            }*/
        }
            
        ldapSearch.setSearchControls(controls);
        ret.setLdapSearch(ldapSearch);
//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0232_LDAP_Denormalize_Delete_Finish"));
        return ret;
    }

    private LDAPAdd populateAdd(final Element element, final OperationMetaData opMetaData) throws MessagingException, Exception {
//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0235_LDAP_Denormalize_Add_Start"));
        LDAPAdd ret = new LDAPAdd();
        ret.setDN(opMetaData.getSearchDN());
        NodeList nodes = element.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++) {
            Node n = nodes.item(i);

            if (n.getLocalName().equals("property")) {
                NodeList nl = n.getChildNodes();
                for (int j = 0; j < nl.getLength(); j++) {
                    Node np = nl.item(j);
                    if (np.getLocalName().equals("dn")) {
                        ret.setDN(np.getTextContent());
                    }
                }
            }
            if (n.getLocalName().equals("attributes")) {
                NodeList nl = n.getChildNodes();
                Attribute attribute = null;
                Attribute objectclass = new BasicAttribute("objectclass");
                Attributes attributes = new BasicAttributes(true);

                Hashtable ht = new Hashtable();
                for (int j = 0; j < nl.getLength(); j++) {
                    Node node = nl.item(j);
                    if (node.getLocalName().equals("MainAttribute")) {
                        String queryName = node.getFirstChild().getLocalName();
                        int index = queryName.indexOf(".");
                        String objName = queryName.substring(0, index);
                        String attrName = queryName.substring(index + 1);
                        if (!ht.containsKey(objName)) {
                            objectclass.add(objName);
                            ht.put(objName, "objectclass");
                        }
                        String value = node.getFirstChild().getTextContent();
                        if (value != null) {
                            attribute = new BasicAttribute(attrName);
                            attribute.add(value);
                            attributes.put(attribute);
                        }
                        String rdn = attrName + "=" + value;
                        ret.setRDN(rdn);
                    }
                    if (node.getLocalName().equals("Attributes")) {
                        NodeList nodeList = node.getChildNodes();
                        for (int k = 0; k < nodeList.getLength(); k++) {

                            Node nd = nodeList.item(k);
                            String queryName = nd.getLocalName();
                            int index = queryName.indexOf(".");
                            String objName = queryName.substring(0, index);
                            String attrName = queryName.substring(index + 1);
                            if (!ht.containsKey(objName)) {
                                objectclass.add(objName);
                                ht.put(objName, "objectclass");
                            }
                            String value = nd.getTextContent();
                            if (value != null) {
                                attribute = new BasicAttribute(attrName);
                                attribute.add(value);
                                attributes.put(attribute);
                            }
                        }
                    }
                }
                attributes.put(objectclass);
                ret.setAttributes(attributes);
            }
            /*if (n.getLocalName().equals("connection")) {
                NodeList nl = n.getChildNodes();
                String url = null;
                LdapConnection poolconn = new LdapConnection();
                boolean found = false;
                for (int j = 0; j < nl.getLength(); j++) {
                    opMetaData.setIsConnectionRecreate(true);
                    Node connNode = nl.item(j);
                    String text = connNode.getTextContent();
                    if (connNode.getLocalName().equals("location")) {
                        url = text;
                        LdapConnection tmp = (LdapConnection) connections.get(url);
                        if (null != tmp) {
                            found = true;
                            poolconn = tmp;
                            break;
                        }
                        poolconn.setLocation(text);
                    }
                    if (poolconn.isProperty(connNode.getLocalName())) {
                        poolconn.setProperty(connNode.getLocalName(), text);
                    }
                }

                if (null != url && !found) {
//                    poolconn.getConnection(opMetaData);
                    connections.put(url, poolconn);
                }
                ret.setLdapConnection(poolconn);
            }*/
        }

//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0236_LDAP_Denormalize_Add_Finish"));
        return ret;
    }

    /**
     * 
     * @param elem
     * @param opMetaData
     * @return
     * @throws javax.jbi.messaging.MessagingException
     */
    private LDAPSearch populateSearch(final Element elem,
            final OperationMetaData opMetaData)
            throws MessagingException, Exception {
//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0233_LDAP_Denormalize_Query_Start"));
        LDAPSearch ret = new LDAPSearch();
        ret.setDN(opMetaData.getSearchDN());
        SearchControls controls = opMetaData.getSearchControls();
        NodeList nodes = elem.getChildNodes();

        for (int i = 0; i < nodes.getLength(); i++) {
            Node n = nodes.item(i);

            if (n.getLocalName().equals("property")) {
                NodeList nl = n.getChildNodes();
                for (int j = 0; j < nl.getLength(); j++) {
                    Node np = nl.item(j);
                    if (np.getLocalName().equals("dn")) {
                        ret.setDN(np.getTextContent());
                    }
                    if (np.getLocalName().equals("scope")) {
                        controls.setSearchScope(Integer.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("size")) {
                        controls.setCountLimit(Long.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("timeout")) {
                        controls.setTimeLimit(Integer.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("deref")) {
                        controls.setDerefLinkFlag(Boolean.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("referral")) {
                        opMetaData.setReferral(np.getTextContent());
                        opMetaData.setIsConnectionRecreate(true);
                    }
                    if (np.getLocalName().equals("recordsperpage")) {
                        opMetaData.setRecordsPerPage(Integer.valueOf(np.getTextContent()).intValue());
                    }
                }
            }

            if (n.getLocalName().equals("attributes")) {
                NodeList nl = n.getChildNodes();
                for (int j = 0; j < nl.getLength(); j++) {
                    Node node = nl.item(j);
                    String queryName = node.getLocalName();
                    int index = queryName.indexOf(".");
                    String objName = queryName.substring(0, index);
                    String attrName = queryName.substring(index + 1);
                    NodeList values = node.getChildNodes();
                    Node v = values.item(0);
                    String value = v.getTextContent();
                    Map queryAttrs = opMetaData.getMQueryAttributes();
                    QueryAttributeBean attrBean = (QueryAttributeBean) queryAttrs.get(queryName);
                    if (attrBean == null) {
                        final String msg = LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0203_LDAP_Failed_Convert_NM");
//                        String msg = "LDAP failed to get query element's attribute";
                        throw new MessagingException(msg);
                    }
                    int positionIndex = attrBean.getPositionIndex();
                    int bracketDepth = attrBean.getBracketDepth();
                    int bracketBeginDepth = attrBean.getBracketBeginDepth();
                    int bracketEndDepth = attrBean.getBracketEndDepth();
                    String logicOp = attrBean.getLogicOp();
                    String compareOp = attrBean.getCompareOp();
                    ret.addFilter(positionIndex, objName, logicOp, attrName, compareOp, bracketDepth, bracketBeginDepth, bracketEndDepth, value);
                }
            }

            /*if (n.getLocalName().equals("connection")) {
                NodeList nl = n.getChildNodes();
                String url = null;
/*              LdapConnection poolconn = (LdapConnection) connections.get(url);
                if (null != poolconn) {
                ret.setLdapConnection(poolconn);
                } else {
                poolconn = new LdapConnection();
                }
                 */
               /* LdapConnection poolconn = new LdapConnection();
                boolean found = false;
                for (int j = 0; j < nl.getLength(); j++) {
                    Node connNode = nl.item(j);
                    String text = connNode.getTextContent();
                    opMetaData.setIsConnectionRecreate(true);
                    if (connNode.getLocalName().equals("location")) {
                        url = text;
                        LdapConnection tmp = (LdapConnection) connections.get(url);
                        if (null != tmp) {
                            found = true;
                            poolconn = tmp;
                            break;
                        }
                        poolconn.setLocation(text);
                    }
                    if (poolconn.isProperty(connNode.getLocalName())) {
                        poolconn.setProperty(connNode.getLocalName(), text);
                    }
                }

                if (null != url && !found) {
//                    poolconn.getConnection(opMetaData);
                    connections.put(url, poolconn);
                }
                ret.setLdapConnection(poolconn);
            }*/
        }
        ret.setSearchControls(controls);
//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0234_LDAP_Denormalize_Query_Finish"));
        return ret;
    }

    private LDAPUpdate populateUpdate(final Element elem,
            final OperationMetaData opMetaData)
            throws MessagingException, Exception {
//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0237_LDAP_Denormalize_Update_Start"));
        LDAPUpdate ret = new LDAPUpdate();
        LDAPSearch ldapSearch = new LDAPSearch();
        ldapSearch.setDN(opMetaData.getSearchDN());
        SearchControls controls = opMetaData.getSearchControls();
        NodeList nodes = elem.getChildNodes();

        for (int i = 0; i < nodes.getLength(); i++) {
            Node n = nodes.item(i);

            if (n.getLocalName().equals("property")) {
                NodeList nl = n.getChildNodes();
                for (int j = 0; j < nl.getLength(); j++) {
                    Node np = nl.item(j);
                    if (np.getLocalName().equals("dn")) {
                        ldapSearch.setDN(np.getTextContent());
                    }
                    if (np.getLocalName().equals("scope")) {
                        controls.setSearchScope(Integer.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("size")) {
                        controls.setCountLimit(Long.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("timeout")) {
                        controls.setTimeLimit(Integer.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("deref")) {
                        controls.setDerefLinkFlag(Boolean.valueOf(np.getTextContent()));
                    }
                    if (np.getLocalName().equals("referral")) {
                        opMetaData.setReferral(np.getTextContent());
                        opMetaData.setIsConnectionRecreate(true);
                    }
                }
            }

            if (n.getLocalName().equals("attributes")) {
                NodeList nla = n.getChildNodes();
                for (int k = 0; k < nla.getLength(); k++) {
                    Node aNode = nla.item(k);
                    if ("SearchFilter".equals(aNode.getLocalName())) {
                        NodeList nl = aNode.getChildNodes();
                        for (int j = 0; j < nl.getLength(); j++) {
                            Node node = nl.item(j);
                            String queryName = node.getLocalName();
                            int index = queryName.indexOf(".");
                            String objName = queryName.substring(0, index);
                            String attrName = queryName.substring(index + 1);
                            NodeList values = node.getChildNodes();
                            Node v = values.item(0);
                            String value = v.getTextContent();
                            Map queryAttrs = opMetaData.getMQueryAttributes();
                            QueryAttributeBean attrBean = (QueryAttributeBean) queryAttrs.get(queryName);
                            if (attrBean == null) {
                        final String msg = LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0203_LDAP_Failed_Convert_NM");
//                                String msg = "LDAP failed to get query element's attribute";
                                throw new MessagingException(msg);
                            }
                            int positionIndex = attrBean.getPositionIndex();
                            int bracketDepth = attrBean.getBracketDepth();
                            int bracketBeginDepth = attrBean.getBracketBeginDepth();
                            int bracketEndDepth = attrBean.getBracketEndDepth();
                            String logicOp = attrBean.getLogicOp();
                            String compareOp = attrBean.getCompareOp();
                            ldapSearch.addFilter(positionIndex, objName, logicOp, attrName, compareOp, bracketDepth, bracketBeginDepth, bracketEndDepth, value);
                        }
                        continue;
                    }
                    if ("UpdateElements".equals(aNode.getLocalName())) {
                        NodeList nl = aNode.getChildNodes();
                        for (int j = 0; j < nl.getLength(); j++) {
                            Node node = nl.item(j);
                            String eleName = node.getLocalName();
                            Map updateAttrs = opMetaData.getMUpdateAttributes();
                            if (updateAttrs != null) {
                                UpdateBean updateBean = (UpdateBean) updateAttrs.get(eleName);
                                if (updateBean != null) {
                                    String opType = updateBean.getOpType();
                                    if (UpdateBean.OPERATION_TYPE_ADD.equals(opType)) {
                                        NodeList values = node.getChildNodes();
                                        Node addNode = values.item(0);
                                        String addValue = addNode.getTextContent();
                                        UpdateBeanAdd updateAdd = new UpdateBeanAdd(updateBean.getObjName(), updateBean.getAttrName(), opType, addValue);
                                        ret.addUpdateAttribute(updateAdd);
                                        continue;
                                    } else if (UpdateBean.OPERATION_TYPE_REPLACE.equals(opType)) {
                                        NodeList values = node.getChildNodes();
//                                        Node primaryNode = values.item(0);
                                        Node newNode = values.item(0);
//                                        String primaryvalue = primaryNode.getTextContent();
                                        String newValue = newNode.getTextContent();
                                        UpdateBeanReplace updateReplace = new UpdateBeanReplace(updateBean.getObjName(), updateBean.getAttrName(), opType, newValue);
                                        ret.addUpdateAttribute(updateReplace);
                                        continue;
                                    }
                                    if (UpdateBean.OPERATION_TYPE_REMOVE.equals(opType)) {
                                        NodeList values = node.getChildNodes();
                                        Node removeNode = values.item(0);
                                        String removeValue = removeNode.getTextContent();
                                        UpdateBeanRemove updateRemove = new UpdateBeanRemove(updateBean.getObjName(), updateBean.getAttrName(), opType, removeValue);
                                        ret.addUpdateAttribute(updateRemove);
                                        continue;
                                    } else if (UpdateBean.OPERATION_TYPE_REMOVEALL.equals(opType)) {
                                        UpdateBeanRemoveAll removeAll = new UpdateBeanRemoveAll(updateBean.getObjName(), updateBean.getAttrName(), opType);
                                        ret.addUpdateAttribute(removeAll);
                                    }
                                }
                            }
                        }
                    }
                }
            }

           /* if (n.getLocalName().equals("connection")) {
                NodeList nl = n.getChildNodes();
                String url = null;
                LdapConnection poolconn = new LdapConnection();
                boolean found = false;
                for (int j = 0; j < nl.getLength(); j++) {
                    Node connNode = nl.item(j);
                    String text = connNode.getTextContent();
                    opMetaData.setIsConnectionRecreate(true);
                    if (connNode.getLocalName().equals("location")) {
                        url = text;
                        LdapConnection tmp = (LdapConnection) connections.get(url);
                        if (null != tmp) {
                            found = true;
                            poolconn = tmp;
                            break;
                        }
                        poolconn.setLocation(text);
                    }
                    if (poolconn.isProperty(connNode.getLocalName())) {
                        poolconn.setProperty(connNode.getLocalName(), text);
                    }
                }

                if (null != url && !found) {
//                    poolconn.getConnection(opMetaData);
                    connections.put(url, poolconn);
                }
                ldapSearch.setLdapConnection(poolconn);
            }*/
        }
        ldapSearch.setSearchControls(controls);
        ret.setLdapSearch(ldapSearch);
//        LDAPDenormalizer.mLogger.log(Level.SEVERE,
//                        LDAPDenormalizer.mMessages.getString("LDAPBC_ACFG0238_LDAP_Denormalize_Update_Finish"));
        return ret;
    }

    /**
     *
     * @param parent
     * @param msgQName
     * @return
     */
    private Element findChildElement(final Element parent, final QName msgQName) {
        final String ns = msgQName.getNamespaceURI();
        final String localName = msgQName.getLocalPart();
        NodeList nl = null;

        if ((ns != null) && !ns.trim().equals("")) {
            nl = parent.getElementsByTagNameNS(ns, localName);
        } else {
            nl = parent.getElementsByTagName(localName);
        }

        if ((nl != null) && (nl.getLength() > 0)) {
            if (LDAPDenormalizer.mLogger.isLoggable(Level.INFO)) {
                LDAPDenormalizer.mLogger.log(Level.INFO, "found element with name, " +
                        localName);
            }

            final Element e2 = (Element) nl.item(0);

            return e2;
        }

        return null;
    }

    /**
     *
     * @param root
     * @param elemName
     * @return
     */
    private Element findPart(final Element root, final String elemName) {
        // parts wrappers never have namespace
        final NodeList nl = root.getElementsByTagName(elemName);

        if ((nl != null) && (nl.getLength() > 0)) {
            if (LDAPDenormalizer.mLogger.isLoggable(Level.INFO)) {
                LDAPDenormalizer.mLogger.log(Level.INFO, "found element with name, " + elemName);
            }

            final Element e2 = (Element) nl.item(0);

            return e2;
        }

        return null;
    }
}
