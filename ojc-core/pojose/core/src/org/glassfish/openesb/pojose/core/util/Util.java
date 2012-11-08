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
 * @(#)Util.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.core.util;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.EndpointInfo.LinkType;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.pool.DocumentBuilderPool;
import org.glassfish.openesb.pojose.core.pool.JBIPartExprPool;
import org.glassfish.openesb.pojose.core.pool.TransformerPool;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;

/**
 *
 * @author gpatil
 */
public class Util {
    public enum MEPStyle {InOnly, InOut};    
    
    //Implicitly unwrap for jbi message WSDL 1.1 wrapper.    
    public static String jbiMessage2String(Source src) throws Exception {
        
        Node jbiPart = null;
        String ret = null;

        if (src instanceof DOMSource) {
            Node jbiMessage = ((DOMSource) src).getNode();
            jbiPart = getFirstJBIPart(jbiMessage);
        } else {
            jbiPart = getFirstJBIPartFromNonDOMSource(src);
        }

        if (jbiPart != null) {
            ret = node2String(jbiPart);
        }
        return ret;
    }

    //Implicitly unwrap for jbi message WSDL 1.1 wrapper.    
    public static Source jbiMessage2Source(Source src) throws Exception {
        Node jbiPart = null;

        if (src instanceof DOMSource) {
            Node jbiMessage = ((DOMSource) src).getNode();
            jbiPart = getFirstJBIPart(jbiMessage);
        } else {
            jbiPart = getFirstJBIPartFromNonDOMSource(src);
        }

        return new DOMSource(jbiPart);
    }
    //Implicitly unwrap for jbi message WSDL 1.1 wrapper.    
    public static Node jbiMessage2Node(Source src) throws Exception {
        Node jbiPart = null;

        if (src instanceof DOMSource) {
            Node jbiMessage = ((DOMSource) src).getNode();
            jbiPart = getFirstJBIPart(jbiMessage);
        } else {
            jbiPart = getFirstJBIPartFromNonDOMSource(src);
        }

        return jbiPart;
    }

    public static Document jbiMessage2Document(Source src) throws Exception {
        Document doc = null;
        Node node = jbiMessage2Node(src);
        
        DocumentBuilder db = null;
        try {
            db = DocumentBuilderPool.getInstance().acquire();
            doc = db.newDocument();
        } finally {
            if (db != null){
                DocumentBuilderPool.getInstance().release(db);
            }
        }
        doc.setXmlVersion("1.0"); //NOI18N
        if (node.getNodeType() == Node.ELEMENT_NODE){
            node = doc.importNode(node, true);
            doc.appendChild(node);
        } else {
            Element elem = doc.createElement("element");//NOI18N
            Text txt = doc.createTextNode(node.getNodeValue());
            elem.appendChild(txt);
            doc.appendChild(elem);
        }

        return doc;
    }

    public static Node source2Node(Source src) {
        Node msgNode = null;

        if (src instanceof DOMSource) {
            msgNode = ((DOMSource) src).getNode();
        } else {
            msgNode = tranform2Node(src);
        }

        return msgNode;
    }

    public static String source2String(Source src) {
        Transformer trf = null;
        try {
            StringWriter w = new StringWriter();
            trf = TransformerPool.getInstance().acquire();
            StreamResult res = new StreamResult(w);
            trf.transform(src, res);
            w.close();
            return w.toString();
        } catch (IOException ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        } catch (TransformerConfigurationException ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        } catch (TransformerException ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            if (trf != null){
                TransformerPool.getInstance().release(trf);
            }
        }

        return null;
    }

    public static Source string2WrappedSource(String inStr, String msgTypeNS,
            String msgTypeName) throws Exception {
        return string2WrappedSource(inStr, msgTypeNS, msgTypeName, null);
    }

    public static Source string2WrapperdSource(String inStr) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("<jbi:message version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">"); //NOI18N
        sb.append("<jbi:part>"); //NOI18N
        sb.append(inStr);
        sb.append("</jbi:part>"); //NOI18N
        sb.append("</jbi:message>"); //NOI18N
        return string2Source(sb.toString());
    }

    public static Source string2Source(String inStr) throws Exception {
        Transformer tra = null;
        try {
            tra = TransformerPool.getInstance().acquire();        
            StringReader sr = new StringReader(inStr);
            StreamSource ss = new StreamSource(sr);
            DOMResult result = new DOMResult();
            tra.transform(ss, result);
            return new DOMSource(result.getNode());
        }finally {
            if (tra != null){
                TransformerPool.getInstance().release(tra);
            }
        }
    }

    public static Source doc2WrappedSource(Document doc, String msgTypeNS,
            String msgTypeName) throws Exception {
        return node2WrappedSource(doc, msgTypeNS, msgTypeName);
    }

    public static Source node2WrappedSource(Node node, String msgTypeNS,
            String msgTypeName) throws Exception {

        if (node.getNodeType() == Node.DOCUMENT_NODE) {
            node = node.getFirstChild();
        }
        
        if ((node.getNodeType() != Node.ELEMENT_NODE) &&
            (node.getNodeType() != Node.TEXT_NODE) &&
            (node.getNodeType() != Node.DOCUMENT_FRAGMENT_NODE) ){
            String msg = I18n.loc("POJOSE-7305: Node is not of type org.w3c.dom.Element, org.w3c.dom.Text or org.w3c.dom.DocumentFragment");
            throw new Exception(msg);
        }
        
        DocumentBuilder db = null;
        Document doc = null;
        try {
            db = DocumentBuilderPool.getInstance().acquire();
            doc = db.newDocument();
        } finally {
            if (db != null){
                DocumentBuilderPool.getInstance().release(db);
            }
        }
        
        doc.setXmlVersion("1.0"); //NOI18N
        Element jbiMsg = doc.createElementNS(Constants.JBI_WRAPPER_NS,
                Constants.JBI_WRAPPER_MSG_ELEM_NAME);//NOI18N
        jbiMsg.setAttribute("version", "1.0"); //NOI18N
        if ((msgTypeNS != null) && (!"".equals(msgTypeNS))){
            jbiMsg.setAttribute("xmlns:msgns", msgTypeNS); //NOI18N
            jbiMsg.setAttribute("type", "msgns:" + msgTypeName); //NOI18N
        } else {
            if ((msgTypeName != null) && (!"".equals(msgTypeName))){
                jbiMsg.setAttribute("type", msgTypeName); //NOI18N
            }
        }
        doc.appendChild(jbiMsg);
        Element jbiPart = doc.createElementNS(Constants.JBI_WRAPPER_NS,
                Constants.JBI_WRAPPER_MSG_ELEM_PART); //NOI18N
        //Node adaptedNode = doc.adoptNode(node); Does not work when source doc
        // is com.sun.xml.messaging.saaj.soap.SOAPDocumentImpl
        // instead com.sun.org.apache.xerces.internal.dom.DocumentImpl
        Node impNode = doc.importNode(node, true);
        jbiPart.appendChild(impNode);
        
        jbiMsg.appendChild(jbiPart);

        return new DOMSource(doc);
    }

    public static Source doc2WrappedSource(Document doc)throws Exception {
        return node2WrappedSource(doc);
    }

    public static Source node2WrappedSource(Node node) throws Exception {
        if (node.getNodeType() == Node.DOCUMENT_NODE) {
            node = node.getFirstChild();
        }

        if ((node.getNodeType() != Node.ELEMENT_NODE) &&
            (node.getNodeType() != Node.TEXT_NODE) &&
            (node.getNodeType() != Node.DOCUMENT_FRAGMENT_NODE) ){
            String msg = I18n.loc("POJOSE-7305: Node is not of type org.w3c.dom.Element, org.w3c.dom.Text or org.w3c.dom.DocumentFragment");
            throw new Exception(msg);
        }

        DocumentBuilder db = null;
        Document doc = null;
        try {
            db = DocumentBuilderPool.getInstance().acquire();
            doc = db.newDocument();
        } finally {
            if (db != null){
                DocumentBuilderPool.getInstance().release(db);
            }
        }
        
        doc.setXmlVersion("1.0"); //NOI18N
        Element jbiMsg = doc.createElementNS(Constants.JBI_WRAPPER_NS,
                Constants.JBI_WRAPPER_MSG_ELEM_NAME);//NOI18N
        doc.appendChild(jbiMsg);
        Element jbiPart = doc.createElementNS(Constants.JBI_WRAPPER_NS,
                Constants.JBI_WRAPPER_MSG_ELEM_PART); //NOI18N
        //Node adaptedNode = doc.adoptNode(node); Does not work when source doc
        // is com.sun.xml.messaging.saaj.soap.SOAPDocumentImpl
        // instead com.sun.org.apache.xerces.internal.dom.DocumentImpl
        Node impNode = doc.importNode(node, true);
        jbiPart.appendChild(impNode);
        
        jbiMsg.appendChild(jbiPart);

        return new DOMSource(doc);
    }
    //Implicitly wrap the source for WSDL1.1 message.
    public static Source source2jbiMessage(Source src) throws Exception {
        Node rootNode = null;
        if (src instanceof DOMSource) {
            rootNode = ((DOMSource) src).getNode();
        } else {
            InputSource is = SAXSource.sourceToInputSource(src);
            DocumentBuilder db = null;
            try {
                db = DocumentBuilderPool.getInstance().acquire();
                rootNode = db.parse(is);
            } finally {
                if (db != null){
                    DocumentBuilderPool.getInstance().release(db);
                }
            }            
        }
        return node2WrappedSource(rootNode);
    }
    
    //Implicitly wrap the source for WSDL1.1 message.
    public static Source source2jbiMessage(Source src, String msgTypeNS,
                                        String msgTypeName) throws Exception {
        Node rootNode = null;
        if (src instanceof DOMSource) {
            rootNode = ((DOMSource) src).getNode();
        } else {
            Transformer tra = null;
            DOMResult res = new DOMResult();
            
            try {
                tra = TransformerPool.getInstance().acquire();        
                tra.transform(src, res);
            }finally {
                if (tra != null){
                    TransformerPool.getInstance().release(tra);
                }
            }            
            
            if (res.getNode() != null) {
                if (res.getNode().getFirstChild() == null){
                    return null;
                } else {
                    rootNode = res.getNode().getFirstChild();
                }
            }
        }
        return node2WrappedSource(rootNode, msgTypeNS, msgTypeName);
    }

    public static String getKey(ServiceEndpoint sep) {
        String key = "";

        if (sep != null) {
            StringBuilder sb = new StringBuilder();
            sb.append("svc:");
            if (sep.getServiceName() != null) {
                sb.append(sep.getServiceName().toString());
            }
            sb.append("ep:");
            if (sep.getEndpointName() != null) {
                sb.append(sep.getEndpointName());
            }
            sb.append("intfc:");
            if (sep.getInterfaces() != null) {
                sb.append(sep.getInterfaces().toString());
            }
            key = sb.toString();
        }
        return key;
    }

    private static String removeXMLDecl(String in){
        if ((in != null) && (in.startsWith("<?xml"))) {
            int index = in.indexOf("?>");
            if (index > 6){
                in = in.substring(index + 2);
            }
        }
        return in;
    }
    private static Source string2WrappedSource(String inStr, String msgTypeNS,
            String msgTypeName, String msgName) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("<jbi:message version=\"1.0\""); //NOI18N

        if ((msgTypeNS != null) && (!"".equals(msgTypeNS))){
            sb.append(" xmlns:msgns=\"");//NOI18N
            sb.append(msgTypeNS);//NOI18N
            sb.append("\"");//NOI18N
            sb.append(" type=\"msgns:");//NOI18N
            sb.append(msgTypeName);
            sb.append("\"");            
        } else {
            if ((msgTypeName != null) && (!"".equals(msgTypeName))){
                sb.append(" type=\"");//NOI18N
                sb.append(msgTypeName);
                sb.append("\"");                            
            }
        }
        
        if ((msgName != null) && (!Constants.ANNOTATION_NULL_VAL.equals(msgName))) {
            sb.append(" name=\"");//NOI18N
            sb.append(msgName);
            sb.append("\" ");
        }
        
        sb.append(" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">");//NOI18N
        sb.append("<jbi:part>"); //NOI18N
        sb.append(removeXMLDecl(inStr));
        sb.append("</jbi:part>"); //NOI18N
        sb.append("</jbi:message>"); //NOI18N
        return string2Source(sb.toString());
    }

    private static String node2String(Node part) throws Exception {
        StringWriter w = new StringWriter();
        Transformer tra = null;
        try {
            tra = TransformerPool.getInstance().acquire();        
            DOMSource src = new DOMSource(part);
            StreamResult res = new StreamResult(w);
            tra.transform(src, res);            
        }finally {
            if (tra != null){
                TransformerPool.getInstance().release(tra);
            }
        }		        

        w.close();
        return w.toString();
    }

    private static Node getFirstJBIPart(Node jbiMsgNode) throws Exception {
        Node ret = null;
        if (jbiMsgNode.getNodeType() == Node.DOCUMENT_NODE) {
            jbiMsgNode = ((Document) jbiMsgNode).getDocumentElement();
        }

        XPathExpression expr = null;
        NodeList nl = null;
        
        try {
            expr = JBIPartExprPool.getInstance().acquire();
            nl = (NodeList) expr.evaluate(jbiMsgNode, XPathConstants.NODESET);
        } finally {
            if (expr != null){
                JBIPartExprPool.getInstance().release(expr);
            }
        }
        
        if ((nl != null) && (nl.getLength() > 0)) {
            ret = nl.item(0).getFirstChild();
            //Check for element to avoid selecting whitespace node.
            NodeList partChildren = nl.item(0).getChildNodes();
            for (int i = 0, len = partChildren.getLength(); i < len; i++) {
                Node child = partChildren.item(i);
                if (child.getNodeType() == Node.ELEMENT_NODE) {
                    ret = (Element)child;
                    break;
                }
            }
        } else {
            //return the original message node.
            ret = jbiMsgNode;
        }

        return ret;
    }

    private static Node getFirstJBIPartFromNonDOMSource(Source src) throws Exception {
        Node rootNode = null; 
        Transformer tra = null;
        DOMResult res = new DOMResult();        
        try {
            tra = TransformerPool.getInstance().acquire();        
            tra.transform(src, res);            
        }finally {
            if (tra != null){
                TransformerPool.getInstance().release(tra);
            }
        }		

        if (res.getNode() != null) {
            if (res.getNode().getFirstChild() == null){
                return null;
            } else {
                rootNode = res.getNode().getFirstChild();
            }
        }
        
        return getFirstJBIPart(rootNode);
    }

    private static Node tranform2Node(Source traxSrc) {
        Transformer tra = null;

        try {
            tra = TransformerPool.getInstance().acquire();        
            DOMResult result = new DOMResult();
            tra.transform(traxSrc, result);
            return result.getNode();
        } catch (TransformerConfigurationException ex) {
            String m = I18n.loc("Exception will transforming Trax Source to String {0}", ex);
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, m);
        } catch (TransformerException ex) {
            String m = I18n.loc("Exception will transforming Trax Source to String {0}", ex);
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, m);
        }finally {
            if (tra != null){
                TransformerPool.getInstance().release(tra);
            }
        }		


        return null;
    }

    public static EndpointInfo getEndpointInfo(POJOClassMetadata pojom) {
        EndpointInfo ei = null;
        if (pojom.getProvider() != null){
            ei = getEndpointInfoFromAnnotationProvider(pojom);
        } else {
            ei = getEndpointInfoFromAnnotationPOJO(pojom);
        }
        return ei;
    }
    
    private static EndpointInfo getEndpointInfoFromAnnotationProvider(POJOClassMetadata pojom) {
        String baseNamespace;
        String svcNs;
        String intfcNs;
        String svcName;
        String intfcName;

        QName s;
        QName i;
        String ep;
        Provider provider;
        EndpointInfo ei;

        provider = pojom.getProvider();
        Class pojoClass = pojom.getPojoClass();
        ep = pojoClass.getSimpleName();
        baseNamespace = getNamespace(pojoClass.getPackage().getName()) +
                ep + "/"; //NOI18N
        svcNs = baseNamespace;
        intfcNs = baseNamespace;
        

        if (!Constants.ANNOTATION_NULL_VAL.equals(provider.name())) {
            ep = provider.name();
        }

        svcName = ep + "Service"; //NOI18N
        intfcName = ep + "Interface"; //NOI18N


        if (!Constants.ANNOTATION_NULL_VAL.equals(provider.serviceQN())){
            s = QName.valueOf(provider.serviceQN());
        } else {
            s = new QName(svcNs, svcName);            
        }

        if (!Constants.ANNOTATION_NULL_VAL.equals(provider.interfaceQN())){
            i = QName.valueOf(provider.interfaceQN());
        } else {
            i = new QName(intfcNs, intfcName);
        }

        ei = new EndpointInfo(true, ep, i, s, LinkType.standard);
        
        return ei;
    }
    
    private static EndpointInfo getEndpointInfoFromAnnotationPOJO(POJOClassMetadata pojom) {
        String baseNamespace;
        String svcNs;
        String intfcNs;
        String svcName;
        String intfcName;

        QName s;
        QName i;
        String ep;
        POJO pojo;
        EndpointInfo ei;

        pojo = pojom.getPOJO();
        Class pojoClass = pojom.getPojoClass();
        ep = pojoClass.getSimpleName();
        baseNamespace = getNamespace(pojoClass.getPackage().getName()) +
                ep + "/"; //NOI18N
        svcNs = baseNamespace;
        intfcNs = baseNamespace;
        

        if (!Constants.ANNOTATION_NULL_VAL.equals(pojo.name())) {
            ep = pojo.name();
        }

        svcName = ep + "Service"; //NOI18N
        intfcName = ep + "Interface"; //NOI18N

        if (!Constants.ANNOTATION_NULL_VAL.equals(pojo.serviceNS())) {
            svcNs = pojo.serviceNS();
        }

        if (!Constants.ANNOTATION_NULL_VAL.equals(pojo.interfaceNS())) {
            intfcNs = pojo.interfaceNS();
        }

        if (!Constants.ANNOTATION_NULL_VAL.equals(pojo.serviceName())) {
            svcName = pojo.serviceName();
        }

        if (!Constants.ANNOTATION_NULL_VAL.equals(pojo.interfaceName())) {
            intfcName = pojo.interfaceName();
        }
        
        if (!Constants.ANNOTATION_NULL_VAL.equals(pojo.serviceQN())){
            s = QName.valueOf(pojo.serviceQN());
        } else {
            s = new QName(svcNs, svcName);            
        }

        if (!Constants.ANNOTATION_NULL_VAL.equals(pojo.interfaceQN())){
            i = QName.valueOf(pojo.interfaceQN());
        } else {
            i = new QName(intfcNs, intfcName);
        }

        ei = new EndpointInfo(true, ep, i, s, LinkType.standard);
        
        return ei;
    }

    private static String getNamespace(String pkg) {
        Stack<String> dotName = new Stack<String>();

        StringTokenizer stk = new StringTokenizer(pkg, "."); //NOI18N
        while (stk.hasMoreTokens()) {
            dotName.push(stk.nextToken());
        }

        StringBuilder sb = new StringBuilder();
        sb.append("http://"); //NOI18N
        boolean first = true;
        while (!dotName.empty()) {
            if (!first) {
                sb.append("."); //NOI18N
            } else {
                first = false;
            }
            sb.append(dotName.pop());
        }
        sb.append("/"); //NOI18N
        return sb.toString();
    }
}
