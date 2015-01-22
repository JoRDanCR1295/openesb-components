package com.sun.jbi.restbc.jbiadapter.util;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import com.sun.jbi.restbc.jbiadapter.wsdl.RestOperation;

/**
 * JbiMessageUtil.java
 *
 * @author Edward Chou
 */
public class JbiMessageUtil {

    private final static Logger logger = Logger.getLogger(JbiMessageUtil.class.getName());
    
    private static final String WSDL_WRAPPER_URI = "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper";
    private static final String XOP_URI = "http://www.w3.org/2004/08/xop/include";
    
    /**
     * 
     * @param doc
     * @return
     */
    public static QName getMsgTypeFromWrappedMsg(Document doc) {
        QName msgTypeQName = null;
        
        try {
            Element jbiMessageWrapper = doc.getDocumentElement();

            if (jbiMessageWrapper == null) {
                return null;
            } else {
                String msgType = jbiMessageWrapper.getAttribute("type");
                if (msgType == null || msgType.equals("")) {
                    return null;
                }

                int colonPos = msgType.indexOf(':');
                String localName = msgType;
                String prefix = null;
                String namespace = null;
                if (colonPos > -1) {
                    prefix = msgType.substring(0, colonPos);
                    localName = msgType.substring(colonPos + 1);
                    namespace = jbiMessageWrapper.lookupNamespaceURI(prefix);
                    if (namespace == null) { // can't find the namespace still, could be that the "xmlns:" attribute is not namespace aware
                        namespace = jbiMessageWrapper.getAttribute("xmlns:" + prefix);
                    }
                }

                if (prefix != null) {
                    msgTypeQName = new QName(namespace, localName, prefix);
                } else {
                    msgTypeQName = new QName(namespace, localName);
                }

            }
        } catch (Exception e) {
            logger.log(Level.WARNING, "unable to getMsgType from JBI wrapped message", e);
        }
        
        return msgTypeQName;
    }
    
    /**
     * 
     * @param msg
     * @return either javax.xml.transform.Source or javax.activation.DataHandler object
     */
    public static Object getPayloadFromWrappedMsg(NormalizedMessage msg) {
        Source payloadSource = null;
        
        try {
            Document doc = getDocument(msg.getContent());
            
            Node payload;
            
            // Check to see if this message is wrapped
            NodeList parts = doc.getDocumentElement().getElementsByTagNameNS(WSDL_WRAPPER_URI, "part");
            if (parts.getLength() > 0) {
                payload = parts.item(0).getFirstChild();
                if (WrapperUtil.isNodeXopInclude(payload)) {
                    String attachmentId = WrapperUtil.getXopContentId(payload);
                    DataHandler dataHandler = msg.getAttachment(attachmentId);
                    return dataHandler;
                }
                
            } else {
                // no wrapper, just return the document
                payload = doc;
            }
            
            payloadSource = new DOMSource(payload);
            
        } catch (Exception e) {
            logger.log(Level.WARNING, "unable to unwrap JBI wrapped message", e);
        }
        
        return payloadSource;
    }
    
    public static Source createJbiWrappedMsg(QName msgType, InputStream payload) {
        Source content = null;

        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.newDocument();
            Element msgElement = doc.createElementNS(WSDL_WRAPPER_URI, "jbi:message");
            msgElement.setAttribute("version", "1.0");
            if (msgType != null) {
                msgElement.setAttribute("xmlns:msgns", msgType.getNamespaceURI());
                msgElement.setAttribute("type", "msgns:" + msgType.getLocalPart());
            }

            Element partElement = doc.createElementNS(WSDL_WRAPPER_URI, "jbi:part");
            if (payload != null) {
                Node node = null;

                DOMResult domResult = new DOMResult();
                TransformerFactory factory = TransformerFactory.newInstance();
                Transformer trans = factory.newTransformer();
                trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
                trans.transform(new StreamSource(payload), domResult);
                node = domResult.getNode();

                if (node instanceof Document) {
                    node = ((Document) node).getDocumentElement();
                }

                node = doc.importNode(node, true);
                partElement.appendChild(node);
            }
            msgElement.appendChild(partElement);

            doc.appendChild(msgElement); 

            content = new DOMSource(doc);

        } catch (Exception e) {
            logger.log(Level.WARNING, "unable to create JIB wrapped message", e);
        }

        return content;
    }
        
    public static Source createEmptyJbiWrappedMsg(RestOperation restOp, Definition definition, boolean isOutbound) {
        Source content = null;
        
        try {
            Message wsdlMessage = null;
            if (isOutbound) {
                wsdlMessage = restOp.getOperation().getOutput().getMessage();
            } else {
                wsdlMessage = restOp.getOperation().getInput().getMessage();
            }
            
            WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
            wrapperBuilder.initialize(null, wsdlMessage, null);
            
            List partList = wsdlMessage.getOrderedParts(null);
            Part part = null;
            if (partList.size() > 0) {
                part = (Part) partList.get(0);
            }
            
            // skip payload, since payload is empty
            
            Document doc = wrapperBuilder.getResult();
            content = new DOMSource(doc);

        } catch (Exception e) {
            logger.log(Level.WARNING, "unable to create JBI wrapped message", e);
        }
        
        return content;
    }
    
    public static Source createJbiWrappedMsg(String payload, RestOperation restOp, Definition definition, boolean isOutbound) throws Exception {
        Source content = null;
        
        try {
            Message wsdlMessage = null;
            if (isOutbound) {
                wsdlMessage = restOp.getOperation().getOutput().getMessage();
            } else {
                wsdlMessage = restOp.getOperation().getInput().getMessage();
            }
            
            WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
            wrapperBuilder.initialize(null, wsdlMessage, null);
            
            List partList = wsdlMessage.getOrderedParts(null);
            Part part = null;
            if (partList.size() > 0) {
                part = (Part) partList.get(0);
            }

            if (payload != null) {
                buildMessagePayload(payload, wrapperBuilder, part);
            }
            
            Document doc = wrapperBuilder.getResult();
            content = new DOMSource(doc);

        } catch (Exception e) {
            logger.log(Level.WARNING, "unable to create JBI wrapped message", e);
        }
        
        return content;
    }
    
    public static Source createJbiWrappedMsg(InputStream payload, RestOperation restOp, Definition definition, boolean isOutbound) throws Exception {
        StringBuilder sb = new StringBuilder();
        BufferedReader reader = new BufferedReader(new InputStreamReader(payload));
        String currentString = reader.readLine();
        while (currentString != null) {
            sb.append(currentString);
            currentString = reader.readLine();
        }
        
        return createJbiWrappedMsg(sb.toString(), restOp, definition, isOutbound);
    }
    
    private static void buildMessagePayload(String payload, WrapperBuilder wrapperBuilder, Part part) throws Exception {
        String partName = "part1";
        if (part != null) {
            partName = part.getName();
        }
        
        if (part != null) {
            if (part.getElementName() != null) {
                // treat as XML
                Document doc = createDocumentFromString(payload);
                wrapperBuilder.addPart(partName, doc.getDocumentElement());
            } else {
                QName partTypeName = part.getTypeName();
                if (isXsdAnyType(partTypeName)) {
                    try {
                        // treat as XML
                        Document doc = createDocumentFromString(payload);
                        wrapperBuilder.addPart(partName, doc.getDocumentElement());
                    } catch (SAXException saxe) {
                        // treat as Text
                        Text textNode = createTextNodeFromString(payload);
                        wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                    }
                } else if (!isBuiltInType(partTypeName)) {
                    // treat as XML
                    Document doc = createDocumentFromString(payload);
                    wrapperBuilder.addPart(partName, doc.getDocumentElement());
                } else {
                    // treat as Text
                    Text textNode = createTextNodeFromString(payload);
                    wrapperBuilder.addPart(partName, new NodeListImpl(textNode));
                }
            }
        } else {
            // treat as XML
            Document doc = createDocumentFromString(payload);
            wrapperBuilder.addPart(partName, doc.getDocumentElement());
        }
        

    }
    
    private static Document createDocumentFromString(String payload) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(new InputSource(new StringReader(payload)));
        
        return document;
    }
    
    private static Text createTextNodeFromString(String payload) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document doc = builder.newDocument();
        
        Text textNode = doc.createTextNode(payload);
        return textNode;
    }
    
    public static Source createJbiAttachmentWrapper(String uuid, RestOperation restOp, Definition definition, boolean isOutbound) {
        Source content = null;
        
        try {
            Message wsdlMessage = null;
            if (isOutbound) {
                wsdlMessage = restOp.getOperation().getOutput().getMessage();
            } else {
                wsdlMessage = restOp.getOperation().getInput().getMessage();
            }
            
            WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
            wrapperBuilder.initialize(null, wsdlMessage, null);
            
            List partList = wsdlMessage.getOrderedParts(null);
            String partName = "part1";
            if (partList.size() > 0) {
                partName = ((Part) partList.get(0)).getName();
            }
            
            wrapperBuilder.addPartWithAttachment(partName, uuid);
            
            Document doc = wrapperBuilder.getResult();
            content = new DOMSource(doc);

        } catch (Exception e) {
            logger.log(Level.WARNING, "unable to create JIB wrapped message", e);
        }
        
        return content;
    }
    
    public static Source createJbiAttachmentWrapper(QName msgType, String uuid) {
        Source content = null;
        
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.newDocument();
            Element msgElement = doc.createElementNS(WSDL_WRAPPER_URI, "jbi:message");
            msgElement.setAttribute("version", "1.0");
            if (msgType != null) {
                msgElement.setAttribute("xmlns:msgns", msgType.getNamespaceURI());
                msgElement.setAttribute("type", "msgns:" + msgType.getLocalPart());
            }
            
            Element partElement = doc.createElementNS(WSDL_WRAPPER_URI, "jbi:part");

            Element xopElement = doc.createElementNS(XOP_URI, "xop:Include");
            xopElement.setAttribute("href", uuid);
            partElement.appendChild(xopElement);
            
            msgElement.appendChild(partElement);
            
            doc.appendChild(msgElement); 
            
            content = new DOMSource(doc);
           
        } catch (Exception e) {
            logger.log(Level.WARNING, "unable to create JIB wrapped message", e);
        }
        
        return content;
    }
    
    public static Document getDocument(Source source) throws Exception {
        if (source == null) {
            return null;
        }
        
        if (source instanceof DOMSource) {
            Node n = ((DOMSource) source).getNode();
            Document doc = (n.getNodeType() == Node.DOCUMENT_NODE) ? (Document) n : n.getOwnerDocument();
            return doc;
        }
        
        DOMResult result = new DOMResult();
        TransformerFactory tf = TransformerFactory.newInstance();
        tf.newTransformer().transform(source, result);
        Document doc = (Document) result.getNode();
        
        return doc;
    }
    
    public static String convertXmlToString(Source source, boolean stripNamespaces) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Transformer transformer = TransformerFactory.newInstance().newTransformer();
//        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        transformer.transform(source, new StreamResult(baos));
        
        String xmlPayload = baos.toString();
        if (stripNamespaces) {
            return stripNamespaces(xmlPayload);
        }
        
        return xmlPayload;
    }
    
    private static String stripNamespaces(String xmlPayload) {
        return xmlPayload.replaceAll("(<\\?[^<]*\\?>)?", ""). /* remove preamble */
                replaceAll("xmlns.*?(\"|\').*?(\"|\')", "") /* remove xmlns declaration */
                .replaceAll("(<)(\\w+:)(.*?>)", "$1$3") /* remove opening tag prefix */
                .replaceAll("(</)(\\w+:)(.*?>)", "$1$3"); /* remove closing tags prefix */

    }
    
    public static String convertXmlToString(Source source) throws Exception {
        return convertXmlToString(source, false);
    }
    
    private static boolean isBuiltInType(QName typeName) {
        return  "http://www.w3.org/2001/XMLSchema".equals(typeName.getNamespaceURI()) ||
        "http://www.w3.org/1999/XMLSchema".equals(typeName.getNamespaceURI());
    }
    
    private static boolean isXsdAnyType(QName typename) {
        return ("http://www.w3.org/2001/XMLSchema".equals(typename.getNamespaceURI()) ||
                "http://www.w3.org/1999/XMLSchema".equals(typename.getNamespaceURI()) ) &&
                "anyType".equals(typename.getLocalPart());
    }
}
