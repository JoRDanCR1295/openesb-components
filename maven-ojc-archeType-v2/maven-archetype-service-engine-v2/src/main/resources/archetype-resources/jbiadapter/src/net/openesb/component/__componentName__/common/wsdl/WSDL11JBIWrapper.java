#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * WSDL11JBIWrapper.java
 *
 */

package net.openesb.component.${componentName}.common.wsdl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.wsdl.Fault;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * This class wraps the wsdl11 messages to the jbi wrapper message suitable for
 * processing in the wsdl2.0 model of the normalized message in jbi as defined in jsr208 like
 *   <jbi:message version="1.0"
 *       type="qname of message attribute from wsdl:input, wsdl:output or wsdl:fault"
 *       name="optional name attribute from wsdl:input, wsdl:output or wsdl:fault"
 *       xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
 *     <jbi:part>
 *         <message-elements/>
 *     </jbi:part>
 *     <jbi:part>
 *       <message-elements/>
 *     </jbi:part>
 *   </jbi:message>
 *
 *
 * @author chikkala
 */
public class WSDL11JBIWrapper {
    
    public static final String XMLNS_NS = "http://www.w3.org/2000/xmlns/";
    public static final String WRAPPER_NAMESPACE = "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper";
    public static final String DEF_NS_PREFIX = "jbiwrp";
    public static final String DEF_MESSAGE_NS_PREFIX = "msgns";
    public static final String JBI_MESSAGE_EL = "message";
    public static final String JBI_PART_EL = "part";
    public static final String VERSION_ATTR = "version";
    public static final String VERSION_ATTR_VALUE = "1.0";
    public static final String TYPE_ATTR = "type";
    public static final String NAME_ATTR = "name";
    
    private static DocumentBuilder sDocBuilder = null;
    /** qname of message attribute from wsdl:input, wsdl:output or wsdl:fault */
    private QName mType;
    /** optional name attribute from wsdl:input, wsdl:output or wsdl:fault */
    private String mName;
    /** each parts contents as node list */
    List<NodeList> mPartConentList = new ArrayList<NodeList>();
    
    /** Creates a new instance of WSDL11JBIWrapper */
    public WSDL11JBIWrapper() {
    }
    /** creates the namespace aware document builder. extended classes can override this method
     * to return the doc builder created else where.
     */
    protected DocumentBuilder createDocumentBuilder() throws ParserConfigurationException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        return factory.newDocumentBuilder();
    }
    /** return the document builder
     * @return DocumentBuilder
     */
    public final DocumentBuilder getDocumentBuilder() throws ParserConfigurationException {
        if ( WSDL11JBIWrapper.sDocBuilder == null ) {
            WSDL11JBIWrapper.sDocBuilder = createDocumentBuilder();
        }
        return WSDL11JBIWrapper.sDocBuilder;
    }
    /**
     * @return type qname of message attribute from wsdl:input, wsdl:output or wsdl:fault
     */
    public QName getType() {
        return this.mType;
    }
    /**
     * @param type qname of message attribute from wsdl:input, wsdl:output or wsdl:fault
     */
    public void setType(QName type) {
        this.mType = type;
    }
    /** @return name attribute from wsdl:input, wsdl:output or wsdl:fault. or null if not set. */
    public String getName() {
        return this.mName;
    }
    /**
     * @param name optional name attribute from wsdl:input, wsdl:output or wsdl:fault. can be null.
     */
    public void setName(String name) {
        this.mName = name;
    }
    /**
     * appends part content as node list
     */
    public void appendPart(NodeList partContent) {
        this.mPartConentList.add(partContent);
    }
    /**
     * append part content from the List of nodes
     */
    public void appendPart(List<? extends Node> partContent) {
        this.mPartConentList.add(new NodeListImpl(partContent));
    }
    /**
     * append part content as element
     */
    public void appendPart(Element partContent) {
        this.mPartConentList.add(new NodeListImpl(partContent));
    }
    /**
     * append part content from DOMSource
     */
    public void appendPart(DOMSource partContent) {
        Element partConentEl = getElement(partContent.getNode());
        this.mPartConentList.add(new NodeListImpl(partConentEl));
    }
    /**
     * append part content as text node.
     */
    public void appendPart(String partContent) {
        Text textContent = null;
        try {
            textContent = this.getDocumentBuilder().newDocument().createTextNode(partContent);
        } catch (ParserConfigurationException ex) {
            ex.printStackTrace();
        }
        if ( textContent != null ) {
            this.mPartConentList.add(new NodeListImpl(textContent));
        }
    }
    /**
     * append list of part contents each is a NodeList to the parts
     */
    public void appendParts(List<NodeList> partContentList) {
        this.mPartConentList.addAll(partContentList);
    }
    /**
     * returned the ordered list of part contents.
     */
    public List<NodeList> getParts() {
        return Collections.unmodifiableList(this.mPartConentList);
    }
    /**
     * return the number of parts
     */
    public int getPartCount() {
        return this.mPartConentList.size();
    }
    /**
     * return the part content at a particular index as NodeList.
     */
    public NodeList getPart(int idx) {
        return this.mPartConentList.get(idx);
    }
    /**
     * return part context at a particular index as text
     */
    public String getPartAsText(int idx) {
        NodeList partNodes = getPart(idx);
        Document doc;
        try {
            doc = this.getDocumentBuilder().newDocument();
            Element el = doc.createElementNS(WRAPPER_NAMESPACE, DEF_NS_PREFIX + ":" + JBI_PART_EL);
            for ( int i=0; i < partNodes.getLength(); ++i) {
                el.appendChild(partNodes.item(i));
            }
            return el.getTextContent();
        } catch (ParserConfigurationException ex) {
            return null;
        }
    }
    /**
     * return part context at a particular index as Element
     */
    public Element getPartAsElement(int idx) {
        Element contentEl = null;
        NodeList partNodes = getPart(idx);
        for ( int i=0; i < partNodes.getLength(); ++i) {
            Node node = partNodes.item(i);
            if ( node instanceof Element) {
                contentEl = (Element)node;
                break;
            }
        }
        return contentEl;
    }
    /**
     * creates Document from this wrapper object. Before calling this method,
     * the type, name (optional) and parts contents should be set.
     * @return Document containing the wsdl 11 wrapper xml
     */
    public Document toDocument() throws ParserConfigurationException {
        
        Document wrapperDoc = getDocumentBuilder().newDocument();
        Element jbiMessage = wrapperDoc.createElementNS(WRAPPER_NAMESPACE, DEF_NS_PREFIX + ":" + JBI_MESSAGE_EL);
        jbiMessage.setAttribute(VERSION_ATTR, VERSION_ATTR_VALUE);
        wrapperDoc.appendChild(jbiMessage);
        
        QName type = this.getType();
        if ( type == null ) {
            type = new QName("", "", DEF_MESSAGE_NS_PREFIX);
        }
        // set the jbiMessage attributes
        String prefix = type.getPrefix();
        if (prefix == null || prefix.length() == 0) {
            prefix = DEF_MESSAGE_NS_PREFIX;
        }
        jbiMessage.setAttribute(TYPE_ATTR, prefix + ":" + type.getLocalPart());
        jbiMessage.setAttributeNS( XMLNS_NS, "xmlns:" + prefix, type.getNamespaceURI());
        
        String name = this.getName();
        if (name != null && name.trim().length() > 0 ) {
            jbiMessage.setAttribute(NAME_ATTR, name);
        }
        
        List<NodeList> partContentList = this.getParts();
        for ( NodeList nodeList : partContentList ) {
            // set jbi part message
            Element jbiPart = wrapperDoc.createElementNS(WRAPPER_NAMESPACE, DEF_NS_PREFIX + ":" + JBI_PART_EL);
            jbiMessage.appendChild(jbiPart);
            for ( int i =0; i < nodeList.getLength(); ++i ) {
                Node importedMsgNode = wrapperDoc.importNode(nodeList.item(i), true);
                jbiPart.appendChild(importedMsgNode);
            }
        }
        
        return  wrapperDoc;
        
    }
    /**
     * creates DOMSource from this wrapper object. Before calling this method,
     * the type, name (optional) and parts contents should be set.
     * @return DOMSource containing the wsdl 11 wrapper xml
     */
    public DOMSource toDOMSource() throws ParserConfigurationException {
        DOMSource wrappedSource = new DOMSource();
        wrappedSource.setNode(toDocument());
        return wrappedSource;
    }
    /**
     * return Element node from a document node or non document. Use to extract
     * the message root element.
     * @root node from which the Element node will be extracted.
     * @return Element node.
     */
    public static Element getElement(Node root) {
        Element msgEl = null;
        if ( root instanceof Document) {
            msgEl = ((Document)root).getDocumentElement();
        } else if (root instanceof Element) {
            msgEl = (Element)root;
        } else {
            NodeList nodeList = root.getChildNodes();
            for ( int i=0; i < nodeList.getLength(); ++i) {
                Node node = nodeList.item(i);
                if ( node instanceof Element ) {
                    msgEl = (Element) node;
                    break;
                }
            }
        }
        return msgEl;
    }
    /**
     * creates wrapper object from the DOMSource that has wsdl 11 wrapper xml.
     * @param wrappedMsg wsdl 11 wrapper xml as DOMSource
     * @return WSDL11JBIWrapper representing the wrapper xml.
     */
    public static WSDL11JBIWrapper sourceToWrapper(DOMSource wrappedMsg) {
        Element jbiMessage = getElement(wrappedMsg.getNode());
        if ( jbiMessage == null ) {
            return null;
        }
        String nsURI = jbiMessage.getNamespaceURI();
        String tagName = jbiMessage.getLocalName();
        if (!WRAPPER_NAMESPACE.equals(nsURI) || !JBI_MESSAGE_EL.equals(tagName)) {
            return null;
        }
        String name = jbiMessage.getAttribute(NAME_ATTR);
        String typeQN = jbiMessage.getAttribute(TYPE_ATTR);
        String typePrefix = "";
        String typeName = "";
        if ( typeQN != null && typeQN.trim().length() > 0 ) {
            int idx = typeQN.indexOf(':');
            if ( idx >= 0 ) {
                typePrefix = typeQN.substring(0, idx);
                if ( typeQN.length() > idx ) {
                    typeName = typeQN.substring(idx+1);
                }
            } else {
                typePrefix = "";
                typeName = typeQN;
            }
        }
        String typeURI = jbiMessage.getAttribute("xmlns:" + typePrefix);
        QName typeQName = new QName(typeURI, typeName, typePrefix);
        
        WSDL11JBIWrapper wrapper = new WSDL11JBIWrapper();
        wrapper.setName(name);
        wrapper.setType(typeQName);
        NodeList jbiPartList = jbiMessage.getElementsByTagNameNS(WRAPPER_NAMESPACE, JBI_PART_EL);
        for ( int i=0; i < jbiPartList.getLength(); ++i) {
            Node jbiPart = jbiPartList.item(i);
            wrapper.appendPart(jbiPart.getChildNodes());
        }
        return wrapper;
    }
    /**
     * creates the WSDL11JBIWrapper object and sets the type and name of the object
     * @param type qname of message attribute from wsdl:input, wsdl:output or wsdl:fault
     * @param name optional name attribute from wsdl:input, wsdl:output or wsdl:fault
     * @return the jbi message wrapper object
     */
    public static WSDL11JBIWrapper createWrapper(QName type, String name ) throws ParserConfigurationException {
        WSDL11JBIWrapper wrapper = new WSDL11JBIWrapper();
        wrapper.setName(name);
        wrapper.setType(type);
        return wrapper;
    }
    /**
     * creates the WSDL11JBIWrapper object. useful when the message has one part and the
     * part content has multiple child element.
     * @param type qname of message attribute from wsdl:input, wsdl:output or wsdl:fault
     * @param name optional name attribute from wsdl:input, wsdl:output or wsdl:fault
     * @param partContent part content as node list
     * @return the jbi message wrapper object
     */
    public static WSDL11JBIWrapper createWrapper(QName type, String name, NodeList partContent) throws ParserConfigurationException {
        WSDL11JBIWrapper wrapper = createWrapper(type, name);
        wrapper.appendPart(partContent);
        return wrapper;
    }
    /**
     * creates the WSDL11JBIWrapper object. useful when the message has one part and the
     * part content is a single element
     * @param type qname of message attribute from wsdl:input, wsdl:output or wsdl:fault
     * @param name optional name attribute from wsdl:input, wsdl:output or wsdl:fault
     * @param partContent part content as node list
     * @return the jbi message wrapper object
     */
    public static WSDL11JBIWrapper createWrapper(QName type, String name,  Element msgEl) throws ParserConfigurationException {
        WSDL11JBIWrapper wrapper = createWrapper(type, name, new WSDL11JBIWrapper.NodeListImpl(msgEl));
        return wrapper;
    }
    
    public static WSDL11JBIWrapper createWrapper(QName type, String name, DOMSource inputSource) throws ParserConfigurationException {
        WSDL11JBIWrapper wrapper = createWrapper(type, name);
        Node msgNode = getElement(inputSource.getNode());
        NodeList nodeList = new WSDL11JBIWrapper.NodeListImpl(msgNode);
        wrapper.appendPart(nodeList);
        return wrapper;
    }
    /**
     * creates the wrapped message using the wsdl4j operations input element for type and name.
     * @param msg DOMSource for the unwrapped message
     * @param operation wsdl4j operation object representing the operation of the PortType
     * @return DOMSource wrapped message as DOMSource
     */
    public static WSDL11JBIWrapper createInputWrapper(Operation operation) throws ParserConfigurationException {
        QName type = null;
        String name = null;
        
        Input input = operation.getInput();
        if ( input == null ) {
            return null;
        }
        name = input.getName();
        Message wsdlMsg = input.getMessage();
        type = wsdlMsg.getQName();
        
        WSDL11JBIWrapper wrapper = createWrapper(type, name);
        return wrapper;
    }
    /**
     * creates the wrapped message using the wsdl4j operations output element for type and name.
     * @param msg DOMSource for the unwrapped message
     * @param operation wsdl4j operation object representing the operation of the PortType
     * @return DOMSource wrapped message as DOMSource
     */
    public static WSDL11JBIWrapper createOutputWrapper(Operation operation) throws ParserConfigurationException {
        QName type = null;
        String name = null;
        
        Output output = operation.getOutput();
        if ( output == null) {
            return null;
        }
        name = output.getName();
        Message wsdlMsg = output.getMessage();
        type = wsdlMsg.getQName();
        
        WSDL11JBIWrapper wrapper = createWrapper(type, name);
        return wrapper;
    }
    /**
     * creates the wrapped message using the wsdl4j operations output element for type and name.
     * @param msg DOMSource for the unwrapped message
     * @param operation wsdl4j operation object representing the operation of the PortType
     * @param faultName fault name, can be null to look for the fault with no name.
     * @return DOMSource wrapped message as DOMSource
     */
    public static WSDL11JBIWrapper createFaultWrapper(Operation operation, String faultName) throws ParserConfigurationException {
        QName type = null;
        String name = null;
        @SuppressWarnings("unchecked")
        Map<String, Fault> faultMap = operation.getFaults();
        Fault fault = faultMap.get(faultName);
        
        if ( fault == null ) {
            return null;
        }
        name = fault.getName();
        Message wsdlMsg = fault.getMessage();
        type = wsdlMsg.getQName();
        
        WSDL11JBIWrapper wrapper = createWrapper(type, name);
        return wrapper;
    }
    /**
     * NodeList implementation.
     */
    public static class NodeListImpl extends ArrayList<Node> implements NodeList {
        
        public NodeListImpl() {
            super();
        }
        
        public NodeListImpl(Node aNode) {
            super();
            if (aNode != null) {
                this.add(aNode);
            }
        }
        
        public NodeListImpl(List<? extends Node> nodes) {
            if (nodes != null) {
                this.addAll(nodes);
            }
        }
        
        public int getLength() {
            return this.size();
        }
        
        public Node item(int idx) {
            return this.get(idx);
        }
        
    }
    
}
