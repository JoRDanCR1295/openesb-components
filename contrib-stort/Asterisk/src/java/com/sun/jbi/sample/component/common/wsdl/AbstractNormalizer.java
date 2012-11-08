/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AbstractNormalizer.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common.wsdl;

import java.util.ArrayList;
import java.util.List;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public abstract class AbstractNormalizer {
    
    public static final String XMLNS_NS = "http://www.w3.org/2000/xmlns/";
    /** wsdl definition to use when normalizing and denormalizing */
    private Definition mWSDL;
    /** Binding definition to use when normalizing and denormalizing */
    private Binding mBinding;
    
    private AbstractNormalizer() {}
    /** Creates a new instance of JMXBCNormalizer */
    public AbstractNormalizer(Definition wsdl, Binding binding) {
        this.mWSDL = wsdl;
        this.mBinding = binding;
    }
    /**
     * normalize the binding protocol specific concrete message to jbi wrapper.
     * @param operation wsdl operation for which a concrete message should be normalized.
     * @param normMsg NoramalizedMessage which will be configurate with normalized data from
     * the concrete message
     * @param msgSource concrete mssage of a particular binding protocol.
     */
    public abstract void normalizeInput(Operation operation, NormalizedMessage normMsg, DOMSource msgSource)
    throws MessagingException;
    /**
     * normalize the binding protocol specific concrete message to jbi wrapper.
     * @param operation wsdl operation for which a concrete message should be normalized.
     * @param normMsg NoramalizedMessage which will be configurate with normalized data from
     * the concrete message
     * @param msgSource concrete message of a particular binding protocol.
     */
    public abstract void normalizeOutput(Operation operation, NormalizedMessage normMsg, DOMSource msgSource)
    throws MessagingException;
    /**
     * normalize the binding protocol specific concrete message to jbi wrapper.
     * @param operation wsdl operation for which a concrete message should be normalized.
     * @param normMsg NoramalizedMessage which will be configurate with normalized data from
     * the concrete message
     * @param msgSource concrete message of a particular binding protocol.
     */
    public abstract void normalizeFault(Operation operation, String faultName, NormalizedMessage normMsg, DOMSource msgSource)
    throws MessagingException;    
    /**
     * denormalize the normalized message into a concrete message for a particular binding protocol
     * @param operation wsdl operation for which a concrete message should be de-normalized.
     * @param normMsg NormalizedMessage which should be used to create de-normalized message.
     */
    public abstract DOMSource denormalizeInput(Operation operation, NormalizedMessage normMsg)
    throws MessagingException;
    /**
     * denormalize the normalized message into a concrete message for a particular binding protocol
     * @param operation wsdl operation for which a concrete message should be denormalized.
     * @param normMsg NormalizedMessage which should be used to create denormalized message.
     */
    public abstract DOMSource denormalizeOutput(Operation operation, NormalizedMessage normMsg)
    throws MessagingException;
    
    /**
     * denormalized the normalized fault message into a concrete message for a particular binding protocol
     * @param operation wsdl operation for which a concrete message should be denormalized.
     * @param normMsg NormalizedMessage which should be used to create denormalized message.
     */    
    public abstract DOMSource denormalizeFault(Operation operation, String faultName, NormalizedMessage normMsg)
    throws MessagingException;
    /**
     * @return the wsdl definition to use in normalizing and denormalizing the message
     */
    protected Definition getWSDL() {
        return this.mWSDL;
    }
    /**
     * @return the wsdl binding definition to use in normalizing and denormalizing the message
     */   
    protected Binding getBinding() {
        return this.mBinding;
    }
    /**
     * create and add message parts to the jbiWrapper according to the abstract message model. This
     * method assumes that the each element in the msgParts list passed to it is mapped to the part
     * of the abstract wsdl message and uses the type or element attribute of the abstract message to
     * determine whether the element is actual part element or a wrapped part type.
     * Use this method in normalizing the concrete protocol specific message to jbi wrapper message.
     * @param jbiWrapper object that holds the jbi wrapper information.
     * @param wsdlMsg abstract message from the wsdl definition 
     * @param msgParts actual message parts from the concrete message
     */
    protected void addMessagePartsToJBIWrapper(WSDL11JBIWrapper jbiWrapper, Message wsdlMsg, List<Element> msgParts) throws MessagingException {
        List wsdlParts = wsdlMsg.getOrderedParts(null);
        for ( int i=0; i < wsdlParts.size(); ++i )  {
            Part wsdlPart = (Part) wsdlParts.get(i);
            if ( i >= msgParts.size() ) {
                throw new MessagingException("missing message content for part " + wsdlPart.getName());
            }
            Element msgPart = msgParts.get(i);
            if ( wsdlPart.getElementName() != null ) {
                jbiWrapper.appendPart(msgPart);
            } else {
                // it is type.
                // check the element name is same as part
                
                if ( !wsdlPart.getName().equals(msgPart.getLocalName()) ) {
                    throw new MessagingException("-> " + msgPart.getLocalName() + " mismatched message content for part " + wsdlPart.getName());
                }
                if ( !wsdlMsg.getQName().getNamespaceURI().equals(msgPart.getNamespaceURI()) ) {
                    throw new MessagingException("qname -> " + wsdlMsg.getQName().getNamespaceURI() + " msgpart -> " + msgPart.getNamespaceURI() +  " mismatched message content namespace for part " + wsdlPart.getName());
                }
                // check the content is text or element.
                List<Element> partContent = getChildElements(msgPart);
                if ( partContent.size() > 0 ) {
                    // add content as part elements
                    jbiWrapper.appendPart(partContent);
                } else {
                    // add the content as text
                    jbiWrapper.appendPart(msgPart.getTextContent());
                }
            }
        }
    }
    /**
     * extracts the message parts from the jbiWrapper according to the abstract wsdl message 
     * definition passed to it. Use this method in denormalizing the jbi wrapper message into the
     * binding protocol specific concrete message.
     * @param jbiWrapper jbi wrapper object that contains message parts and the message type information.
     * @param wsdlMsg abstract wsdl message definition to use in constructing the part elements.
     */
    protected List<Element> getMessagePartsFromJBIWrapper(WSDL11JBIWrapper jbiWrapper, Message wsdlMsg)
    throws MessagingException, ParserConfigurationException {
        
        List<Element> msgParts = new ArrayList<Element>();
        int jbiPartCount = jbiWrapper.getPartCount();
        List wsdlParts = wsdlMsg.getOrderedParts(null);
        QName msgType = jbiWrapper.getType();
        if (!wsdlMsg.getQName().getNamespaceURI().equals(msgType.getNamespaceURI())) {
            throw new MessagingException("Namespace mismatch between jbi wrapper message type and wsdl message");
        }
        Document newDoc = jbiWrapper.getDocumentBuilder().newDocument();
        for ( int i=0; i < wsdlParts.size(); ++i )  {
            Part wsdlPart = (Part) wsdlParts.get(i);
            if ( i >= jbiPartCount ) {
                throw new MessagingException("missing message content for part " + wsdlPart.getName());
            }
            if ( wsdlPart.getElementName() != null ) {
                msgParts.add(jbiWrapper.getPartAsElement(i));
            } else {
                // it is type. create a new element for a typed part
                // check the element name is same as part
                String prefix = msgType.getPrefix();
                String nsURI = msgType.getNamespaceURI();
                String localName = wsdlPart.getName();
                Element partEl = newDoc.createElementNS(nsURI, prefix + ":" + localName);
                partEl.setAttributeNS(XMLNS_NS, "xmlns:"+prefix, nsURI);
                NodeList partContent = jbiWrapper.getPart(i);
                appendChildren(partEl, partContent, newDoc, true);
                msgParts.add(partEl);
            }
        }
        return msgParts;
    }
    /**
     * utility method that can append the nodeList passed to it to the element children.
     * @param el element node to which the nodeList should be appended
     * @param doc the document object that should be used to import the nodeList
     * @param importNode true if the nodeList should be imported while appending the nodeList to the 
     * element children. false if no import is necessary.
     */
    protected void appendChildren(Element el, NodeList nodeList, Document doc, boolean importNode) {
        
        for ( int pIdx = 0; pIdx < nodeList.getLength(); ++pIdx) {
            Node node = nodeList.item(pIdx);
            if ( importNode ) {
                node = doc.importNode(node, true);
            }
            el.appendChild(node);
        }
    }
    /**
     * @param el element from which to extract the child elements
     * @return List<Element> list of child Element nodes.
     */
    protected List<Element> getChildElements(Element el) {
        List<Element> list = new ArrayList<Element>();
        NodeList nodeList = el.getChildNodes();
        for ( int i=0; i < nodeList.getLength(); ++i) {
            Node node = nodeList.item(i);
            if (!(node instanceof Element) ){
                continue;
            }
            list.add((Element)node);
        }
        return list;
    }
    
}
