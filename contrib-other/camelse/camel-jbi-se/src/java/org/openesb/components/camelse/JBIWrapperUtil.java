/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openesb.components.camelse;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.wsdl.WSDL11JBIWrapper;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author chikkala
 */
public class JBIWrapperUtil {
    
    private static final Logger LOG = Logger.getLogger(JBIWrapperUtil.class.getName());
    
    /**
     * removes the jbi wrapper elements from output if it is not a multipart message. If it is
     * a multipart message, it returns the message as it is with the wrapper.
     * @param operation
     * @param outMsg
     * @return
     * @throws javax.jbi.messaging.MessagingException
     */
    public static DOMSource removeJBIWrapperFromOutput(Operation operation, NormalizedMessage outMsg)
    throws MessagingException {
        try {
            
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(
                RuntimeHelper.sourceToDOMSource(outMsg.getContent()) );
            Output output = operation.getOutput();
            Message wsdlMsg = output.getMessage();
            List<Element> msgParts = JBIWrapperUtil.getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            if ( msgParts.size() > 1 ) {
                // just return the wrapped message
                return jbiWrapper.toDOMSource();
            }
            
            Document msgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            msgDoc.appendChild(msgDoc.importNode(msgParts.get(0), true));
            
            DOMSource unwrappedSource = new DOMSource(msgDoc);
            return unwrappedSource;
        } catch (DOMException ex) {
            throw new MessagingException(ex);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }        
    /**
     * normalizes the input message to jbi normalized message
     */
    public static void addJBIWrapperToInput(Operation operation, NormalizedMessage normMsg,
        DOMSource msgContent)  throws MessagingException {
        try {
            WSDL11JBIWrapper msgWrapper = WSDL11JBIWrapper.sourceToWrapper(msgContent);
            if ( msgWrapper != null ) {
                // already wrappred element. For multipart msg, a jbi wrapped content expected.
                // so set the content directly and return.
                //TODO: validate the msgContent against the wsdl input content
                LOG.fine("*** Wrapped content is passed as Input");
                if (LOG.isLoggable(Level.FINE)) {
                    LOG.fine(RuntimeHelper.readFromDOMSource(msgContent).toString());
                }
                normMsg.setContent(msgContent);
                return;
            }
                    
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.createInputWrapper(operation);
            Input input = operation.getInput();
            Message wsdlMsg = input.getMessage();
            //TODO: get the binding input extension here if needed.
            Element msgRoot = RuntimeHelper.getElement(msgContent);
            
            List<Element> msgParts = new ArrayList<Element>();
            msgParts.add(msgRoot);  
            
            addMessagePartsToJBIWrapper(jbiWrapper, wsdlMsg, msgParts);
            
            DOMSource wrappedContent = jbiWrapper.toDOMSource();
            normMsg.setContent(wrappedContent);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
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
    public static void addMessagePartsToJBIWrapper(WSDL11JBIWrapper jbiWrapper, Message wsdlMsg, List<Element> msgParts) throws MessagingException {
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
                    throw new MessagingException("mismatched message content for part " + wsdlPart.getName());
                }
                if ( !wsdlMsg.getQName().getNamespaceURI().equals(msgPart.getNamespaceURI()) ) {
                    throw new MessagingException("mismatched message content namespace for part " + wsdlPart.getName());
                }
                // check the content is text or element.
                List<Element> partContent = JBIWrapperUtil.getChildElements(msgPart);
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
     * removes the jbi wrpper elements from the Normalized message if it is not a multipart message. If
     * it is multipart message, it leaves the jbi wrapper elements.
     * @param operation
     * @param inMsg
     * @throws javax.jbi.messaging.MessagingException
     */
    public static void removeJBIWrapperFromInMessage(Operation operation, NormalizedMessage inMsg)
    throws MessagingException {
        try {
            DOMSource inContent = RuntimeHelper.sourceToDOMSource(inMsg.getContent());
            
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(inContent);
            Input input = operation.getInput();
            Message wsdlMsg = input.getMessage();
            List<Element> msgParts = JBIWrapperUtil.getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            if ( msgParts.size() > 1 ) {
                // don't change the inMessage as it is multi-part and jbiwrapper is what we 
                // return in that case.
                return;
            }            
            Document msgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            msgDoc.appendChild(msgDoc.importNode(msgParts.get(0), true));
            
            DOMSource unwrappedSource = new DOMSource(msgDoc);
            inMsg.setContent(unwrappedSource);
            
        } catch (DOMException ex) {
            throw new MessagingException(ex);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }      
    /**
     * extracts the message parts from the jbiWrapper according to the abstract wsdl message 
     * definition passed to it. Use this method in denormalizing the jbi wrapper message into the
     * binding protocol specific concrete message.
     * @param jbiWrapper jbi wrapper object that contains message parts and the message type information.
     * @param wsdlMsg abstract wsdl message definition to use in constructing the part elements.
     */
    public static List<Element> getMessagePartsFromJBIWrapper(WSDL11JBIWrapper jbiWrapper, Message wsdlMsg)
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
                partEl.setAttributeNS(WSDL11JBIWrapper.XMLNS_NS, "xmlns:"+prefix, nsURI);
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
    public static void appendChildren(Element el, NodeList nodeList, Document doc, boolean importNode) {
        
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
    public static List<Element> getChildElements(Element el) {
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
