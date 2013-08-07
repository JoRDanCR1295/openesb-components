#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * JMXBindingNormalizer.java
 */
package net.openesb.component.${artifactId};

import net.openesb.component.${artifactId}.common.wsdl.AbstractNormalizer;
import net.openesb.component.${artifactId}.common.wsdl.WSDL11JBIWrapper;
import net.openesb.component.${artifactId}.common.RuntimeHelper;
import java.util.List;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Fault;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This class is a concrete implementation of the AbstractNormalizer to
 * implement normalize jmx binding specific concrete messages to jbi wrapper
 * messages and denormalize the jbi wrapper messages to jmx binding specific
 * messages. The jmx binding concrete message has "message" as the root element
 * that contains the part elements as its children. for example <jmxbc:message
 * xmlns:jmxbc="http://java.sun.com/jbi/sample/jmx-bc/envelope/"
 * xmlns:msgns="http://samples.com/wsdl/definition/types/"
 * xmlns:wsdlns="http://samples.com/wsdl/definition/" >
 * <msgns-ns:part1-elememnt/> <!-- part has element attribute -->
 * <msgns:part2-elememnt/> <wsdlns:part3-type-name> <!-- part has type
 * attribute. so element of the type are wrapped with part name -->
 * <msgns:part3-type-elements> <wsdlns:part3-type-name> </jmxbc:message>
 *
 * This is the sample code that demonstrate how to normalize and denormalize
 * messages to/from jbi wrapper from/to concrete messages specific to a binding
 * protocol
 *
 * @author chikkala
 */
public class JMXBindingNormalizer extends AbstractNormalizer {
    
    public static final String JMXBC_MSG_NS = "http://java.sun.com/jbi/sample/jmx-bc/envelope/";
    public static final String DEF_NS_PREFIX = "jmxbc";
    public static final String MESSAGE_EL = "message";

    /**
     * Creates a new instance of JMXBCNormalizer
     */
    public JMXBindingNormalizer(Definition wsdl, Binding binding) {
        super(wsdl, binding);
    }

    /**
     * normalizes the jmx binding protocol specific concrete message to jbi
     * normalized message
     */
    public void normalizeInput(Operation operation, NormalizedMessage normMsg,
            DOMSource msgSource) throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.createInputWrapper(operation);
            Input input = operation.getInput();
            Message wsdlMsg = input.getMessage();
            //TODO: get the binding input extension here if needed.
            Element jmxMessage = RuntimeHelper.getElement(msgSource);
            
            if (!JMXBC_MSG_NS.equals(jmxMessage.getNamespaceURI())
                    || !MESSAGE_EL.equals(jmxMessage.getLocalName())) {
                throw new MessagingException("invalid root element for jmxbc envelope");
            }
            List<Element> jmxMsgParts = getChildElements(jmxMessage);
            
            addMessagePartsToJBIWrapper(jbiWrapper, wsdlMsg, jmxMsgParts);
            
            DOMSource wrappedSource = jbiWrapper.toDOMSource();
            normMsg.setContent(wrappedSource);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }

    /**
     * normalizes the jmx binding protocol specific concrete message to jbi
     * normalized message
     */
    public void normalizeOutput(Operation operation, NormalizedMessage normMsg,
            DOMSource msgSource) throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.createInputWrapper(operation);
            Output output = operation.getOutput();
            Message wsdlMsg = output.getMessage();
            //TODO: get the binding output extension here if needed.
            Element jmxMessage = RuntimeHelper.getElement(msgSource);
            
            if (!JMXBC_MSG_NS.equals(jmxMessage.getNamespaceURI())
                    || !MESSAGE_EL.equals(jmxMessage.getLocalName())) {
                throw new MessagingException("invalid root element for jmxbc envelope");
            }
            List<Element> jmxMsgParts = getChildElements(jmxMessage);
            
            addMessagePartsToJBIWrapper(jbiWrapper, wsdlMsg, jmxMsgParts);
            
            DOMSource wrappedSource = jbiWrapper.toDOMSource();
            normMsg.setContent(wrappedSource);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }

    /**
     * normalizes the jmx binding protocol specific concrete message to jbi
     * normalized message
     */
    public void normalizeFault(Operation operation, String faultName, NormalizedMessage normMsg,
            DOMSource msgSource) throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.createInputWrapper(operation);
            Fault wsdlFault = operation.getFault(faultName);
            Message wsdlMsg = wsdlFault.getMessage();
            //TODO: get the binding fault extension here if needed.
            Element jmxMessage = RuntimeHelper.getElement(msgSource);
            
            if (!JMXBC_MSG_NS.equals(jmxMessage.getNamespaceURI())
                    || !MESSAGE_EL.equals(jmxMessage.getLocalName())) {
                throw new MessagingException("invalid root element for jmxbc envelope");
            }
            List<Element> jmxMsgParts = getChildElements(jmxMessage);
            
            addMessagePartsToJBIWrapper(jbiWrapper, wsdlMsg, jmxMsgParts);
            
            DOMSource wrappedSource = jbiWrapper.toDOMSource();
            normMsg.setContent(wrappedSource);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }

    /**
     * denormalizes the jbi message to the jmx binding protocol specific
     * concrete message.
     */
    public DOMSource denormalizeInput(Operation operation, NormalizedMessage normMsg)
            throws MessagingException {
        try {
            
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(normMsg.getContent()));
            Input input = operation.getInput();
            Message wsdlMsg = input.getMessage();
            //TODO: get the binding input extension here if needed.
            
            Document jmxMsgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            Element jmxMessage = jmxMsgDoc.createElementNS(JMXBC_MSG_NS, DEF_NS_PREFIX + ":" + MESSAGE_EL);
            jmxMessage.setAttributeNS(XMLNS_NS, "xmlns:" + DEF_NS_PREFIX, JMXBC_MSG_NS);
            jmxMsgDoc.appendChild(jmxMessage);
            
            List<Element> jmxMsgParts = getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            for (int i = 0; i < jmxMsgParts.size(); ++i) {
                jmxMessage.appendChild(jmxMsgDoc.importNode(jmxMsgParts.get(i), true));
            }
            
            DOMSource unwrappedSource = new DOMSource(jmxMsgDoc);
            return unwrappedSource;
        } catch (DOMException ex) {
            throw new MessagingException(ex);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }

    /**
     * denormalizes the jbi message to the jmx binding protocol specific
     * concrete message.
     */
    public DOMSource denormalizeOutput(Operation operation, NormalizedMessage normMsg)
            throws MessagingException {
        try {
            
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(normMsg.getContent()));
            Output output = operation.getOutput();
            Message wsdlMsg = output.getMessage();
            //TODO: get the binding output extension here if needed.
            
            Document jmxMsgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            Element jmxMessage = jmxMsgDoc.createElementNS(JMXBC_MSG_NS, DEF_NS_PREFIX + ":" + MESSAGE_EL);
            jmxMessage.setAttributeNS(XMLNS_NS, "xmlns:" + DEF_NS_PREFIX, JMXBC_MSG_NS);
            
            List<Element> jmxMsgParts = getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            
            for (int i = 0; i < jmxMsgParts.size(); ++i) {
                jmxMessage.appendChild(jmxMsgDoc.importNode(jmxMsgParts.get(i), true));
            }
            jmxMsgDoc.appendChild(jmxMessage);
            DOMSource unwrappedSource = new DOMSource(jmxMsgDoc);
            return unwrappedSource;
        } catch (DOMException ex) {
            throw new MessagingException(ex);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }

    /**
     * denormalizes the jbi message to the jmx binding protocol specific
     * concrete message.
     */
    public DOMSource denormalizeFault(Operation operation, String faultName, NormalizedMessage normMsg)
            throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(normMsg.getContent()));
            Fault wsdlFault = operation.getFault(faultName);
            Message wsdlMsg = wsdlFault.getMessage();
            //TODO: get the binding fault extension here if needed.
            
            Document jmxMsgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            Element jmxMessage = jmxMsgDoc.createElementNS(JMXBC_MSG_NS, DEF_NS_PREFIX + ":" + MESSAGE_EL);
            jmxMessage.setAttributeNS(XMLNS_NS, "xmlns:" + DEF_NS_PREFIX, JMXBC_MSG_NS);
            
            List<Element> jmxMsgParts = getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            
            for (int i = 0; i < jmxMsgParts.size(); ++i) {
                jmxMessage.appendChild(jmxMsgDoc.importNode(jmxMsgParts.get(i), true));
            }
            jmxMsgDoc.appendChild(jmxMessage);
            DOMSource unwrappedSource = new DOMSource(jmxMsgDoc);
            return unwrappedSource;
        } catch (DOMException ex) {
            throw new MessagingException(ex);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }
}
