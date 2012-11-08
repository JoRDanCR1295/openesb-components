/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskNormalizer.java,v 1.2 2008/01/27 20:59:42 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.sun.jbi.sample.component.common.wsdl.*;
import com.sun.jbi.sample.component.common.RuntimeHelper;
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

/*
 *
 * <asterisk-concrete-message >  --> this is your concrete message your bc can understand and extract the abstract message to normalize.
 *  <Asterisk-event xmlns="http://www.stortsystems.com/asterisk-bc/events">  ---> this is your abstract message for which you define a schema that will be included in wsdl types.
 *    <Event>
 *      <Event>Newexten</Event>
 *      <Privilege>call,all</Privilege>
 *      ....
 *     </Event>
 *  </Asterisk-event>
 * </asterisk-concrete-message > 
 *
 */

public class AsteriskNormalizer extends AbstractNormalizer {
    
    public static final String ASTERISKBC_MSG_NS = "http://www.stortsystems.com/asterisk-bc/events";
    public static final String DEF_NS_PREFIX = "asterisk-bc";
    public static final String MESSAGE_EL = "Asterisk-event";
    
    public AsteriskNormalizer(Definition wsdl, Binding binding) {
        super(wsdl, binding);
    }
    
    public void normalizeInput(Operation operation, NormalizedMessage normMsg,
            DOMSource msgSource)  throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.createInputWrapper(operation);
            Input input = operation.getInput();
            Message wsdlMsg = input.getMessage();
            //TODO: get the binding input extension here if needed.
            Element jmxMessage = RuntimeHelper.getElement(msgSource);
            
            if (!ASTERISKBC_MSG_NS.equals(jmxMessage.getNamespaceURI()) ||
                    !MESSAGE_EL.equals(jmxMessage.getLocalName())  ) {
                throw new MessagingException("invalid root element for asteriskbc envelope");
            }
             
            List<Element> jmxMsgParts = getChildElements(jmxMessage);
            
            addMessagePartsToJBIWrapper(jbiWrapper, wsdlMsg, jmxMsgParts);
            
            DOMSource wrappedSource = jbiWrapper.toDOMSource();
            normMsg.setContent(wrappedSource);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }
    
    public void normalizeOutput(Operation operation, NormalizedMessage normMsg,
            DOMSource msgSource)  throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.createInputWrapper(operation);
            Output output = operation.getOutput();
            Message wsdlMsg = output.getMessage();
            //TODO: get the binding output extension here if needed.
            Element jmxMessage = RuntimeHelper.getElement(msgSource);
            
            if (!ASTERISKBC_MSG_NS.equals(jmxMessage.getNamespaceURI()) ||
                    !MESSAGE_EL.equals(jmxMessage.getLocalName())  ) {
                throw new MessagingException("invalid root element for asteriskbc envelope");
            }
            
            List<Element> jmxMsgParts = getChildElements(jmxMessage);
            
            addMessagePartsToJBIWrapper(jbiWrapper, wsdlMsg, jmxMsgParts);
            
            DOMSource wrappedSource = jbiWrapper.toDOMSource();
            normMsg.setContent(wrappedSource);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }
    
    public void normalizeFault(Operation operation, String faultName, NormalizedMessage normMsg,
            DOMSource msgSource)  throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.createInputWrapper(operation);
            Fault wsdlFault = operation.getFault(faultName);
            Message wsdlMsg = wsdlFault.getMessage();
            //TODO: get the binding fault extension here if needed.
            Element jmxMessage = RuntimeHelper.getElement(msgSource);
            
            if (!ASTERISKBC_MSG_NS.equals(jmxMessage.getNamespaceURI()) ||
                    !MESSAGE_EL.equals(jmxMessage.getLocalName())  ) {
                throw new MessagingException("invalid root element for asteriskbc envelope");
            }
            
            List<Element> jmxMsgParts = getChildElements(jmxMessage);
            
            addMessagePartsToJBIWrapper(jbiWrapper, wsdlMsg, jmxMsgParts);
            
            DOMSource wrappedSource = jbiWrapper.toDOMSource();
            normMsg.setContent(wrappedSource);
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }
    
    public DOMSource denormalizeInput(Operation operation, NormalizedMessage normMsg)
            throws MessagingException {
        try {
            
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(normMsg.getContent()) );
            Input input = operation.getInput();
            Message wsdlMsg = input.getMessage();
            //TODO: get the binding input extension here if needed.
            
            Document jmxMsgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            Element jmxMessage = jmxMsgDoc.createElementNS(ASTERISKBC_MSG_NS, DEF_NS_PREFIX + ":" + MESSAGE_EL);
            jmxMessage.setAttributeNS(XMLNS_NS, "xmlns:"+DEF_NS_PREFIX, ASTERISKBC_MSG_NS);
            jmxMsgDoc.appendChild(jmxMessage);
            
            List<Element> jmxMsgParts = getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            for ( int i =0; i < jmxMsgParts.size(); ++i) {
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
    
    public DOMSource denormalizeOutput(Operation operation, NormalizedMessage normMsg)
            throws MessagingException {
        try {
            
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(normMsg.getContent()) );
            Output output = operation.getOutput();
            Message wsdlMsg = output.getMessage();
            //TODO: get the binding output extension here if needed.
            
            Document jmxMsgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            Element jmxMessage = jmxMsgDoc.createElementNS(ASTERISKBC_MSG_NS, DEF_NS_PREFIX + ":" + MESSAGE_EL);
            jmxMessage.setAttributeNS(XMLNS_NS, "xmlns:"+DEF_NS_PREFIX, ASTERISKBC_MSG_NS);
            
            List<Element> jmxMsgParts = getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            
            for ( int i =0; i < jmxMsgParts.size(); ++i) {
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
    
    public DOMSource denormalizeFault(Operation operation, String faultName, NormalizedMessage normMsg)
            throws MessagingException {
        try {
            WSDL11JBIWrapper jbiWrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(normMsg.getContent()) );
            Fault wsdlFault = operation.getFault(faultName);
            Message wsdlMsg = wsdlFault.getMessage();
            //TODO: get the binding fault extension here if needed.
            
            Document jmxMsgDoc = jbiWrapper.getDocumentBuilder().newDocument();
            Element jmxMessage = jmxMsgDoc.createElementNS(ASTERISKBC_MSG_NS, DEF_NS_PREFIX + ":" + MESSAGE_EL);
            jmxMessage.setAttributeNS(XMLNS_NS, "xmlns:"+DEF_NS_PREFIX, ASTERISKBC_MSG_NS);
            
            List<Element> jmxMsgParts = getMessagePartsFromJBIWrapper(jbiWrapper, wsdlMsg);
            
            for ( int i =0; i < jmxMsgParts.size(); ++i) {
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
