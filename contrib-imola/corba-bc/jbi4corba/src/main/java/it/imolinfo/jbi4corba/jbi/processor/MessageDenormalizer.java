 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.jbi.processor;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaEndpoint;

import java.util.Iterator;

import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

/**
 *  Message denormalizer class. Extract from the NRM message the message part.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
@SuppressWarnings("unchecked")
public class MessageDenormalizer {

    /** The Constant LOG. */
    private static final Logger LOG
        = LoggerFactory.getLogger(MessageDenormalizer.class);

    /** The wrapper parser. */
    private WrapperParser wrapperParser = null;
    
    /** The m trans. */
    private Transformer mTrans = null;   
    
	/**
	 * The responsible to translate localized messages.
	 */
	private static final Messages MESSAGES = Messages
			.getMessages(MessageDenormalizer.class);
    
    /**
     * Instantiates a new denormalizer.
     *             
     * @throws Jbi4corbaException
     *                       if some problem occurs in denormalizing 
     */
    public MessageDenormalizer() throws Jbi4CorbaException {
        
        try {
            wrapperParser = HelperFactory.createParser();
        } catch (WrapperProcessingException ex) {
            throw new Jbi4CorbaException("Failed to create WrapperParser", ex);
        }
        
        try {        
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();            
        } catch (TransformerFactoryConfigurationError ex) {
			String msg = MESSAGES.getString("CRB000805_Unable_to_instantiate_denormalizer");
			LOG.error(msg, ex.getMessage());
            throw new Jbi4CorbaException(ex);
        } catch (TransformerConfigurationException e) {
			String msg = MESSAGES.getString("CRB000805_Unable_to_instantiate_denormalizer");
			LOG.error(msg, e.getMessage());
            throw new Jbi4CorbaException(e);
        }                        
  
    }
    
        
    /**
     * Denormalize the message to the source. This implementation takes the
     * first (should be the only) part
     * 
     * @param normalizedMessage the message to denormalize
     * @param endpoint the endpoint invoked
     * @param operation the opration invoked
     * @param isInput if it's an input message
     * @param isFault if it's a fault message
     * 
     * @return the <code>Jbi4corbaMessage containing the source.
     * 
     * @throws Jbi4CorbaException if some problem occurs in message denormalizing
     */    
	public JbiMessage denormalize(NormalizedMessage normalizedMessage, 
            Jbi4CorbaEndpoint endpoint,
            QName operation, boolean isInput, boolean isFault) throws Jbi4CorbaException {

        try {
            // Gets the WSDL data
            final Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
            final Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
            final PortType portType = port.getBinding().getPortType();

            // Grab the operation that matches the operationName.  There actually may
            // be more than one operation with the same name (but different input/output)
            // names.  We need to fix this so that we uniquely identify which operation we're
            // going after
            final Iterator it = portType.getOperations().iterator();
            Message wsdlMessage = null;
            while (it.hasNext()) {
                final Operation op = (Operation)it.next();
                if (op.getName().equals(operation.toString()) ||
                        op.getName().equals(operation.getLocalPart())) {
                    if (!isFault) {
                        if (isInput) {
                            wsdlMessage = op.getInput().getMessage();
                        } else {
                            wsdlMessage = op.getOutput().getMessage();
                        }
                    } 
                }
            }

            // Convert the normalizedMessage into a DOM object
            final DOMResult result = new DOMResult();
            final Source src = normalizedMessage.getContent();
            if (src != null) {
                final TransformerFactory fact = TransformerFactory.newInstance();
                final Transformer transformer = fact.newTransformer();
                transformer.transform( src, result );
            }
            Node node = result.getNode();
            Document normalizedDoc = null;
            if (node instanceof Document) {
                normalizedDoc = (Document) node;
            } else {
                normalizedDoc = ((Element) node).getOwnerDocument();
            }

            // Use the WrapperParser to help in parsing out the Parts
            if (LOG.isDebugEnabled()){
            LOG.debug("About to denormalize: "+normalizedDoc +" for endpoint: "+endpoint+" and definition: "+endpoint.getDefinition());
            }
            wrapperParser.parse(normalizedDoc, endpoint.getDefinition());
                        
            // use helper class to parse wrapped msg

            Source source = normalizedMessage.getContent();

            if (source instanceof DOMSource) {
                // saves a transformation
                node = ((DOMSource) source).getNode();
            } else {
                DOMResult domResult = new DOMResult();
                mTrans.transform(source, domResult);
                node = domResult.getNode();
            }
            
            JbiMessage jbiMessage = null;
            // Gets the JbiMessage (if input or output)
            if (!isFault) {
                jbiMessage = denormalizeNode(node, wsdlMessage);
            } else {
                // Gets the wrapped exception message from the definition
                // TODO: Verify if this is possible even for the  input/output messages. 
                jbiMessage = denormalizeFaultNode(node, endpoint.getDefinition());
            }
            
            return jbiMessage;                    

        } catch (TransformerException ex) {
			String msg = MESSAGES.getString("CRB000806_Error_in_denormalizing_the_message");
			LOG.error(msg, ex.getMessage());
            throw new Jbi4CorbaException(ex);
        }
        catch (WrapperProcessingException ex) {
			String msg = MESSAGES.getString("CRB000806_Error_in_denormalizing_the_message");
			LOG.error(msg, ex.getMessage());
            throw new Jbi4CorbaException(ex);
        }
    }
    
    /**
     * Denormalizes the message (DOM version). This implementation takes the
     * first (should be the only) part
     * 
     * @param endpoint the endpoint invoked
     * @param node the node
     * @param operation
     * @param isInput if it's an input message
     * 
     * @return the <code>Jbi4corbaMessage containing the source.
     * 
     * @throws Jbi4CorbaException if some problem occurs in message denormalizing
     */
	public JbiMessage denormalize(Node node, 
                                  Jbi4CorbaEndpoint endpoint,
                                  QName operation, boolean isInput) throws Jbi4CorbaException {

        // Gets the WSDL data
        final Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
        final Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        final PortType portType = port.getBinding().getPortType();

        // Grab the operation that matches the operationName.  There actually may
        // be more than one operation with the same name (but different input/output)
        // names.  We need to fix this so that we uniquely identify which operation we're
        // going after
        final Iterator it = portType.getOperations().iterator();
        Message wsdlMessage = null;
        while (it.hasNext()) {
            final Operation op = (Operation)it.next();
            if (op.getName().equals(operation.toString()) ||
                op.getName().equals(operation.getLocalPart())) {
                if (isInput) {
                    wsdlMessage = op.getInput().getMessage();
                } else {
                    wsdlMessage = op.getOutput().getMessage();
                }
            }
        }
        // Gets the JbiMessage
        JbiMessage jbiMessage = denormalizeNode(node, wsdlMessage);
        return jbiMessage;                    
    }    
    
    /**
     * Denormalizes the message (DOM version). 
     * This implementation takes the first (should be the only) part
     * 
     * @param endpoint the endpoint invoked
     * @param node the node
     * @param wsdlMessage the message
     * 
     * @return the <code>Jbi4corbaMessage containing the source.
     * 
     * @throws Jbi4corbaException if some problem occurs in message denormalizing
     * @throws Jbi4CorbaException
     */
    private JbiMessage denormalizeNode(Node node, Message wsdlMessage) throws Jbi4CorbaException {

        try {           
            if (node instanceof Document) {
                wrapperParser.parse((Document) node, wsdlMessage);
            } else {
                wrapperParser.parse(node.getOwnerDocument(), wsdlMessage);
            }

            if (wrapperParser.getNoOfParts() != 0) {
            // Take ALWAYS the first part
                String[] partNames = wrapperParser.getPartNames(); 
                NodeList nodes = wrapperParser.getPartNodes(partNames[0]);

                if (nodes == null || nodes.getLength() == 0) {
                    throw new Jbi4CorbaException("Unable to find valid part during denormalization");
                }
                // return the first node
                return new JbiMessage(new DOMSource(nodes.item(0)), true);
            } else {
                // Support if no parts are found
                return new JbiMessage(new DOMSource((Document) node), false); 
            }           

        } catch (WrapperProcessingException ex) {
			String msg = MESSAGES.getString("CRB000806_Error_in_denormalizing_the_message");
			LOG.error(msg, ex.getMessage());
            throw new Jbi4CorbaException(ex);
        }
    }    
    
    /**
     * Denormalizes the message (DOM version), trying the wsdl messages decription passed.
     * This implementation takes the first (should be the only) part
     * 
     * @param endpoint the endpoint invoked
     * @param node the node
     * @param wsdlMessage the message
     * 
     * @return the <code>Jbi4corbaMessage containing the source.
     * 
     * @throws Jbi4corbaException if some problem occurs in message denormalizing
     * @throws Jbi4CorbaException
     */
    private JbiMessage denormalizeFaultNode(Node node, Definition wsdlDef) throws Jbi4CorbaException {

        try {           
            if (node instanceof Document) {
                wrapperParser.parse((Document) node, wsdlDef);
            } else {
                wrapperParser.parse(node.getOwnerDocument(), wsdlDef);
            }
            if (wrapperParser.getNoOfParts() != 0) {
                // Message fault found.
                // Take ALWAYS the first part
                String[] partNames = wrapperParser.getPartNames(); 
                NodeList nodes = wrapperParser.getPartNodes(partNames[0]);                   
                if ((nodes != null) && (nodes.getLength() != 0)) {
                    return new JbiMessage(new DOMSource(nodes.item(0)), true);
                }                    
            }    

        } catch (WrapperProcessingException ex) {
			String msg = MESSAGES.getString("CRB000806_Error_in_denormalizing_the_message");
			LOG.error(msg, ex.getMessage());
            throw new Jbi4CorbaException(ex);
        }

        // If no wrap is found, returns the node
        return new JbiMessage(new DOMSource((Document) node), false);
    }    


}

