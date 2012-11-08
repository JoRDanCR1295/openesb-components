/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/


package it.imolinfo.jbi4ejb.processor;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.Jbi4EjbException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbEndpoint;
import it.imolinfo.jbi4ejb.jbi.xfire.EjbChannel;

import java.util.Iterator;

import javax.jbi.messaging.NormalizedMessage;
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
public class Jbi4EjbDenormalizer {

    /** The Constant LOG. */
    private static final Logger LOG
        = LoggerFactory.getLogger(Jbi4EjbDenormalizer.class);
    private static final Messages MESSAGES 
    = Messages.getMessages(Jbi4EjbDenormalizer.class);

    /** The wrapper parser. */
    private WrapperParser wrapperParser = null;
    
    /** The m trans. */
    private Transformer mTrans = null;         
    
    /**
     * Instantiates a new denormalizer.
     *             
     * @throws Jbi4EjbException
     *                       if some problem occurs in denormalizing 
     */
    public Jbi4EjbDenormalizer() throws Jbi4EjbException {
        
        try {
            wrapperParser = HelperFactory.createParser();
        } catch (WrapperProcessingException ex) {
        	String msg=MESSAGES.getString("EJB000601_Failed_to_create_WrapperParser");
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);   
        }
        
        try {        
            TransformerFactory factory = TransformerFactory.newInstance();
            mTrans = factory.newTransformer();            
        } catch (TransformerFactoryConfigurationError ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);
        } catch (TransformerConfigurationException e) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new Jbi4EjbException(msg,e);
        }                        
  
    }
    
    
    
    /**denormalize
     * Denormalize the message to the source. This implementation takes the
     * first (shuold be th only) part
     * 
     * @param normalizedMessage
     *            the message to denormalize
     * @param endpoint
     *            the endpoint invoked
     * @param operation
     *            the opration invoked
     * 
     * @return the <code>Jbi4EjbMessage containing the source.
     * 
     * @throws Jbi4EjbException
     *             if some problem occurs in message denormalizing
     */
    public Jbi4EjbMessage denormalize(NormalizedMessage normalizedMessage, 
            Jbi4EjbEndpoint endpoint,
            QName operation) throws Jbi4EjbException {

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
                    wsdlMessage = op.getInput().getMessage();
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
                    throw new Jbi4EjbException("Unable to find valid part during denormalization");
                }
                // return the first node
                return new Jbi4EjbMessage(new DOMSource(nodes.item(0)), true);
            } else {
                // Support if no parts are found
                return new Jbi4EjbMessage(new DOMSource((Document) node), false); 
            }           

        } catch (TransformerException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);
        }
        catch (WrapperProcessingException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);
        }
    }


}

