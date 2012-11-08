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

import java.util.Iterator;
import java.util.Map;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Operation;
import javax.wsdl.Part;
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

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;


/**
 * Jbi4Ejb normalizer class (convert Source to nmr message).
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbNormalizer {
    
    /** The Constant LOG. */
    private static final Logger LOG
        = LoggerFactory.getLogger(Jbi4EjbNormalizer.class);
    private static final Messages MESSAGES 
    = Messages.getMessages(Jbi4EjbNormalizer.class);

    /** The m trans. */
    private Transformer mTrans = null;
    
    /** The wrapper builder. */
    private WrapperBuilder wrapperBuilder = null;
    
    
    /**
     * Instantiates a new jbi4 ejb normalizer.
     * 
     * @throws Jbi4EjbException if some error occurs in the normalizer creation
     */
    public Jbi4EjbNormalizer() throws Jbi4EjbException {
                
        try {
            wrapperBuilder = HelperFactory.createBuilder();        
        } catch (WrapperProcessingException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
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
        
    /**
     * Normalize the message.
     * 
     * @param xmlSource
     *            the xmlSource to normalize
     * @param normalizedMsg
     *            the normlized message
     * @param endpoint
     *            the invoked endpoint
     * @param operation
     *            the invoked operation
     * @param toWrap
     *              if the message must be wrapped
     * @throws Jbi4EjbException
     *             if some problem occurs in normalization
     */
    public void normalize(Source xmlSource,
            NormalizedMessage normalizedMsg,
            Jbi4EjbEndpoint endpoint,
            QName operation, boolean toWrap) throws Jbi4EjbException {

        try {

            Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
            Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
            PortType portType = port.getBinding().getPortType();

            // Grab the operation that matches the operationName.  There actually may
            // be more than one operation with the same name (but different input/output)
            // names.  We need to fix this so that we uniquely identify which operation we're
            // going after
            Iterator it = portType.getOperations().iterator();
            javax.wsdl.Message wsdlMessage = null;
            while (it.hasNext()) {
                Operation op = (Operation)it.next();
                if (op.getName().equals(operation.toString()) ||
                        op.getName().equals(operation.getLocalPart())) {                    
                    // Its' always an outputisWrapped
                    wsdlMessage = op.getOutput().getMessage();
                }
            }
            
            wrapperBuilder.initialize(null,
                    wsdlMessage,
                    null);

            if (LOG.isDebugEnabled()) {                           
                LOG.debug("WSDL Message: " + wsdlMessage);
                LOG.debug("WSDL Message Parts: " + wsdlMessage.getParts());
            }

            // Take ALWAYS the first (should be the only) part
            if (wsdlMessage.getParts().values().size() == 0) {
            	String msg=MESSAGES.getString("EJB000603_No_message_parts_found");
                LOG.error(msg);
                throw new Jbi4EjbException(msg);
            } else if (wsdlMessage.getParts().values().size() > 1) {
            	LOG.warn("EJB000604_More_than_one message_part_found_using_the_first");
            }
            
            Part part = (Part)wsdlMessage.getParts().values().iterator().next();        

            // String[] partNames = wsdlMessage.getParts().values().iterator().next(); 
            String partName = part.getName();

            Node node = null;
            if (xmlSource instanceof DOMSource) {
                // saves a transformation
                node = ((DOMSource) xmlSource).getNode();
            } else {
                DOMResult domResult = new DOMResult();
                mTrans.transform(xmlSource, domResult);
                node = domResult.getNode();                
            }
            
            DOMSource domSource = null;
            // If do not need wrapping, return the document
            if (!toWrap) {
                domSource =  new DOMSource(node);
            } else {
                // needs wrapping
                if (node instanceof Document) {
                    wrapperBuilder.addPart(partName, ((Document) node).getDocumentElement());
                } else if (node instanceof Element) {
                    wrapperBuilder.addPart(partName, (Element) node);
                } else {
                	String msg=MESSAGES.getString("EJB000605_Invalid_result_from_XML_transformation", new Object[]{node.getClass()});
                    LOG.error(msg);
                    throw new Jbi4EjbException(msg);
                }
                Document doc = wrapperBuilder.getResult();            
                domSource = new DOMSource(doc);
            }

            normalizedMsg.setContent(domSource);
        } catch (WrapperProcessingException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);
        } catch (TransformerException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);           
        } catch (MessagingException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);                         
        }
    }
    

    /**
     * Normalize a fault message.
     * 
     * @param xmlSource
     *            The xml fault source
     * @param fault
     *            The Fault
     * @param endpoint
     *            The jbi4ejb endpoint
     * @param operation
     *            The operation
     * @param faultName
     *            The fault name
     * @param toWrap
     *            If the fault must be wrapped
     * @throws Jbi4EjbException
     *             If some problem occurs
     */
    public void normalizeFault(Source xmlSource,
            Fault fault,
            Jbi4EjbEndpoint endpoint,
            QName operation, String faultName, boolean toWrap) throws Jbi4EjbException {
        try {

            Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
            Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
            PortType portType = port.getBinding().getPortType();

            // Grab the operation that matches the operationName.  There actually may
            // be more than one operation with the same name (but different input/output)
            // names.  We need to fix this so that we uniquely identify which operation we're
            // going after
            Iterator it = portType.getOperations().iterator();
            javax.wsdl.Message wsdlFault = null;

            while (it.hasNext()) {
                Operation op = (Operation)it.next();
                LOG.debug("Looking for operation: " + op.getName());
                if (op.getName().equals(operation.toString()) ||
                        op.getName().equals(operation.getLocalPart())) {                    
                    // Its' always an output
                    Map faults = op.getFaults();
                    Iterator faultIt = faults.values().iterator();                 
                    while (faultIt.hasNext()) {                        
                        javax.wsdl.Fault wsdlFaultTmp = (javax.wsdl.Fault) faultIt.next();
                        LOG.debug("Looking for fault: " + faultName + ", found fault: " + wsdlFaultTmp.getName());
                        if (wsdlFaultTmp.getName().equals(faultName)) {
                            wsdlFault = wsdlFaultTmp.getMessage();
                        }
                        
                    }
                }
            }

            wrapperBuilder.initialize(null,
                    wsdlFault,
                    null);

            if (LOG.isDebugEnabled()) {                           
                LOG.debug("WSDL Fault: " + wsdlFault);
                LOG.debug("WSDL Fault Parts: " + wsdlFault.getParts());
            }

            // Take ALWAYS the first (should be the only) part
            if (wsdlFault.getParts().values().size() == 0) {
            	String msg=MESSAGES.getString("EJB000603_No_message_parts_found");
                LOG.error(msg);
                throw new Jbi4EjbException(msg);
            } else if (wsdlFault.getParts().values().size() > 1) {
            	LOG.warn("EJB000604_More_than_one message_part_found_using_the_first");
            }
            
            Part part = (Part)wsdlFault.getParts().values().iterator().next();        

            // String[] partNames = wsdlMessage.getParts().values().iterator().next(); 
            String partName = part.getName();

            Node node = null;
            if (xmlSource instanceof DOMSource) {
                // saves a transformation
                node = ((DOMSource) xmlSource).getNode();
            } else {
                DOMResult domResult = new DOMResult();
                mTrans.transform(xmlSource, domResult);
                node = domResult.getNode();
            }

            DOMSource domSource = null;
            
            if (!toWrap) {
                // If do not need wrapping, return the document
                domSource =  new DOMSource(node);
            } else {
                // needs wrapping
                if (node instanceof Document) {
                    wrapperBuilder.addPart(partName, ((Document) node).getDocumentElement());
                } else if (node instanceof Element) {
                    wrapperBuilder.addPart(partName, (Element) node);
                } else {
                	String msg=MESSAGES.getString("EJB000605_Invalid_result_from_XML_transformation", new Object[]{node.getClass()});
                    LOG.error(msg);
                    throw new Jbi4EjbException(msg);
                }

                Document doc = wrapperBuilder.getResult();
                domSource = new DOMSource(doc);
            }
            
            fault.setContent(domSource);
        } catch (WrapperProcessingException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);
        } catch (TransformerException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);          
        } catch (MessagingException ex) {
        	String msg=MESSAGES.getString("EJB000602_Jbi4Ejb_processor", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new Jbi4EjbException(msg,ex);                        
        }
    }
    
           
}

