/*
 * @(#)Test4RulesComponent.java        $Revision: 1.4 $ $Date: 2009/01/27 21:42:47 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.test4rules;

import java.io.StringWriter;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.logging.LoggerFactory;
import org.openesb.components.rules4jbi.test4rules.domain.Customer;
import org.openesb.components.rules4jbi.test4rules.domain.Invoice;
import org.openesb.components.rules4jbi.engine.util.DOMUtils;
import org.openesb.components.rules4jbi.engine.wsdl.JBIWrapper;
import org.openesb.components.rules4jbi.engine.component.Serializer;

/**
 * Test component used for functional testing of the rules service engine.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2009/01/27 21:42:47 $
 * 
 * @since 0.1
 */
public class Test4RulesComponent implements Component, ComponentLifeCycle {

    private Logger logger = null;
    
    private ComponentContext componentContext = null;
    
    private DeliveryChannel deliveryChannel = null;
    
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }
    
    public ObjectName getExtensionMBeanName() {
        logger.entering(this.getClass(), "getExtensionMBeanName");
        
        return null;
    }
    
    public void init(ComponentContext componentContext) throws JBIException {
        if (componentContext == null) {
            throw new JBIException("Null component context received during bootstrap");
        }

        this.componentContext = componentContext;
        LoggerFactory.getInstance().init(Logger.TEST_PREFIX, componentContext);
        logger = LoggerFactory.getInstance().getLogger(this.getClass());

        logger.entering(this.getClass(), "init");

        logger.info("Component name: %s", componentContext.getComponentName());

        deliveryChannel = this.componentContext.getDeliveryChannel();

        logger.exiting(this.getClass(), "init");
    }

    private ScheduledExecutorService scheduledExecutorService = Executors.newScheduledThreadPool(10);
    
    public void start() throws JBIException {
        logger.entering(this.getClass(), "start");

//        final Thread taskThread = Thread.currentThread();
//        
//        scheduledExecutorService.schedule(new Runnable() {
//            public void run() { 
//                try {
//                    logger.fine("Interrupting task thread");
//                    taskThread.interrupt();
////                    deliveryChannel.close();
//                    logger.fine("Interruption successful");
//                } catch (/*Messaging*/Exception e) {
//                    logger.severe("Exception while interupting", e);
//                }
//            }
//        }, 10, TimeUnit.SECONDS);
//        
//        
//        try {
//            logger.fine("Waiting to accept a message");
//            MessageExchange me = deliveryChannel.accept();
//
//            if (me == null) {
//                logger.fine("No message received");
//            } else {
//                logger.fine("Received a message!");
//            }
//        } catch (MessagingException e) {
//            Throwable cause = e.getCause();
//            
//            if (cause != null && cause instanceof InterruptedException) {
//                logger.fine("The accept() method was interrupted");
//                
//            } else {
//                throw new RuntimeException("Unknown messaging exception occured", e);
//            }
//        }
//        
//        logger.fine("Waiting to accept a message again, now for 6 seconds");
//        
//        //re-opening the delivery channel
//        //deliveryChannel = componentContext.getDeliveryChannel();
//        MessageExchange me = deliveryChannel.accept(6000);
//        if (me == null) {
//            logger.fine("No message received");
//        } else {
//            logger.fine("Received a message!");
//        }

        

        ServiceEndpoint[] endpoints = componentContext.getEndpoints(null);

        if (endpoints.length == 0) {
            logger.severe("No registered endpoints found");
            
            return;
            
        } else {
            logger.info("Found %d endpoints", endpoints.length);
        }
        
        logger.info("--- Begin Registered Endpoints ---");
        for (ServiceEndpoint serviceEndpoint : endpoints) {
            logEndpoint(serviceEndpoint);
        }
        logger.info("--- End Registered Endpoints ---");
        
        MessageExchangeFactory messageExchangeFactory = null;
        for (ServiceEndpoint serviceEndpoint : endpoints) {
            if (serviceEndpoint.getEndpointName().endsWith("ServiceProvider")) {
                messageExchangeFactory = deliveryChannel.createExchangeFactory(serviceEndpoint);
                
                break;
            }
        }
        
        if (messageExchangeFactory == null) {
            logger.severe("Could not find rules4jbi service provider to test");
            
            return;
        }

        InOut inOutExchange = messageExchangeFactory.createInOutExchange();

        NormalizedMessage normalizedMessage = inOutExchange.createMessage();
        
        
        normalizedMessage.setContent(createWrapper());
        
//        normalizedMessage.setContent(createContent());
        
        
        
        
        
        inOutExchange.setInMessage(normalizedMessage);

        boolean processed  = deliveryChannel.sendSync(inOutExchange, 6000);
        
        logger.fine("Sending message %s", processed ? "succeeded" : "failed");


        if (processed) {

            NormalizedMessage outMessage = inOutExchange.getOutMessage();

            Source output = outMessage.getContent();

            logger.fine("Output message is null? %B", output == null);

            if (output instanceof DOMSource) {
                DOMSource domOutput = (DOMSource) output;

                logger.fine("Output: %s", DOMUtils.domSourceToString(domOutput, false));
            }


            inOutExchange.setStatus(ExchangeStatus.DONE);

            deliveryChannel.send(inOutExchange);

            logger.fine("Sending DONE message successfull");
        }

    }

    private Source createWrapper() {
//        String customerXML = "<ns1:customer xmlns:ns1='http://www.example.org/data'>"
//                + " <ns1:name>Joe User</ns1:name><ns1:creditLimit>500</ns1:creditLimit>"
//                + "</ns1:customer>";
//        
//        Element customer = XOMUtils.toElement(customerXML);
        
        Customer customer = new Customer();
        customer.setName("Johnny Bravo");
        customer.setCreditLimit(500);

        Invoice invoice = new Invoice();
        invoice.setAmount(300);
        invoice.setStatus("unpaid");
        invoice.setDescription("the invoice");
        
        Serializer serializer = new Serializer(new Class<?>[] {Customer.class, Invoice.class});
        
        JBIWrapper inputDataWrapper = new JBIWrapper(JBIWrapper.Type.INPUT, "http://www.abc.com/rules");
        inputDataWrapper.addBusinessObject(serializer.serialize(customer));
        inputDataWrapper.addBusinessObject(serializer.serialize(invoice));
        
        return inputDataWrapper.toDOMSource();
    }
    
    //TODO: replace with the JBIUtils version of logEndpoint, while simultaneously improving JBIUtils version
    private void logEndpoint(ServiceEndpoint serviceEndpoint) {
        logger.fine("--- Endpoint Start ---");

        try {
            Document endpointDescriptor = componentContext.getEndpointDescriptor(serviceEndpoint);
            
            if (endpointDescriptor != null) {
//                logger.fine("Endpoint descriptor: %s", XMLUtils.documentToString(endpointDescriptor));
                  logger.fine("Endpoint descriptor found");
                    
            } else {
                logger.fine("No endpoint descriptor found.");
            }
        
        } catch (Exception e) {
            logger.warning("Could not retrieve endpoint descriptor");
        }
        
        QName[] interfaces = serviceEndpoint.getInterfaces();
        if (interfaces != null) {
            for (QName anInterface : interfaces) {
                logger.fine("Interface: %s", anInterface);
            }

        } else {
            logger.fine("Interfaces is null");
        }

        logger.fine("Service name: %s", serviceEndpoint.getServiceName());
        logger.fine("Endpoint name: %s", serviceEndpoint.getEndpointName());
        
//        QName operationName = new QName(EchoServiceDescriptor.SERVICE_NS, EchoServiceDescriptor.OPERATION_NAME);
        DocumentFragment reference = serviceEndpoint.getAsReference(/* operationName */ null);

        
        String xml = extractXml(new DOMSource(reference));
        logger.fine("serviceEndpoint.getAsReference(): %s", xml);
        
        
        logger.fine("Trying to resolve endpoint reference");
        ServiceEndpoint endpoint = componentContext.resolveEndpointReference(reference);

        
        if (endpoint != null) {
            logger.fine("Service name: %s", endpoint.getServiceName());
            logger.fine("Endpoint name: %s", endpoint.getEndpointName());

        } else {
            logger.fine("Resolved endpoint is null");
        }
        
        logger.fine("--- Endpoint End ---");
        
        
//        if (reference != null) {
//            logger.fine("Reference is NOT null");
//
//            logger.config("Node name: %s", reference.getNodeName());  //#document-fragment
//            logger.config("Node value: %s", reference.getNodeValue()); // null
//            logger.config("Base URI: %s", reference.getBaseURI());   // null
//            logger.config("First child is null: %b", reference.getFirstChild() == null); // false
//
//            Node firstChild = reference.getFirstChild();
//            logger.config("First child: %s", firstChild.getNodeName()); // jbi:end-point-reference
//            
//            logger.config("First child's child is null: %b", firstChild.getFirstChild() == null); // true
//
//            logger.config("First child's value: %s", firstChild.getTextContent()); // ""
//
//        } else {
//            logger.fine("Reference is null");
//        }
        
        
    }
    
    private String extractXml(DOMSource domSource) {
        StringWriter writer = new StringWriter();

        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer trans = null;
        try {
            trans = tFactory.newTransformer();
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            StreamResult result = new StreamResult(writer);
            trans.transform(domSource, result);
        } catch (TransformerConfigurationException e) {
            e.printStackTrace();
        } catch (TransformerException e) {
            e.printStackTrace();
        }

        return writer.getBuffer().toString();
    }
    
    public void stop() throws JBIException {
        logger.entering(this.getClass(), "stop");
    }
    
    public void shutDown() throws JBIException {
        logger.entering(this.getClass(), "shutDown");
    }

    public Document getServiceDescription(ServiceEndpoint arg0) {
        logger.entering(this.getClass(), "getServiceDescription");
        
        return null;
    }

    public ServiceUnitManager getServiceUnitManager() {
        logger.entering(this.getClass(), "getServiceUnitManager");
        
        return null;
    }

    public boolean isExchangeWithConsumerOkay(ServiceEndpoint arg0, MessageExchange arg1) {
        logger.entering(this.getClass(), "isExchangeWithConsumerOkay");
        
        return true;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint arg0, MessageExchange arg1) {
        logger.entering(this.getClass(), "isExchangeWithProviderOkay");
        
        return true;
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
        logger.entering(this.getClass(), "resolveEndpointReference");
        
        return null;
    }
}
