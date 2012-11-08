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
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeHelper;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.jbi.endpoint.ProviderEndpoint;
import it.imolinfo.jbi4corba.jbi.processor.transform.SourceTransformer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import net.java.hulp.measure.Probe;

import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.ExchangeImpl;
import org.apache.cxf.message.FaultMode;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.phase.PhaseInterceptorChain;
import org.apache.cxf.phase.PhaseManager;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.BindingInfo;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.service.model.ServiceInfo;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;


/**
 * Provider Exchange processor
 */
public class ProviderExchangeProcessor implements ExchangeProcessor {



    private static final TransformerFactory TRANSFORMER_FACTORY = TransformerFactory.newInstance();

    private static final Logger LOG = LoggerFactory.getLogger(ProviderExchangeProcessor.class);

    /** The endpoint */

    protected ProviderEndpoint endpoint;

    /** The SourceTransformer. */

    private SourceTransformer transformer;

    /** message denormalizer. */

    //private MessageDenormalizer messageDenormalizer;

    /** message normalizer. */

    //private MessageNormalizer messageNormalizer;

    /** Performance Measurement Probe Object **/

    private Probe mMeasurement = null;



    /**

     * The Exchange processor

     * @param endpoint  The endpoint

     */

    public ProviderExchangeProcessor(ProviderEndpoint endpoint) throws Jbi4CorbaException {

        this.endpoint = endpoint;

        this.transformer = new SourceTransformer();

        //messageDenormalizer = new MessageDenormalizer();

        //messageNormalizer = new MessageNormalizer();

    }



    /**

     * @param  exchange   The message exchange

     * @throws Exception  The exception

     */

    public void process(MessageExchange jbiExchange) {

        // IOR for dynamic exchanges
        Object IOR = jbiExchange.getProperty("EPR_IOR");

        endpoint.getEndpointStatus().incrementReceivedRequests();



        LOG.debug(">>>>> process - begin. MessageExchange=" + jbiExchange);



        try {

            // 

            Service cxfService = endpoint.getCXFService();

            Exchange cxfExchange = new ExchangeImpl();



            NormalizedMessage in = jbiExchange.getMessage("in");



            // Gets the JBI message                

            if (LOG.isDebugEnabled()) {

                String inMessage = "In message, before unwrapping: " + transformer.contentToString(in);

                LOG.debug(inMessage);

            }



            // Performance Measurement Chrono begins

            String topic = new String("Denormalization");

            mMeasurement = Probe.fine(getClass(), endpoint.getUniqueName(), topic);


            MessageDenormalizer messageDenormalizer = new MessageDenormalizer();
            JbiMessage inMsg = messageDenormalizer.denormalize(in, endpoint, jbiExchange.getOperation(), true, false);

            if (LOG.isDebugEnabled()) {

                String inMessage = "In message, after unwrapping: " + transformer.toString(inMsg.getMessageSource());

                LOG.debug(inMessage);

            }





            if (jbiExchange.getOperation() != null) {



                // Gets the operation exchange

                QName operation = jbiExchange.getOperation();
                
                
                //if the exchange is a Dynamic excchange
                if (IOR != null) {
                    cxfExchange.put("EPR_IOR", IOR);
                }

                //Add Measurement object to Context, as it will need to be stopped in a different class                

                cxfExchange.put("Measure-deN", mMeasurement);



                //Add endpoint name also.

                cxfExchange.put("EndpointName", endpoint.getUniqueName());



                // Invokes the CXF service        

                ByteArrayOutputStream cxfOut = invokeCXFOperation(cxfExchange, cxfService, operation, inMsg.getMessageSource(), isOneWay(jbiExchange));



                // Set response or DONE status

                if (isInAndOut(jbiExchange)) {



                    // If the fault mode is not null in the out message, a fault has been raised.

                    if (cxfExchange.getOutMessage().get(FaultMode.class) != null) {



                        // The Fault mode can be CHECKED_APPLICATION_FAULT/UNCHECKED_APPLICATION_FAULT

                        FaultMode faultMode = (FaultMode) cxfExchange.getOutMessage().get(FaultMode.class);

                        org.apache.cxf.interceptor.Fault cxfFault = (org.apache.cxf.interceptor.Fault) cxfExchange.getOutFaultMessage().getContent(Exception.class);



                        if (faultMode.equals(FaultMode.CHECKED_APPLICATION_FAULT)) {



                            // Gets the fault sorce                    

                            InputStream inputStream = new ByteArrayInputStream(cxfOut.toByteArray());

                            Source faultSource = transformer.toDOMSourceFromStream(new StreamSource(inputStream));



                            QName faultName = (QName) cxfExchange.getOutFaultMessage().getContent(QName.class);


                            LOG.info("CRB000801_Raised_checked_application_fault_with_code",
                                      new Object[]{faultName});



                            // Creates a JBI fault

                            Fault jbiFault = jbiExchange.createFault();

                            jbiFault.setContent(faultSource);



                            // Normalizes the message
                            MessageNormalizer messageNormalizer = new MessageNormalizer();
                            messageNormalizer.normalizeFault(faultSource, jbiFault, endpoint, jbiExchange.getOperation(), faultName.getLocalPart(), inMsg.isWrapped());



                            if (LOG.isDebugEnabled()) {

                                String inMessage = "Fault message, after wrapping: " + transformer.contentToString(jbiFault);

                                LOG.debug(inMessage);

                            }



                            // Sets the jbi fault
                            
                            jbiExchange.setFault(jbiFault);
                            
                            endpoint.getEndpointStatus().incrementSentReplies();

                        } else {

                            // Unchecked fault



                            
                            LOG.info("CRB000802_Raised_unchecked_application_fault_with_exception",
                                            new Object[]{cxfFault.getCause()});
                            



                            // Sets the cause. Notice that (from the specs):

                            // Used to specify the source of a failure status.

                            // Invoking this method automatically adjusts the status of the

                            // ME to ExchangeStatus.ERROR.

                            // So, the cause can be setted only if no Fault is setted. 

                            endpoint.getEndpointStatus().incrementSentErrors();

                            if (cxfFault.getCause() instanceof Exception) {

                                jbiExchange.setError((Exception) cxfFault.getCause());
                                
                            } else {

                                jbiExchange.setError(new Exception(cxfFault.getCause()));
                                
                            }
                            
                            // get value of faultdetail property
                            
                            OutputStream detail = new ByteArrayOutputStream();
                            PrintWriter writer = new PrintWriter(detail,true);
                            cxfFault.getCause().printStackTrace(writer);

                            // Set Message Exchange Properties

                            jbiExchange.setProperty("com.sun.jbi.crl.faultcode", 
                                                    "Server");
                            jbiExchange.setProperty("com.sun.jbi.crl.faultstring", 
                                                cxfFault.getCause().toString());                              
                            jbiExchange.setProperty("com.sun.jbi.crl.faultdetail", 
                                                     detail.toString());
                            jbiExchange.setProperty("com.sun.jbi.crl.faultactor", 
                                                    "jbi4corba");
                            LOG.debug(">>>>> Set Message Exchange Properties");
                        }



                    } else {



                        NormalizedMessage outMsg = jbiExchange.createMessage();



                        // Retrieve the Performance Measurement object from context

                        mMeasurement = (Probe) cxfExchange.get("Measure-N");

                        if (mMeasurement == null) {

                            // this prevents error when an exception is raised in a CXF interceptor

                            //Normalization timer starts

                            String topicn = new String("Normalization");

                            String endpointName = (String) endpoint.getUniqueName();

                            mMeasurement = Probe.fine(getClass(), endpointName, topicn);

                        }



                        // Gets the output sorce                    

                        InputStream inputStream = new ByteArrayInputStream(cxfOut.toByteArray());

                        Source outSource = transformer.toDOMSourceFromStream(new StreamSource(inputStream));



                        if (LOG.isDebugEnabled()) {

                            String inMessage = "Out message, before wrapping: " + transformer.toString(outSource);

                            LOG.debug(inMessage);

                        }



                        // Normalize the source and sets to the output message.
                        MessageNormalizer messageNormalizer = new MessageNormalizer();
                        messageNormalizer.normalize(outSource, outMsg, endpoint, jbiExchange.getOperation(), inMsg.isWrapped(), true);



                        //Normalization process complete, timer to be stopped now.

                        mMeasurement.end();



                        if (LOG.isDebugEnabled()) {

                            String inMessage = "Out message, after wrapping: " + transformer.contentToString(outMsg);

                            LOG.debug(inMessage);

                        }



                        jbiExchange.setMessage(outMsg, "out");

                        endpoint.getEndpointStatus().incrementSentReplies();

                    }



                } else {

                    LOG.debug("MessageExchange - InOnly");

                    jbiExchange.setStatus(ExchangeStatus.DONE);

                    endpoint.getEndpointStatus().incrementSentDones();

                }

            }

            LOG.debug("before - Channel.send");

            RuntimeHelper.getDeliveryChannel().send(jbiExchange);
            
            LOG.debug("after - Channel.send");

            //LOG.debug(">>>>> process - end. MessageExchange=" + jbiExchange);

        } catch (Exception ex) {

            LOG.error("CRB000800_Error_in_message_exchange", new Object[]{ex.getMessage()}, ex);

            // No exception is thrown...

            jbiExchange.setError(ex);

        }

    }



    /**

     * 

     * @param  source                The source

     * @return                       The return

     * @throws TransformerException  The transformer exception

     * @throws XMLStreamException    The XML stream exception

     */

    protected XMLStreamReader getXMLStreamReader(Source source) throws TransformerException, XMLStreamException {

        LOG.debug("Returning stream reader");

        return transformer.toXMLStreamReader(source);

    }



    /**

     * 

     * @param  source                        The source

     * @return                               The return

     * @throws TransformerException          The transformer exception

     * @throws ParserConfigurationException  The parser configuration exception

     * @throws IOException                   The IO exception

     * @throws SAXException                  The SAX exception

     */

    protected Document getXMLDom(Source source) throws TransformerException, ParserConfigurationException, IOException, SAXException {

        LOG.debug("Returning document from source");

        return transformer.toDOMDocument(source);

    }



    /**

     * 

     * @param exchange  The exchange

     * @return          The return

     */

    protected boolean isInAndOut(MessageExchange exchange) {

        return exchange instanceof InOut || exchange instanceof InOptionalOut;

    }



    /**

     * True if the exchange is InOnly.

     * @param exchange  The exchange

     * @return          True if the exchange is InOnly

     */

    protected boolean isOneWay(MessageExchange exchange) {

        return exchange instanceof InOnly;

    }



    private static InputStream convertMessageToInputStream(Source src) throws IOException,

            TransformerConfigurationException, TransformerException {



        final Transformer transformer = TRANSFORMER_FACTORY.newTransformer();



        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        StreamResult result = new StreamResult(baos);

        transformer.transform(src, result);



        return new ByteArrayInputStream(baos.toByteArray());

    }



    /**

     * Inits the CXF exchange ane invokes the operation on the CXF service. 

     * The service must have registered a invoker.

     * Returns null for the one-way operations

     * 

     * @param service

     * @param operation

     * @param inSource

     * @return the message OutputStream (in both cases, correct out and Fault)

     * @throws IOException

     * @throws TransformerConfigurationException

     * @throws TransformerException

     */

    private ByteArrayOutputStream invokeCXFOperation(Exchange cxfExchange, 

                                                     Service service, QName operation, Source inSource, boolean oneWay) throws IOException, TransformerConfigurationException, TransformerException {

        

        // Gets the CXF Endpoint

        EndpointInfo ei = CXFUtils.getEndpointInfo(service);
        LOG.debug(">>>Interface Endpoint"+ei.getInterface().getName().getLocalPart());
        Endpoint ep = CXFUtils.getEndpoint(service, ei);
                   
        ServiceInfo si = service.getServiceInfos().get(0);
        
        BindingInfo bi = (BindingInfo) si.getBindings().iterator().next();
       
        if (operation.getNamespaceURI() == null || operation.getNamespaceURI().length() == 0){
          String namespacePortType = bi.getInterface().getName().getNamespaceURI();
          LOG.debug(">>>>> NameSpace PortType: "+namespacePortType);        
          operation = new QName(namespacePortType, operation.getLocalPart());
        }

        LOG.debug(">>>>>> Operation "+operation.toString());
        BindingOperationInfo bio = bi.getOperation(operation);    

        if (bio == null){
          LOG.error(">>>>> BindingOperationInfo is  NULL");
        }

        String parameterStyle = CXFUtils.getParameterStyle(bio);
        String bindingStyle = CXFUtils.getBindingStyle(bio);


        // Inits the CXF Exchange

        Message cxfMessage = new MessageImpl();

        cxfExchange.setOneWay(oneWay);

        cxfExchange.put(BindingOperationInfo.class, bio);

        cxfExchange.put(Service.class, service);

        cxfExchange.put(Endpoint.class, ep);

        cxfMessage.setExchange(cxfExchange);

        cxfExchange.setInMessage(cxfMessage);

        cxfExchange.setOutMessage(new MessageImpl());

        cxfExchange.setOutFaultMessage(new MessageImpl());        



        // Sets the Source as In message

        cxfMessage.setContent(InputStream.class, convertMessageToInputStream(inSource));
        
        // Sets the interceptor chain on the message
        PhaseInterceptorChain inInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getInPhases());                
        CXFUtils.populateInInterceptorsForProvider(inInterceptorChain, parameterStyle, bindingStyle);       
        cxfMessage.setInterceptorChain(inInterceptorChain);
        inInterceptorChain.doIntercept(cxfMessage);

        // Tests if Fault are present

        org.apache.cxf.interceptor.Fault fault = (org.apache.cxf.interceptor.Fault) cxfExchange.getOutFaultMessage().getContent(Exception.class);

        ByteArrayOutputStream out = new ByteArrayOutputStream();


        if (fault != null) {



            FaultMode faultMode = (FaultMode) cxfExchange.getOutMessage().get(FaultMode.class);



            boolean isCheckedApplication = false;

            if (faultMode.equals(FaultMode.CHECKED_APPLICATION_FAULT)) {

                isCheckedApplication = true;

            } else {

                isCheckedApplication = false;

            }



            LOG.info("CRB000803_Fault_raised");



            // Fault

            // Gets the generated fault from the message

            org.apache.cxf.interceptor.Fault outFault = (org.apache.cxf.interceptor.Fault) cxfMessage.getContent(Exception.class);



            // The FaultOutInterceptor creates adnd fills the XML details from the cause Exception 

            PhaseInterceptorChain faultInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getOutPhases());
            CXFUtils.populateFaultInterceptors(faultInterceptorChain, parameterStyle, bindingStyle);

            // Call the interceptor chain                

            faultInterceptorChain.doIntercept(cxfMessage);

            // Gets the details element as Element...it's not very optimized...

            if (isCheckedApplication) {

                Element detail = outFault.getDetail();
                if (detail != null) {

                    Node exceptionDoc = detail.getFirstChild();

                    // Gets the fault QName and sets it on the OutFaultMessage.
                    QName faultName = new QName(exceptionDoc.getNamespaceURI(), exceptionDoc.getLocalName());
                    cxfExchange.getOutFaultMessage().setContent(QName.class, faultName);

                    // Converts the Element to a StreamSource

                    StreamResult result = new StreamResult(out);
                    transformer.toResult(new DOMSource(exceptionDoc), result);

                }

                if (LOG.isDebugEnabled()) {

                    //  Converts to String to assert                
                    LOG.debug("Fault message: " + out.toString());

                }
            } else {
                LOG.debug("Uncheked fault, returning empty ByteArrayOutputStream");
            }

        } else {

            // If one way, return null

            if (oneWay) {
                return null;
            }
            Message outCXFMessage = cxfExchange.getOutMessage();

            /** 

             *Sets the interceptor chain

             */

            PhaseInterceptorChain outInterceptorChain = new PhaseInterceptorChain(CXFUtils.getBus().getExtension(PhaseManager.class).getOutPhases());
            CXFUtils.populateOutInterceptors(outInterceptorChain, parameterStyle, bindingStyle, true);

            outCXFMessage.setInterceptorChain(outInterceptorChain);
            outCXFMessage.setContent(OutputStream.class, out);

            // Call the interceptor chain
            outInterceptorChain.doIntercept(outCXFMessage);
        }

        return out;

    }

}


