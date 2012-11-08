/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaRuntimeException;
import it.imolinfo.jbi4corba.exception.ServiceCreationException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaConsumerExceptionInterceptor;
import it.imolinfo.jbi4corba.jbi.endpoint.ConsumerEndpoint;
import it.imolinfo.jbi4corba.jbi.processor.JbiMessage;
import it.imolinfo.jbi4corba.jbi.processor.MessageDenormalizer;
import it.imolinfo.jbi4corba.jbi.processor.MessageNormalizer;
import it.imolinfo.jbi4corba.jbi.processor.transform.SourceTransformer;
import it.imolinfo.jbi4corba.jbi.processor.transform.StringSource;
import it.imolinfo.jbi4corba.webservice.descriptor.ConsumerServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.Param;
import it.imolinfo.jbi4corba.webservice.generator.WsdlInformation;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.ws.Holder;

import net.java.hulp.measure.Probe;

import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.frontend.MethodDispatcher;
import org.apache.cxf.frontend.SimpleMethodDispatcher;
import org.apache.cxf.helpers.DOMUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.StaxOutInterceptor;
import org.apache.cxf.jaxws.JAXWSMethodDispatcher;
import org.apache.cxf.jaxws.JaxWsClientFactoryBean;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.ExchangeImpl;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.phase.PhaseInterceptorChain;
import org.apache.cxf.phase.PhaseManager;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.service.model.OperationInfo;
import org.apache.cxf.staxutils.FragmentStreamReader;
import org.apache.cxf.staxutils.StaxUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import java.lang.reflect.Array;
import java.lang.reflect.Field;

/**
 * This class is used at runtime to create the MessageExchange and sending the
 * message on the bus.
 * 
 */
@SuppressWarnings("unchecked")
public class ConsumerInvocationHandler implements InvocationHandler {

    /** The logger for this class and its instances. */
    private static final transient Logger LOG = LoggerFactory.getLogger(ConsumerInvocationHandler.class);
    /** A kind of MEP. */
    public static final URI IN_ONLY = URI.create("http://www.w3.org/2004/08/wsdl/in-only");
    /** A kind of MEP. */
    public static final URI IN_OUT = URI.create("http://www.w3.org/2004/08/wsdl/in-out");
    /** A kind of MEP. */
    public static final URI ROBUST_IN_ONLY = URI.create("http://www.w3.org/2004/08/wsdl/robust-in-only");
    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES = Messages.getMessages(ConsumerInvocationHandler.class);
    private static final TransformerFactory TRANSFORMER_FACTORY = TransformerFactory.newInstance();

    // endpoint data
    private ConsumerEndpoint endpoint;
    private ServiceEndpoint mirroredEndpoint;
    private DeliveryChannel channel;

    // local Source Transformer
    private SourceTransformer sourceTransformer = new SourceTransformer();
    /** The Consumer Service Descriptor. */
    private ConsumerServiceDescriptor consumerServiceDescriptor;
    /** message denormalizer. */
    //private MessageDenormalizer messageDenormalizer;
    /** message normalizer. */
    //private MessageNormalizer messageNormalizer;
    /** Probe. */
    private Probe mMeasurement = null;
    /** The CXF service */
    private Service service = null;
    /** The EndpointInfo */
    private EndpointInfo ei = null;
    /** The Endpoint */
    private Endpoint ep = null;
    /** Method Names to BindingOperationInfo and mapping */
    private Map<String, BindingOperationInfo> methodToBio = new HashMap<String, BindingOperationInfo>();
    /** true if the endpoint is a consumser in the "from IDL" case. */
    boolean isFromIdl = false;

    /**
     * Constructor.
     *
     * @param sd
     *            The service descriptor used inside the class.
     *
     * @throws ServiceCreationException
     *             The service creation exception
     */
    public ConsumerInvocationHandler(ConsumerServiceDescriptor sd)
            throws ServiceCreationException {

        // Init the message normalizer/denormalizer
//		try {
//			//messageDenormalizer = new MessageDenormalizer();
//			//messageNormalizer = new MessageNormalizer();
//
//		} catch (Jbi4CorbaException e) {
//
//			Object[] logArgs = new Object[] { e.getMessage() };
//			LOG.error("CRB000749_XML_toString_Error", logArgs, e);
//			throw new ServiceCreationException("CRB000749_XML_toString_Error",
//					logArgs, e);
//		}

        consumerServiceDescriptor = sd;

        endpoint = consumerServiceDescriptor.getEndpoint();

        mirroredEndpoint = consumerServiceDescriptor.getProxiedService();

        isFromIdl = consumerServiceDescriptor.getServerCorbaClassesHolder().isGenerateClassesFromIDL();

        // Creates the endpoint factory
        JaxWsClientFactoryBean endpointFactory;
        try {
            endpointFactory = CXFUtils.getJaxWsClientFactoryBean();
        } catch (Jbi4CorbaException e1) {
            String msg = MESSAGES.getString("CRB000750_Error_in_creating_the_EndpointFactory");
            LOG.error(msg, e1);
            throw new ServiceCreationException(msg, e1);
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("--- wsdl definition:\n" + consumerServiceDescriptor.getServiceWSDLDefinition() + "\n ---");
        }

        // Creates the service, using the correct ClassLoader
        // SWAP ClassLoader 1/2
        ClassLoader oldCL = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(
                consumerServiceDescriptor.getServerCorbaClassesHolder().getUrlClassLoader());
        try {

            // if we use the interface the endpoint registration fails.
            Class impl = consumerServiceDescriptor.getServerCorbaClassesHolder().getWebServiceImpl();

            LOG.debug("Creating CXF service for class:" + impl + " in classloader: " + ((URLClassLoader) impl.getClassLoader()).getURLs()[0]);

            // Sets the service class
            endpointFactory.setServiceClass(impl);

            if (sd.getEndpoint() != null) {

                endpointFactory.setServiceName(sd.getEndpoint().getServiceName());
                // For Generate Service With Correct EndpointName
                endpointFactory.setEndpointName(new QName(sd.getEndpoint().getEndpointName()));

            }

            // If generated from IDL we have to set the same WSDL settings
            // used in the WSDL generations
            if (isFromIdl) {

                endpointFactory.getServiceFactory().setAnonymousWrapperTypes(true);
                // To qualify schema elements
                endpointFactory.getServiceFactory().setQualifyWrapperSchema(true);
                endpointFactory.getServiceFactory().setWrapped(true);
            }

            // Creates the endpoint
            endpointFactory.create();

            // Gets the service model
            service = endpointFactory.getServiceFactory().getService();

            // Gets the CXF EndpointInfo
            try {
                ei = CXFUtils.getEndpointInfo(service);
            } catch (IOException e1) {
                String msg = MESSAGES.getString("CRB000750_Error_in_creating_the_EndpointFactory");
                LOG.error(msg, e1);
                throw new ServiceCreationException(msg, e1);
            }

            // Gets the CXF Endpoint
            ep = CXFUtils.getEndpoint(service, ei);

            // Init the BindingOperationInfo/method mapper.
            SimpleMethodDispatcher md = (SimpleMethodDispatcher) ep.getService().get(MethodDispatcher.class.getName());

            Collection<BindingOperationInfo> bios = ep.getBinding().getBindingInfo().getOperations();
            Iterator<BindingOperationInfo> it = bios.iterator();
            while (it.hasNext()) {
                BindingOperationInfo bio = (BindingOperationInfo) it.next();
                Method meth = md.getMethod(bio);
                LOG.debug("bio: " + bio + " binded to :" + meth);
                methodToBio.put(meth.toString(), bio);
            }

        } catch (RuntimeException e) {
            String msg = MESSAGES.getString("CRB000751_RuntimeExcection",
                    new Object[]{e.getMessage()});
            LOG.error(msg, e);
            throw new ServiceCreationException(msg, e);
        } finally {
            // SWAP ClassLoader 2/2
            Thread.currentThread().setContextClassLoader(oldCL);
        }

        // Printing out the WSDL for logging pourpose
        if (LOG.isDebugEnabled()) {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            try {
                CXFUtils.writeDefinitionOnOutputStream(service, baos);
            } catch (WSDLException e) {
                LOG.warn("CRB000763_Error_in_producing_the_WSDL_for_logging",
                        new Object[]{e.getMessage()});
                e.printStackTrace();
            }
            LOG.debug("Crated CXF service: " + baos.toString());
        }
        

        ComponentContext context = RuntimeContext.getInstance().getComponentContext();
        try {

            channel = context.getDeliveryChannel();

        } catch (MessagingException e) {
            Object[] args = new Object[]{context};

            LOG.error("CRB000700_Unable_to_get_delivery_channel_from_context",
                    args, e);
            throw new ServiceCreationException(
                    "CRB000700_Unable_to_get_delivery_channel_from_context",
                    args, e);
        }

        // if we are on servicemix we don't wrap
        LOG.debug("context class name: " + context.getClass().getName());

    }

    /**
     * @param proxy
     *            The proxy
     * @param method
     *            The method
     * @param args
     *            The args
     * @return The return
     *
     * @throws Throwable
     *             This method should be raise only 2 kind of exception: -
     *             Jbi4CorbaRuntimeException; - Jbi4CorbaException. The last one
     *             is used to wrap a soap fault and the first one to wrap all
     *             the other errors. To avoid classpath problems, the runtime
     *             exception set the message exception with the original message
     *             exception but doesn't include the original exception itself.
     */
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        Method origMethod=method;
        Object exchangeResult = null;
        ClassLoader oldCL = Thread.currentThread().getContextClassLoader();
        Object[] returnedArgs = null;
        //Not supported before jdk 1.6
        //Object[] origArgs=Arrays.copyOf(args, args.length);
        //4-09-2009 Changed by Luca to be checked
        Object[] origArgs = new Object[args.length];
        System.arraycopy(args, 0, origArgs, 0, args.length);
        //--------------------------------------------------

        try {
            try {
                LOG.debug("ConsumerInvocationHandler.invoke; invoked endpoint: " + endpoint + " Role " + endpoint.getRole() + " " + " with method: " + method + " and args: " + Arrays.toString(args));

                LOG.debug("INVOKE");
                List argList = new ArrayList();
                for (int i = 0; i < args.length; i++) {
                    argList.add(args[i]);
                }

                // Chrono for Consumer-Normalization -start
                String topic = new String("Normalization");

                mMeasurement = Probe.fine(getClass(), endpoint.getUniqueName(),
                        topic);
                // To make the measurement thread safe, add the measurement
                // object
                // to context, and retrieve before stopping
                Exchange cxfExchange = new ExchangeImpl();
                cxfExchange.put("Measure-N", mMeasurement);

                // OriginalCLassLoader
                // This Transformation is Only for Consumer IDL FIRST
                // ************************* TRANSFORMATION CORBA TO SERVICE
                // ******************************************************************
                RuntimeInformation rInfo = consumerServiceDescriptor.getRuntimeInformation();


                MethodSignature methodSignature = null;

                // This must be done only when processing a "from IDL" consumer
                // Endpoint
                if (consumerServiceDescriptor.getServerCorbaClassesHolder().isGenerateClassesFromIDL()) {

                    Thread.currentThread().setContextClassLoader(
                            consumerServiceDescriptor.getServerCorbaClassesHolder().getUrlClassLoader());

                    methodSignature = CorbaTransformationUtils.getMetodSignatureFromMethod(method,
                            consumerServiceDescriptor.getServerCorbaClassesHolder().getMethodSignatures());

                    // In this case we have to specify the CXF service
                    // method...not the CORBA one.

                    method = methodSignature.getChangedMethod();

                    if (methodSignature.isContainsHolder()) {

                        for (int i = 0; i < methodSignature.getParameters().size(); i++) {
                            Param paramSig = (Param) methodSignature.getParameters().get(i);
                            if (paramSig.isHolder()) {
                                argList.set(i, new Holder());
                            }
                        }
                        CorbaTransformationUtils.changeHoldersFromCorbaToServiceObjects(
                                methodSignature, argList, args,
                                rInfo);
                    }
                    Object[] serviceArgs = null;
                    serviceArgs = CorbaTransformationUtils.changeFromCorbaToServiceObjects(args, rInfo,
                            methodSignature);
                    for (int i = 0; i < methodSignature.getParameters().size(); i++) {
                        Param paramSig = (Param) methodSignature.getParameters().get(i);
                        if (!paramSig.isHolder()) {
                            argList.set(i, serviceArgs[i]);
                        }
                    }


                }
                // **********************************************************************
                OutputStream xmlResult = fromObjectToXML(ep, method, argList);

                LOG.debug("XML request: " + xmlResult.toString());

                exchangeResult = null;
                QName portType = ei.getInterface().getName();

                LOG.debug("Portype" + ei.getInterface().getName());

                // Gets the BindingOperationInfo from the method name
                BindingOperationInfo bio = methodToBio.get(method.toString());

                QName operation = bio.getOperationInfo().getName();
                LOG.debug("operation to call:" + operation);

                // Tests if the operation is In-Only
                boolean isAsync = false;
                if (consumerServiceDescriptor.getServerCorbaClassesHolder().isGenerateClassesFromIDL()) {
                    // is Async if the original from IDL was oneway
                    isAsync = methodSignature.isOneway();
                } else {
                    isAsync = isAsynch(portType, operation);
                }

                if (isAsync) {
                    Object[] logArgs = new Object[]{operation.getLocalPart()};
                    LOG.debug("CRB000747_ConsumerInOnlyInvoke", logArgs);
                    exchangeInOnly(cxfExchange, operation, xmlResult);
                    return null;


                } else {

                    Object[] logArgs = new Object[]{operation.getLocalPart()};
                    LOG.debug("CRB000748_ConsumerInOutInvoke", logArgs);
                    exchangeResult = exchangeInOut(cxfExchange, operation,
                            method, xmlResult);

                }

//				LOG.debug("exchangeResult="
//						+ exchangeResult
//						+ " of classloader:"
//						+ ((URLClassLoader) (exchangeResult.getClass()
//								.getClassLoader())).getURLs()[0]);

                // If throwable, must be thrown
                if (exchangeResult instanceof Throwable) {
                    String msg = MESSAGES.getString(
                            "CRB000752_About_throwing_exception",
                            new Object[]{exchangeResult});
                    LOG.error(msg);

                    // Transform the Exception
                    // This Transformation is Only for Consumer IDL FIRST
                    if (consumerServiceDescriptor.getServerCorbaClassesHolder().isGenerateClassesFromIDL()) {
                        Thread.currentThread().setContextClassLoader(
                                consumerServiceDescriptor.getServerCorbaClassesHolder().getOriginalClassLoader());

                        exchangeResult = (Throwable) CorbaTransformationUtils.changeFromServiceToCorbaObject(
                                exchangeResult, rInfo, exchangeResult.getClass());

                    }

                    throw new Throwable((Throwable) exchangeResult);
                }

                LOG.debug("CRB000764_Received_message_invocation_for_endpoint");
                // Transform the Result in a Service Format

                // ************************* TRANSFORMATION SERVICE TO CORBA
                // This is necessary only when processing "from idl" endpoints.

                if (consumerServiceDescriptor.getServerCorbaClassesHolder().isGenerateClassesFromIDL()) {

                    Thread.currentThread().setContextClassLoader(
                            consumerServiceDescriptor.getServerCorbaClassesHolder().getOriginalClassLoader());
                    /*
                     * se abbiamo un holder assumiamo che cxf ci abbia tornato una lista
                     * il primo elemento è return gli altri sono i parametri tra cui gli eventuali holder
                     * facciamo la conversione degli holder e del return
                     */

                    if (methodSignature.isContainsHolder()) {
                        List argsSubList = null;
                        argsSubList = ((List) exchangeResult).subList(1, ((List) exchangeResult).size());

                        for (int p = 0; p < methodSignature.getParameters().size(); p++) {
                            Param paramSig = (Param) methodSignature.getParameters().get(p);
                            if (paramSig.isHolder()) {

                                Object o = argsSubList.get(p);
                                argsSubList.set(p, new javax.xml.ws.Holder(o));

                            }
                        }
                        returnedArgs = CorbaTransformationUtils.changeHoldersFromServiceToCorbaObjects(
                                methodSignature, argsSubList, rInfo);
                        LOG.debug("returned args: " + Arrays.toString(returnedArgs));
                        LOG.debug("initial args: " + Arrays.toString(origArgs));
                        for (int i = 0; i < origArgs.length; i++) {
                            if (argsSubList.get(i) != null) {
                                // assumo che l'elemento i-esimo di args[] sia un holder
                                Class holderClass = origArgs[i].getClass();
                                Field valueField = holderClass.getField("value");
                                valueField.set(origArgs[i], valueField.get(returnedArgs[i]));
                            }
                        }
                        exchangeResult = CorbaTransformationUtils.changeFromServiceToCorbaObject(((List) exchangeResult).get(0),
                                rInfo, methodSignature.getMethod().getReturnType());
                    /*
                     * se non ci sono holder trasformo solo retunr
                     */
                    } else {

                        exchangeResult = CorbaTransformationUtils.changeFromServiceToCorbaObject(exchangeResult,
                                rInfo, methodSignature.getMethod().getReturnType());
                    }
                }
            // **********************************************************************

            } catch (Throwable th) {
                if (LOG.isDebugEnabled()) {
                    // For debug pourpouse
                    th.printStackTrace();
                }
                throw th;
            }
            // Else, the object is returned.

            //patch for ossupport11 if return result is null and it is an type array the return a void array
            if (method.getReturnType().isArray() && exchangeResult == null) {
                LOG.debug("result is array and value is null, not valid for corba, instancing a array of lenght 0 and type: " + origMethod.getReturnType().getComponentType());
                try {
                    exchangeResult = Array.newInstance(origMethod.getReturnType().getComponentType(), 0);
                    LOG.debug("echangeResult classloader: " + exchangeResult.getClass().getClassLoader());
                } catch (Throwable e) {
                    LOG.error("error in creating default array: ", e);
                    throw e;
                }
            }

            String resultAsString = ReflectionToStringBuilder.toString(exchangeResult);
            String outArgsAsString = ReflectionToStringBuilder.toString(origArgs);

            LOG.debug("returning from invokation: return value: " + resultAsString + " out parameters: " + outArgsAsString);


            return exchangeResult;
        } finally {
            Thread.currentThread().setContextClassLoader(oldCL);
        }
    }

    /**
     * This method implements the 'InOnly' Message Exchange MEP.
     *
     * @param executionContext
     * @param op
     * @param nodeResult
     *
     * @return The result of the exchange.
     *
     * @throws it.imolinfo.jbi4corba.jbi.processor.wsdl11wrapper.WrapperProcessingException
     * @throws javax.xml.transform.TransformerException
     * @throws javax.jbi.messaging.MessagingException
     * @throws javax.xml.parsers.ParserConfigurationException
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    protected Object exchangeInOnly(Exchange cxfExchange, QName operation,
            OutputStream xmlResult) throws Jbi4CorbaRuntimeException {

        javax.jbi.messaging.InOnly jbiExchange = createInOnlyExchange(operation);
        LOG.debug("Created InOnlyExchange: " + jbiExchange);

        // ==========================================
        // Creating 'IN' message
        // ==========================================
        NormalizedMessage inMessage;
        try {
            inMessage = jbiExchange.createMessage();
            LOG.debug("created inMessage: " + inMessage);
        } catch (MessagingException e) {
            Object[] params = new Object[]{jbiExchange};

            LOG.error("CRB000701_Unable_to_create_message_from_exchange",
                    params, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000701_Unable_to_create_message_from_exchange", params,
                    e);
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("stringContent: " + xmlResult.toString());
        }

        Source wrappedContent = null;
        Source xmlSource = new StringSource(xmlResult.toString());
        try {
            MessageNormalizer messageNormalizer = new MessageNormalizer();
            wrappedContent = messageNormalizer.normalize(xmlSource, endpoint,
                    operation, true, false);
        } catch (Jbi4CorbaException e) {
            Object[] logArgs = new Object[]{e.getMessage()};
            LOG.error("CRB000744_MessageWrapping_Error", logArgs, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000744_MessageWrapping_Error", logArgs, e);
        } finally {
            // Chrono for Customer-Normalization -end(1)
            // First retrieve Perf Measurement object from context
            mMeasurement = (Probe) cxfExchange.get("Measure-N");
            mMeasurement.end(); // Normalization Chrono stopped
        }

        try {
            inMessage.setContent(wrappedContent);
        } catch (MessagingException e) {
            Object[] logArgs = new Object[]{e.getMessage()};
            LOG.error("CRB000745_NormalizedMessageSetContent_Error",
                    logArgs, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000745_NormalizedMessageSetContent_Error", logArgs, e);
        }

        if (LOG.isDebugEnabled()) {
            String in2string = null;
            try {
                in2string = sourceTransformer.contentToString(inMessage);
            } catch (MessagingException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (TransformerException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (ParserConfigurationException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (IOException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (SAXException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            }
            LOG.debug("JBI-inMessage.getContent()=" + in2string);
        }

        try {
            jbiExchange.setInMessage(inMessage);
        } catch (MessagingException e) {
            Object[] params = new Object[]{inMessage, jbiExchange};

            LOG.error("CRB000703_Unable_to_set_message_in_exchange", params, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000703_Unable_to_set_message_in_exchange", params, e);
        }

        // ==========================================
        // Sending Asynchronously
        // ==========================================

        if (LOG.isDebugEnabled()) {
            String in2string = null;
            try {
                in2string = sourceTransformer.contentToString(inMessage);
            } catch (MessagingException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (TransformerException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (ParserConfigurationException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (IOException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (SAXException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            }
            LOG.debug("Sending asynch message: InMessage: " + in2string + " on exchange: " + jbiExchange.getExchangeId());
        }

        try {
            endpoint.sendAsynch(jbiExchange, channel);
            endpoint.getEndpointStatus().incrementSentRequests();
        } catch (MessagingException e) {
            Object[] logArgs = new Object[]{jbiExchange, endpoint};
            LOG.error("CRB000704_Unable_to_send_exchange_with_endpoint",
                    logArgs, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000704_Unable_to_send_exchange_with_endpoint", logArgs,
                    e);
        }

        LOG.debug("... message sending complete.");
        LOG.debug("jbiExchange.getStatus()=" + jbiExchange.getStatus());
        return null;
    }

    /**
     * This method implements the 'InOut' Message Exchange MEP.
     *
     * @param executionContext
     * @param op
     * @param nodeResult
     *
     * @return The result of the exchange.
     *
     * @throws it.imolinfo.jbi4corba.jbi.processor.wsdl11wrapper.WrapperProcessingException
     * @throws javax.xml.transform.TransformerException
     * @throws javax.jbi.messaging.MessagingException
     * @throws javax.xml.parsers.ParserConfigurationException
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    protected Object exchangeInOut(Exchange cxfExchange, QName operation,
            Method method, OutputStream xmlResult)
            throws Jbi4CorbaRuntimeException {

        javax.jbi.messaging.InOut jbiExchange = (InOut) createInOutExchange(operation);
        LOG.debug("Created InOutExchange: " + jbiExchange);

        // executionContext.setJbiExchange(jbiExchange);
        NormalizedMessage inMessage;
        try {
            inMessage = jbiExchange.createMessage();
            LOG.debug("created inMessage: " + inMessage);
        } catch (MessagingException e) {
            Object[] params = new Object[]{jbiExchange};

            LOG.error("CRB000701_Unable_to_create_message_from_exchange",
                    params, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000701_Unable_to_create_message_from_exchange", params,
                    e);
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("stringContent: " + xmlResult.toString());
        }

        LOG.debug("The message needs wrapping");

        Source wrappedContent = null;
        Source xmlSource = new StringSource(xmlResult.toString());
        try {
            MessageNormalizer messageNormalizer = new MessageNormalizer();
            wrappedContent = messageNormalizer.normalize(xmlSource, endpoint,
                    operation, true, false);
        } catch (Jbi4CorbaException e) {
            Object[] logArgs = new Object[]{e.getMessage()};
            LOG.error("CRB000744_MessageWrapping_Error", logArgs, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000744_MessageWrapping_Error", logArgs, e);
        } finally {
            // Chrono for Customer-Normalization -end(2)
            // First retrieve Perf Measurement object from context
            mMeasurement = (Probe) cxfExchange.get("Measure-N");
            mMeasurement.end(); // Normalization Chrono stopped
        }

        try {
            inMessage.setContent(wrappedContent);
        } catch (MessagingException e) {
            Object[] logArgs = new Object[]{e.getMessage()};
            LOG.error("CRB000745_NormalizedMessageSetContent_Error",
                    logArgs, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000745_NormalizedMessageSetContent_Error", logArgs, e);
        }

        if (LOG.isDebugEnabled()) {
            String in2string = null;
            try {
                in2string = sourceTransformer.contentToString(inMessage);
            } catch (MessagingException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (TransformerException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (ParserConfigurationException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (IOException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (SAXException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            }
            LOG.debug("JBI-inMessage.getContent()=" + in2string);
        }

        try {

            jbiExchange.setInMessage(inMessage);

        } catch (MessagingException e) {
            Object[] params = new Object[]{inMessage, jbiExchange};

            LOG.error("CRB000703_Unable_to_set_message_in_exchange", params, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000703_Unable_to_set_message_in_exchange", params, e);
        }

        if (LOG.isDebugEnabled()) {
            String in2string2 = null;
            try {
                in2string2 = sourceTransformer.contentToString(inMessage);
            } catch (MessagingException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (TransformerException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (ParserConfigurationException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (IOException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            } catch (SAXException e) {
                Object[] logArgs = new Object[]{e.getMessage()};
                LOG.error("CRB000743_XML_toString_Error", logArgs, e);
                throw new Jbi4CorbaRuntimeException(
                        "CRB000743_XML_toString_Error", logArgs, e);
            }
            LOG.debug("Sending synch message: InMessage: " + in2string2 + " on exchange: " + jbiExchange.getExchangeId());
        }

        boolean sendingReturn = false;
        try {
            sendingReturn = sendSynch(jbiExchange);
            endpoint.getEndpointStatus().incrementSentRequests();

        } catch (MessagingException e1) {
            Object[] logArgs = new Object[]{jbiExchange, ""};
            LOG.error("CRB000704_Unable_to_send_exchange_with_endpoint",
                    logArgs, e1);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000704_Unable_to_send_exchange_with_endpoint", logArgs,
                    e1);
        }
        LOG.debug("... message response:" + sendingReturn);

        if (jbiExchange.getStatus() == ExchangeStatus.ERROR) {

            Exception err = jbiExchange.getError();
            endpoint.getEndpointStatus().incrementReceivedErrors();

            LOG.error("CRB000705_Exchange_status_error", err);
            if (err != null) {
                throw new Jbi4CorbaRuntimeException(err);
            } else {
                throw new Jbi4CorbaRuntimeException("CRB000706_Unknown_error");
            }

        } else if (jbiExchange.getStatus() == ExchangeStatus.ACTIVE) {
            LOG.debug("exchange status ACTIVE");
            endpoint.getEndpointStatus().incrementReceivedReplies();
            try {

                try {
                    return exchangeStatusACTIVE(jbiExchange, method);
                } catch (MessagingException e) {
                    Object[] logArgs = new Object[]{e.getMessage()};
                    LOG.error("CRB000746_SetExchangeStatus_Error", logArgs, e);
                    throw new Jbi4CorbaRuntimeException(
                            "CRB000746_SetExchangeStatus_Error", logArgs, e);
                } catch (TransformerException e) {
                    Object[] logArgs = new Object[]{e.getMessage()};
                    LOG.error("CRB000746_SetExchangeStatus_Error", logArgs, e);
                    throw new Jbi4CorbaRuntimeException(
                            "CRB000746_SetExchangeStatus_Error", logArgs, e);
                } catch (ParserConfigurationException e) {
                    Object[] logArgs = new Object[]{e.getMessage()};
                    LOG.error("CRB000746_SetExchangeStatus_Error", logArgs, e);
                    throw new Jbi4CorbaRuntimeException(
                            "CRB000746_SetExchangeStatus_Error", logArgs, e);
                } catch (IOException e) {
                    Object[] logArgs = new Object[]{e.getMessage()};
                    LOG.error("CRB000746_SetExchangeStatus_Error", logArgs, e);
                    throw new Jbi4CorbaRuntimeException(
                            "CRB000746_SetExchangeStatus_Error", logArgs, e);
                } catch (SAXException e) {
                    Object[] logArgs = new Object[]{e.getMessage()};
                    LOG.error("CRB000746_SetExchangeStatus_Error", logArgs, e);
                    throw new Jbi4CorbaRuntimeException(
                            "CRB000746_SetExchangeStatus_Error", logArgs, e);
                } catch (WrapperProcessingException e) {
                    Object[] logArgs = new Object[]{e.getMessage()};
                    LOG.error("CRB000746_SetExchangeStatus_Error", logArgs, e);
                    throw new Jbi4CorbaRuntimeException(
                            "CRB000746_SetExchangeStatus_Error", logArgs, e);
                }

            } finally {

                setJbiExchangeToDONE(jbiExchange);
                endpoint.getEndpointStatus().incrementSentDones();

                try {
                    channel.send(jbiExchange);
                } catch (MessagingException e) {
                    Object[] params = new Object[]{jbiExchange, channel};

                    LOG.error("CRB000711_Unable_to_send_exchange_on_channel",
                            params, e);
                    throw new Jbi4CorbaRuntimeException(
                            "CRB000711_Unable_to_send_exchange_on_channel",
                            params, e);
                }

            }
        } // jbiexchange in illegal status
        else {
            throw new IllegalStateException(MESSAGES.getString(
                    "CRB000712_Unexpected_exchange_status", jbiExchange.getStatus()));
        }
    }

    /**
     * This method is used to set the status of the JbiExchange to DONE.
     *
     * @param jbiExchange
     *            The ME.
     *
     * @throws Jbi4CorbaRuntimeException
     *             If is impossible set up the status.
     */
    protected void setJbiExchangeToDONE(
            javax.jbi.messaging.MessageExchange jbiExchange)
            throws Jbi4CorbaRuntimeException {

        try {
            LOG.debug("Setting exchange to DONE");
            // Sending DONE to provider
            jbiExchange.setStatus(ExchangeStatus.DONE);

        } catch (MessagingException e) {
            Object[] par = new Object[]{jbiExchange};

            LOG.error("CRB000710_Unable_to_set_status_DONE_for_exchange", par,
                    e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000710_Unable_to_set_status_DONE_for_exchange", par, e);
        }
    }

    /**
     * Exchange status active. Converts the XML to the object to be sent back.
     *
     * @param jbiExchange
     *
     * @return the object (or a <code>Throwable</code> if error)
     *
     * @throws MessagingException
     * @throws TransformerException
     * @throws ParserConfigurationException
     * @throws IOException
     * @throws SAXException
     * @throws WrapperProcessingException
     */
    protected Object exchangeStatusACTIVE(
            javax.jbi.messaging.InOut jbiExchange, Method method)
            throws MessagingException, TransformerException,
            ParserConfigurationException, IOException, SAXException,
            WrapperProcessingException {

        JbiMessage jbiMessage = null;

        if (jbiExchange.getFault() != null) {
            LOG.warn("CRB000707_Exchange_returned_a_fault", jbiExchange.getFault());
            // Fault Path
            // Unwraps the fault
            try {
                LOG.debug("CRB000753_Before_messageDenormalizer");
                MessageDenormalizer messageDenormalizer = new MessageDenormalizer();
                jbiMessage = messageDenormalizer.denormalize(jbiExchange.getFault(), endpoint, jbiExchange.getOperation(),
                        false, true);
                LOG.debug("CRB000754_Before_fromXMLToException");

                String xmlException = sourceTransformer.toString(jbiMessage.getMessageSource());

                Throwable corbaException = fromXMLToException(xmlException,
                        method);
                LOG.debug("CRB000755_After_fromXMLToException",
                        new Object[]{corbaException});

                return corbaException;
            } catch (Jbi4CorbaException ex) {
                ex.printStackTrace();
                throw new Jbi4CorbaRuntimeException("CRB000708_SOAP_fault",
                        new Object[]{ex}, null);
            } catch (RuntimeException th) {
                String msg = MESSAGES.getString(
                        "CRB000756_Error_in_convertion_of_XML_to_object_to_be_sent_back",
                        new java.lang.Object[]{th.getMessage()});
                LOG.error(msg, th);
                throw new Jbi4CorbaRuntimeException(msg, th);
            }
        }

        // this is the normal path: deserialize the result
        // NormalizedMessage outMsg = jbiExchange.getMessage("out");
        NormalizedMessage outMsg = jbiExchange.getOutMessage();

        if (LOG.isDebugEnabled()) {
            LOG.debug("Received Message class:" + outMsg.getClass().getName());
            LOG.debug("Received Message content: " + sourceTransformer.contentToString(outMsg));
        }

        // this may be may in inonly mep
        if (outMsg == null) {
            throw new NullPointerException(MESSAGES.getString("CRB000709_Exchange_returned_a_null_message"));
        }

        // Chrono for Customer-Denormalization -start
        String topic = new String("Denormalization");
        mMeasurement = Probe.fine(getClass(), endpoint.getUniqueName(), topic);
        // this is a simple unwrap

        try {
            // The first "false" is isInput, the second is isFault
            MessageDenormalizer messageDenormalizer = new MessageDenormalizer();
            jbiMessage = messageDenormalizer.denormalize(outMsg, endpoint,
                    jbiExchange.getOperation(), false, false);
        } catch (Jbi4CorbaException e) {
            Object[] par = new Object[]{jbiExchange};

            LOG.error("CRB000710_Unable_to_set_status_DONE_for_exchange", par,
                    e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000710_Unable_to_set_status_DONE_for_exchange", par, e);
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("jbi unwrapped content:" + sourceTransformer.toString(jbiMessage.getMessageSource()));
        }

        Object result = fromXMLToObject(sourceTransformer.toString(jbiMessage.getMessageSource()), method);

        // Chrono for Customer-Denormalization -stop
        mMeasurement.end();

        if (LOG.isDebugEnabled()) {
            LOG.debug("Deserialized soap message returned: " + result);
        }
        return result;

    }

    /**
     * This method is used to send asynchronously a MessageExchange.
     *
     * @param jbiExchange
     *            The message to send.
     *
     *
     * @throws MessagingException
     */
    protected void sendASynch(InOnly jbiExchange) throws MessagingException {
        try {
            // synchronous send
            LOG.debug("Before sendASync. Sending message:");
            LOG.debug("Channel:" + channel.getClass().getName());

            LOG.debug("CRB000736_Sending_message");

            channel.send(jbiExchange);

            LOG.debug("After sendASync");

        } catch (MessagingException e) {
            Object[] params = new Object[]{jbiExchange, endpoint};

            LOG.error("CRB000704_Unable_to_send_exchange_with_endpoint",
                    params, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000704_Unable_to_send_exchange_with_endpoint", params,
                    e);
        }
    }

    /**
     * This method is used to send synchronously a MessageExchange.
     *
     * @param jbiExchange
     *            The message to send.
     *
     * @return true for 'done'; false for 'error'.
     *
     * @throws MessagingException
     */
    protected boolean sendSynch(InOut jbiExchange) throws MessagingException {
        try {
            // synchronous send
            LOG.debug("Before sendSync. Sending message:");
            LOG.debug("Channel:" + channel.getClass().getName());

            LOG.debug("CRB000736_Sending_message");

            boolean ret = channel.sendSync(jbiExchange);

            LOG.debug("After sendSync (send sync returned: " + ret + ")");

            return ret;
        } catch (MessagingException e) {
            Object[] params = new Object[]{jbiExchange, endpoint};

            LOG.error("CRB000704_Unable_to_send_exchange_with_endpoint",
                    params, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000704_Unable_to_send_exchange_with_endpoint", params,
                    e);
        }
    }

    /**
     * Xml-> Object conversion for the specified method using CXF.
     *
     * @param xmlOut
     * @param method
     *
     * @return The return
     */
    private Object fromXMLToObject(String xmlOut, Method method) {

        LOG.debug(">>>>> fromXMLToObject - begin" + ". xmlOut=" + xmlOut + ". method=" + method);

        List outObjs = null;

        // Gets the BindingOperationInfo from the method name
        BindingOperationInfo bio = null;
        if (isFromIdl) {
            // PATCH for a CXF
            bio = getBindingOperationInfo(method);
        } else {
            // Gets the BindingOperationInfo from the method
            JAXWSMethodDispatcher md = (JAXWSMethodDispatcher) ep.getService().get(MethodDispatcher.class.getName());
            bio = md.getBindingOperation(method, ep);
        }
        String parameterStyle = CXFUtils.getParameterStyle(bio);
        String bindingStyle = CXFUtils.getBindingStyle(bio);


        // Creates the CXF message
        Message message = new MessageImpl();
        Exchange exchange = new ExchangeImpl();
        exchange.setOneWay(false);
        exchange.put(BindingOperationInfo.class, bio);
        exchange.put(Service.class, ep.getService());
        exchange.put(Endpoint.class, ep);
        message.setExchange(exchange);
        exchange.setInMessage(message);
        exchange.setOutMessage(new MessageImpl());

        // Important! If not set, maps the object in the input message (the
        // opposite as the objectToXml method)..
        message.put(Message.REQUESTOR_ROLE, Boolean.TRUE);

        // Sets the Source as In message
        try {
            message.setContent(InputStream.class,
                    convertMessageToInputStream(new StringSource(xmlOut)));
        } catch (TransformerConfigurationException e) {
            String msg = MESSAGES.getString(
                    "CRB000757_Error_converting_messeage_to_InputStream",
                    new java.lang.Object[]{xmlOut,});
            LOG.error(msg, e);
            throw new Jbi4CorbaRuntimeException(msg, e);
        } catch (IOException e) {
            String msg = MESSAGES.getString(
                    "CRB000757_Error_converting_messeage_to_InputStream",
                    new java.lang.Object[]{xmlOut,});
            LOG.error(msg, e);
            throw new Jbi4CorbaRuntimeException(msg, e);
        } catch (TransformerException e) {
            String msg = MESSAGES.getString(
                    "CRB000757_Error_converting_messeage_to_InputStream",
                    new java.lang.Object[]{xmlOut,});
            LOG.error(msg, e);
            throw new Jbi4CorbaRuntimeException(msg, e);
        }

        PhaseInterceptorChain inInterceptorChain = new PhaseInterceptorChain(
                CXFUtils.getBus().getExtension(PhaseManager.class).getInPhases());

        CXFUtils.populateInInterceptorsForConsumer(inInterceptorChain,
                parameterStyle, bindingStyle, isFromIdl);

        message.setInterceptorChain(inInterceptorChain);

        // Process the message
        inInterceptorChain.doIntercept(message);

        // Gets the returned objects
        outObjs = (List) message.getContent(List.class);
        LOG.debug("Object returned: " + outObjs);

        LOG.debug("Deserializing message ...");
        Object result = outObjs;
//		if ((outObjs != null) && (outObjs.size() != 0)) {
//			result = outObjs.get(0);
//			LOG.debug("Returned result: " + result);
//		}

        if (LOG.isDebugEnabled()) {
            String bodyAsString = (result == null) ? "null"
                    : ReflectionToStringBuilder.toString(result);
            LOG.debug("InMessage.bodyAsString=" + bodyAsString);
        }


        if (result != null && (result instanceof List)) {

            LOG.debug("Returned result: " + result.getClass().getName());
            List list = (List) result;
            if (list.isEmpty()) {
                LOG.warn("CRB000715_Deserialized_list_empty", list);
                LOG.debug("<<<<< fromXMLToObject - end");
                return null;
            }

            if (list.size() == 1) {
                LOG.debug("returned list contains one element, returning the element itself");
                return list.get(0);
            }

//			LOG.debug("returned list contains: " + list.size() + "elements.");
//			if (list.size() != 1) {
//				throw new Jbi4CorbaRuntimeException(
//						"CRB000716_Deserialized_list_too_big",
//						new Object[] { list }, null);
//			}
//
//			result = list.get(0);
            if (LOG.isDebugEnabled()) {
                String resultAsString = ReflectionToStringBuilder.toString(result);
                LOG.debug("resultAsString=" + resultAsString);
            }
            return result;

        } else {
            LOG.debug("Deserializing message ... nothing to do because the body " + "of the InMessage is null OR because the body is not a List" + ". InMessage=" + result + ". InMessage 'is a' List=" + (result instanceof List));
        }
        LOG.debug("Deserializing message ... done");

        LOG.debug("<<<<< fromXMLToObject - end" + ". ExecutionContext.outResult=" + result + ". result=" + result);
        return result;
    }

    /**
     * Converts the method call in an XML message
     *
     * @param ep
     * @param methodCalled
     * @param params
     * @return
     */
    private Throwable fromXMLToException(String xmlFault, Method method)
            throws Jbi4CorbaRuntimeException {

        LOG.debug(">>>>> fromXMLToException - begin" + ". xmlOut=" + xmlFault + ". method=" + method);

        LOG.debug("CRB000758_fromXMLToException");
        LOG.debug("CRB000759_fromXMLToException_xmlFault",
                new Object[]{xmlFault});
        LOG.debug("CRB000758_fromXMLToException");

        // Gets the BindingOperationInfo from the method name
        BindingOperationInfo bio = null;
        if (isFromIdl) {
            // PATCH for a CXF
            bio = getBindingOperationInfo(method);
        } else {
            // Gets the BindingOperationInfo from the method
            JAXWSMethodDispatcher md = (JAXWSMethodDispatcher) ep.getService().get(MethodDispatcher.class.getName());
            bio = md.getBindingOperation(method, ep);
        }
        QName operation = bio.getOperationInfo().getName();
        LOG.debug("operation to call:" + operation);

        // Creates the message
        Message message = new MessageImpl();
        Exchange exchange = new ExchangeImpl();
        exchange.setOneWay(false);
        exchange.put(BindingOperationInfo.class, bio);
        exchange.put(Service.class, ep.getService());
        exchange.put(Endpoint.class, ep);
        message.setExchange(exchange);
        exchange.setInMessage(message);
        exchange.setOutMessage(new MessageImpl());

        // Preapres the CXF fault to be processed
        // Mandatory to pass a message...
        String msg = "ERROR_IN_JBI_PROCESSING";
        Fault fault = new Fault(new org.apache.cxf.common.i18n.Message(msg,
                (ResourceBundle) null));

        StringSource bodySource = new StringSource(xmlFault);
        XMLStreamReader xmlReader = StaxUtils.createXMLStreamReader(bodySource);
        Element detailContent = null;
        try {
            detailContent = StaxUtils.read(new FragmentStreamReader(xmlReader)).getDocumentElement();
        } catch (XMLStreamException e) {
            LOG.error("CRB000760_Error_in_reading_XML_fault", e);
            throw new Jbi4CorbaRuntimeException(msg, e);
        }

        // Here the faul xml is not wrapped into a detail element.
        // For CXF to correctly map the XML, we must wrap the message in a
        // <detail> eleemnt.
        Document detailDocument = DOMUtils.createDocument();
        Element detail = detailDocument.createElement("detail");
        detailDocument.adoptNode(detailContent);
        detail.appendChild(detailContent);

        fault.setDetail(detail);

        // Sets the fault on the message
        message.setContent(Exception.class, fault);

        // Handle the message using the ClientFaultConverter;
        PhaseInterceptorChain faultInterceptorChain = new PhaseInterceptorChain(
                CXFUtils.getBus().getExtension(PhaseManager.class).getInPhases());
        message.setInterceptorChain(faultInterceptorChain);
        faultInterceptorChain.add(new Jbi4CorbaConsumerExceptionInterceptor());

        // Process the message
        faultInterceptorChain.doIntercept(message);

        // The ClientFaultConverter replaces the fault with the Exception object
        // in the message for the exception class...
        Throwable th = message.getContent(Exception.class);

        LOG.debug("CRB000761_Returnin_th_getName", new Object[]{th.getClass().getName()});

        return th;
    }

    /**
     * This method is used to create an InOut MessageExchange.
     *
     * @param op
     *            The QName of the OperationInfo is used to set the operation of
     *            the JBI MessageExchange.
     *
     * @return The InOut ME just created.
     */
    private javax.jbi.messaging.InOut createInOutExchange(QName operation) {

        javax.jbi.messaging.InOut jbiExchange = createInOutExchange();

        jbiExchange.setOperation(operation);
        jbiExchange.setEndpoint(mirroredEndpoint);

        if (LOG.isDebugEnabled()) {
            LOG.debug("Mirrored endpoint:" + mirroredEndpoint);
        }
        return jbiExchange;
    }

    /**
     * This method is used to create an InOut MessageExchange.
     *
     * @param op
     *            The QName of the OperationInfo is used to set the operation of
     *            the JBI MessageExchange.
     *
     * @return The InOnly ME just created.
     */
    private javax.jbi.messaging.InOnly createInOnlyExchange(QName operation) {

        javax.jbi.messaging.InOnly jbiExchange = createInOnlyExchange();

        jbiExchange.setOperation(operation);
        jbiExchange.setEndpoint(mirroredEndpoint);

        if (LOG.isDebugEnabled()) {
            LOG.debug("Mirrored endpoint:" + mirroredEndpoint);
        }
        return jbiExchange;
    }

    /**
     * This method is used to discover an asynchronous operation.
     *
     * @param portType
     *            The QName of the PortType.
     * @param operationName
     *            The name of the operation controlled.
     *
     * @return true, when the operation is asynchronous. false, when the
     *         operation is synchronous or when an error occurs.
     */
    protected boolean isAsynch(QName portType, QName operation) {
        if (portType == null || operation == null) {
            return false;
        }

        WsdlInformation wi = consumerServiceDescriptor.getServerCorbaClassesHolder().getWsdlInformation();
        if (wi == null) {
            return false;
        }

        List<QName> opList = wi.getAsynchOperationMap().get(portType);

        if (opList == null) {
            return false;
        }

        // true if the operation is found
        for (QName op : opList) {
            if (operation.equals(op)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Converts the method call in an XML message
     *
     * @param ep
     * @param methodCalled
     * @param params
     * @return the XML OutputStream
     */
    private OutputStream fromObjectToXML(Endpoint ep, Method methodCalled,
            List params) {

        LOG.debug("From Object to XML" + params + " " + ep + " " + methodCalled + " of classloader:" + ((URLClassLoader) (methodCalled.getDeclaringClass().getClassLoader())).getURLs()[0]);

        // Gets the BindingOperationInfo from the method name
        BindingOperationInfo bio = getBindingOperationInfo(methodCalled);
        QName operation = bio.getOperationInfo().getName();
        String parameterStyle = CXFUtils.getParameterStyle(bio);
        String bindingStyle = CXFUtils.getBindingStyle(bio);

        LOG.debug("operation to call:" + operation);

        // Creates the message
        Message message = new MessageImpl();
        Exchange exchange = new ExchangeImpl();
        exchange.setOneWay(false);
        exchange.put(BindingOperationInfo.class, bio);
        if (isFromIdl) {
            // If is from idl, it's always wrapped
            exchange.put(OperationInfo.class, bio.getWrappedOperation().getOperationInfo());
        }
        exchange.put(Service.class, ep.getService());
        exchange.put(Endpoint.class, ep);
        message.setExchange(exchange);
        exchange.setOutMessage(message);
        exchange.put(StaxOutInterceptor.FORCE_START_DOCUMENT, Boolean.TRUE);

        // We have to map the request (input operation).
        message.put(Message.REQUESTOR_ROLE, Boolean.TRUE);

        // Sets the in content
        message.setContent(List.class, params);

        // Sets the output stream
        OutputStream out = new ByteArrayOutputStream();
        message.setContent(OutputStream.class, out);

        PhaseInterceptorChain outInterceptorChain = new PhaseInterceptorChain(
                CXFUtils.getBus().getExtension(PhaseManager.class).getOutPhases());

        CXFUtils.populateOutInterceptors(outInterceptorChain, parameterStyle,
                bindingStyle, isFromIdl);
        // Process the message
        message.setInterceptorChain(outInterceptorChain);
        outInterceptorChain.doIntercept(message);

        return out;
    }

    /**
     * Returns the BIO of the service model from ths <code>Method</code> called.
     *
     * @param methodCalled
     * @return
     */
    private BindingOperationInfo getBindingOperationInfo(Method methodCalled) {
        BindingOperationInfo bio = methodToBio.get(methodCalled.toString());
        LOG.debug("BIO is unwrapped:" + bio.isUnwrapped());

        if (bio.isUnwrappedCapable()) {
            bio = bio.getWrappedOperation();
        }
        return bio;
    }

    /**
     * Creates the InOut Exchange.
     *
     * @return The return
     */
    protected javax.jbi.messaging.InOut createInOutExchange() {

        // ComponentContext context = endpoint
        ComponentContext context = RuntimeContext.getInstance().getComponentContext();

        LOG.debug("Component context class:" + context.getClass().getName());
        DeliveryChannel channel;
        try {
            channel = context.getDeliveryChannel();

            LOG.debug("Channel:" + channel.getClass().getName());
        } catch (MessagingException e) {
            Object[] args = new Object[]{context};

            LOG.error("CRB000719_Unable_to_get_delivery_channel_from_context",
                    args, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000719_Unable_to_get_delivery_channel_from_context",
                    args, e);
        }
        MessageExchangeFactory factory = channel.createExchangeFactory();

        javax.jbi.messaging.InOut exchange;
        try {
            exchange = factory.createInOutExchange();
        } catch (MessagingException e) {
            Object[] args = new Object[]{factory};

            LOG.error("CRB000720_Unable_to_create_exchange_from_factory", args,
                    e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000720_Unable_to_create_exchange_from_factory", args, e);
        }
        return exchange;
    }

    /**
     * Creates the InOnly exchange.
     *
     * @return
     */
    protected javax.jbi.messaging.InOnly createInOnlyExchange() {

        ComponentContext context = RuntimeContext.getInstance().getComponentContext();

        LOG.debug("Component context class:" + context.getClass().getName());
        DeliveryChannel channel;
        try {
            channel = context.getDeliveryChannel();
            LOG.debug("Channel:" + channel.getClass().getName());
        } catch (MessagingException e) {
            Object[] args = new Object[]{context};

            LOG.error("CRB000719_Unable_to_get_delivery_channel_from_context",
                    args, e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000719_Unable_to_get_delivery_channel_from_context",
                    args, e);
        }
        MessageExchangeFactory factory = channel.createExchangeFactory();

        javax.jbi.messaging.InOnly exchange;
        try {
            exchange = factory.createInOnlyExchange();
        } catch (MessagingException e) {
            Object[] args = new Object[]{factory};

            LOG.error("CRB000720_Unable_to_create_exchange_from_factory", args,
                    e);
            throw new Jbi4CorbaRuntimeException(
                    "CRB000720_Unable_to_create_exchange_from_factory", args, e);
        }
        return exchange;
    }

    /**
     * This method is called by the ESB after the message is sent.
     *
     * @param exchange
     *            The message exchange
     * @throws Exception
     *             The exception
     *
     * @see ConsumerExchangeProcessor.
     */
    public void process(javax.jbi.messaging.MessageExchange exchange)
            throws Exception {
        LOG.debug(">>>>> process - begin. MessageExchange=" + exchange);

        if (IN_ONLY.equals(exchange.getPattern())) { // IN ONLY

            processInOnly(exchange);

        } else if (IN_OUT.equals(exchange.getPattern())) { // IN OUT

            processInOut(exchange);

        } else { // UNSUPPORTED MEP
            Object[] args = new Object[]{exchange.getPattern(),
                exchange.getStatus(), exchange.getExchangeId()};
            LOG.error("CRB000740_ConsumerUnsupportedMep", args);
            throw new Jbi4CorbaException("CRB000740_ConsumerUnsupportedMep",
                    args);
        }

        LOG.debug("<<<<< process - end");
    }

    /**
     * This method is called during the ExchanProcessor when the MEP is InOnly.
     *
     * @param exchange
     *            The MessageExchange.
     *
     * @throws Jbi4CorbaException
     *             If the ExchangeStatus is 'Active'.
     */
    protected void processInOnly(javax.jbi.messaging.MessageExchange exchange)
            throws Jbi4CorbaException {

        if (exchange.getStatus() == ExchangeStatus.DONE) {

            LOG.debug("PROCESS_OK[Pattern=InOnly; Status=Done]");

        } else if (exchange.getStatus() == ExchangeStatus.ERROR) {

            Object[] args = new Object[]{exchange.getExchangeId()};
            LOG.error("CRB000737_ExchangeProcessorConsumerInOnlyError", args);

        } else if (exchange.getStatus() == ExchangeStatus.ACTIVE) {

            Object[] args = new Object[]{"Active", exchange.getExchangeId()};
            LOG.error(
                    "CRB000738_ExchangeProcessorConsumerInOnlyUnexpectedStatus",
                    args);
            throw new Jbi4CorbaException(
                    "CRB000738_ExchangeProcessorConsumerInOnlyUnexpectedStatus",
                    args);
        }

    }

    /**
     * This method is called during the ExchanProcessor when the MEP is InOut.
     *
     * @param exchange
     *            The MessageExchange.
     *
     * @throws Jbi4CorbaException
     *             Always, because this MEP shoud not call the process.
     */
    protected void processInOut(javax.jbi.messaging.MessageExchange exchange)
            throws Jbi4CorbaException {

        Object[] args = new Object[]{exchange.getStatus(),
            exchange.getExchangeId()};
        LOG.error("CRB000739_UnexpectedExchangeProcessorForConsumerInOutMep",
                args);
        throw new Jbi4CorbaException(
                "CRB000739_UnexpectedExchangeProcessorForConsumerInOutMep",
                args);
    }

    /**
     * Converts the message to an input stream.
     *
     * @param src
     * @return
     * @throws IOException
     * @throws TransformerConfigurationException
     * @throws TransformerException
     */
    private static InputStream convertMessageToInputStream(Source src)
            throws IOException, TransformerConfigurationException,
            TransformerException {

        final Transformer transformer = TRANSFORMER_FACTORY.newTransformer();

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        StreamResult result = new StreamResult(baos);
        transformer.transform(src, result);

        return new ByteArrayInputStream(baos.toByteArray());
    }
}
