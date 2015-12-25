/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)JBIProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.FaultException;
import com.sun.jbi.httpsoapbc.InboundMessageProcessor;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.HttpSoapComponentContext;
import com.sun.jbi.httpsoapbc.Normalizer;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.SynchronousReplySupport;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

import javax.annotation.Resource;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.soap.AttachmentPart;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.Provider;
import javax.xml.ws.Service;
import javax.xml.ws.ServiceMode;
import javax.xml.ws.WebServiceProvider;
import javax.xml.ws.WebServiceContext;
import javax.xml.ws.handler.MessageContext;

/**
 * JAX-WS Provider for use in Java SE
 *
 * @author aegloff
 */
@WebServiceProvider
@ServiceMode(Service.Mode.MESSAGE)
public class JBIProvider implements Provider<SOAPMessage> {

    @Resource
    protected WebServiceContext wsContext;

// TODO: i18n for this class    
    private static final Messages mMessages =
        Messages.getMessages(JBIProvider.class);
    private static final Logger mLogger =
        Messages.getLogger(JBIProvider.class);    
    
    private Endpoint targetEndpoint;
    private HttpSoapBindingLifeCycle lifeCycle = null;
    private SynchronousReplySupport replyListener = new SynchronousReplySupport();

    public JBIProvider(Endpoint endpoint) {
        targetEndpoint = endpoint;
        lifeCycle = (HttpSoapBindingLifeCycle) HttpSoapComponentContext.getInstance().getAssociatedLifeCycle();
        replyListener = new SynchronousReplySupport();
    }    

    /**
     * Ptocess the Provider invoke
     * @param request the Provider reqeust
     * @return the provider response
     */
    public SOAPMessage invoke(SOAPMessage request) {        
        // TODO: reduce and remove logging after prototyping
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Processing SOAPMessage received.");
            mLogger.log(Level.FINE, "WebServiceContext: " + wsContext);
        }
        SOAPMessage response = null;

        /* 
        // TESTs of the message context
        MessageContext mc = wsContext.getMessageContext();        
        //Map requestHeaders = (Map) context.get(MessageContext.HTTP_REQUEST_HEADERS) ;
        mLogger.fine("MessageContext: " + mc);
        String path = (String)mc.get(MessageContext.PATH_INFO);        
        mLogger.fine("MessageContext PATH_INFO: " + path);
        String context = path;
        javax.servlet.http.HttpServletRequest servletReq = (javax.servlet.http.HttpServletRequest) mc.get(MessageContext.SERVLET_REQUEST);                
        if (servletReq != null) {
            mLogger.fine("MessageContext SERVLET_REQUEST contectpath:" + servletReq.getContextPath());
            mLogger.fine("MessageContext SERVLET_REQUEST pathinfo:" + servletReq.getPathInfo());
            context = servletReq.getContextPath() + servletReq.getPathInfo();
            mLogger.fine("MessageContext SERVLET_REQUEST port:" + servletReq.getServerPort());
            port = servletReq.getServerPort();
        } else {
            mLogger.fine("MessageContext SERVLET_REQUEST returned null");
        }
        */
                
        try {
            
            try {                    
                InboundMessageProcessor anInboundProcessor = getProcessorSupport().inboundProcessor;
                anInboundProcessor.setInput(request);
                anInboundProcessor.setTargetEndpoint(targetEndpoint);
                String exchangeID = anInboundProcessor.execute(null);
                
                // TODO: does this only makes sense for InOut ?
                MessageExchange exchange = replyListener.waitForReply();
        
                NormalizedMessage outMsg = null;
  
                if (exchange instanceof InOut) {
                    InOut inout = (InOut) exchange;
                    outMsg = inout.getOutMessage();
                
                    Map nameToMeta = targetEndpoint.getOperationNameToMetaData();
                    String operation = exchange.getOperation().getLocalPart();
                    OperationMetaData operationMetaData = (OperationMetaData) nameToMeta.get(operation);
                    if (operationMetaData == null) {
                        // TODO: i18n                        
                        throw new MessagingException(mMessages.getString("HTTPBC-E00667.No_opmeta_for_operation", exchange.getOperation()));
                    }

                    Denormalizer denormalizer = getProcessorSupport().denormalizer;
                    response = (SOAPMessage) denormalizer.denormalize(outMsg, exchange, response, operationMetaData);
                }                    
                // TODO: ELSE handle status to report HTTP status errors
                
            } catch (MessagingException ex) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, "HTTPBC-W00625.Exception_during_request_processing", ex);
                }
                throw new FaultException(ex);
                // TODO: tranform to soap fault instead
            }            
            
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "response: " + response);            
            }
            
            
// WORK-AROUND start for SAAJ NPE. 
            javax.xml.transform.Transformer transformer = null;
            try {
                javax.xml.transform.TransformerFactory fact = javax.xml.transform.TransformerFactory.newInstance();
                transformer = fact.newTransformer();
            } catch (Exception ex) {
                mLogger.log(Level.SEVERE, "failed", ex);
            }

            MessageFactory testFactory = MessageFactory.newInstance();
            SOAPMessage testMsg = testFactory.createMessage();
            //testMsg.getSOAPPart().setContent(new javax.xml.transform.stream.StreamSource(new java.io.StringReader(String.format(sm, "10"))));
            java.io.StringWriter writer = new java.io.StringWriter();
            javax.xml.transform.stream.StreamResult dest = new javax.xml.transform.stream.StreamResult(writer);

            try {
                transformer.transform(response.getSOAPPart().getContent(), dest);
            } catch (Exception ex) {
                mLogger.log(Level.SEVERE, "transform failed", ex);
            }

            String asStr = writer.toString();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Transformed: " + asStr);  
            }
            testMsg.getSOAPPart().setContent(new javax.xml.transform.stream.StreamSource(new java.io.StringReader(asStr))); 
// WORK-AROUND end for SAAJ NPE

            
            return testMsg;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

//TODO: BEST TO REMOVE ALL THREAD LOCAL ARTIFACTS    
    
    /**
     * Get the thread specific processor support
     * Beware: Do not use the processor support instances in a different thread than 
     * the one calling getProcessorSupport. 
     */
    ProcessorSupport getProcessorSupport() throws MessagingException {
        // Get the processor support instances associated with the thread if present, create if not.
        ProcessorSupport currentProcSupport = (ProcessorSupport) processorSupport.get();
        if (currentProcSupport == null) {
            currentProcSupport = new ProcessorSupport();
            currentProcSupport.normalizer = new JAXWSNormalizer();
            currentProcSupport.denormalizer = new JAXWSDenormalizer();
            currentProcSupport.inboundProcessor = new InboundMessageProcessor(currentProcSupport.normalizer, replyListener);
            processorSupport.set(currentProcSupport);
        }
        return currentProcSupport;
    }
    
    /**
     * Holds instances that are not thread safe
     */    
    private static ThreadLocal processorSupport = new ThreadLocal();

    /**
     * Holds instances that are not thread safe
     */
    static class ProcessorSupport {
        Normalizer normalizer;
        Denormalizer denormalizer;
        InboundMessageProcessor inboundProcessor;        
    }    
    
}



    
