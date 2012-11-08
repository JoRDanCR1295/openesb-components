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
 * @(#)GrizzlyRequestProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import com.sun.jbi.httpsoapbc.HttpSoapComponentContext;
import com.sun.jbi.httpsoapbc.MessageExchangeSupport;
import com.sun.jbi.httpsoapbc.ReplyListener;
import com.sun.jbi.httpsoapbc.servletsupport.HttpServletDenormalizer;
import com.sun.jbi.httpsoapbc.servletsupport.HttpServletNormalizer;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.httpsoapbc.Normalizer;
import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.ReplyListener;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.InboundMessageProcessor;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.FaultException;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.enterprise.web.connector.grizzly.AsyncTask;
import com.sun.enterprise.web.connector.grizzly.ByteBufferStream;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.net.URI;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;


import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.component.ComponentLifeCycle;
import java.util.Set;


import org.apache.catalina.Connector;
import org.apache.coyote.Adapter;
import org.apache.coyote.Request;
import org.apache.coyote.Response;
import org.apache.coyote.http11.InternalInputBuffer;
import org.apache.coyote.http11.InternalOutputBuffer;
import org.apache.coyote.tomcat5.CoyoteConnector;
import org.apache.coyote.tomcat5.CoyoteRequest;
import org.apache.coyote.tomcat5.CoyoteResponse;


/**
 * Implementation of a coyote adapter to process HTTP requests asynchronously
 *
 */
public class GrizzlyRequestProcessor implements Adapter, ReplyListener {

    private static final Messages mMessages =
        Messages.getMessages(GrizzlyRequestProcessor.class);
    private final static Logger mLogger =
        Messages.getLogger(GrizzlyRequestProcessor.class);

    /**
     * Index into the requests and response notes
     */
    final static int ADAPTER_NOTES = 1;    
    
    /**
     * The CoyoteConnector with which this processor is associated.
     */
    private CoyoteConnector connector = null;

    /**
     * A mapping from the JBI message exchange ID to the request context
     */
    Map exchangeIDToContext = new java.util.concurrent.ConcurrentHashMap();

    HttpSoapBindingLifeCycle lifeCycle;    
    
    /** 
     * Creates a new instance 
     * @param connector CoyoteConnector that owns this processor
     */
    public GrizzlyRequestProcessor(CoyoteConnector connector) throws MessagingException {    
        this.connector = connector;
        initialize();
    }
    
    /** 
     * Initialize the request processor 
     */
    void initialize() throws MessagingException {
        lifeCycle = (HttpSoapBindingLifeCycle) HttpSoapComponentContext.getInstance().getAssociatedLifeCycle();
    }
    
    /**
     * Main entry point of the adapter to service a request
     * @param the incoming http request
     * @param the http response to prepare
     */
    public void service(Request req, Response res) {
        
        // Get the task associated with this request. This could be solved as a request note instead.
        AsyncTask asyncTask = JBIGrizzlyAsyncFilter.removeTaskMapping(req);
        if (mLogger.isLoggable(Level.FINEST)) {
            mLogger.log(Level.FINEST, "Got task mapping from request "
                    + req.toString() + ", asyncProcessorTask " + asyncTask);
        }
        int port = connector.getPort();
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Servicing async request for " + req.requestURI());
        }
                
        CoyoteRequest request = (CoyoteRequest) req.getNote(ADAPTER_NOTES);
        CoyoteResponse response = (CoyoteResponse) res.getNote(ADAPTER_NOTES);
        
        // TODO: we should be able to re-use the CoyoteRequest/CoyoteResponse instances
        //if (request == null) {
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Initializing servicing objects");
            }
            // Create objects
            request = (CoyoteRequest) connector.createRequest();
            request.setCoyoteRequest(req);
            response = (CoyoteResponse) connector.createResponse();
            response.setCoyoteResponse(res);

            // Link objects
            request.setResponse(response);
            response.setRequest(request);

            // Set as notes
            req.setNote(ADAPTER_NOTES, request);
            res.setNote(ADAPTER_NOTES, response);

            // Set query string encoding
            req.getParameters().setQueryStringEncoding(connector.getURIEncoding());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Query string encoding: " + connector.getURIEncoding());
            }
        //}

        // Prepare the request context
        Context currentContext = new Context();
        currentContext.port = port;
        currentContext.anInputBuffer = (InternalInputBuffer) req.getInputBuffer();
        currentContext.anOutputBuffer = (InternalOutputBuffer) res.getOutputBuffer();
        currentContext.req = req;        
        currentContext.res = res;
        currentContext.coyoteRequest = request;
        currentContext.coyoteResponse = response;
        currentContext.connector = connector;
        currentContext.asyncTask = asyncTask;
        
        // TODO: beware, request parsing does not always seem intuitive 
        //currentContext.contextPath = req.localName().toString();
        currentContext.contextPath = ""; 
        currentContext.pathInfo = req.requestURI().toString();

        // if the request is looking for the WSDL, don't process the actual message
        // just let the reply handler load the WSDL and return it.
        if ("WSDL".equalsIgnoreCase(request.getQueryString())) {
            processSynchronousReply(currentContext, null, null);
        } else {
            try {
                String exchangeID = processAsynchRequest(currentContext);
            } catch (Exception ex) {
                // Trigger an immediate reply if the request processing resulted in an exception
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Exception reported in process synchronous request-reply.", ex);
                }
                processSynchronousReply(currentContext, null, ex);
            }
        }
    }

    /**
     * @see Adapter
     */
    public void afterService(Request req, Response res) {
    }


    /** 
     * Process a HttpRequest and send a JBI request.
     * @param request embedded server request
     * @return JBI message exchange ID
     */
    public String processAsynchRequest(Context reqContext) throws FaultException, HttpException {

        String exchangeID = null;
        CoyoteRequest request = reqContext.coyoteRequest;
        String context = reqContext.contextPath + reqContext.pathInfo;
        int port = reqContext.port;

	Endpoint targetEndpoint = lifeCycle.getEndpointBeanForContext(context, port);
                
	if (targetEndpoint == null) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING,
                        "HTTPBC-W00652.Web_service_mapping_failed",
                        new Object[] { context, new Integer(port)});
            }

            // Send an HTTP 404 error
            throw new HttpException(CoyoteResponse.SC_NOT_FOUND,
                    mMessages.getString("HTTPBC-W00652.Web_service_mapping_failed",
                                        new Object[] {context, new Integer(port)}));
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Web service mapping found for the requested URL. " + context + " at port " + port);
            }

            try {                    
                InboundMessageProcessor anInboundProcessor = getProcessorSupport().inboundProcessor;
                anInboundProcessor.setInput(request);
                anInboundProcessor.setTargetEndpoint(targetEndpoint);
                exchangeID = anInboundProcessor.execute(reqContext);
            } catch (MessagingException ex) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, "HTTPBC-W00653.Exception_during_request_processing", ex);
                }
                throw new FaultException(ex);
            }
        }
        return exchangeID;
    }
    
    /**
     * The inbound message processor will call us back in execute() once it knows the message exchange for the request.
     * @see ReplyListener
     */
    public void setMessageExchangeId(String messageExchangeId, Object clientContext) {
        if (mLogger.isLoggable(Level.FINEST)) {
            mLogger.log(Level.FINEST, "setMessageExchangeId: " + messageExchangeId + ", clientContext: " + clientContext);
        }
        exchangeIDToContext.put(messageExchangeId, clientContext);        
    }    
    
    
    public void setMessageContextForCallback(Object obj1, Object obj2) {
        // do nothing
    }
    
    /**
     * Removes a message exchange ID and its associated call back context
     * @see ReplyListener
     */
    public void removeMessageExchangeId(String messageExchangeId) {
        exchangeIDToContext.remove(messageExchangeId);
    }    
    
    /**
     * Handle the reply available from JBI.
     */
    public void onReply(MessageExchange exchange) throws MessagingException {    

        // MEP is complete, we do not expect any further replies. Remove from MessageExchangeSupport.
        MessageExchangeSupport.removeReplyListener(exchange.getExchangeId());
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Got reply message exchange " + exchange.getExchangeId());
        }
        Context context = (Context) exchangeIDToContext.remove(exchange.getExchangeId());
        if (mLogger.isLoggable(Level.FINEST)) {
            StringBuffer idsStr = new StringBuffer();
            Set s = exchangeIDToContext.keySet();
            if (s.size() == 0) {
                idsStr.append("none");
            } else {
                for (Object key : s) {
                    idsStr.append(key.toString()).append(" ");
                }
            }
            mLogger.log(Level.FINEST, "Outstanding exchanges: " + idsStr.toString());
        }

        try {
            processAsynchReply(context, exchange, null);
        } catch (RuntimeException ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00654.Exception_during_reply_processing"),
                                         ex);            
        } finally {
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Finishing response");
            }
            JBIGrizzlyAsyncFilter.finishResponse(context.asyncTask);

            /* This clean-up help may affect coyote/grizzly. Disable for now.
            // As currently we can not re-use these, help in cleaning up request and response
            if (context != null && context.coyoteRequest != null ) {
                if (context.req != null) {
                    context.req.setNote(ADAPTER_NOTES, null);            
                    context.req.recycle();
                    context.req = null;
                }
                if (context.res != null) {
                    context.res.setNote(ADAPTER_NOTES, null);
                    context.res.recycle();
                    context.res = null;
                }
                // remove linking
                if (context.coyoteRequest != null) {
                    context.coyoteRequest.setResponse(null);
                    context.coyoteRequest.setCoyoteRequest(null);
                    context.coyoteRequest.recycle();
                    context.coyoteRequest = null;
                }
                if (context.coyoteResponse != null) {
                    context.coyoteResponse.setRequest(null);
                    context.coyoteResponse.setCoyoteResponse(null);
                    context.coyoteResponse.recycle();
                    context.coyoteResponse = null;
                }
            } 
            */
        }
    }    

    /**
     * Reply synchronously in this service() invocation. 
     * This is useful for responding with errors and any other exchanges where no 
     * asynchronous exchange with the JBI NMR will occur.
     */
    public void processSynchronousReply(Context reqContext, MessageExchange exchange, Exception requestFailedException) {    
            processAsynchReply(reqContext, exchange, requestFailedException);
            JBIGrizzlyAsyncFilter.finishResponseSynchronously(reqContext.asyncTask);
    }
    
    /**
     * Process a JBI reply and prepare an HttpResponse
     * @param reqContext the original request context
     * @param exchange the JBI message exchange which has a reply available. Maybe null if requestFailedException is not null.
     * @param requestFailedException The exception that occurred in the processAsynchRequest phase.
     */
    public void processAsynchReply(Context reqContext, MessageExchange exchange, Exception requestFailedException) {    

        CoyoteResponse response = reqContext.coyoteResponse;
        try {
            CoyoteRequest request = reqContext.coyoteRequest;
            String context = reqContext.contextPath + reqContext.pathInfo;
            
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Lifecycle: "
                        + lifeCycle + ", context path: " + context
                        + ", request context: " + reqContext);
            }
            Endpoint targetEndpoint = lifeCycle.getEndpointBeanForContext(context, reqContext.port);
            OperationMetaData operationMetaData = null;

            // SOAP 1.1 we'll set it as text/xml
            // TODO: for SOAP 1.2, content type will have to be application/soap+xml
            response.setContentType("text/xml");

            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Request query string: " + request.getQueryString());
            }
            
            // if user is looking for the wsdl file, read it from disk and return it synchronously
            if (targetEndpoint != null && "WSDL".equalsIgnoreCase(request.getQueryString())) {
                try {
                    ByteBuffer mbb = targetEndpoint.getServiceDescriptorAsByteBuffer();
                    java.io.OutputStream os = response.getOutputStream();                            
                    java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(mbb);
                    os.flush();
                    response.setStatus(CoyoteResponse.SC_OK);
                } catch (Exception e) {
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, "HTTPBC-W00651.WSDL_retrieval_exception", e);
                    }
                    // Reply with http error
                    response.setStatus(CoyoteResponse.SC_INTERNAL_SERVER_ERROR);
                }

                return;
            }

            String pat = null;
            String operation = null;
            if (exchange != null) {
                URI pattern = exchange.getPattern();
                if (mLogger.isLoggable(Level.FINEST)) {
                    mLogger.log(Level.FINEST, "Pattern for exchange id "
                            + exchange.getExchangeId() + " is " + pattern);
                }
                pat = pattern.toString().trim();
                operation = exchange.getOperation().getLocalPart();
            } else {
                // If the exchange is null make sure there is an exception reported.
                if (requestFailedException == null) {
                    requestFailedException = new MessagingException("Null message exchange");
                }
            }

            // Get the operation meta data if available
            if (targetEndpoint != null) {
                Map nameToMeta = targetEndpoint.getOperationNameToMetaData();
                operationMetaData = (OperationMetaData) nameToMeta.get(operation);
            }

            if (targetEndpoint == null || exchange == null || ExchangePattern.isInOut(exchange)) {
                try { 
                    if (requestFailedException != null) {
                        // Process a failure in processing the request
                        if (requestFailedException instanceof HttpException) {
                            
                            // Temporary Support for retrieving resources for WSDLs retrieved via ?WSDL that have relative imports
                            // If an address context is not unique (e.g. multiple endpoints are deployed under /service that import resources with the same name), 
                            // this will simply return the first match it can find.
                            ByteBuffer resource = lifeCycle.queryResource(context, targetEndpoint);
                            if (resource != null) {
                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.log(Level.FINE, "Request context "
                                            + context + " at port " + reqContext.port
                                            + " mapped to resource " + resource);
                                }
                                ByteBuffer mbb = resource;
                                java.io.OutputStream os = response.getOutputStream();                            
                                java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                                channel.write(mbb);
                                os.flush();
                                response.setStatus(CoyoteResponse.SC_OK);
                            } else {
                                response.setStatus(((HttpException) requestFailedException).getErrorCode());
                            }

                        } else {
                            Denormalizer aDenormalizer = getProcessorSupport().denormalizer;
                            response = (CoyoteResponse) aDenormalizer.denormalizeException(requestFailedException, response);
                            int statusCode = CoyoteResponse.SC_INTERNAL_SERVER_ERROR;
                            response.setStatus(statusCode);
                        }

                    } else {

                        if (operationMetaData == null) {
                            throw new MessagingException(mMessages.getString("HTTPBC-E00667.No_opmeta_for_operation", operation));
                        }

                        if (exchange.getError() != null) {
                            Denormalizer aDenormalizer = getProcessorSupport().denormalizer;
                            response = (CoyoteResponse) aDenormalizer.denormalizeError(exchange, response);
                        } else {
                            NormalizedMessage outMsg = null;

                            if (exchange.getFault() != null) {
                                // TODO: check that a message exchange fault can be used                       
                                // the same way as an output message!
                                Fault aFault = exchange.getFault();                    
                                outMsg = aFault;
                            } else {
                                if (exchange instanceof InOut) {
                                    InOut inout = (InOut) exchange;
                                    outMsg = inout.getOutMessage();
                                }
                            }

                            if (mLogger.isLoggable(Level.FINE)) {
                                if (outMsg != null) {
                                    DebugLog.debugLog(mLogger, Level.FINE, "Denormalizing received msg", outMsg.getContent());
                                } else {
                                    mLogger.log(Level.FINE, "Message received is empty");
                                }
                            }
                            Denormalizer aDenormalizer = getProcessorSupport().denormalizer;
                            response = (CoyoteResponse) aDenormalizer.denormalize(outMsg, exchange, response, operationMetaData);
                        }
                    }
                } catch (Throwable ex) {
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, "HTTPBC-W00654.Exception_during_reply_processing",ex);
                    }
                    // Reply with fault.
                    Denormalizer aDenormalizer = getProcessorSupport().denormalizer;
                    response = (CoyoteResponse) aDenormalizer.denormalizeException(ex, response);
                }                
            } else if (ExchangePattern.isInOnly(exchange)) {
                int statusCode = CoyoteResponse.SC_ACCEPTED; // one-way should send accepted 202 - not OK 200
                if (requestFailedException != null) {
                    if (requestFailedException instanceof HttpException) {
                        statusCode = ((HttpException) requestFailedException).getErrorCode();
                    } else {
                        mLogger.log(Level.SEVERE, "HTTPBC-E00653.Exception_during_request_processing",
                                    requestFailedException);
                        statusCode = CoyoteResponse.SC_INTERNAL_SERVER_ERROR;
                    }
                } else {
                    // If the SE does not report a successful 'transmission' (for in-only exchange status DONE) 
                    // respond with an http error
                    if (exchange.getStatus().equals(ExchangeStatus.ERROR)) {
                        mLogger.log(Level.SEVERE, "HTTPBC-E00653.Exception_during_request_processing");
                        statusCode = CoyoteResponse.SC_INTERNAL_SERVER_ERROR;
                    }
                }
                response.setStatus(statusCode);
            } else {
                mLogger.log(Level.SEVERE,"HTTPBC-E00668.Unsupported_message_type", pat);
                int statusCode = CoyoteResponse.SC_INTERNAL_SERVER_ERROR;
                response.setStatus(statusCode);
            }
        } catch (Throwable ex) {
            // Make sure that no exceptions get propagated to the embedded server, this might terminate the server
            mLogger.log(Level.SEVERE, "HTTPBC-E00654.Exception_during_reply_processing", ex);
            int statusCode = CoyoteResponse.SC_INTERNAL_SERVER_ERROR;
            response.setStatus(statusCode);
        } finally {
            try {
                response.finishResponse();            
            } catch (IOException ex) {
                mLogger.log(Level.SEVERE, "HTTPBC-E00654.Exception_during_reply_processing", ex);
            }

            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Wrote response");
            }
        }
    }    
    
    // START SJSAS 6349248
    /**
     * Not supported by this adapter implementation.
     *
     * Notify all container event listeners that a particular event has
     * occurred for this Adapter.  The default implementation performs
     * this notification synchronously using the calling thread.
     *
     * @param type Event type
     * @param data Event data
     */
    public void fireAdapterEvent(String type, Object data) {
        mLogger.log(Level.FINE, "Not supported by this implementation");
    }
    // END SJSAS 6349248    

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
            currentProcSupport.normalizer = new HttpServletNormalizer();
            currentProcSupport.denormalizer = new HttpServletDenormalizer();        
            currentProcSupport.inboundProcessor = new InboundMessageProcessor(currentProcSupport.normalizer, this);
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

    /**
     * Holds request context information
     */
    static class Context {
        int port;
        InternalInputBuffer anInputBuffer;
        InternalOutputBuffer anOutputBuffer;
        Request req;
        Response res;
        CoyoteRequest coyoteRequest;
        CoyoteResponse coyoteResponse;
        Connector connector;
        String contextPath;
        String pathInfo;
        AsyncTask asyncTask;
    }

}
