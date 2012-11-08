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
 * @(#)JAXWSGrizzlyRequestProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.security.auth.Subject;

import org.apache.catalina.Connector;
import org.apache.coyote.Adapter;
import org.apache.coyote.Request;
import org.apache.coyote.Response;
import org.apache.coyote.http11.InternalInputBuffer;
import org.apache.coyote.http11.InternalOutputBuffer;
import org.apache.coyote.tomcat5.CoyoteConnector;
import org.apache.coyote.tomcat5.CoyoteRequest;
import org.apache.coyote.tomcat5.CoyoteResponse;

import com.sun.enterprise.web.connector.grizzly.AsyncTask;
import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.FaultException;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.HttpSoapComponentContext;
import com.sun.jbi.httpsoapbc.Normalizer;
import com.sun.jbi.httpsoapbc.WsdlQueryHelper;
import com.sun.jbi.httpsoapbc.security.api.HttpBcSecurityException;
import com.sun.jbi.httpsoapbc.servletsupport.HttpServletDenormalizer;
import com.sun.jbi.httpsoapbc.servletsupport.HttpServletNormalizer;
import com.sun.jbi.internationalization.Messages;
import com.sun.xml.ws.api.server.WSEndpoint;
import com.sun.xml.ws.transport.http.HttpAdapter;


/**
 * Implementation of a coyote adapter to process HTTP requests asynchronously
 *
 * This file uses the API in Glassfish / Grizzly 9.1 
 *
 * The invocation pipeline when the <code>EmbeddedServerController</code> sets this 
 * request processor looks as follows:
 *
 * -> Grizzly subsystem with ARP (asynchronous request processing) enabled
 *     -> ** JBIGrizzlyAsyncFilter doFilter
 *         -> Grizzly ProcessorTask invokeAdapter
 *             -> ** JAXWSGrizzlyRequestProcessor service (this is our Adapter implementation for Grizzly)
 *                 -> JAX-WS HttpAdapter invokeAsync (invokes the JAX-WS WSEndpoint asynchronously)
 *                     -> JAX-WS tube / pipeline plug-ins
 *                         -> ** AsyncJBIProvider 
 *                             ->  ** InboundMessageProcessor
 *                                 -> NMR
 *
 * The classes marked with ** are implemented by this BC
 *
 */
public class JAXWSGrizzlyRequestProcessor implements Adapter {


    private static final Messages mMessages =
        Messages.getMessages(JAXWSGrizzlyRequestProcessor.class);
    private final static Logger mLogger =
        Messages.getLogger(JAXWSGrizzlyRequestProcessor.class);

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
    public JAXWSGrizzlyRequestProcessor(CoyoteConnector connector) throws MessagingException {    
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
        
        /* Debug
        try {
            org.apache.tomcat.util.buf.ByteChunk bc = new org.apache.tomcat.util.buf.ByteChunk();
            req.doRead(bc);
            String bufStr = new String(bc.getBuffer());
            mLogger.fine("####debug service: buffer is:");
            mLogger.fine(bufStr);
        } catch(Exception ex){
            
        }
        */
                
        // Get the task associated with this request. This could be solved as a request note instead.
        AsyncTask asyncTask = JBIGrizzlyAsyncFilter.removeTaskMapping(req);
        if (mLogger.isLoggable(Level.FINEST)) {
            mLogger.log(Level.FINEST, "Got task mapping for request " + req.toString() + ", asyncProcessorTask " + asyncTask);
        }
        int port = connector.getPort();
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Service async request for: " + req.requestURI());
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
        // There are 3 types of requests we handle here
        //   1. An actual message
        //   2. ?WSDL
        //   3. Querying resources like XSD & WSDL
        
        String context = currentContext.contextPath + currentContext.pathInfo; 
        if ("WSDL".equalsIgnoreCase(request.getQueryString())) {
            processSynchronousQueryResource(currentContext);
        } else if (context.toLowerCase().endsWith(".xsd") || context.toLowerCase().endsWith(".wsdl")) {
            processSynchronousQueryResource(currentContext);
        } else {
            try {
                processAsynchRequest(currentContext);
            } catch (Exception ex) {
                // Trigger an immediate reply if the request processing resulted in an exception
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Exception reported in processing synchronous request-reply", ex);
                }
                processSynchronousReply(currentContext, ex);
            }
        }
    }

    /**
     * @see Adapter
     */
    public void afterService(Request req, Response res) {
    }

    public void processSynchronousQueryResource(Context reqContext) {
        CoyoteResponse response = reqContext.coyoteResponse;
        try {
            CoyoteRequest request = reqContext.coyoteRequest;
            String context = reqContext.contextPath + reqContext.pathInfo;
            int port = reqContext.port;
            
            response.setContentType("text/xml");

            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Request query string: " + request.getQueryString());
            }
            
	    
            // if user is looking for the wsdl file, read it from disk and return it synchronously
            if ("WSDL".equalsIgnoreCase(request.getQueryString())) {
    	        Endpoint targetEndpoint = lifeCycle.getEndpointBeanForContext(context, port);
    	        if (targetEndpoint == null) {
    	            ByteBuffer bb = ByteBuffer.wrap("Unable to locate the service endpoint for this soap address".getBytes());
                    java.io.OutputStream os = response.getOutputStream();          
    	            java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(bb);
                    os.flush();
    	            response.setStatus(CoyoteResponse.SC_SERVICE_UNAVAILABLE);
                    response.finishResponse();      
                    JBIGrizzlyAsyncFilter.finishResponseSynchronously(reqContext.asyncTask);
    	            return;
    	        }
    	        
    	        if (!targetEndpoint.getEnableWsdlQuery()) {
    	            if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-W00656.Wsdl_query_unavailable"));
                    }
                    ByteBuffer bb = ByteBuffer.wrap("?WSDL query is disabled for this soap address".getBytes());
                    java.io.OutputStream os = response.getOutputStream();             
    	            java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(bb);
                    os.flush();
    	            response.setStatus(CoyoteResponse.SC_OK);
    	            response.finishResponse();
                    JBIGrizzlyAsyncFilter.finishResponseSynchronously(reqContext.asyncTask);
    	            return;
    	        }
    	        
                try {
                    
                    WsdlQueryHelper helper = new WsdlQueryHelper(request,port,targetEndpoint.getServiceDescriptorAsByteBuffer(),true);
                    //ByteBuffer mbb = targetEndpoint.getServiceDescriptorAsByteBuffer();
                    ByteBuffer mbb = helper.getServiceDescriptorAsByteBuffer();
                    java.io.OutputStream os = response.getOutputStream();                            
                    java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(mbb);
                    os.flush();
                    response.setStatus(CoyoteResponse.SC_OK);
                } catch (Exception e) {
                    mLogger.log(Level.WARNING, "HTTPBC-W00651.WSDL_retrieval_exception", e);
                    // Reply with http error
                    response.setStatus(CoyoteResponse.SC_INTERNAL_SERVER_ERROR);
                }

                
            } else {
                // Temporary Support for retrieving resources for WSDLs retrieved via ?WSDL that have relative imports
                // If an address context is not unique (e.g. multiple endpoints are deployed under /service that import resources with the same name), 
                // this will simply return the first match it can find.
                ByteBuffer resource = lifeCycle.queryResource(context, null);
                
                if (resource != null) {
                    
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Request context "
                                + context + " at port " + reqContext.port
                                + " mapped to resource " + resource);
                    }
                    boolean b = (context.toLowerCase().endsWith(".wsdl"));
                    WsdlQueryHelper helper = new WsdlQueryHelper(request,port,resource,b);
                    
                    //ByteBuffer mbb = resource;
                    ByteBuffer mbb = helper.getServiceDescriptorAsByteBuffer();
                    java.io.OutputStream os = response.getOutputStream();                            
                    java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(mbb);
                    os.flush();
                    response.setStatus(CoyoteResponse.SC_OK);
                } else {
                    mLogger.log(Level.SEVERE, "HTTPBC-S00670.Resource_not_available", context);
                    response.setStatus(CoyoteResponse.SC_INTERNAL_SERVER_ERROR);
                }

            }

        } catch (Throwable ex) {
            // Make sure that no exceptions get propagated to the embedded server, this might terminate the server
            mLogger.log(Level.SEVERE, "HTTPBC-E00669.Exception_during_query", ex);
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
        JBIGrizzlyAsyncFilter.finishResponseSynchronously(reqContext.asyncTask);
    }
    /** 
     * Process a HttpRequest and send a JBI request.
     * @param request embedded server request
     * @return JBI message exchange ID
     */
    public void processAsynchRequest(Context reqContext) throws FaultException, HttpException {

        String exchangeID = null;
        CoyoteRequest request = reqContext.coyoteRequest;
        String context = reqContext.contextPath + reqContext.pathInfo;
        int port = reqContext.port;

	Endpoint targetEndpoint = lifeCycle.getEndpointBeanForContext(context, port);
                
	
	
	Subject subj=null;
	
	if (targetEndpoint == null) {
            mLogger.log(Level.WARNING, "HTTPBC-W00652.Web_service_mapping_failed", new Object[] { context, new Integer(port)});

            // Send an HTTP 404 error
            throw new HttpException(CoyoteResponse.SC_NOT_FOUND,
                                    mMessages.getString("HTTPBC-W00652.Web_service_mapping_failed",
                                                       new Object[] {context, new Integer(port)}));
        } else {
            if(targetEndpoint.isInbound() && targetEndpoint.isBasicAuthenticationEnabled()) {
                String authHeader = request.getHeader("authorization");
                if(authHeader == null) {
                    reqContext.coyoteResponse.setHeader("WWW-Authenticate", "Basic realm=\"ANY\"");
                    reqContext.coyoteResponse.setStatus(CoyoteResponse.SC_UNAUTHORIZED);
                    throw new HttpException(401, "Unauthorized");
                } else {
                    try {
			subj = targetEndpoint.handleSecurity(authHeader);
			if (subj != null && !subj.getPrincipals().isEmpty()) {
			    reqContext.coyoteRequest.setUserPrincipal(subj.getPrincipals().iterator().next());
			}
		    } catch (HttpBcSecurityException se) {
			mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-E01036.Authentication_failed", new Object[] { se.getLocalizedMessage() }), se);
			reqContext.coyoteResponse.setStatus(CoyoteResponse.SC_FORBIDDEN);
			throw new HttpException(403, "Forbidden");

		    }
                }
            }

            // TODO: can we re-use this connection?
            JAXWSGrizzlyHttpConnection con = new JAXWSGrizzlyHttpConnection(reqContext.req, reqContext.res, reqContext.coyoteRequest, reqContext.coyoteResponse, reqContext.asyncTask, connector.getSecure());
            if(subj!=null){
        	con.setBasicAuthSubject(subj);
            }
            
            
            WSEndpoint wsEndpoint = targetEndpoint.getWSEndpoint();
            // TODO: don't re-create the httpadapter each time
            HttpAdapter httpAdapter = HttpAdapter.createAlone(wsEndpoint);
            
            try {
                httpAdapter.invokeAsync(con);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Completed async invoke");
                }
            } catch (IOException ex) {
                throw new FaultException(new MessagingException(mMessages.getString("HTTPBC-W00653.Exception_during_request_processing"), ex));
            }

        }
    }

    /**
     * Reply synchronously in this service() invocation. 
     * This is useful for responding with errors and any other exchanges where no 
     * asynchronous exchange with the JBI NMR will occur.
     */
    public void processSynchronousReply(Context reqContext, Exception requestFailedException) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Replying synchronously.  The request failed. Here's the exception.", requestFailedException);      
        }
            processAsynchReply(reqContext, requestFailedException);
            JBIGrizzlyAsyncFilter.finishResponseSynchronously(reqContext.asyncTask);
    }
    
    /**
     * Rreply and prepare an HttpResponse, when using JAX-WS this means before it reached the JAX-WS tube.
     * Typically this means an exception happened where we could not hand it to JAX-WS
     *
     * @param reqContext the original request context
     * @param requestFailedException The exception that occurred in the processAsynchRequest phase.
     */
    public void processAsynchReply(Context reqContext, Exception requestFailedException) {    

        CoyoteResponse response = reqContext.coyoteResponse;
        try {
            CoyoteRequest request = reqContext.coyoteRequest;

            // SOAP 1.1 we'll set it as text/xml
            // for SOAP 1.2, content type would have to be application/soap+xml
            response.setContentType("text/xml");

            if (requestFailedException != null) {
                // Process a failure in processing the request
                if (requestFailedException instanceof HttpException) {
                    response.setStatus(((HttpException) requestFailedException).getErrorCode());
                } else {
                    // TODO: it may not be worth keeping the whole ProcessorSupport just for this handling of an exception
                    Denormalizer aDenormalizer = getProcessorSupport().denormalizer;
                    response = (CoyoteResponse) aDenormalizer.denormalizeException(requestFailedException, response);
                    int statusCode = CoyoteResponse.SC_INTERNAL_SERVER_ERROR;
                    response.setStatus(statusCode);
                }
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
            //currentProcSupport.inboundProcessor = new InboundMessageProcessor(currentProcSupport.normalizer, this);
            processorSupport.set(currentProcSupport);
        }    
        return currentProcSupport;
    }
    /**
     * Holds instances that are not thread safe
     * Note that this is not suitable when using shared thread pools, only if a limited
     * number of threads are used by this component only
     */    
    private static ThreadLocal processorSupport = new ThreadLocal();
    
    /**
     * Holds instances that are not thread safe
     */
    static class ProcessorSupport {
        Normalizer normalizer;
        Denormalizer denormalizer;
        //InboundMessageProcessor inboundProcessor;        
    }

    /**
     * Holds request context information
     */
    public static class Context {
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
