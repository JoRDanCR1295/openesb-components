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
import org.glassfish.grizzly.http.io.InputBuffer;
import org.glassfish.grizzly.http.io.OutputBuffer;
import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.NetworkListener;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.Response;
import org.glassfish.grizzly.http.util.HttpStatus;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Implementation of a coyote adapter to process HTTP requests asynchronously
 *
 * This file uses the API in Grizzly 2
 *
 * The invocation pipeline when the <code>EmbeddedServerController</code>
 * sets this request processor looks as follows:
 * -> Grizzly subsystem with ARP (asynchronous request processing) enabled 
 *      -> JBIGrizzlyAsyncFilter doFilter -> Grizzly ProcessorTask invokeAdapter 
 *          -> JAXWSGrizzlyRequestProcessor service (this is our Adapter implementation for Grizzly)
 *              -> JAX-WS HttpAdapter invokeAsync (invokes the JAX-WS WSEndpoint asynchronously)
 *                  -> JAX-WS tube / pipeline plug-ins 
 *                      -> AsyncJBIProvider
 *                          -> InboundMessageProcessor
 *                              -> NMR
 *
 * The classes marked with ** are implemented by this BC
 *
 */
public class JAXWSGrizzlyRequestProcessor extends HttpHandler {

    private static final Messages mMessages =
            Messages.getMessages(JAXWSGrizzlyRequestProcessor.class);
    private final static Logger mLogger =
            Messages.getLogger(JAXWSGrizzlyRequestProcessor.class);
    /**
     * Index into the requests and response notes
     */
    final static int ADAPTER_NOTES = 1;
    /**
     * The NetworkListener with which this processor is associated.
     */
    private NetworkListener listener = null;
    /**
     * A mapping from the JBI message exchange ID to the request context
     */
    Map exchangeIDToContext = new java.util.concurrent.ConcurrentHashMap();
    HttpSoapBindingLifeCycle lifeCycle;

    private static Map<WSEndpoint, HttpAdapter> httpAdapterMap = new ConcurrentHashMap<WSEndpoint, HttpAdapter>();
    
    
    /** 
     * Creates a new instance 
     * @param connector CoyoteConnector that owns this processor
     */
    public JAXWSGrizzlyRequestProcessor(NetworkListener listener) throws MessagingException {
        this.listener = listener;
        initialize();
    }

    /**
     * Initialize the request processor
     */
    private void initialize() throws MessagingException {
        lifeCycle = (HttpSoapBindingLifeCycle) HttpSoapComponentContext.getInstance().getAssociatedLifeCycle();
    }

    /**
     * Main entry point of the adapter to service a request
     *
     * @param the incoming http request
     * @param the http response to prepare
     */
    public void service(Request request, Response response) {

        // Get the task associated with this request. This could be solved as a request note instead.
        /*
        AsyncTask asyncTask = JBIGrizzlyAsyncFilter.removeTaskMapping(request);
        if (mLogger.isLoggable(Level.FINEST)) {
            mLogger.log(Level.FINEST, "Got task mapping for request " + request.toString() + ", asyncProcessorTask " + asyncTask);
        }
        */

        int port = listener.getPort();

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Service async request for: {0}", request.getRequestURI());
        }

        // TODO: we should be able to re-use the CoyoteRequest/CoyoteResponse instances
        if (mLogger.isLoggable(Level.FINEST)) {
            mLogger.log(Level.FINEST, "Initializing servicing objects");
        }

        // Prepare the request context
        Context currentContext = new Context();
        
        currentContext.port = port;
        currentContext.anInputBuffer = request.getInputBuffer();
        currentContext.anOutputBuffer = response.getOutputBuffer();
        currentContext.request = request;
        currentContext.response = response;
        currentContext.listener = listener;

        // TODO: beware, request parsing does not always seem intuitive 
        //currentContext.contextPath = req.localName().toString();
        currentContext.contextPath = "";
        currentContext.pathInfo = request.getRequestURI().toString();

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
        Response response = reqContext.response;
        try {
            String context = reqContext.contextPath + reqContext.pathInfo;
            int port = reqContext.port;

            response.setContentType("text/xml");

            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Request query string: {0}", reqContext.request.getQueryString());
            }

            // if user is looking for the wsdl file, read it from disk and return it synchronously
            if ("WSDL".equalsIgnoreCase(reqContext.request.getQueryString())) {
                Endpoint targetEndpoint = lifeCycle.getEndpointBeanForContext(context, port);
                if (targetEndpoint == null) {
                    ByteBuffer bb = ByteBuffer.wrap("Unable to locate the service endpoint for this soap address".getBytes());
                    java.io.OutputStream os = response.getOutputStream();
                    java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(bb);
                    os.flush();
                    response.setStatus(HttpStatus.SERVICE_UNAVAILABLE_503);
                    response.finish();
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
                    response.setStatus(HttpStatus.OK_200);
                    response.finish();
                    return;
                }

                try {

                    WsdlQueryHelper helper = new WsdlQueryHelper(reqContext.request, port, targetEndpoint.getServiceDescriptorAsByteBuffer(), true);
                    //ByteBuffer mbb = targetEndpoint.getServiceDescriptorAsByteBuffer();
                    ByteBuffer mbb = helper.getServiceDescriptorAsByteBuffer();
                    java.io.OutputStream os = response.getOutputStream();
                    java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(mbb);
                    os.flush();
                    response.setStatus(HttpStatus.OK_200);
                } catch (Exception e) {
                    mLogger.log(Level.WARNING, "HTTPBC-W00651.WSDL_retrieval_exception", e);
                    // Reply with http error
                    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500);
                }


            } else {
                // Temporary Support for retrieving resources for WSDLs retrieved via ?WSDL that have relative imports
                // If an address context is not unique (e.g. multiple endpoints are deployed under /service that import resources with the same name), 
                // this will simply return the first match it can find.
                ByteBuffer resource = lifeCycle.queryResource(context, null);

                if (resource != null) {

                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Request context {0} at port {1} mapped to resource {2}", 
                                new Object[]{context, reqContext.port, resource});
                    }
                    boolean b = (context.toLowerCase().endsWith(".wsdl"));
                    WsdlQueryHelper helper = new WsdlQueryHelper(reqContext.request, port, resource, b);

                    //ByteBuffer mbb = resource;
                    ByteBuffer mbb = helper.getServiceDescriptorAsByteBuffer();
                    java.io.OutputStream os = response.getOutputStream();
                    java.nio.channels.WritableByteChannel channel = java.nio.channels.Channels.newChannel(os);
                    channel.write(mbb);
                    os.flush();
                    response.setStatus(HttpStatus.OK_200);
                } else {
                    mLogger.log(Level.SEVERE, "HTTPBC-S00670.Resource_not_available", context);
                    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500);
                }

            }

        } catch (Throwable ex) {
            // Make sure that no exceptions get propagated to the embedded server, this might terminate the server
            mLogger.log(Level.SEVERE, "HTTPBC-E00669.Exception_during_query", ex);
            int statusCode = HttpStatus.INTERNAL_SERVER_ERROR_500.getStatusCode();
            response.setStatus(statusCode);
        } finally {
            // try {
                response.finish();
            /*} catch (IOException ex) {
                mLogger.log(Level.SEVERE, "HTTPBC-E00654.Exception_during_reply_processing", ex);
            }*/

            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Wrote response");
            }
        }
    }

    /**
     * Process a HttpRequest and send a JBI request.
     *
     * @param request embedded server request
     * @return JBI message exchange ID
     */
    public void processAsynchRequest(Context reqContext) throws FaultException, HttpException {
        // Instruct Grizzly to not flush response, once we exit the service(...) method
        reqContext.response.suspend();
        String context = reqContext.contextPath + reqContext.pathInfo;
        int port = reqContext.port;

        Endpoint targetEndpoint = lifeCycle.getEndpointBeanForContext(context, port);
        Subject subj = null;

        if (targetEndpoint == null) {
            mLogger.log(Level.WARNING, "HTTPBC-W00652.Web_service_mapping_failed", new Object[]{context, port});

            // Send an HTTP 404 error
            throw new HttpException(HttpStatus.NOT_FOUND_404.getStatusCode(),
                    mMessages.getString("HTTPBC-W00652.Web_service_mapping_failed",
                    new Object[]{context, port}));
        } else {
            if (targetEndpoint.isInbound() && targetEndpoint.isBasicAuthenticationEnabled()) {
                String authHeader = reqContext.request.getHeader("authorization");
                if (authHeader == null) {
                    reqContext.response.setHeader("WWW-Authenticate", "Basic realm=\"ANY\"");
                    reqContext.response.setStatus(HttpStatus.UNAUTHORIZED_401);
                    throw new HttpException(401, "Unauthorized");
                } else {
                    try {
                        subj = targetEndpoint.handleSecurity(authHeader);
                        if (subj != null && !subj.getPrincipals().isEmpty()) {
                            reqContext.request.setUserPrincipal(subj.getPrincipals().iterator().next());
                        }
                    } catch (HttpBcSecurityException se) {
                        mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-E01036.Authentication_failed", new Object[]{se.getLocalizedMessage()}), se);
                        reqContext.response.setStatus(HttpStatus.FORBIDDEN_403);
                        throw new HttpException(403, "Forbidden");

                    }
                }
            }

	    
            WSEndpoint wsEndpoint = targetEndpoint.getWSEndpoint();

            
            HttpAdapter httpAdapter = getAdapter(wsEndpoint);

	    // TODO: can we re-use this connection?
            JAXWSGrizzlyHttpConnection con = new JAXWSGrizzlyHttpConnection(httpAdapter, reqContext.request, reqContext.response,       listener.isSecure(), true);
            if (subj != null) {
                con.setBasicAuthSubject(subj);
            }

            
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
    
    private static Lock createLock = new ReentrantLock();
    
    private static HttpAdapter getAdapter(WSEndpoint wsEndpoint) {
        HttpAdapter adapter = httpAdapterMap.get(wsEndpoint);
        if (adapter == null) {
            createLock.lock();
            try {
                if (adapter == null) {
                    adapter = HttpAdapter.createAlone(wsEndpoint);
                    httpAdapterMap.put(wsEndpoint, adapter);
                }
            } finally {
                createLock.unlock();
            }
        }
        return adapter;
    }

    /**
     * Reply synchronously in this service() invocation. This is useful for
     * responding with errors and any other exchanges where no asynchronous
     * exchange with the JBI NMR will occur.
     */
    public void processSynchronousReply(Context reqContext, Exception requestFailedException) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Replying synchronously.  The request failed. Here's the exception.", requestFailedException);
        }
        processAsynchReply(reqContext, requestFailedException);
    }

    /**
     * Rreply and prepare an HttpResponse, when using JAX-WS this means before
     * it reached the JAX-WS tube. Typically this means an exception happened
     * where we could not hand it to JAX-WS
     *
     * @param reqContext the original request context
     * @param requestFailedException The exception that occurred in the
     * processAsynchRequest phase.
     */
    public void processAsynchReply(Context reqContext, Exception requestFailedException) {

        Response response = reqContext.response;
        try {
            Request request = reqContext.request;

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
                    response = (Response) aDenormalizer.denormalizeException(requestFailedException, response);
                    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500);
                }
            }

        } catch (Throwable ex) {
            // Make sure that no exceptions get propagated to the embedded server, this might terminate the server
            mLogger.log(Level.SEVERE, "HTTPBC-E00654.Exception_during_reply_processing", ex);
            response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR_500);
        } finally {
            try {
                response.resume();
            } catch (Exception ex) {
                mLogger.log(Level.SEVERE, "HTTPBC-E00654.Exception_during_reply_processing", ex);
            }

            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "Wrote response");
            }
        }
    }

    /**
     * Get the thread specific processor support Beware: Do not use the
     * processor support instances in a different thread than the one calling
     * getProcessorSupport.
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
     * Holds instances that are not thread safe Note that this is not suitable
     * when using shared thread pools, only if a limited number of threads are
     * used by this component only
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
        InputBuffer anInputBuffer;
        OutputBuffer anOutputBuffer;
        Request request;
        Response response;
        NetworkListener listener;
        String contextPath;
        String pathInfo;
    }
}