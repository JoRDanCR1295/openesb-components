package com.sun.jbi.restbc.jbiadapter.inbound;

import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.HEAD;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.core.Response.ResponseBuilder;

import com.sun.jbi.restbc.jbiadapter.I18n;

/**
 * JerseyRootResource.java
 *
 * @author Edward Chou
 */
@Path("{path:.*}")
public class JerseyRootResource {

    /*
     * 101-110
     */
    private final static Logger logger = Logger.getLogger(JerseyRootResource.class.getName());
    
    @Context UriInfo uriInfo;
    @Context HttpHeaders headers;
    @Context SecurityContext security;

    @GET
    public Response get() {
        return delegate("GET", null);
    }
    
    @PUT
    public Response put(InputStream payload) {
        return delegate("PUT", payload);
    }
    
    @POST
    public Response post(InputStream payload) {
        return delegate("POST", payload);
    }
    
    @DELETE
    public Response delete() {
        return delegate("DELETE", null);
    }
    
    @HEAD
    public Response head() {
        return delegate("HEAD", null);
    }
    
    private Response delegate(String method, InputStream payload) {
        if (logger.isLoggable(Level.FINEST)) {
            StringBuilder sb = new StringBuilder();
            sb.append("\n");
            sb.append("  URI: " + uriInfo.getRequestUri().toString() + "\n");
            sb.append("  Method: " + method + "\n");
            sb.append("  Headers: " + headers.getRequestHeaders() + "\n");
            
            String msg = I18n.lf("RESTBC-1101: Inbound Request: {0}", sb.toString());//NOI18N
            logger.finest(msg);
        }
        
        InboundDelegator inboundDelegator = InboundDelegator.getInstance();
        if (inboundDelegator == null) {
            String msg = I18n.loc("RESTBC-7101: Inbound delegator not initialized yet");
            logger.severe(msg);
            ResponseBuilder responseBuilder = Response.serverError();
            responseBuilder.entity(msg);
            return responseBuilder.build();
        }
        
        try {
            ResponseBuilder responseBuilder = inboundDelegator.delegateRequest(method, uriInfo, headers, security, payload);
            
            Response response = responseBuilder.build();
            
            if (logger.isLoggable(Level.FINEST)) {
                StringBuilder sb = new StringBuilder();
                sb.append("\n");
                sb.append("  URI: " + uriInfo.getRequestUri().toString() + "\n");
                sb.append("  Method: " + method + "\n");
                sb.append("  Status: " + response.getStatus());
                sb.append("  Headers: " + response.getMetadata() + "\n");
                
                String msg = I18n.lf("RESTBC-1102: Inbound Response: {0}", sb.toString()); // NOI18N
                logger.finest(msg);
            }
            
            return response;
            
        } catch (Exception e) {
            String msg = I18n.loc("RESTBC-7102: Error while serving inbound request {0}", e);
            logger.severe(msg);
            ResponseBuilder responseBuilder = Response.serverError();
            responseBuilder.entity(msg);
            
            Response response = responseBuilder.build();
            
            if (logger.isLoggable(Level.FINEST)) {
                StringBuilder sb = new StringBuilder();
                sb.append("\n");
                sb.append("  URI: " + uriInfo.getRequestUri().toString() + "\n");
                sb.append("  Method: " + method + "\n");
                sb.append("  Status: " + response.getStatus());
                sb.append("  Headers: " + response.getMetadata() + "\n");
                
                String responseMsg = I18n.lf("RESTBC-1102: Inbound Response: {0}", sb.toString()); // NOI18N
                logger.finest(responseMsg);
            }
            
            return response;
        }
        
        
    }
    
}
