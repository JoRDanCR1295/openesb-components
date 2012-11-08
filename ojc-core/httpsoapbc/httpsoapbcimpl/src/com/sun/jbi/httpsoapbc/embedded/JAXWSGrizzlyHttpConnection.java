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
 * @(#)JAXWSGrizzlyHttpConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import com.sun.jbi.internationalization.Messages;
import com.sun.enterprise.web.connector.grizzly.AsyncTask;
import com.sun.istack.NotNull;
import com.sun.xml.ws.api.message.Packet;
import com.sun.xml.ws.api.server.WSEndpoint;
import com.sun.xml.ws.api.server.WebServiceContextDelegate;
import com.sun.xml.ws.transport.http.WSHTTPConnection;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.Map;

import javax.security.auth.Subject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.ws.handler.MessageContext;

import org.apache.coyote.Request;
import org.apache.coyote.Response;
import org.apache.coyote.tomcat5.CoyoteRequest;
import org.apache.coyote.tomcat5.CoyoteResponse;
import org.apache.tomcat.util.http.MimeHeaders;

/**
 * Based on JAX-WS WSHTTPConnection used with Java SE endpoints. It provides connection
 * implementation using Grizzly.
 */
public final class JAXWSGrizzlyHttpConnection extends WSHTTPConnection implements WebServiceContextDelegate {

    private static final Messages mMessages =
        Messages.getMessages(JAXWSGrizzlyHttpConnection.class);
    private final static Logger mLogger =
        Messages.getLogger(JAXWSGrizzlyHttpConnection.class);    
    
    //private final HttpExchange httpExchange;
    private Request req;
    private Response res;
    private CoyoteRequest coyoteRequest;
    private CoyoteResponse coyoteResponse;
    private int status;
    private int responseContentLength = 0;
    private Subject basicAuthSubject;

    private boolean outputWritten;
    private boolean isSecure;

    private AsyncTask grizzlyAsyncTask;

    public JAXWSGrizzlyHttpConnection(@NotNull Request request, @NotNull Response response, @NotNull CoyoteRequest coyoteRequest, 
                                      @NotNull CoyoteResponse coyoteResponse, AsyncTask grizzlyAsyncTask, boolean isSecure) {
        this.req = request;
        this.res = response;
        this.coyoteRequest = coyoteRequest;
        this.coyoteResponse = coyoteResponse;
        this.grizzlyAsyncTask = grizzlyAsyncTask;
        this.isSecure = isSecure;
    }

    @Override
    @Property({MessageContext.HTTP_REQUEST_HEADERS, Packet.INBOUND_TRANSPORT_HEADERS})
    public @NotNull Map<String,List<String>> getRequestHeaders() {
        MimeHeaders mimeHeaders = req.getMimeHeaders();
        Map<String, List<String>> jaxWSHeaders = convertHeaders(mimeHeaders);
        return jaxWSHeaders;
    }

    @Override
    public String getRequestHeader(String headerName) {
        return req.getHeader(headerName);
    }

    
    /**
     * there seems to issue with metro implementation while creating soap 1.2
     * mime headers, for the content-type mime header it does not propagate
     * the exact value for the content-type mime header, specially in this case
     * though the http-request contents the value of the action in the content
     * type , the same value is not propagated from the http transport layer to
     * the soap message layer
     * 
     * this is a work around which is used to pass on the action value if
     * present in the content-type , this code needs to removed once we have the
     * metro fix
     * 
     * @param headerName
     * @return
     */
    private String soapActionHeader(String value) {
	
	StringTokenizer tk = new StringTokenizer(value,";");
	String soapAction=null;
	while(tk.hasMoreTokens()){
	    String t = tk.nextToken();
	    if(t.startsWith("action=")){
		soapAction= t.substring("action=".length());	
	    }
	}
	return soapAction;
    }
    
   

    @Override
    public void setResponseHeaders(Map<String,List<String>> headers) {
        if (headers != null) {
            for (Map.Entry<String,List<String>> entry : headers.entrySet()) {
                String key = entry.getKey();
                List<String> values = entry.getValue();
                if (values.size() == 1) {
                    res.setHeader(key, values.get(1));
                } else {
                    // If the header has multiple values, comma separte them
                    StringBuffer concat = new StringBuffer();
                    boolean firstTime = true;
                    for (String aValue : values) {
                        if (!firstTime) {
                            concat.append(',');
                        }
                        concat.append(aValue);
                        firstTime = false;
                    }
                    res.setHeader(key, concat.toString());
                }
            }                    
        }
    }

    @Override
    @Property(MessageContext.HTTP_RESPONSE_HEADERS)
    public Map<String,List<String>> getResponseHeaders() {
        MimeHeaders mimeHeaders = res.getMimeHeaders();
        Map<String, List<String>> jaxWSHeaders = convertHeaders(mimeHeaders);
        return jaxWSHeaders;
    }
    
    @Override
    public void setContentTypeResponseHeader(@NotNull String value) {
        res.setHeader("Content-Type", value);
    }

    @Override
    public void setStatus(int status) {
        this.status = status;
    }

    @Override
    @Property(MessageContext.HTTP_RESPONSE_CODE)
    public int getStatus() {
        return status;
    }
    
    @Property(MessageContext.SERVLET_REQUEST)
    public HttpServletRequest getRequest() {
        return coyoteRequest;
    }
	
    @Property(MessageContext.SERVLET_RESPONSE)
    public HttpServletResponse getResponse() {
        return coyoteResponse;
    }	

    public @NotNull InputStream getInput() throws IOException{
        return coyoteRequest.getInputStream();
        //return httpExchange.getRequestBody();
    }

    public @NotNull OutputStream getOutput() throws IOException {
        assert !outputWritten;
        outputWritten = true;

        res.setStatus(getStatus());
        return coyoteResponse.getOutputStream();
    }

    public @NotNull WebServiceContextDelegate getWebServiceContextDelegate() {
        return this;
    }

    public Principal getUserPrincipal(Packet request) {
        return coyoteRequest.getUserPrincipal();
    }

    public boolean isUserInRole(Packet request, String role) {
        return false;
    }

    public @NotNull String getEPRAddress(Packet request, WSEndpoint endpoint) {
        // TODO: verify the kind of address it wants here
        return coyoteRequest.getRequestURL().toString();
    }
   
    public String getWSDLAddress(@NotNull Packet request, @NotNull WSEndpoint endpoint) { 
        String eprAddress = getEPRAddress(request,endpoint); 
        String wsdlAddress = eprAddress + "?wsdl";
        return wsdlAddress; 
    } 

    //@Override  
    public boolean isSecure() {  
        return this.isSecure;
    }
    
    @Override
    @Property(MessageContext.HTTP_REQUEST_METHOD)
    public @NotNull String getRequestMethod() {
        return coyoteRequest.getMethod();
    }

    @Override
    @Property(MessageContext.QUERY_STRING)
    public String getQueryString() {
        return coyoteRequest.getQueryString();
    }

    @Override
    @Property(MessageContext.PATH_INFO)
    public String getPathInfo() {
        return coyoteRequest.getRequestURI();
    }

    /**
     * Override the close to make sure the Grizzly ARP processing completes
     * Delegate further processing to parent class
     */
    public void close() {
        finishGrizzlyResponse();
        super.close();
    }
    
    protected PropertyMap getPropertyMap() {
        return model;
    }
    
    void finishGrizzlyResponse() {
        if (grizzlyAsyncTask != null) {
            JBIGrizzlyAsyncFilter.finishResponse(grizzlyAsyncTask);
            // TODO: setting this to null is a work-around for JAX-WS calling onCompletion / close twice 
            // to make sure finish response is only called once
            grizzlyAsyncTask = null;
        }
    }

    /**
     * Convert from MimeHeaders to the format JAX-WS uses
     * with the header name as the map key pointing to a list of values 
     * for that header
     * 
     * This conversion might be expesive, if this is frequently used it may 
     * be worth changing Grizzly or JAX-WS to remove the need for the conversion
     */
    Map<String, List<String>> convertHeaders(MimeHeaders mimeHeaders) {
	Map<String, List<String>> jaxWSHeaders = new HashMap();
	Enumeration names = mimeHeaders.names();
	while (names.hasMoreElements()) {
	    String name = (String) names.nextElement();
	    List jaxWSHeaderValues = new ArrayList();
	    Enumeration values = mimeHeaders.values(name);

	    if (name.equalsIgnoreCase("Content-Type")) {
		while (values.hasMoreElements()) {
		    String aValue = (String) values.nextElement();
		    jaxWSHeaderValues.add(aValue);
		    if(aValue.indexOf("action=") > 0){			
			List soapActions =  new ArrayList();
			soapActions.add(soapActionHeader(aValue));
			jaxWSHeaders.put("SOAPAction", soapActions);
		    }
		}
	    } else {

		while (values.hasMoreElements()) {
		    String aValue = (String) values.nextElement();
		    jaxWSHeaderValues.add(aValue);
		}
	    }
	    jaxWSHeaders.put(name, jaxWSHeaderValues);
	}

	return jaxWSHeaders;
    }

    
    private static final PropertyMap model;

    static {
        model = parse(JAXWSGrizzlyHttpConnection.class);
    }

    /**
     * @return the basicAuthSubject
     */
    
    @Property("basicAuthSubject")
    public Subject getBasicAuthSubject() {
        return basicAuthSubject;
    }

    /**
     * @param basicAuthSubject the basicAuthSubject to set
     */
    public void setBasicAuthSubject(Subject basicAuthSubject) {
        this.basicAuthSubject = basicAuthSubject;
    }
}
