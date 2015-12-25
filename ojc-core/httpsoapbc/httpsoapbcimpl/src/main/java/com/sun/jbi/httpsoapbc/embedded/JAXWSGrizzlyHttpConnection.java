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
import com.sun.istack.NotNull;
import com.sun.xml.ws.api.message.Packet;
import com.sun.xml.ws.api.server.PortAddressResolver;
import com.sun.xml.ws.api.server.WSEndpoint;
import com.sun.xml.ws.api.server.WebServiceContextDelegate;
import com.sun.xml.ws.transport.http.HttpAdapter;
import com.sun.xml.ws.transport.http.WSHTTPConnection;

import com.oracle.webservices.api.message.PropertySet.Property;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.Principal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Set;

import javax.security.auth.Subject;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.handler.MessageContext;

import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.Response;

/**
 * Based on JAX-WS WSHTTPConnection used with Java SE endpoints. It provides
 * connection implementation using Grizzly.
 */
public final class JAXWSGrizzlyHttpConnection extends WSHTTPConnection implements WebServiceContextDelegate {

    private static final Messages mMessages =
            Messages.getMessages(JAXWSGrizzlyHttpConnection.class);
    private final static Logger mLogger =
            Messages.getLogger(JAXWSGrizzlyHttpConnection.class);
    
    private final Request request;
    private final Response response;

    private int status;
    private Subject basicAuthSubject;
    private boolean outputWritten;
    private final boolean isSecure;
    private final boolean isAsync;
    
    final HttpAdapter httpAdapter;
    
    private Map<String, List<String>> requestHeaders;
    private Map<String, List<String>> responseHeaders;

    public JAXWSGrizzlyHttpConnection(final HttpAdapter httpAdapter,
            final Request request, final Response response,
            final boolean isSecure, final boolean isAsync) {
        this.httpAdapter = httpAdapter;
        this.request = request;
        this.response = response;
        this.isSecure = isSecure;
        this.isAsync = isAsync;
    }

    @Override
    @Property({MessageContext.HTTP_REQUEST_HEADERS, Packet.INBOUND_TRANSPORT_HEADERS})
    public @NotNull
    Map<String, List<String>> getRequestHeaders() {
        if (requestHeaders == null) {
            requestHeaders = initializeRequestHeaders();
        }

        return requestHeaders;
    }

    @Override
    public String getRequestHeader(String headerName) {
        return request.getHeader(headerName);
    }

    /**
     * there seems to issue with metro implementation while creating soap 1.2
     * mime headers, for the content-type mime header it does not propagate the
     * exact value for the content-type mime header, specially in this case
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

        StringTokenizer tk = new StringTokenizer(value, ";");
        String soapAction = null;
        while (tk.hasMoreTokens()) {
            String t = tk.nextToken();
            if (t.startsWith("action=")) {
                soapAction = t.substring("action=".length());
            }
        }
        return soapAction;
    }

    @Override
    public void setResponseHeaders(Map<String, List<String>> headers) {
        this.responseHeaders = headers;
        if (headers == null) {
            return;

        }
        if (status != 0) {
            response.setStatus(status);
        }

        response.reset(); // clear all the headers

        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            String name = entry.getKey();
            if (name.equalsIgnoreCase("Content-Type") || name.equalsIgnoreCase("Content-Length")) {
                continue; // ignore headers that interfere with the operation
            }
            for (String value : entry.getValue()) {
                response.addHeader(name, value);
            }
    	}
    }

    @Override
    public void setResponseHeader(String headerName, List<String> values) {
        responseHeaders.put(headerName, values);
    }

    @Override
    @Property(MessageContext.HTTP_RESPONSE_HEADERS)
    public Map<String, List<String>> getResponseHeaders() {
        return responseHeaders;
    }

    @Override
    public void setContentTypeResponseHeader(@NotNull String value) {
        response.setContentType(value);
    }

    @Override
    public void setStatus(int status) {
        this.status = status;
    }

    @Override
    @com.oracle.webservices.api.message.PropertySet.Property(MessageContext.HTTP_RESPONSE_CODE)
    public int getStatus() {
        return status;
    }

    @NotNull
    @Override
    public InputStream getInput() throws IOException {
        return request.getInputStream();
    }

    @NotNull
    @Override
    public OutputStream getOutput() throws IOException {
        assert !outputWritten;
        outputWritten = true;

        response.setStatus(getStatus());
        return response.getOutputStream();
    }

    @NotNull
    @Override
    public WebServiceContextDelegate getWebServiceContextDelegate() {
        return this;
    }

    @Override
    public Principal getUserPrincipal(Packet request) {
        return this.request.getUserPrincipal();
    }

    @Override
    public boolean isUserInRole(Packet request, String role) {
        return false;
    }

    @NotNull
    @Override
    public String getEPRAddress(Packet p, WSEndpoint endpoint) {
        return request.getRequestURL().toString();
        /*
        PortAddressResolver resolver = httpAdapter.owner.createPortAddressResolver(getBaseAddress());
        String address = resolver.getAddressFor(endpoint.getServiceName(), endpoint.getPortName().getLocalPart());
        if (address == null) {
            throw new WebServiceException("WsservletMessages.SERVLET_NO_ADDRESS_AVAILABLE(" + endpoint.getPortName() + ")");
        }
        return address;
        */
    }

    @Override
    public String getWSDLAddress(@NotNull Packet request, @NotNull WSEndpoint endpoint) {
        String eprAddress = getEPRAddress(request, endpoint);
        return eprAddress + "?wsdl";
    }

    @NotNull
    @Override
    public String getBaseAddress() {
        return getBaseAddress(request);
    }

    static @NotNull
    String getBaseAddress(Request request) {
        StringBuilder buf = new StringBuilder();
        buf.append(request.getScheme());
        buf.append("://");
        buf.append(request.getServerName());
        buf.append(':');
        buf.append(request.getServerPort());
        buf.append(request.getContextPath());

        final String httpHandlerPath = request.getHttpHandlerPath();
        if (httpHandlerPath != null) {
            buf.append(httpHandlerPath);
        }

        return buf.toString();
    }

    @Override
    public boolean isSecure() {
        return this.isSecure;
    }

    @Override
    @Property(MessageContext.HTTP_REQUEST_METHOD)
    public @NotNull
    String getRequestMethod() {
        return request.getMethod().getMethodString();
    }

    @Override
    @Property(MessageContext.QUERY_STRING)
    public String getQueryString() {
        return request.getQueryString();
    }

    @Override
    @Property(MessageContext.PATH_INFO)
    public String getPathInfo() {
        return request.getPathInfo();
    }

    /**
     * Override the close to make sure the Grizzly ARP processing completes
     * Delegate further processing to parent class
     */
    @Override
    public void close() {
        try {
            super.close();
        } finally {
            if (isAsync) {
                response.resume();
            }
        }
    }

    @Override
    protected PropertyMap getPropertyMap() {
        return model;
    }
    private static final PropertyMap model;

    static {
        model =  parse(JAXWSGrizzlyHttpConnection.class);
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

    private Map<String, List<String>> initializeRequestHeaders() {
        final Map<String, List<String>> headers = new HashMap<String, List<String>>();
        for (String name : request.getHeaderNames()) {
            final List<String> values = new ArrayList<String>(2);
            for (String value : request.getHeaders(name)) {
                values.add(value);
            }

            headers.put(name, values);
        }

        return headers;
    }

    @Override
    public Set<String> getRequestHeaderNames() {
        return requestHeaders.keySet();
    }

    @Override
    public List<String> getRequestHeaderValues(String headerName) {
        return requestHeaders.get(headerName);
    }

    @Override
    public String getRequestURI() {
        return request.getRequestURI();
    }

    @Override
    public String getRequestScheme() {
        return request.getScheme();
    }

    @Override
    public String getServerName() {
        return request.getServerName();
    }

    @Override
    public int getServerPort() {
        return request.getServerPort();
    }
}