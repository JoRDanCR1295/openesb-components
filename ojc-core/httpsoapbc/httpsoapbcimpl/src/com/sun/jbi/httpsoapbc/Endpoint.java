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
 * @(#)Endpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.httpsoapbc.descriptors.HttpSoapHandler;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityManager;
import com.sun.jbi.httpsoapbc.security.api.HttpBcSecurityException;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;

import com.sun.xml.ws.api.server.WSEndpoint;

import java.io.File;
import java.net.URL;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.JBIException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.security.auth.Subject;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

/**
 * An Endpoint represents the data of a physical, external endpoint.
 * This interface captures the metadata and monitoring information
 * about an endpoint.  It also provides lifecycle methods to
 * initialize, activate, deactivate, and shutdown the endpoint.  Other
 * properties of the endpoint include:
 * <ul>
 * <li>Name</li>
 * <li>Service name this Endpoint belongs to</li>
 * <li>Location of the Endpoint as denoted by a URL</li>
 * <li>Whether this Endpoint is a provider or consumer of services</li>
 * <li>The original WSDL Definition that describes this endpoint</li>
 * <li>Metadata concerning the bindings and porttypes associated with the 
 * Endpoint</li>
 * <li>The underlying represention of an Endpoint in the JBI framework --
 * the ServiceEndpoint.</li>
 * <li>The status object used for monitoring</li>
 * </ul>
 * <p>
 * TODO: It's not completely clear to me that the lifecycle methods are
 * correct.  The arguments may not be necessary and the type of exception
 * isn't quite right.
 * <p>
 * TODO: Should monitoring stuff be associated with the Endpoint?  At first
 * glance, it should be.  However, it makes more sense that monitoring should
 * be more of an Observer rather than something that makes up an endpoint.
 */
public interface Endpoint {

    void init() throws Exception;

    void activate(ComponentContext context) throws JBIException;

    void deactivate(ComponentContext context) throws JBIException;
    
    void shutdown() throws JBIException;
    
    String getEndpointName();

    void setEndpointName(String endpointName);

    QName getServiceName();
    
    void setServiceName(QName serviceName);
    
    QName getInterfaceName();
    
    void setInterfaceName(QName interfaces);

    URL getEndpointUrl();
    
    void setEndpointUrl(URL endpointUrl);

    int getUrlPort();
    
    String getUrlContext();

    boolean isInbound();

    void setIsInbound(boolean isInbound);

    HttpClientConnectionProperties getHttpClientConnectionProperties();
    
    void setHttpClientConnectionProperties(HttpClientConnectionProperties clientConnProps);
    
    boolean getPropagateSoapHeader();
    
    void setPropagateSoapHeader(boolean propagateSoapHeader);
    
    ServiceEndpoint getEndpointReference();

    void setEndpointReference(ServiceEndpoint serviceEndpoint);

    EndpointStatus getEndpointStatus();

    void setEndpointStatus(EndpointStatus val);

    WSEndpoint getWSEndpoint();

    void setWSEndpoint(WSEndpoint wsEndpoint);
    
    Definition getServiceDescriptor();

    void setServiceDescriptor(Definition def);

    ByteBuffer getServiceDescriptorAsByteBuffer();

    Document getServiceDescriptorAsDocument();
    
    void setOriginalWSDL(File originalWSDL);
    
    File getOriginalWSDL();
    
    void addWSDLImport(URL anImport);
    
    List getWSDLImports();
    
    void addImportedWSDLNamespace(String namespace);
    
    List getImportedWSDLNamespaces();
    
    void addImportedXSDNamespace(String namespace);
    
    List getImportedXSDNamespaces();

    Map getOperationNameToMetaData();

    void setOperationNameToMetaData(Map opNameToMetaData);
    
    List<HttpSoapHandler> getHandlers();
    
    void setHandlers(List<HttpSoapHandler> handlers);
    
    void setHandlerLibPaths(List libPaths);
    
    List getHandlerLibPaths();

    String getUniqueName();
    
    Subject handleSecurity (String authorizationHeader) throws HttpBcSecurityException;

    HTTPBasicAuthCredential getBasicAuthCredential();
    
    boolean isBasicAuthenticationEnabled();
    
    EndpointSecurityManager getEndpointSecurityManager();
    
    QName createOperationAddress(OperationMetaData opMetaData);
    
    String getServiceUnitID();
    
    String getServiceUnitRootPath();
    
    int getMaxConcurrencyLimit();
    
    void setMaxConcurrencyLimit(int maxConcurrencyLimit);
    
    RedeliveryConfig getRedeliveryConfiguration();
    
    void setRedeliveryConfiguration(RedeliveryConfig redeliveryConfig);

    void setSubject(Subject subject);

    Subject getSubject();

    int getPendingExchangeReplies();
    
    void incrementPendingExchangeReplies();
    
    void decrementPendingExchangeReplies();
    
    void setInboundRequestThrottlingController (RequestThrottlingController controller);
    
    long getCRMPMessageId();  
    
    void setEnableWsdlQuery(boolean enabled);
    
    boolean getEnableWsdlQuery();
    
    void setValidHostnames(String hostnames);
    
    Map getValidHostnames();
    
    //comments out - they were designed to populate WSDL monitoring MBeans
    /*
    
    Map <String, Definition> getImportedURL2WSDL();
    
    void setImportedURL2WSDL(Map<String, Definition> importedXSDs);
    
    Map <String, Element> getImportedURL2XSD();
    
    void setImportedURL2XSD(Map<String, Element> importedXSDs);
    
    void addImportedWSDLDefinition(String namespace, Definition definition);
    
    Map getImportedWSDLDefinitions();
    
    void addImportedXSDSchema(String namespace, Element schemaElement);
    
    Map getImportedXSDSchemas();
    */
}
