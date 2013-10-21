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
 * @(#)JAXWSEndpointFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.HttpEndpoint;
import com.sun.jbi.httpsoapbc.HttpSoapEndpoint;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.descriptors.HttpSoapHandler;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.xml.stream.buffer.XMLStreamBuffer;
import com.sun.xml.ws.api.BindingID;
import com.sun.xml.ws.api.WSBinding;
import com.sun.xml.ws.api.model.wsdl.WSDLPort;
import com.sun.xml.ws.api.server.Adapter;
import com.sun.xml.ws.api.server.Container;
import com.sun.xml.ws.api.server.ServiceDefinition;
import com.sun.xml.ws.api.server.WSEndpoint;
import com.sun.xml.ws.api.server.SDDocumentSource;
import com.sun.xml.ws.binding.BindingImpl;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.xml.sax.EntityResolver;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.net.URL;
import java.net.URLClassLoader;

import javax.xml.namespace.QName;

/**
 * Factory to create JAX-WS WSEndpoint directly
 */
public class JAXWSEndpointFactory {
    
    private static final Messages mMessages =
        Messages.getMessages(JAXWSEndpointFactory.class);
    private static final Logger mLogger =
        Messages.getLogger(JAXWSEndpointFactory.class);    
    
    /** Creates a new instance of JAXWSEndpointFactory */
    public JAXWSEndpointFactory() {
    }

    /**
     * Create a JAX-WS WSEndpoint
     * @param port The port for the endpoint to listen on
     * @param urlContext the URL context at which to expose the endpoint
     * @param endpoint the metadata related to the endpoint 
     */
    public WSEndpoint createWSEndpoint(int port, String urlContext, Endpoint endpoint) throws Exception {

        Class serviceEndpointClass = com.sun.jbi.httpsoapbc.jaxwssupport.AsyncJBIProvider.class; 
        Object serviceEndpoint = new com.sun.jbi.httpsoapbc.jaxwssupport.AsyncJBIProvider(endpoint);
        boolean processHandlerAnnotation = false; // we do not want JAXWS to process @HandlerChain?
        
        if (endpoint instanceof HttpSoapEndpoint) {
            serviceEndpointClass = com.sun.jbi.httpsoapbc.jaxwssupport.AsyncJBIProvider.class;
            serviceEndpoint = new com.sun.jbi.httpsoapbc.jaxwssupport.AsyncJBIProvider(endpoint);
        } else if (endpoint instanceof HttpEndpoint) {
            serviceEndpointClass = com.sun.jbi.httpsoapbc.jaxwssupport.AsyncXmlHttpJBIProvider.class;
            serviceEndpoint = new com.sun.jbi.httpsoapbc.jaxwssupport.AsyncXmlHttpJBIProvider(endpoint);
        } else {
            String msg = mMessages.getString("HTTPBC-E00633.Unsupported_endpoint_type",
                                             new Object[] { endpoint.getEndpointName(),
                                                            endpoint.getClass().toString()
                                                          });
            throw new IllegalArgumentException(msg);
        }
        
        com.sun.jbi.httpsoapbc.jaxwssupport.NewWebServiceContextImpl wsc = new com.sun.jbi.httpsoapbc.jaxwssupport.NewWebServiceContextImpl();
        // It should be possible to use the default invoker instead of this custom one
        com.sun.xml.ws.api.server.Invoker invoker = new com.sun.jbi.httpsoapbc.jaxwssupport.InvokerImpl(serviceEndpoint, wsc); 
        QName serviceName = endpoint.getServiceName(); 
        
        QName portName = new QName(endpoint.getServiceName().getNamespaceURI(), endpoint.getEndpointName());
        Container container = null; // This counter can contain info on security/monitoring pipe 

        String lexicalBindingType = null;
        
        if (endpoint instanceof com.sun.jbi.httpsoapbc.HttpSoap12Endpoint) {
            if (endpoint.isMTOMEnabled()) {
                lexicalBindingType = javax.xml.ws.soap.SOAPBinding.SOAP12HTTP_MTOM_BINDING;
            } else {
                lexicalBindingType = javax.xml.ws.soap.SOAPBinding.SOAP12HTTP_BINDING;
            }
        }else if (endpoint instanceof HttpSoapEndpoint) {
            if (endpoint.isMTOMEnabled()) {
                lexicalBindingType = javax.xml.ws.soap.SOAPBinding.SOAP11HTTP_MTOM_BINDING;
            } else {
                lexicalBindingType = javax.xml.ws.soap.SOAPBinding.SOAP11HTTP_BINDING;
            }
        } else if (endpoint instanceof HttpEndpoint) {
            lexicalBindingType = javax.xml.ws.http.HTTPBinding.HTTP_BINDING;
        }
        
        BindingID bindingID = BindingID.parse(lexicalBindingType);
        WSBinding binding = bindingID.createBinding();
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Creating WSEndpoint with primary WSDL URL: " + endpoint.getOriginalWSDL().toURL());
        }
        SDDocumentSource primaryWsdl = SDDocumentSource.create(endpoint.getOriginalWSDL().toURL());
        // Collection of imported WSDLs and schema, in the form of java.util.Collection<SDDocumentSource>
        java.util.Collection<SDDocumentSource> docs = null;

        //TODO: Clarify if we need to do this recursively
        // Add imported resources need to be added to the java.util.Collection<SDDocumentSource> list
        // as required by the JAX-WS EndpointFactory.
        if (endpoint.getWSDLImports() != null) {
            docs = new java.util.ArrayList<SDDocumentSource>();
            Iterator iter = endpoint.getWSDLImports().iterator();
            while (iter.hasNext()) {
                java.net.URL entry = (java.net.URL) iter.next();
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Adding imported WSDL document with URL: " + entry + " to the list of SDDocumentSource...");
                }
                SDDocumentSource importedDoc = SDDocumentSource.create(entry);
                docs.add(importedDoc);
            }
        }
        
        // Initialize the Catalog URL, such that an EntityResolver can be created from it
        // in the JAX-WS layer.
        URL catalogURL = null;
        File catalog = new File(endpoint.getServiceUnitRootPath() +
            File.separator + "meta-inf" + File.separator +
            "catalog.xml");
        if (catalog.exists()) {
              catalogURL = catalog.toURL();
        }
        
        // add JAX-WS handlers if any
        enableJAXWSHandlers(endpoint, binding);
        
        WSEndpoint wsep = WSEndpoint.create(
                serviceEndpointClass, // The endpoint class
                processHandlerAnnotation,
                invoker,
                serviceName,
                portName, 
                container, 
                binding, 
                primaryWsdl, // primary WSDL
                docs,        // all imported WSDL documents
                catalogURL);
        
        endpoint.setWSEndpoint(wsep);
        
        return wsep;
    }    
    
    
    private void enableJAXWSHandlers(Endpoint endpoint, WSBinding binding) throws Exception {
        List handlerList = endpoint.getHandlers();
        if (handlerList.size() == 0) {
            return;
        }
        
        // load the handler jars
        List handlerLibs = endpoint.getHandlerLibPaths();
        URL[] handlerLibUrls = new URL[handlerLibs.size()];
        int count = 0;
        
        for (Iterator it = handlerLibs.iterator(); it.hasNext();) {
    	    File libFile = (File) it.next();
    	    if(libFile.exists()) {
    	        handlerLibUrls[count] = libFile.toURL();
    	        count++;
    	    }
    	}
        
        if (handlerLibUrls.length > 0) {
            URLClassLoader handlerClassLoader = new URLClassLoader(handlerLibUrls, Thread.currentThread().getContextClassLoader());
            Thread.currentThread().setContextClassLoader(handlerClassLoader);
        }
            
        // instantiate the handler instances 
        List handlers = new ArrayList();
        for (Iterator it = handlerList.iterator(); it.hasNext();) {
            // get the fully qualified handler class name
            HttpSoapHandler handlerMeta = (HttpSoapHandler)it.next();
            try {
                Class handlerClass = Class.forName(handlerMeta.getHandlerClassName(), true, Thread.currentThread().getContextClassLoader());
                // we require handler implementation to follow the Java Bean convention
                handlers.add(handlerClass.newInstance());
            } catch (Exception e) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-E01056.Failed_to_instantiate_handler");
                    mLogger.log(Level.WARNING, text, e);
                    AlertsUtil.getAlerter().warning(text, 
                                                    HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                    endpoint.getServiceUnitID(), 
                                                    AlertsUtil.getServerType(),
                                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                    NotificationEvent.EVENT_TYPE_ALERT,
                                                    "HTTPBC-E01056");
                }
                throw e;
            }
        }
        ((BindingImpl)binding).setHandlerChain(handlers);
    }
}
