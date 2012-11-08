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
 * @(#)HttpSoapBindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;

/**
 *
 */
public class HttpSoapBindingComponent implements Component {
    private static final String WS_ADDRESSING_NS = "http://schemas.xmlsoap.org/ws/2004/08/addressing";
    private static final Messages mMessages =
        Messages.getMessages(HttpSoapBindingComponent.class);
    private static final Logger mLogger = 
        Messages.getLogger(HttpSoapBindingComponent.class);
    
    /**
     * Internal handle to the component lifecycle implementation.
     */
    private HttpSoapBindingLifeCycle mComponentLifeCycle;

    /** Creates a new instance of HttpSoapBindingComponent */
    public HttpSoapBindingComponent() {
        mComponentLifeCycle = new HttpSoapBindingLifeCycle();
    }
    
    public boolean isExchangeWithConsumerOkay(ServiceEndpoint endpoint, MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return true;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint endpoint, MessageExchange exchange) {
        return true;
    }
    
    public Document getServiceDescription(ServiceEndpoint endpoint) {
    	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "getServiceDescription is called with endpoint [" + endpoint.getServiceName() + "," + endpoint.getEndpointName() + "]");
        }
        Document result = null;
        // TODO: The document returned should be a stand-alone document
        //       (no imports or includes)
        // TODO: consider whether it should only return the abstract wsdl
        //       concerning the endpoint
        // TODO: Beware for service engines that they HAVE TO include a specific
        //       binding type defined for Service Engines
        
        // Per JBI spec, component should always return a WSDL document for endpoints it's providing
        // So, we will look for provisioning endpoints first
        String uniqueName =
            AbstractEndpoint.getUniqueName(endpoint.getServiceName().getNamespaceURI(),
                                           endpoint.getServiceName().getLocalPart(),
                                           endpoint.getEndpointName(),
                                           false);
        Map endpoints = mComponentLifeCycle.getEndpointBeans();
        HttpSoapEndpoint foundEndpoint = (HttpSoapEndpoint) endpoints.get(uniqueName);
        if (foundEndpoint != null) {
            return foundEndpoint.getServiceDescriptorAsDocument();
        }
        
        if (foundEndpoint == null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Endpoint [" + endpoint.getServiceName() + "," + endpoint.getEndpointName() + 
                                         "] is not an endpoint HTTP BC provides. Looking to see if it is an external endpoint registered by HTTP BC...");
            }
            uniqueName = AbstractEndpoint.getUniqueName(endpoint.getServiceName().getNamespaceURI(),
                                           endpoint.getServiceName().getLocalPart(),
                                           endpoint.getEndpointName(),
                                           true);
                                           
            foundEndpoint = (HttpSoapEndpoint) endpoints.get(uniqueName);
            if (foundEndpoint != null && foundEndpoint.getEndpointReference() != null) {
                return foundEndpoint.getServiceDescriptorAsDocument();
            }
            
        } 
        
        return result;
    }
    
    public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
    	HttpSoapDynamicEndpoint sep = null;
        Endpoint endpoint = null;
        String urlLocation = null;
        
        try {
            NodeList children = epr.getChildNodes();
            if (children != null && children.getLength() > 1) {
                endpoint = getEndpointInfo(epr);
                urlLocation = getDynamicUrlFromEPR(epr);
            } else if (children.getLength() == 1) {   // "wrapper" element found
                // wsa:EPR element or dummy
                Node child = children.item(0);
                //if (WS_ADDRESSING_NS.equals(child.getNamespaceURI())) {
                urlLocation = getDynamicUrlFromEPR(child);
                endpoint = getEndpointInfo(child);
                //}
            } else {
                throw new Exception (mMessages.getString("HttpSoapBindingComponent.Invalid_epr_format"));
            }
            
            if (endpoint != null && urlLocation != null) {
                sep = new HttpSoapDynamicEndpoint(endpoint.getServiceName(), endpoint.getEndpointName(), null, epr);
                // populate additional info in my SE implementation
                sep.setEndpointInfo(endpoint);
                sep.setDynamicUrl(urlLocation);
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "HTTP BC can't locate an activated endpoint with the service/endpoint name specified in the endpoint reference document fragment. Returning null...");
                }
            } 
            
        } catch (Exception e) {
            if (mLogger.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-W00671.Exception_during_endpoint_resolution", e.getLocalizedMessage());
                mLogger.log(Level.WARNING, text, e);
                AlertsUtil.getAlerter().warning(text, 
                                                HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                null, 
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "HTTPBC-W00671");
            }
        }
        
        return sep;
    }
    
    private String getDynamicUrlFromEPR(Node parentNode) throws Exception {
        String urlLocation = null;
        URL url = null;
        NodeList children = parentNode.getChildNodes();
    	for(int ii=0; ii< children.getLength(); ii++) {
            Node child = children.item(ii);
            if ("Address".equalsIgnoreCase(child.getLocalName())  &&
                WS_ADDRESSING_NS.equals(child.getNamespaceURI())) {
                urlLocation = child.getTextContent().trim(); 	
                break;
            } 
        }
        
        if (urlLocation == null) {
            return null;
        }
        
        try {
            url = new URL(urlLocation);
        } catch (MalformedURLException e) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-W00764.Invalid_url_in_documentFragment", urlLocation));
            }
            return null;
        }   
        
        return urlLocation;
        
    }
    
    private Endpoint getEndpointInfo(Node parentNode) throws Exception {
        Endpoint endpoint = null;
    	Map endpoints = mComponentLifeCycle.getEndpointBeans();
    	
    	NodeList children = parentNode.getChildNodes();
    	for(int ii=0; ii< children.getLength(); ii++) {
            Node child = children.item(ii);
            if ("ServiceName".equalsIgnoreCase(child.getLocalName())  &&
                WS_ADDRESSING_NS.equals(child.getNamespaceURI())) {
                NamedNodeMap attributes = child.getAttributes();
                Node portNode = attributes.getNamedItem("PortName");
                String content = child.getTextContent().trim();
                int index = content.indexOf(":"); 			// locate the namespace prefix
                if (index <= 0) {
                    throw new Exception(mMessages.getString("HTTPBC-E00305.Service_name_invalid", content));
                }
                String prefix = content.substring(0, index);
                String serviceLocalName = content.substring(index + 1);
                String namespaceURI = (child.lookupNamespaceURI(prefix) == null)? parentNode.lookupNamespaceURI(prefix) :
                                      child.lookupNamespaceURI(prefix);
                if (namespaceURI == null) {
                    throw new Exception(mMessages.getString("HTTPBC-E00306.Namespace_prefix_not_defined", prefix));
                }
                String endpointName = portNode != null? portNode.getNodeValue() : null;
                QName serviceQName = new QName(namespaceURI, serviceLocalName);
                String uniqueName = AbstractEndpoint.getUniqueName(serviceQName.getNamespaceURI(),
                                                                   serviceQName.getLocalPart(),
                                                                   endpointName,
                                                                   false);
                if (uniqueName != null && endpoints.get(uniqueName) != null) {
                    endpoint = (Endpoint) endpoints.get(uniqueName);
                }
            }
        }
       
        return endpoint;
    }

    public ComponentLifeCycle getLifeCycle() {
        return mComponentLifeCycle;
    }

    public ServiceUnitManager getServiceUnitManager() {
        return mComponentLifeCycle.getServiceUnitManager();
    }
    
    
   
    
}
