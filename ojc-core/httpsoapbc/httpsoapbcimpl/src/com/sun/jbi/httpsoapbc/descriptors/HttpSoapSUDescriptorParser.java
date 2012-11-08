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
 * @(#)SUDescriptorSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.descriptors;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.sun.jbi.management.descriptor.ConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


/**
 * Parses and provides information about the HTTP BC service unit JBI descriptor
 * @author Sun Microsystems
 */
public class HttpSoapSUDescriptorParser {
    public final static String META_INF_DIR = "META-INF";
    public final static String JBI_DESC_FILE_NAME = "jbi.xml";

    public final static String SERVICES_TAG_NAME = "services";
    public final static String BC_TAG_NAME = "binding-component";
    public final static String PROVIDES_TAG_NAME = "provides";
    public final static String CONSUMES_TAG_NAME = "consumes";
    public final static String INTERFACE_TAG_NAME = "interface-name";
    public final static String SERVICE_TAG_NAME = "service-name";
    public final static String ENDPOINT_TAG_NAME = "endpoint-name";
    public final static String LINK_TYPE_TAG_NAME = "link-type";
    
    // application configuration object extension
    public final static String SUN_CONFIG_OBJECT_EXTENSION_NAMESPACE = "http://www.sun.com/jbi/descriptor/configuration";
    public final static String SUN_CONFIG_OBJECT_EXTENSION_ELEMENT_NAME = "application-config";
    public final static String SUN_CONFIG_OBJECT_ATTRIBUTE_NAME = "name";

    // hostname verifier extension
    public final static String HTTPBC_HOSTNAME_VERIFIER_NAMESPACE = "http://www.sun.com/jbi/httpbc/ssl/hostname_verifier";
    public final static String HTTPBC_HOSTNAME_VERIFIER_EXTENSION_ELEMENT_NAME = "hostname-verifier";
    public final static String HTTPBC_HOSTNAME_VERIFICATION_ATTRIBUTE_NAME = "verification";
    
    // HTTP timeout extension 
    public final static String HTTPBC_TIMEOUT_EXTENSION_NAMESPACE = "http://www.sun.com/jbi/httpbc/client_connection";
    public final static String HTTPBC_TIMEOUT_EXTENSION_ELEMENT_NAME = "client-connection";
    public final static String HTTPBC_CONNECT_TIMEOUT_ATTRIBUTE_NAME = "connectTimeout";
    public final static String HTTPBC_READ_TIMEOUT_ATTRIBUTE_NAME = "readTimeout";
    
    // SOAP header propagation extension
    public final static String HTTPBC_SOAPHEADER_EXTENSION_NAMESPACE = "http://www.sun.com/jbi/httpbc/soapheaders";
    public final static String HTTPBC_SOAPHEADER_EXTENSION_ELEMENT_NAME = "soap-headers";
    public final static String HTTPBC_SOAPHEADER_PROPAGATE_ATTRIBUTE_NAME = "propagated";
    
    // Enable ?wsdl query
    public final static String HTTPBC_WSDL_QUERY_EXTENSION_NAMESPACE = "http://www.sun.com/jbi/httpbc/wsdl_query";
    public final static String HTTPBC_WSDL_QUERY_EXTENSION_ELEMENT_NAME = "wsdl-query";
    public final static String HTTPBC_WSDL_QUERY_ENABLE_ATTRIBUTE_NAME = "enabled";
    
    // Handler extension
    public final static String HTTPBC_HANDLER_CHAIN_EXTENSION_NAMESPACE = "http://www.sun.com/jbi/httpbc/jaxws_handlers";
    public final static String HTTPBC_HANDLER_CHAIN_EXTENSION = "handler-chain";
    public final static String HTTPBC_HANDLER_EXTENSION = "handler";
    public final static String HTTPBC_HANDLER_NAME_ATTRIBUTE_NAME = "name";
    public final static String HTTPBC_HANDLER_CLASSNAME_ATTRIBUTE_NAME = "classname";
    public final static String HTTPBC_HANDLER_INIT_PROPS_EXTENSION = "init-properties";
    public final static String HTTPBC_HANDLER_PROPERTY_EXTENSION = "property";
    public final static String HTTPBC_HANDLER_PROPERTY_NAME_ATTRIBUTE = "name";
    public final static String HTTPBC_HANDLER_PROPERTY_DEFAULT_VALUE_ATTRIBUTE = "default-value";
    
    boolean descriptorExists;
    HttpSoapProvides[] provides;
    HttpSoapConsumes[] consumes;
    boolean bindingComponentAttr;

    /**
     * Creates a new instance of SUDescriptorSupport
     * @param serviceUnitRootPath the root path passed to the service unit manager
     * for service unit deployment
     */
    public HttpSoapSUDescriptorParser(String serviceUnitRootPath) throws ConfigurationException {

        File jbiDescriptorFile = new File(serviceUnitRootPath + File.separator + META_INF_DIR, JBI_DESC_FILE_NAME);

        descriptorExists = jbiDescriptorFile.exists();
        if (descriptorExists) {
            DocumentBuilder builder = null;
            try {
                DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
                fact.setNamespaceAware(true);
                builder = fact.newDocumentBuilder();
            }
            catch (Exception ex) {
                throw new ConfigurationException("Failure in initalizing parser " + ex.getMessage(), ex);
            }

            try {
                Document doc = builder.parse(jbiDescriptorFile);
                Element root = doc.getDocumentElement();


                NodeList serviceNL = root.getElementsByTagName(SERVICES_TAG_NAME);
                if (serviceNL != null && serviceNL.getLength() > 0) {
                    Element serviceElem = (Element) serviceNL.item(0);
                    String bcTagString = serviceElem.getAttribute(BC_TAG_NAME);
                    bindingComponentAttr = Boolean.valueOf(bcTagString).booleanValue();
                }

                NodeList nl = root.getElementsByTagName(PROVIDES_TAG_NAME);
                if (nl != null) {
                    int providesNoOfItems = nl.getLength();
                    provides = new HttpSoapProvides[providesNoOfItems];

                    for (int providesCount = 0; providesCount < providesNoOfItems; providesCount++) {
                        Node n = nl.item(providesCount);
                        Element e = (Element) n;

                        String ifLocalName = e.getAttribute(INTERFACE_TAG_NAME);
                        String serviceLocalName = e.getAttribute(SERVICE_TAG_NAME);
                        String endpointName = e.getAttribute(ENDPOINT_TAG_NAME);
                        // application configuration name is optional, defaults to null
                        String configName = null;
                        // hostname verification flag is optional, defaults to false
                        boolean hostnameVerification = true;  // SSL hostname verification should be turned on by default
                        boolean propagateSoapHeader = true;   // flag to determine whether or not to propagate the SOAP header
                        // HTTP timeout configurations are optional, defaults to null
                        Integer connectTimeout = null;
                        Integer readTimeout = null;
                        
                        // JAX-WS handler configurations
                        List<HttpSoapHandler> handlers = new ArrayList();

                        QName ifName = resolveAttrQName(ifLocalName, e);
                        QName serviceName = resolveAttrQName(serviceLocalName, e);
                        
                        // find the <application-configuration> child element in the <provides> entry
                        NodeList childNodes = e.getChildNodes();
                        for (int ii = 0; ii < childNodes.getLength(); ii++) {
                            // ignore any unknown extensions
                            Node aNode = childNodes.item(ii);
                            if (aNode instanceof Element) {
                                Element child = (Element) aNode;
                                if (SUN_CONFIG_OBJECT_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) &&
                                    SUN_CONFIG_OBJECT_EXTENSION_ELEMENT_NAME.equals(child.getLocalName())) {
                                    
                                    // get configuration name attribute	
                                    configName = child.getAttribute(SUN_CONFIG_OBJECT_ATTRIBUTE_NAME);
                                } else if (HTTPBC_HOSTNAME_VERIFIER_NAMESPACE.equals(child.getNamespaceURI()) &&
                                           HTTPBC_HOSTNAME_VERIFIER_EXTENSION_ELEMENT_NAME.equals(child.getLocalName())) {
                                    
                                    // get the hostname verification attribute	
                                    hostnameVerification = Boolean.valueOf(child.getAttribute(HTTPBC_HOSTNAME_VERIFICATION_ATTRIBUTE_NAME));
                                } else if (HTTPBC_TIMEOUT_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) &&
                                           HTTPBC_TIMEOUT_EXTENSION_ELEMENT_NAME.equals(child.getLocalName())) {
                                    // get the time out attributes
                                    String connectTimeoutStr = child.getAttribute(HTTPBC_CONNECT_TIMEOUT_ATTRIBUTE_NAME);
                                    String readTimeoutStr = child.getAttribute(HTTPBC_READ_TIMEOUT_ATTRIBUTE_NAME);
                                    if (connectTimeoutStr != null && !"".equals(connectTimeoutStr)) {
                                        connectTimeout = Integer.parseInt(connectTimeoutStr);
                                    }
                                    if (readTimeoutStr != null && !"".equals(readTimeoutStr)) {
                                        readTimeout = Integer.parseInt(readTimeoutStr);
                                    }
                                } else if (HTTPBC_SOAPHEADER_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) &&
                                           HTTPBC_SOAPHEADER_EXTENSION_ELEMENT_NAME.equals(child.getLocalName())) {
                                    // get the boolean value for SOAP header propagation
                                    propagateSoapHeader = Boolean.valueOf(child.getAttribute(HTTPBC_SOAPHEADER_PROPAGATE_ATTRIBUTE_NAME));
                                } else if (HTTPBC_HANDLER_CHAIN_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) && 
                                           HTTPBC_HANDLER_CHAIN_EXTENSION.equals(child.getLocalName())) {
                                    handlers = parseHandlerConfiguration(child);
                                }
                            }
                        }

                        provides[providesCount] = 
                            new HttpSoapProvides(ifName, serviceName, endpointName, configName, hostnameVerification, connectTimeout, readTimeout, propagateSoapHeader, handlers);
                    }
                }

                NodeList nl2 = root.getElementsByTagName(CONSUMES_TAG_NAME);
                if (nl2 != null) {
                    int consumesNoOfItems = nl2.getLength();
                    consumes = new HttpSoapConsumes[consumesNoOfItems];

                    for (int consumesCount = 0; consumesCount < consumesNoOfItems; consumesCount++) {
                        Node n = nl2.item(consumesCount);
                        Element e = (Element) n;

                        String ifLocalName = e.getAttribute(INTERFACE_TAG_NAME);
                        String serviceLocalName = e.getAttribute(SERVICE_TAG_NAME);
                        String endpointName = e.getAttribute(ENDPOINT_TAG_NAME);
                        
                        // application configuration name is optional, defaults to null
                        String configName = null;
                        // leave the optional link type null if it is not defined
                        String linkType = null;
                        boolean propagateSoapHeader = true;   // flag to determine whether or not to propagate the SOAP header
                        boolean enableWsdlQuery = true; // flag to enable ?wsdl
                        
                        // JAX-WS handler configurations
                        List<HttpSoapHandler> handlers = new ArrayList();
                        
                        if (e.hasAttribute(LINK_TYPE_TAG_NAME)) {
                            linkType = e.getAttribute(LINK_TYPE_TAG_NAME);
                        }

                        QName ifName = resolveAttrQName(ifLocalName, e);
                        QName serviceName = resolveAttrQName(serviceLocalName, e);

                        // find the <application-configuration> child element in the <provides> entry
                        NodeList childNodes = e.getChildNodes();
                        for (int ii = 0; ii < childNodes.getLength(); ii++) {
                            // ignore any unknown extensions
                            Node aNode = childNodes.item(ii);
                            if (aNode instanceof Element) {
                                Element child = (Element) aNode;
                                if (SUN_CONFIG_OBJECT_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) &&
                                    SUN_CONFIG_OBJECT_EXTENSION_ELEMENT_NAME.equals(child.getLocalName())) {
                                    // get configuration name attribute	
                                    configName = child.getAttribute(SUN_CONFIG_OBJECT_ATTRIBUTE_NAME);
                                } else if (HTTPBC_SOAPHEADER_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) &&
                                    HTTPBC_SOAPHEADER_EXTENSION_ELEMENT_NAME.equals(child.getLocalName())) {
                                    // get the boolean value for SOAP header propagation
                                    propagateSoapHeader = Boolean.valueOf(child.getAttribute(HTTPBC_SOAPHEADER_PROPAGATE_ATTRIBUTE_NAME));
                                } else if (HTTPBC_HANDLER_CHAIN_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) && 
                                           HTTPBC_HANDLER_CHAIN_EXTENSION.equals(child.getLocalName())) {
                                    handlers = parseHandlerConfiguration(child);
                                } else if (HTTPBC_WSDL_QUERY_EXTENSION_NAMESPACE.equals(child.getNamespaceURI()) &&
                                           HTTPBC_WSDL_QUERY_EXTENSION_ELEMENT_NAME.equals(child.getLocalName())) {
                                    enableWsdlQuery = Boolean.valueOf(child.getAttribute(HTTPBC_WSDL_QUERY_ENABLE_ATTRIBUTE_NAME));
                                }
                            }
                        }
                        
                        consumes[consumesCount] = new HttpSoapConsumes(ifName, serviceName, endpointName, linkType, configName, propagateSoapHeader, enableWsdlQuery, handlers);
                    }
                }
            } catch (SAXException ex) {
                throw new ConfigurationException("Failure in parsing HTTP BC service unit descriptor " + ex.getMessage(), ex);
            } catch (IOException ex) {
                throw new ConfigurationException("IO failure while parsing HTTP BC service unit descriptor " + ex.getMessage(), ex);
            } catch  (NumberFormatException ex) {
            	throw new ConfigurationException("Failure in parsing HTTP BC service unit descriptor " + ex.getMessage(), ex);
            }
        }
    }

    /**
     * @return the <provides> entries in the descriptor
     */
    public HttpSoapProvides[] getProvides() {
        // FindBug warning fix - make copy to avoid exposing internal representation
        // Fix to preserve original semantics.  provides object may be null;
        if (provides == null) {
            return provides;
        }

        int len = provides.length;
        HttpSoapProvides[] dest = new HttpSoapProvides[len];
        System.arraycopy(provides, 0, dest, 0, len);
        return dest;
    }

    /**
     * @return the <consumes> entries in the descriptor
     */
    public HttpSoapConsumes[] getConsumes() {
        // FindBug warning fix - make copy to avoid exposing internal representation
        // Fix to preserve original semantics.  consumes object may be null;
        if (consumes == null) {
            return consumes;
        }
        int len = consumes.length;
        HttpSoapConsumes[] dest = new HttpSoapConsumes[len];
        System.arraycopy(consumes, 0, dest, 0, len);
        return dest;
    }

    /**
     * @return the <provides> and <consumes> entries in the descriptor
     */
    public HttpSoapEndpointIdentifier[] getServices() {

        int providesLen = (provides != null ? provides.length : 0);
        int consumesLen = (consumes != null ? consumes.length : 0);
        int len = consumesLen + providesLen;
        HttpSoapEndpointIdentifier[] svcs = new HttpSoapEndpointIdentifier[len];
        if (provides != null) {
            System.arraycopy(provides, 0, svcs, 0, providesLen);
        }
        if (consumes != null) {
            System.arraycopy(consumes, 0, svcs, providesLen, consumesLen);
        }

        return svcs;
    }


    /**
     * Returns the services binding-component attribute hint
     * NOTE: this truly is a hint, the spec allows leeway for this to be
     * inconsistent with the actual deployment environment
     */
    public boolean isBindingComponentDescriptor() {
        return bindingComponentAttr;
    }

    /**
     * @return whether a descriptor was found in the SU deployment root path
     */
    public boolean isDescriptorPresent() {
        return descriptorExists;
    }

    /**
     * For attribute values which denote a QName, i.e. include a namespace prefix,
     * resolve the value into a QName.
     * If a namespace can not be resolved, it is set to empty - it does not
     * result in an exception
     * @param attrValue the string value of the attribute
     * @param element the element the attribute belongs to
     */
    private QName resolveAttrQName(String attrValue, Element element) {
        int aColonLoc = attrValue.indexOf(":");
        String aLocalName = attrValue;
        String aPrefix = null;
        String aNS = null;
        if (aColonLoc > -1) {
            aPrefix = attrValue.substring(0, aColonLoc);
            aLocalName = attrValue.substring(aColonLoc + 1);

            // Traverse up the hierarchy until a namespace definition is found
            // or the top of the document is reached.
            Node currNode = element;
            while ((aNS == null || aNS.equals("")) && currNode != null) {
                if (currNode.getNodeType() == Node.ELEMENT_NODE) {
                    aNS = ((Element) currNode).getAttribute("xmlns:" + aPrefix);
                }
                currNode = currNode.getParentNode();
            }
        }

        QName qName = new QName(aNS, aLocalName, aPrefix);

        return qName;
    }
    
    private List<HttpSoapHandler>  parseHandlerConfiguration(Element handlerChainRoot) {
    	List<HttpSoapHandler> handlerList = new ArrayList();
        
        NodeList handlerNodes = handlerChainRoot.getElementsByTagNameNS(HTTPBC_HANDLER_CHAIN_EXTENSION_NAMESPACE, HTTPBC_HANDLER_EXTENSION);
        if (handlerNodes != null) {
            int numOfHandlers = handlerNodes.getLength();
            
            for (int ii = 0; ii < numOfHandlers; ii++) {
                Element handlerElem = (Element) handlerNodes.item(ii);

                String displayname = handlerElem.getAttribute(HTTPBC_HANDLER_NAME_ATTRIBUTE_NAME);
                String classname = handlerElem.getAttribute(HTTPBC_HANDLER_CLASSNAME_ATTRIBUTE_NAME);
                
                // find the <application-configuration> child element in the <provides> entry
                NodeList childNodes = handlerElem.getChildNodes();
                Map attributes = new HashMap ();
                for (int jj = 0; jj < childNodes.getLength(); jj++) {
                    // ignore any unknown extensions
                    Node aNode = childNodes.item(jj);
                    if (aNode instanceof Element) {
                        Element child = (Element) aNode;
                        if (HTTPBC_HANDLER_INIT_PROPS_EXTENSION.equals(child.getLocalName())) {
                            NodeList propsNodes = child.getElementsByTagNameNS(HTTPBC_HANDLER_CHAIN_EXTENSION_NAMESPACE, HTTPBC_HANDLER_PROPERTY_EXTENSION);
                            for (int kk = 0; kk < propsNodes.getLength(); kk++) {
                            	Node propsNode = propsNodes.item(kk);
                            	if (propsNode instanceof Element) {
                                    attributes.put(((Element)propsNode).getAttribute(HTTPBC_HANDLER_PROPERTY_NAME_ATTRIBUTE), 
                                                   ((Element)propsNode).getAttribute(HTTPBC_HANDLER_PROPERTY_DEFAULT_VALUE_ATTRIBUTE));
                                }
                            }
                        }
                    }
                }       
                handlerList.add(new HttpSoapHandler(displayname, classname, attributes));
            }
        }
        
        return handlerList;
    }

}
