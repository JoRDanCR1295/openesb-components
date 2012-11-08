/*
 * @(#)Rules4JBIServiceEndpoint.java        $Revision: 1.3 $ $Date: 2008/07/14 16:30:26 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.config;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.DocumentFragment;

import nu.xom.Attribute;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Nodes;
import nu.xom.converters.DOMConverter;

import net.jcip.annotations.Immutable;

/**
 * Service engine specific <code>ServiceEndpoint</code> implementation.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/07/14 16:30:26 $
 * 
 * @see javax.jbi.servicedesc.ServiceEndpoint
 * @since 0.1
 */
@Immutable
public class Rules4JBIServiceEndpoint implements ServiceEndpoint {
    
    private static final String JBI_NAMESPACE_URI = "http://java.sun.com/xml/ns/jbi";

    private final QName interfaceName;
    
    private final QName serviceName;
    
    private final String endpointName;

    public Rules4JBIServiceEndpoint(String namespace, String interfaceName, String serviceName,
            String endpointName) 
    {
        this.interfaceName = new QName(namespace, interfaceName);
        this.serviceName = new QName(namespace, serviceName);
        this.endpointName = endpointName;
    }

    public DocumentFragment getAsReference(QName operationName) {
        try {
            Element endpointReference = createEndpointReference(serviceName.getNamespaceURI(),
                                                                serviceName.getLocalPart(),
                                                                endpointName);

            Document document = new Document(endpointReference);

            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            DOMImplementation domImplementation = builder.getDOMImplementation();

            org.w3c.dom.Document domDocument = DOMConverter.convert(document, domImplementation);
            DocumentFragment result = domDocument.createDocumentFragment();
            result.appendChild(domDocument.getDocumentElement());

            return result;
            
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
        }
    }
    
    /**
     * Creates an internal EPR for the given service endpoint parameters.
     * See JBI spec, section [5.5.4.1].
     * <p>
     * Example:
     * <code>
     * <jbi:end-point-reference xmlns:jbi="http://java.sun.com/xml/ns/jbi" 
     *                          xmlns:s="http://www.example.org/xml/ns/echo" 
     *                          jbi:end-point-name="echoPort" 
     *                          jbi:service-name="s:echoService"/>
     * </code>
     * @param uri namespace uri.
     * @param serviceName name of the service; will be qualified with the specified namespace uri.
     * @param endpointName name of the endpoint within the specified service.
     * @return internal endpoint reference as described in the JBI spec, section [5.5.4.1].
     */
    static Element createEndpointReference(String uri, String serviceName, String endpointName) {
        Element endpointReference = new Element("jbi:end-point-reference", JBI_NAMESPACE_URI);
        
        final String prefix = "s";
        endpointReference.addNamespaceDeclaration(prefix, uri);

        Attribute endpoint = new Attribute("jbi:end-point-name", JBI_NAMESPACE_URI, endpointName);
        endpointReference.addAttribute(endpoint);
        
        Attribute service = new Attribute("jbi:service-name", JBI_NAMESPACE_URI, prefix + ":" + serviceName);
        endpointReference.addAttribute(service);

        return endpointReference;
    }
    
    public String getEndpointName() {
        return endpointName;
    }

    public QName[] getInterfaces() {
        return new QName[] {interfaceName};
    }

    public QName getServiceName() {
        return serviceName;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Service Endpoint\n");
        String indent = "    ";

        sb.append(indent);
        sb.append("Interface: ");
        sb.append(interfaceName);
        sb.append("\n");

        sb.append(indent);
        sb.append("Service: ");
        sb.append(serviceName);
        sb.append("\n");

        sb.append(indent);
        sb.append("Endpoint: ");
        sb.append(endpointName);

        return sb.toString();
    }
    
    public static void main(String[] args) {
        Rules4JBIServiceEndpoint endpoint = 
                new Rules4JBIServiceEndpoint("http://example.org", "HelloPortType", "HelloService", "HelloPort");

        System.out.println(endpoint.toString());
        
        DocumentFragment reference = endpoint.getAsReference(null);
        Nodes nodes = DOMConverter.convert(reference);
        
        assert nodes.size() == 1;
        
        System.out.println(nodes.get(0).toXML());
    }
}
