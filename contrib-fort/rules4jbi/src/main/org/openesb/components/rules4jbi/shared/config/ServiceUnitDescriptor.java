/*
 * @(#)ServiceUnitDescriptor.java        $Revision: 1.4 $ $Date: 2008/11/11 00:25:29 $
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import nu.xom.Attribute;
import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Namespace;
import nu.xom.ParsingException;
import nu.xom.Serializer;
import nu.xom.ValidityException;
import nu.xom.XPathContext;

import net.jcip.annotations.Immutable;

/**
 * Deployment descriptor of a sevice unit. Used for service units that provide exactly
 * one service, don't consume any, and are deployed to a service engine (not to a binding component).
 * See the JBI spec, sections [6.3.1], [6.3.1.10], and [6.3.1.11].
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/11/11 00:25:29 $
 * 
 * @since 0.1
 */
@Immutable
public final class ServiceUnitDescriptor implements Saveable {
    
    private static final String JBI_NAMESPACE_URI = "http://java.sun.com/xml/ns/jbi";
    
    private static final String VERSION_ATTRIBUTE_VALUE = "1.0";
    
    private static final String NAMESPACE_PREFIX = "ns1";
    
    private final QName interfaceName;
    
    private final QName serviceName;
    
    private final String endpointName;
    
    private final ServiceEndpoint serviceEndpoint;
    
    /** XML representation of this service unit descriptor. */
    private final Document document;

    public ServiceUnitDescriptor(String namespace, String interfaceName, String serviceName, String endpointName) 
    {
        this.interfaceName = new QName(namespace, interfaceName);
        
        this.serviceName = new QName(namespace, serviceName);
        
        this.endpointName = endpointName;
        
        serviceEndpoint = new Rules4JBIServiceEndpoint(namespace, interfaceName, serviceName, endpointName);
        
        document = new Document(createDescriptor(namespace, interfaceName, serviceName, endpointName));
    }

    static Element createDescriptor(String namespace, String interfaceName, String serviceName, 
            String endpointName)
    {
        Element descriptor = new Element("jbi", JBI_NAMESPACE_URI);

        Attribute version = new Attribute("version", VERSION_ATTRIBUTE_VALUE);
        descriptor.addAttribute(version);
        
        descriptor.addNamespaceDeclaration(NAMESPACE_PREFIX, namespace);

        Element services = new Element("services", JBI_NAMESPACE_URI);

        Attribute bindingComponent = new Attribute("binding-component", "false");
        services.addAttribute(bindingComponent);

        Element provides = new Element("provides", JBI_NAMESPACE_URI);

        Attribute interfaceAttribute = new Attribute("interface-name", NAMESPACE_PREFIX + ":" + interfaceName);
        provides.addAttribute(interfaceAttribute);

        Attribute serviceAttribute = new Attribute("service-name", NAMESPACE_PREFIX + ":" + serviceName);
        provides.addAttribute(serviceAttribute);

        Attribute endpointAttribute = new Attribute("endpoint-name", endpointName);
        provides.addAttribute(endpointAttribute);

        services.appendChild(provides);

        descriptor.appendChild(services);
        
        return descriptor;
    }
    
    public static ServiceUnitDescriptor load(InputStream inputStream) throws InvalidServiceUnitDescriptorException {
        try {
            Builder builder = new Builder();
            Document descriptor = builder.build(inputStream);

            XPathContext context = new XPathContext("jbi", JBI_NAMESPACE_URI);
            
            
            /* extract interface-name attribute value from the provides element */
            
            Attribute interfaceNameAttribute =
                    (Attribute) descriptor.query("//jbi:provides/@interface-name", context).get(0);
            
            String prefixedInterfaceName = interfaceNameAttribute.getValue();

            String[] parsedInterfaceName = prefixedInterfaceName.split(":");

            if (parsedInterfaceName.length != 2) {
                throw new InvalidServiceUnitDescriptorException("Could not parse the interface name");
            }
            
            final String prefix = parsedInterfaceName[0];
            String interfaceName = parsedInterfaceName[1];

            
            /* extract service-name attribute value from the provides element */
            
            Attribute serviceNameAttribute =
                    (Attribute) descriptor.query("//jbi:provides/@service-name", context).get(0);

            String prefixedServiceName = serviceNameAttribute.getValue();

            String[] parsedServiceName = prefixedServiceName.split(":");

            if (parsedServiceName.length != 2) {
                throw new InvalidServiceUnitDescriptorException("Could not parse the service name");
            }

            if (!prefix.equals(parsedServiceName[0])) {
                throw new InvalidServiceUnitDescriptorException("Different namespace prefixes found");
            }
            
            String serviceName = parsedServiceName[1];

            
            /* extract endpoint-name attribute value from the provides element */

            Attribute endpointNameAttribute =
                    (Attribute) descriptor.query("//jbi:provides/@endpoint-name", context).get(0);

            String endpointName = endpointNameAttribute.getValue();

            
            /* extract namespace uri value that qualifies both interfaceName and serviceName */

            Namespace namespace = 
                    (Namespace) descriptor.query("//jbi:provides/namespace::" + prefix, context).get(0);

            String namespaceValue = namespace.getValue();

            return new ServiceUnitDescriptor(namespaceValue, interfaceName, serviceName, endpointName);

        } catch (ValidityException e) {
            throw new InvalidServiceUnitDescriptorException(e);

        } catch (ParsingException e) {
            throw new InvalidServiceUnitDescriptorException(e);
            
        } catch (IOException e) {
            throw new InvalidServiceUnitDescriptorException(e);
            
        } catch (Exception e) {
            throw new InvalidServiceUnitDescriptorException(e);
        }
    }
    
    public void save(OutputStream outputStream) throws SaveFailedException {
        try {
//            Serializer serializer = new Serializer(outputStream, "ISO-8859-1");
            Serializer serializer = new Serializer(outputStream, "UTF-8");
            serializer.setIndent(4);
//            serializer.setMaxLength(64);
            serializer.write(document);

        } catch (IOException e) {
            throw new SaveFailedException(e);
        }
    }
    
    public QName getInterfaceName() {
        return interfaceName;
    }

    public QName getServiceName() {
        return serviceName;
    }
    
    public String getEndpointName() {
        return endpointName;
    }

    public ServiceEndpoint getServiceEndpoint() {
        return serviceEndpoint;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Service Unit Descriptor\n");
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
        sb.append("\n");
        
        return sb.toString();
    }
    
    public String toXML() {
        return document.toXML();
    }
    
//    public static void main(String[] args) throws FileNotFoundException, SaveFailedException {
//        ServiceUnitDescriptor descriptor = new ServiceUnitDescriptor(
//                "http://www.example.com/xml/ns/descriptor", "TestPortType", "TestService", "TestPort");
//        
//        System.out.println(descriptor.toString());
//        
//        File file = new File("/tmp/jbi.xml");
//        
//        descriptor.save(new FileOutputStream(file));
//
//        ServiceUnitDescriptor loadedDescriptor = ServiceUnitDescriptor.load(new FileInputStream(file));
//        
//        System.out.println(loadedDescriptor.toString());
//        
//        System.out.println(loadedDescriptor.toXML());
//     
//        System.out.println(loadedDescriptor.getServiceEndpoint().toString());
//    }
}
