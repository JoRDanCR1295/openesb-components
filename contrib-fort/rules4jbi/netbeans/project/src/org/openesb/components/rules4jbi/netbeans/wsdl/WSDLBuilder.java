/*
 * @(#)WSDLBuilder.java        $Revision: 1.2 $ $Date: 2008/11/24 12:47:18 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl;

import java.util.List;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.Types;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.factory.WSDLFactory;
import javax.xml.namespace.QName;

import nu.xom.Element;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import org.openesb.components.rules4jbi.shared.wsdl.WSDLConstants;
import org.openesb.components.rules4jbi.shared.wsdl.extension.ExtensionRegistrySupport;
import org.openesb.components.rules4jbi.shared.wsdl.extension.jbi.ServiceEngineExtensibilityElement;
import org.openesb.components.rules4jbi.shared.wsdl.extension.bpel.PartnerLinkExtensibilityElement;

import org.openesb.components.rules4jbi.netbeans.wsdl.schema.BusinessObjectManager;
import org.openesb.components.rules4jbi.netbeans.wsdl.schema.MainSchemaCreator;
import org.openesb.components.rules4jbi.netbeans.wsdl.schema.SecondarySchemaCreator;

/**
 * Builder for WSDL documents. Encapsulates the process of constructing
 * a valid JBI service engine WSDL. Before constructing the WSDL, i.e. calling
 * <code>createWSDL()</code> method, mandatory parameters must be provided.
 * Mandatory paramenters include porttype name, service name, port name,
 * partner link type and role.
 * You can provide also values for some other parts of the wsdl; default names
 * will be used if you don't do so.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/24 12:47:18 $
 * 
 * @since 0.1
 */
public class WSDLBuilder {
    
    private static final String DEFAULT_DEFINITIONS_LOCAL_NAME = "RuleDefinitions";

    private static final String DEFAULT_INPUT_MESSAGE_PART_NAME = "InputPart";

    private static final String DEFAULT_OUTPUT_MESSAGE_PART_NAME = "OutputPart";
    
    private static final String DEFAULT_OPERATION_NAME = "execute";
    
    private static final String DEFAULT_INPUT_NAME = "executeInput";

    private static final String DEFAULT_OUTPUT_NAME = "executeOutput";
    
    private static final String DEFAULT_BINDING_LOCAL_NAME = "RulesBinding";
    
    private Definition definition = null;
    
    private String targetNamespace = null;

    private String portTypeName = null;
    
    private String portName = null;

    private String serviceName = null;
    
    private String definitionsName = null;
    
    private String partnerLinkTypeName = null;
            
    private String partnerLinkRoleName = null;

    private BusinessObjectManager businessObjectManager = null;
    
    public WSDLBuilder(String targetNamespace) throws WSDLException {
        this.targetNamespace = targetNamespace;
        
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        definition = wsdlFactory.newDefinition();
        
        definition.setTargetNamespace(targetNamespace);
        definition.addNamespace("tns", targetNamespace);
        definition.addNamespace("xs", WSDLConstants.XML_SCHEMA_NAMESPACE_URI);
        
        //TODO: should we make types a constant and use it also from jbi wrapper?
        definition.addNamespace("types", WSDLConstants.TYPES_NAMESPACE_URI);
        
        definition.addNamespace(ServiceEngineExtensibilityElement.PREFERRED_NAMESPACE_PREFIX,
                                ServiceEngineExtensibilityElement.NAMESPACE_URI);
        
        /*
         * Note that we do not add the bpel extension namespace declaration to the root element;
         * it will be defined locally within the partnerLinkType element.
         */
        
        /* We need to use populated registry, because it contains Schema Extensibility elements */
        ExtensionRegistry registry = wsdlFactory.newPopulatedExtensionRegistry();
        
        ExtensionRegistrySupport.registerExtensions(registry);

        /*
         * We want the registry to throw an exception if it encounters
         * unknown extensions in the WSDL document
         */
        registry.setDefaultDeserializer(null);
        registry.setDefaultSerializer(null);
        
        definition.setExtensionRegistry(registry);
        
        businessObjectManager = new BusinessObjectManager();
    }
    
    public WSDLBuilder definitions(String definitionsName) {
        this.definitionsName = definitionsName;
        
        definition.setQName(new QName(targetNamespace, definitionsName));

        return this;
    }

    public WSDLBuilder inputObject(Class<?> objectType, int cardinality) {
        businessObjectManager.addInputObject(objectType, cardinality);
        
        return this;
    }
    
    public WSDLBuilder outputObject(Class<?> objectType, int cardinality) {
        businessObjectManager.addOutputObject(objectType, cardinality);
        
        return this;
    }
    
    public WSDLBuilder service(String serviceName) {
        this.serviceName = serviceName;
        
        return this;
    }
    
    public WSDLBuilder portType(String portTypeName) {
        this.portTypeName = portTypeName;
        
        return this;
    }

    public WSDLBuilder port(String portName) {
        this.portName = portName;
        
        return this;
    }
    
    public WSDLBuilder partnerLink(String typeName, String roleName) {
        partnerLinkTypeName = typeName;
        partnerLinkRoleName = roleName;
        
        return this;
    }
    
    public Definition createWSDL() throws WSDLException {
        
        //Test whether all mandatory components are definded
        
        if (portTypeName == null) {
            throw new IllegalStateException("Port type name must be specified before invoking this method");
        }
        
        if (portName == null) {
            throw new IllegalStateException("Port name must be specified before invoking this method");
        }

        if (serviceName == null) {
            throw new IllegalStateException("Service name must be specified before invoking this method");
        }
        
        if (partnerLinkTypeName == null) {
            throw new IllegalStateException(
                    "Partner link type name must be specified before invoking this method");
        }
        
        if (partnerLinkRoleName == null) {
            throw new IllegalStateException(
                    "Partner link role name must be specified before invoking this method");
        }

        
        //TODO: Test all optional components - like this:
        if (definitionsName == null) {
            definitions(DEFAULT_DEFINITIONS_LOCAL_NAME);
        }
        
        // Construct the WSDL
        
        SecondarySchemaCreator secondarySchemaCreator = new SecondarySchemaCreator(businessObjectManager);
        
        List<nu.xom.Element> schemas = secondarySchemaCreator.createSchemas();
        
        MainSchemaCreator mainSchemaCreator = new MainSchemaCreator(businessObjectManager);
        
        schemas.add(mainSchemaCreator.createSchema());
        
        buildTypes(schemas);
        
        Part inputMessagePart = 
                buildPart(DEFAULT_INPUT_MESSAGE_PART_NAME, WSDLConstants.INPUT_ELEMENT_QUALIFIED_NAME);
        Part outputMessagePart = 
                buildPart(DEFAULT_OUTPUT_MESSAGE_PART_NAME, WSDLConstants.OUTPUT_ELEMENT_QUALIFIED_NAME);
        
        Message inputMessage = buildMessage(WSDLConstants.INPUT_MESSAGE_LOCAL_NAME, inputMessagePart);
        Message outputMessage = buildMessage(WSDLConstants.OUTPUT_MESSAGE_LOCAL_NAME, outputMessagePart);
        Input input = buildInput(DEFAULT_INPUT_NAME, inputMessage);
        Output output = buildOutput(DEFAULT_OUTPUT_NAME, outputMessage);
        Operation operation = buildOperation(DEFAULT_OPERATION_NAME, input, output);
        PortType portType = buildPortType(operation);
        Binding binding = buildBinding(DEFAULT_BINDING_LOCAL_NAME, portType);
        Port port = buildPort(binding);
        buildService(port);
        buildPartnerLink(portType);
        
        return definition;
    }

    private void buildTypes(List<nu.xom.Element> schemas) throws WSDLException {
        Types types = definition.createTypes();

        for (Element schemaRoot : schemas) {
            Schema schema = (Schema) definition.getExtensionRegistry().createExtension(
                    Types.class, new QName(WSDLConstants.XML_SCHEMA_NAMESPACE_URI, "schema"));
            org.w3c.dom.Element domSchema = XOMUtils.elementToDocument(schemaRoot).getDocumentElement();
            schema.setElement(domSchema);
            types.addExtensibilityElement(schema);
        }

        definition.setTypes(types);
    }

    private Part buildPart(String name, QName elementName) {
        Part part = definition.createPart();
        part.setName(name);
        part.setElementName(elementName);
        
        return part;
    }

    private Message buildMessage(String name, Part part) {
        Message message = definition.createMessage();
        message.setQName(new QName(targetNamespace, name));
        message.addPart(part);
        message.setUndefined(false);
        definition.addMessage(message);

        return message;
    }

    private Input buildInput(String name, Message inputMessage) {
        Input input = definition.createInput();
        input.setName(name);
        input.setMessage(inputMessage);

        return input;
    }

    private Output buildOutput(String name, Message outputMessage) {
        Output output = definition.createOutput();
        output.setName(name);
        output.setMessage(outputMessage);

        return output;
    }

    private Operation buildOperation(String name, Input input, Output output) {
        Operation operation = definition.createOperation();
        operation.setName(name);
        operation.setInput(input);
        operation.setOutput(output);
        operation.setUndefined(false);

        return operation;
    }
    
    private PortType buildPortType(Operation operation) {
        PortType portType = definition.createPortType();
        portType.setQName(new QName(targetNamespace, portTypeName));
        portType.addOperation(operation);
        portType.setUndefined(false);
        definition.addPortType(portType);

        return portType;
    }

    private Binding buildBinding(String name, PortType portType) throws WSDLException {
        Binding binding = definition.createBinding();
        binding.setQName(new QName(targetNamespace, name));
        binding.setPortType(portType);
        
        binding.addExtensibilityElement(
                definition.getExtensionRegistry().createExtension(
                        Binding.class, ServiceEngineExtensibilityElement.QUALIFIED_NAME));
        
        binding.setUndefined(false);
        definition.addBinding(binding);

        return binding;
    }

    private Port buildPort(Binding binding) {
        Port port = definition.createPort();
        port.setName(portName);
        port.setBinding(binding);

        return port;
    }
    
    private void buildService(Port port) {
        Service service = definition.createService();
        service.setQName(new QName(targetNamespace, serviceName));
        service.addPort(port);
        definition.addService(service);
    }

    private void buildPartnerLink(PortType portType) throws WSDLException {
                        
        PartnerLinkExtensibilityElement partnerLinkExtensibilityElement =
                (PartnerLinkExtensibilityElement) definition.getExtensionRegistry().createExtension(
                        Definition.class, PartnerLinkExtensibilityElement.QUALIFIED_NAME);
        
        partnerLinkExtensibilityElement.setTypeName(partnerLinkTypeName);
        partnerLinkExtensibilityElement.setRoleName(partnerLinkRoleName);
        partnerLinkExtensibilityElement.setPortType(portType);
        
        definition.addExtensibilityElement(partnerLinkExtensibilityElement);
    }
    
    /** Self-testing module. */
//    public static void main(String[] args) throws WSDLException, FileNotFoundException {
//        WSDLBuilder builder = new WSDLBuilder(WSDLConstants.DEFINITIONS_NAMESPACE_URI);
//        WSDLBuilder builder = new WSDLBuilder("http://www.example.org/definitions/rules");
//        
//        builder.definitions("Rules").portType("RulePortType").service("RuleService").port("RulePort");
//        
//        builder.inputObject(Customer.class, 1).inputObject(Invoice.class, 1).outputObject(Invoice.class, 1);
//        
//        Definition wsdl = builder.createWSDL();
//        
//        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
//        WSDLWriter writer = wsdlFactory.newWSDLWriter();
//        Document document = writer.getDocument(wsdl);
//        nu.xom.Document xomDocument = DOMConverter.convert(document);
//        XOMUtils.prettyPrint(xomDocument);
//        
////        System.out.println();
////        writer.writeWSDL(wsdl, System.out);
////        System.out.println();
//        writer.writeWSDL(wsdl, new FileOutputStream(new File("/tmp/rules4jbi.wsdl")));
//    }
}
