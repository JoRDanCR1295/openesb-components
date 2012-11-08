/*
 * @(#)ServiceUnit.java        $Revision: 1.10 $ $Date: 2009/01/14 02:53:12 $
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

package org.openesb.components.rules4jbi.engine.component;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletionService;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.InOut;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

import com.google.inject.Inject;

import nu.xom.Attribute;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Node;
import nu.xom.Nodes;
import nu.xom.XPathContext;
import nu.xom.converters.DOMConverter;

import org.openesb.components.rules4jbi.shared.config.Configuration;
import org.openesb.components.rules4jbi.shared.config.InvalidConfigurationException;
import org.openesb.components.rules4jbi.shared.config.InvalidServiceUnitDescriptorException;
import org.openesb.components.rules4jbi.shared.config.ServiceUnitDescriptor;
import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import org.openesb.components.rules4jbi.shared.wsdl.WSDLConstants;
import org.openesb.components.rules4jbi.shared.wsdl.extension.ExtensionRegistrySupport;

import org.openesb.components.rules4jbi.engine.guice.annotations.MessageExchangeProcessor;
import org.openesb.components.rules4jbi.engine.guice.annotations.Unit;

import org.openesb.components.rules4jbi.shared.classloader.BusinessObjectsNotFoundException;
import org.openesb.components.rules4jbi.shared.classloader.ClassLoaderFactory;

import static org.openesb.components.rules4jbi.shared.GlobalConstants.CLASSES_DIR;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.ENGINE_DIR;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.LIBRARIES_DIR;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.CONFIG_FILE_NAME;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.JBI_FILE_NAME;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.META_INF_DIR;

/**
 * This class represents a service unit; a single deployed executable ruleset.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.10 $ $Date: 2009/01/14 02:53:12 $
 * 
 * @since 0.1
 */
public final class ServiceUnit {
    
    private static final String SERVICE_UNIT_DESCRIPTOR_LOCATION =
            META_INF_DIR + File.separator + JBI_FILE_NAME;
    
    
    /* Injected resources */
    
    @Inject @Unit
    private Logger logger;
    
    @Inject
    private ComponentContext componentContext;
    
    @Inject @MessageExchangeProcessor
    private CompletionService<InOut> messageExchangeProcessor;

    @Inject
    private RuleExecutionTaskFactory ruleExecutionTaskFactory;

    
    /* User provided resources */

    private String name = null;
    
    private String rootPath = null;
    
    
    /* Computed resources */

    private Configuration configuration;
    
    private Definition definition;
    
    private ServiceUnitDescriptor descriptor;
    
    private ServiceEndpoint serviceEndpoint;
    
    private volatile boolean initialized = false;
    
    private volatile boolean activated = false;

    /** Ordered list of output object element names. */
    private List<QName> outputElements;
    
    private ClassLoader classLoader;
    
    /** Objects representing facts used by this service unit. */
    private Class<?>[] classes;
    
    /* Instances of this class should be created only by Guice */
    private ServiceUnit() {}

    void setName(String name) {
        this.name = name;
    }

    void setRootPath(String rootPath) {
        this.rootPath = rootPath;
    }
    
    public void init() throws DeploymentException {
        if (name == null || rootPath == null) {
            throw new IllegalStateException(
                    "This service unit was not created properly via the ServiceUnitFactory");
        }
        
        configuration = loadConfiguration(new File(rootPath, CONFIG_FILE_NAME));
        
        descriptor = loadServiceUnitDescriptor(
                new File(rootPath + File.separator + SERVICE_UNIT_DESCRIPTOR_LOCATION));
        
        logger.fine("Deployment descriptor for this service unit: %s", descriptor.toString());
        
        definition = loadWSDL(rootPath + File.separator + configuration.getWSDLFile());
        
        logger.fine("WSDL target namespace: '%s'", definition.getTargetNamespace());

        List<?> extensibilityElements = definition.getExtensibilityElements();
        
        for (Object extensibilityElement : extensibilityElements) {
            logger.fine("Found extensibility element: %s", extensibilityElement);
        }
        
//        Document wsdlDocument = DOMConverter.convert(getServiceDescription());
//        XOMUtils.prettyPrint(wsdlDocument);
        
        outputElements = parseOutputElements(definition);
        
        logger.fine("Parsed output elements: %s", outputElements);
        
        classLoader = initClassLoader(
                new File(rootPath, CLASSES_DIR),
                new File(rootPath, LIBRARIES_DIR),
                new File(rootPath, ENGINE_DIR));
        
        classes = loadClasses(classLoader, configuration.getClasses());
            
        initialized = true;
    }

    private Configuration loadConfiguration(File file) throws DeploymentException {
        InputStream inputStream = null;
        
        try {
            logger.fine("Loading configuration from file '%s'", file.getAbsolutePath());
            
            inputStream = new FileInputStream(file);
            
            return Configuration.load(inputStream);

        } catch (FileNotFoundException e) {
            logger.severe("Could not find the configuration file");

            throw new DeploymentException(e);
            
        } catch (InvalidConfigurationException e) {
            logger.severe("Could not retrieve the configuration file");

            throw new DeploymentException(e);

        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                    
                } catch (IOException e) {
                    logger.fine("Failed to close the input stream", e);
                }
            }
        }
    }
    
    private ServiceUnitDescriptor loadServiceUnitDescriptor(File file) throws DeploymentException {
        InputStream inputStream = null;

        try {
            logger.fine("Loading service unit descriptor from file '%s'", file.getAbsolutePath());
            
            inputStream = new FileInputStream(file);
            
            return ServiceUnitDescriptor.load(inputStream);

        } catch (FileNotFoundException e) {
            logger.severe("Could not find the service unit descriptor file");

            throw new DeploymentException(e);

        } catch (InvalidServiceUnitDescriptorException e) {
            logger.severe("Could not retrieve the deployment descriptor");

            throw new DeploymentException(e);

        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();

                } catch (IOException e) {
                    logger.fine("Failed to close the input stream", e);
                }
            }
        }
    }
    
    private Definition loadWSDL(String wsdlFilePath) throws DeploymentException {
        
        try {
            logger.fine("Loading WSDL from file '%s'", wsdlFilePath);
            
            WSDLFactory wsdlFactory = WSDLFactory.newInstance();

            /* We need to use populated registry, because it contains Schema Extensibility elements */
            ExtensionRegistry registry = wsdlFactory.newPopulatedExtensionRegistry();

            ExtensionRegistrySupport.registerExtensions(registry);

            /*
             * We want the registry to throw an exception if it encounters
             * unknown extensions in the WSDL document
             */
            registry.setDefaultDeserializer(null);
            registry.setDefaultSerializer(null);

            WSDLReader wsdlReader = wsdlFactory.newWSDLReader();

            wsdlReader.setExtensionRegistry(registry);

            /* Do not send anything to the standard output stream */
            wsdlReader.setFeature("javax.wsdl.verbose", false);

            /* The WSDL used to describe this jbi component's services does not import anything */
            wsdlReader.setFeature("javax.wsdl.importDocuments", false);
            
            return wsdlReader.readWSDL(wsdlFilePath);
            
        } catch (WSDLException e) {
            logger.severe("Could not read the WSDL document", e);

            throw new DeploymentException(e);
        }
    }
    
    private List<QName> parseOutputElements(final Definition definition) throws DeploymentException {
        try {
            WSDLFactory factory = WSDLFactory.newInstance();
            WSDLWriter writer = factory.newWSDLWriter();

            Document wsdl = DOMConverter.convert(writer.getDocument(definition));
            
//            logger.fine(wsdl.toXML());

            XPathContext context = new XPathContext("xs", WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

            Nodes query = wsdl.query("//xs:schema[@targetNamespace='" + WSDLConstants.TYPES_NAMESPACE_URI + "']",
                    context);

            if (query.size() != 1) {
                throw new DeploymentException("Invalid WSDL; could not find the output element");
            }

            Element typesSchemaElement = (Element) query.get(0);

            logger.fine("Found types schema element:");
            logger.finest(typesSchemaElement.toXML());

            query = typesSchemaElement.query("//xs:element[@name='" + WSDLConstants.OUTPUT_ELEMENT_NAME + "']"
                    +"/xs:complexType/xs:sequence/xs:element/@ref", context);

            List<Node> refAttributes = XOMUtils.asList(query);

            logger.fine("Found %d output element references", refAttributes.size());
            
            List<QName> result = new ArrayList<QName>();
            
            for (Node node : refAttributes) {
                Attribute refAttribute = (Attribute) node;

                String prefixedElementName = refAttribute.getValue();

                logger.fine("Parsing element reference: %s", prefixedElementName);

                String[] parsedElementName = prefixedElementName.split(":");

                if (parsedElementName.length != 2) {
                    throw new DeploymentException("Invalid WSDL; could not parse output element reference");
                }

                String prefix = parsedElementName[0];
                String localName = parsedElementName[1];

                String namespaceURI = typesSchemaElement.getNamespaceURI(prefix);

                if (namespaceURI == null) {
                    throw new DeploymentException(
                            "Invalid WSDL; could not find namespace uri of a referenced element");
                }
                
                logger.fine("Namespace uri corresponding to prefix '%s' is '%s'", prefix, namespaceURI);
                
                QName elementQualifiedName = new QName(namespaceURI, localName);

                logger.fine("Successfully parsed output element: %s", elementQualifiedName);
                
                result.add(elementQualifiedName);
            }

            return Collections.unmodifiableList(result);

        } catch (WSDLException e) {
            logger.severe("Unable to parse service description", e);

            throw new DeploymentException(e);
            
        } catch (Exception e) {
            logger.severe("Unknown error occurred while parsing the output elements", e);
            
            throw new DeploymentException(e);
        }
    }
    
    private ClassLoader initClassLoader(final File classesDir, final File libDir, final File engineDir)
            throws DeploymentException
    {
        try {
            logger.fine("Initializing class loader");
            
            logger.fine("Classes directory URL: '%s'", classesDir.toURI().toURL().toString());
            logger.fine("Libraries directory URL: '%s'", libDir.toURI().toURL().toString());
            logger.fine("Rules Engine directory URL: '%s'", engineDir.toURI().toURL().toString());
            
            return ClassLoaderFactory.createServiceUnitClassLoader(
                    classesDir, libDir, engineDir, this.getClass().getClassLoader());
            
        } catch (MalformedURLException e) {
            logger.severe("Failed to initialize the classloader");
            
            throw new DeploymentException(e);
            
        } catch (BusinessObjectsNotFoundException e) {
            logger.severe("Failed to initialize the classloader; no business objects found");
            
            throw new DeploymentException(e);
        } 
    }
    
    private Class<?>[] loadClasses(ClassLoader classLoader, List<String> classNames) throws DeploymentException {
        logger.fine("Loading classes: %s", classNames);

        Class<?>[] result = new Class<?>[classNames.size()];

        for (int i = 0; i < result.length; i++) {
            String className = classNames.get(i);

            try {
                result[i] = Class.forName(className, true, classLoader);

                logger.fine("Successfully loaded class '%s'", className);

            } catch (ClassNotFoundException e) {
                logger.severe("Failed to load class '%s'", className);

                throw new DeploymentException(e);
            }
        }

        return result;
    }
    
    public void start() throws DeploymentException {
        logger.entering(this.getClass(), "start");
        
        if (!initialized) {
            throw new IllegalStateException("This service unit is not yet initialized");
        }
        
        try {
            serviceEndpoint = 
                    componentContext.activateEndpoint(descriptor.getServiceName(), descriptor.getEndpointName());
            activated = true;
            
        } catch (JBIException e) {
            throw new DeploymentException(e);
        }
    }
    
    public void stop() throws DeploymentException {
        if (!activated) {
            throw new IllegalStateException("This service unit was not yet activated");
        }
        
        try {
            componentContext.deactivateEndpoint(serviceEndpoint);
            activated = false;
            
        } catch (JBIException e) {
            throw new DeploymentException(e);
        }
    }
    
    public void shutDown() {
        initialized = false;
    }

    public void process(final InOut messageExchange) {
        
        final RuleExecutionTask task = ruleExecutionTaskFactory.createNewRuleExecutionTask(
                configuration.getRuleServiceProvider(),
                configuration.getRuleServiceProviderClass(),
                new File(rootPath, configuration.getRulesetFile()),
                classes,
                classLoader,
                definition.getTargetNamespace(),
                outputElements,
                messageExchange
        );
        
        messageExchangeProcessor.submit(task);
    }
    
    public org.w3c.dom.Document getServiceDescription() {
        try {
            
            WSDLFactory factory = WSDLFactory.newInstance();
            WSDLWriter writer = factory.newWSDLWriter();

            return writer.getDocument(definition);
            
        } catch (WSDLException e) {
            logger.severe("Unable to create service description", e);
            
            return null;
        }
    }
    
    public ServiceEndpoint getServiceEndpoint() {
        
        return descriptor.getServiceEndpoint();
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        sb.append("Service Unit '");
        sb.append(name);
        sb.append("'\n");
        sb.append("root-path: ");
        sb.append(rootPath);
        sb.append("\n");
        
        if (initialized) {
            sb.append(configuration);
            sb.append("\n");
            sb.append(descriptor);
            sb.append("\n");
            sb.append("wsdl-name: ");
            sb.append(definition.getQName());
            sb.append("\n");
        }
        
        return sb.toString();
    }
}
