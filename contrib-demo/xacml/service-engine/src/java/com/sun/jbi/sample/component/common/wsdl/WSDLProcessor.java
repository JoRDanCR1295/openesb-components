/*
 * WSDLProcessor.java
 */
package com.sun.jbi.sample.component.common.wsdl;

import java.io.File;
import java.io.FilenameFilter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ElementExtensible;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

/**
 * This class is used to configure jwsdl(wsdl4j) to read and process wsdl documents with wsdl extensions.
 * It provides set of helper methods to read and process the wsdl definitions from files with .wsdl
 * extension from a specified directory.
 * <p>
 * A Binding Component that is processing the wsdl extensions for its deployment configuration would
 * extend this class and provide the required ExtensionRegistry that will have the extension serializers
 * and deserializers configured to read/write the extensions from/to the java model.
 * <p>
 * A Service Engine that is processing the wsdl during deployment can directly use this class
 * to process the wsdl as the default implementation returned by this class configures the wsdl extension
 * registry to read/write the service engine binding extensions.
 *
 * @see AbstractExtensionRegistry
 * @see SEBindingExt
 * @author chikkala
 */
public class WSDLProcessor {

    private String mXmlCatalogPath = "xml-catalog.xml";
    private String mWsdlDirPath = "";
    private WSDLReader mReader;

    /** Creates a new instance of WSDLProcessor
     */
    public WSDLProcessor(String wsdlDir) {
        this(wsdlDir, null);
    }

    /** Creates a new instance of WSDLProcessor
     */
    public WSDLProcessor(String wsdlDir, String xmlCatPath) {
        if (wsdlDir != null) {
            this.mWsdlDirPath = wsdlDir;
        }
        if (xmlCatPath != null) {
            this.mXmlCatalogPath = xmlCatPath;
        }
    }

    /** @return directory path from which this class reads the wsdl files with .wsdl as file extension. */
    public String getWSDLDirectory() {
        return this.mWsdlDirPath;
    }

    /** path to the xml catalog file in the service unit which can be used for Catalog-based entity
     * and URI resolution.
     */
    public String getXmlCatelogPath() {
        return this.mXmlCatalogPath;
    }

    /** wsdl extension registry required for processing the wsdl extensions in the wsdl definition to
     * java model. Binding component that is processing the wsdl extensions for its deployment
     * configuration would provide the required ExtensionRegistry that will have the extension serializers
     * and deserializers configured to read/write the extensions from/to the java model.
     * @return ExtensionSerializer
     * @see AbstractExtensionSerializer
     */
    protected ExtensionRegistry getExtensionRegistry() {
        return new AbstractExtensionRegistry() {

            protected List<AbstractExtensionSerializer> createSerializers() {
                return new ArrayList<AbstractExtensionSerializer>();
            }
        };
    }

    /**
     * @return the WSDLReader configured with extension registry to process the wsdl extensions.
     */
    public final WSDLReader getWSDLReader() throws WSDLException {
        if (this.mReader == null) {
            WSDLFactory factory = WSDLFactory.newInstance();
            this.mReader = factory.newWSDLReader();
            // reader.setFeature("javax.wsdl.verbose", true);
            // reader.setFeature("javax.wsdl.importDocuments", true);
            this.mReader.setExtensionRegistry(getExtensionRegistry());
        }
        return this.mReader;
    }

    /**
     * reads the wsdl file and returns the wsdl definition jwsdl model.
     * @param wsldFilePath relative path to wsdl file from the the root wsdl directory returns from
     * #getWSDLDirectory in the service unit or or absolute path .
     * @return Definition
     */
    public Definition readWSDL(String wsdlFilePath) throws WSDLException {
        File wsdlFile = new File(wsdlFilePath);
        if (!wsdlFile.isAbsolute()) {
            wsdlFile = new File(this.mWsdlDirPath, wsdlFilePath);
        }
        return getWSDLReader().readWSDL(wsdlFile.getAbsolutePath());
    }

    /**
     * reads the files with .wsdl file extension in a directory fromDir and return the list of
     * wsdl definitions corresponding to them.
     * @param fromDir path to the directory relative to the root wsdl directory returns from
     * #getWSDLDirectory or the absolute path to the directory.
     */
    public List<Definition> readWSDLs(String fromDir) throws WSDLException {
        if (fromDir == null) {
            fromDir = "";
        }
        File wsdlDir = new File(fromDir);
        if (!wsdlDir.isAbsolute()) {
            wsdlDir = new File(this.mWsdlDirPath, fromDir);
        }
        String[] wsdlFiles = wsdlDir.list(new FilenameFilter() {

            public boolean accept(File dir,
                    String name) {
                return (name != null && name.endsWith(".wsdl"));
            }
        });
        List<Definition> wsdlList = new ArrayList<Definition>();
        for (String wsdlFile : wsdlFiles) {
            Definition wsdlDef = readWSDL(wsdlFile);
            wsdlList.add(wsdlDef);
        }
        return wsdlList;
    }

    /**
     * finds PortType using port type ( interface ) qname.
     */
    public static PortType findInterface(Definition wsdlDef, QName interfaceName) {
        return wsdlDef.getPortType(interfaceName);
    }

    /** finds the Service using service qname */
    public static Service findService(Definition wsdlDef, QName serviceName) {
        return wsdlDef.getService(serviceName);
    }

    /** finds the wsdl port using service qname and endpoint name */
    public static Port findServiceEndpoint(Definition wsdlDef, QName serviceName, String endpointName) {
        Service service = null;
        Port port = null;
        service = findService(wsdlDef, serviceName);
        if (service != null) {
            port = service.getPort(endpointName);
        }
        return port;
    }

    /**
     * finds the binding definition to which the service with serviceName and endpointName was bound.
     */
    public static Binding findServiceBinding(Definition wsdlDef, QName serviceName, String endpointName) {
        Binding binding = null;
        Port port = findServiceEndpoint(wsdlDef, serviceName, endpointName);
        if (port != null) {
            binding = port.getBinding();
        }
        return binding;
    }

    /**
     * finds the binding definition using the interface(portType) qname with a
     */
    public static Binding findInterfaceBinding(Definition wsdlDef,
            QName interfaceQName, QName extQName) {
        Map bindingMap = wsdlDef.getBindings();
        @SuppressWarnings("unchecked")
        Collection<Binding> bindings = bindingMap.values();
        for ( Binding binding : bindings) {
            if (binding.getPortType().getQName().equals(interfaceQName)) {
                return binding;
            }
        }
        return null;
    }

    /**
     * find the wsdl4j operation corresponds to the interface+operation.
     * @param wsdlDef wsdl definition
     * @param portTypeQName portType QName
     * @param opName operation name. if null, first operation in the portType
     * is returned.
     * @return Operation corresponding to the portType+opName
     */
    public static Operation findOperation(Definition wsdlDef,
            QName portTypeQName, String opName) {
        Operation operation = null;
        PortType portType = wsdlDef.getPortType(portTypeQName);
        if (portType != null) {
            if (opName != null) {
                operation = portType.getOperation(opName, null, null);
            } else {
                @SuppressWarnings("unchecked")
                List<Operation> list = portType.getOperations();
                if (list != null && list.size() > 0) {
                    operation = list.get(0);
                }
            }
        }
        return operation;
    }

    /**
     * Verifies whether the wsdl definition contains the specified service descriptions or not. Used
     * to locate the wsdl definition for the services described in the service unit deployment
     * descriptor(jbi.xml). (sblais, fixed some spell checks)
     * @param interfaceName portType qname to find in the definition. can be null if you are trying to
     * find only service endpoint description.
     * @param serviceName qname for the service to find in this wsdl. can be null if
     * you are trying to find only portType (abstract service) description.
     * @param endpointName port name to find in the service definition. null if only
     * service with any port should be looked up.
     *
     * @return true if the wsdl definition contains the specified service description.
     */
    public static boolean isWSDLFor(Definition wsdlDef,
            QName interfaceName, QName serviceName, String endpointName) {
        PortType portType = null;
        Service service = null;
        Port port = null;
        if (interfaceName != null) {
            portType = findInterface(wsdlDef, interfaceName);
        }

        if (serviceName != null) {
            service = findService(wsdlDef, serviceName);
        }

        if (endpointName != null && service != null) {
            port = service.getPort(endpointName);
        }

        boolean isWSDL = true;

        if ((interfaceName != null && portType == null) ||
                (serviceName != null && service == null) ||
                (endpointName != null && (service == null || port == null))) {
            isWSDL = false;
        }
        // sblais adding some traces.
        if (!isWSDL) {
            if (interfaceName == null) {
                System.out.println("the interface name is null");
            } else {
                System.out.println("the interface name is " + interfaceName);
            }
            if (serviceName == null) {
                System.out.println("the service name is null");
            } else {
                System.out.println("the service name is " + serviceName);
            }

            if (service == null) {
                System.out.println("the service is null");
            } else {
                System.out.println("the service is " + service);
            }

            if (endpointName == null) {
                System.out.println("the endpoint name is null");
            } else {
                System.out.println("The endpoint name is " + endpointName);
            }
            if (portType == null) {
                System.out.println("the port type is null");
            } else {
                System.out.println("The port type is " + portType);
            }

            if (port == null) {
                System.out.println("the port is null");
            } else {
                System.out.println("The port is " + port);
            }
        }
        return isWSDL;
    }

    /**
     * creates a binding definition that contains a service engine binding elements in the specified
     * wsdl definition for a portType. It will try to find/create the binding element with interface
     * local name with a "_JBISEBinding" suffix and add service engine binding element to it if it
     * is not present.
     * @param wsdl wsdl definition
     * @param interfaceName portType qname to which the binding is created.
     * @return a Binding contains service engine binding that is created for the portType.
     */
    public Binding createServiceEngineBinding(Definition wsdl, QName interfaceName) {
        QName bindingQName = new QName(wsdl.getQName().getNamespaceURI(),
                interfaceName.getLocalPart() + "_JBISEBinding");
        Binding binding = wsdl.getBinding(bindingQName);
        if (binding == null) {
            binding = wsdl.createBinding();
            binding.setQName(bindingQName);
            binding.setPortType(wsdl.getPortType(interfaceName));
            binding.setUndefined(false);

            ExtensibilityElement bindingExt =
                    SEBindingExt.SEBindingExtImpl.addExtensibilityElement(wsdl, binding);

            wsdl.addBinding(binding);
        }
        return binding;
    }

    /**
     * creates port and binding elements that provide the the service engine binding for a service.
     * @param wsdl wsdl definition
     * @param interfaceName portType qname to which the binding is created.
     * @param serviceName service under which the port definition bound to the service engine binding
     * should be created.
     * @param endpointName port name.
     * @return a Binding contains service engine binding that is created for the portType.
     */
    public Binding createServiceEngineBinding(Definition wsdl, QName interfaceName, QName serviceName, String endpointName) {
        Binding binding = null;
        Service service = findService(wsdl, serviceName);
        if (service == null) {
            return null;
        }
        Port port = service.getPort(endpointName);
        if (port != null) {
            binding = port.getBinding();
        } else {
            // create port
            port = wsdl.createPort();
            port.setName(endpointName);
            binding = createServiceEngineBinding(wsdl, interfaceName);
            port.setBinding(binding);
            service.addPort(port);
        }
        return binding;
    }

    /** prints the wsdl to text from the wsdl definition */
    public static void printWSDL(PrintWriter out, Definition def) {
        try {
            WSDLFactory factory = WSDLFactory.newInstance();
            WSDLWriter wsdlWriter = factory.newWSDLWriter();
            wsdlWriter.writeWSDL(def, out);
        } catch (WSDLException ex) {
            ex.printStackTrace(out);
        }
    }

    /** prints the wsdl to text from the wsdl definition */
    public static String printWSDLToString(Definition def) {
        StringWriter writer = new StringWriter();
        PrintWriter out = new PrintWriter(writer);
        printWSDL(out, def);
        out.close();
        return writer.getBuffer().toString();
    }

    /** returns an existing namespace prefix or create one if not exists for the corresponding namespaceURI */
    public static String getNamespacePrefix(Definition def, String namespaceURI, String defPrefix) {
        String prefix = null;
        prefix = def.getPrefix(namespaceURI);
        if (prefix == null) {
            Set keySet = def.getNamespaces().keySet();
            String newPrefix = "ns";
            if (defPrefix != null && defPrefix.trim().length() > 0) {
                newPrefix = defPrefix;
            }
            prefix = newPrefix;
            for (int i = 0; i < Integer.MAX_VALUE; ++i) {
                if (!keySet.contains(prefix)) {
                    break;
                } else {
                    prefix = newPrefix + i;
                }
            }
        }
        return prefix;
    }

    /**
     * creates and adds the jbi service engine binding extensibility element to the wsdl definition
     * under specified binding definition.
     */
    public static void addExtensibilityElement(Definition wsdlDef,
            ElementExtensible extensibleEl, ExtensibilityElement extEl, String defPrefix) {
        QName elementType = extEl.getElementType();
        String namespaceURI = elementType.getNamespaceURI();
        String prefix = wsdlDef.getPrefix(namespaceURI);
        if (prefix == null) {
            // no namespace prefix defined. create one.
            prefix = WSDLProcessor.getNamespacePrefix(wsdlDef, namespaceURI, defPrefix);
            wsdlDef.addNamespace(prefix, namespaceURI);
        }
        extensibleEl.addExtensibilityElement(extEl);
    }
}
