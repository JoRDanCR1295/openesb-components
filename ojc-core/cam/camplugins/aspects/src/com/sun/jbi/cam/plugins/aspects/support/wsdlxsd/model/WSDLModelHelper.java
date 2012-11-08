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
 * @(#)WSDLModelHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Fault;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.http.HTTPAddress;
import javax.wsdl.extensions.http.HTTPBinding;
import javax.wsdl.extensions.mime.MIMEContent;
import javax.wsdl.extensions.mime.MIMEMultipartRelated;
import javax.wsdl.extensions.mime.MIMEPart;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap.SOAPFault;
import javax.wsdl.extensions.soap.SOAPHeader;
import javax.wsdl.extensions.soap.SOAPOperation;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaLocalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlException;
import org.w3c.dom.Element;

import com.ibm.wsdl.extensions.soap.SOAPAddressImpl;
import com.sun.jbi.cam.common.StringHelper;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.parsers.WSDLParser;

/**
 * @author Graj
 * 
 */
public class WSDLModelHelper implements Serializable, WSDLModel {

    final String SOAP_HEADER_KEY = "com.ibm.wsdl.extensions.soap.SOAPHeaderImpl";

    final String SOAP_BODY_KEY = "com.ibm.wsdl.extensions.soap.SOAPBodyImpl";

    final QName SOAP_ADDRESS_QNAME = QName
            .valueOf("{http://schemas.xmlsoap.org/wsdl/soap/}address");

    final Class SOAP_ADDRESS_CLASS = SOAPAddressImpl.class;

    WSDLParser wsdlParser = new WSDLParser();

    XSDModel xsdHelper = new XSDModelHelper();
    
	private static final long serialVersionUID = 1L;


    /**
     * 
     */
    public WSDLModelHelper() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getWsdlParser()
     */
    public WSDLParser getWsdlParser() {
        return this.wsdlParser;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getXsdHelper()
     */
    public XSDModel getXsdHelper() {
        return this.xsdHelper;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getDefinition()
     */
    public Definition getDefinition() {
        return this.wsdlParser.getDefinition();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#populate(java.lang.String)
     */
    public void populate(String wsdlURI) throws WSDLException, XmlException {
        List schemaList = null;
        this.wsdlParser.loadWsdl(wsdlURI);
        List typeNodes = this.wsdlParser.retrieveTypeNodes();
        Map namespaces = this.wsdlParser.retrieveNamespaces();
        Map importMap = this.wsdlParser.retrieveImports();
        SchemaTypeSystem typeSystem = this.xsdHelper.populate(importMap,
                typeNodes, namespaces);

    }

    public QName getDefinitionQName() {
        return this.wsdlParser.getDefinition().getQName();
    }

    public Map<String /*prefix*/, String /*namespaceUri*/> getNamespaces() {
        return this.wsdlParser.retrieveNamespaces();
    }

    public Map getImports() {
        return this.wsdlParser.retrieveImports();
    }

    public String getTargetNamespace() {
        return this.wsdlParser.getDefinition().getTargetNamespace();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getWSDLString(java.lang.String)
     */
    public String getWSDLString(String wsdlURI) {
        StringBuffer contents = new StringBuffer();
        String contextURI = null;
        InputStream inputStream = null;
        InputStreamReader inputStreamReader = null;
        BufferedReader input = null;
        System.out.println("Retrieving document at '"
                + wsdlURI
                + "'"
                + (contextURI == null ? "." : ", relative to '" + contextURI
                        + "'."));

        try {
            URL contextURL = (contextURI != null) ? StringHelper.getURL(null,
                    contextURI) : null;
            URL url = StringHelper.getURL(contextURL, wsdlURI);
            inputStream = StringHelper.getContentAsInputStream(url);
            inputStreamReader = new InputStreamReader(inputStream);
            input = new BufferedReader(inputStreamReader);
            String line = null; // not declared within while loop
            while ((line = input.readLine()) != null) {
                contents.append(line);
                contents.append(System.getProperty("line.separator"));
            }
        } catch (MalformedURLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (SecurityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            if (inputStreamReader != null) {
                try {
                    inputStreamReader.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            if (input != null) {
                try {
                    input.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }

        return contents.toString();

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#retrieveServices()
     */
    public Service[] retrieveServices() {
        List<Service> serviceImplList = new ArrayList<Service>();
        Map serviceMap = this.getDefinition().getServices();
        QName serviceQName = null;
        Service serviceImpl = null;
        Set keySet = serviceMap.keySet();
        for (Iterator iterator = keySet.iterator(); iterator.hasNext() == true;) {
            serviceQName = (QName) iterator.next();
            if (serviceQName != null) {
                serviceImpl = (Service) serviceMap.get(serviceQName);
                if (serviceImpl != null) {
                    serviceImplList.add(serviceImpl);
                }
            }
        }
        return toArray(serviceImplList, Service.class);
    }

    // //////////////////////////////////////////////
    // -- SERVICE PORT HELPERS --
    // //////////////////////////////////////////////
    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#retrieveServicePorts(javax.wsdl.Service)
     */
    public Port[] retrieveServicePorts(Service service) {
        List<Port> portImplList = new ArrayList<Port>();
        if (service == null) {
            return toArray(portImplList, Port.class);
        }
        Map portMap = service.getPorts();
        Port portImpl = null;
        String portLocalName = null;
        Set keySet = portMap.keySet();
        for (Iterator iterator = keySet.iterator(); iterator.hasNext() == true;) {
            portLocalName = (String) iterator.next();
            if (portLocalName != null) {
                portImpl = (Port) portMap.get(portLocalName);
                if (portImpl != null) {
                    portImplList.add(portImpl);
                }
            }
        }

        return toArray(portImplList, Port.class);
    }

    // //////////////////////////////////////////////
    // -- BINDING HELPERS --
    // //////////////////////////////////////////////

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#hasSOAPBindingPort(javax.wsdl.Port)
     */
    public boolean hasSOAPBindingPort(Port port) {
        if (port == null) {
            return false;
        }
        return this.isSOAPBinding(port.getBinding());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#isSOAPBinding(javax.wsdl.Binding)
     */
    public boolean isSOAPBinding(Binding binding) {
        if (binding == null) {
            return false;
        }
        Collection extensibilityElementsVector = binding
                .getExtensibilityElements();
        for (Iterator extIterator = extensibilityElementsVector.iterator(); extIterator
                .hasNext() == true;) {
            Object element = extIterator.next();
            if ((element != null) && (element instanceof SOAPBinding)) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getSOAPBindings(javax.wsdl.Binding)
     */
    public SOAPBinding[] getSOAPBindings(Binding binding) {
        List<SOAPBinding> soapBindingList = new ArrayList<SOAPBinding>();
        if (binding == null) {
            return toArray(soapBindingList, SOAPBinding.class);
        }
        Collection extensibilityElementsVector = binding
                .getExtensibilityElements();
        for (Iterator extIterator = extensibilityElementsVector.iterator(); extIterator
                .hasNext() == true;) {
            Object element = extIterator.next();
            if ((element != null) && (element instanceof SOAPBinding)) {
                soapBindingList.add((SOAPBinding) element);
            }
        }
        return toArray(soapBindingList, SOAPBinding.class);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getHTTPBindings(javax.wsdl.Binding)
     */
    public HTTPBinding[] getHTTPBindings(Binding binding) {
        List<HTTPBinding> httpBindingList = new ArrayList<HTTPBinding>();
        if (binding == null) {
            return toArray(httpBindingList, HTTPBinding.class);
        }
        Collection extensibilityElementsVector = binding
                .getExtensibilityElements();
        for (Iterator extIterator = extensibilityElementsVector.iterator(); extIterator
                .hasNext() == true;) {
            Object element = extIterator.next();
            if ((element != null) && (element instanceof HTTPBinding)) {
                httpBindingList.add((HTTPBinding) element);
            }
        }
        return toArray(httpBindingList, HTTPBinding.class);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#findBindingOperation(javax.wsdl.Binding,
     *      javax.wsdl.Operation)
     */
    public BindingOperation findBindingOperation(Binding binding,
            Operation operation) {
        if ((binding == null) || (operation == null)) {
            return null;
        }
        BindingOperation bindingOperation = null;
        List bindingOperations = binding.getBindingOperations();
        for (Iterator iterator = bindingOperations.iterator(); iterator
                .hasNext() == true;) {
            bindingOperation = (BindingOperation) iterator.next();
            if (bindingOperation != null) {
                if (bindingOperation.getName().equals(operation.getName()) == true) {
                    return bindingOperation;
                }
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getSOAPBindingInputHeader(javax.wsdl.BindingInput)
     */
    public SOAPHeader getSOAPBindingInputHeader(BindingInput input) {
        return (SOAPHeader) this.getBindingInputElement(input, SOAP_HEADER_KEY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getSOAPBindingInputBody(javax.wsdl.BindingInput)
     */
    public SOAPBody getSOAPBindingInputBody(BindingInput input) {
        return (SOAPBody) this.getBindingInputElement(input, SOAP_BODY_KEY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getBindingInputElement(javax.wsdl.BindingInput,
     *      java.lang.String)
     */
    public Object getBindingInputElement(BindingInput input, String className) {
        if (input == null) {
            return null;
        }
        Collection extList = input.getExtensibilityElements();
        for (Iterator extIterator = extList.iterator(); extIterator.hasNext() == true;) {
            Object element = extIterator.next();
            String elementClassName = element.getClass().getName();
            if ((element != null)
                    && (elementClassName.equals(className) == true)) {
                return element;
            }

        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getSOAPBindingOutputHeader(javax.wsdl.BindingOutput)
     */
    public SOAPHeader getSOAPBindingOutputHeader(BindingOutput output) {
        return (SOAPHeader) this.getBindingOutputElement(output,
                SOAP_HEADER_KEY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getSOAPBindingOutputBody(javax.wsdl.BindingOutput)
     */
    public SOAPBody getSOAPBindingOutputBody(BindingOutput output) {
        return (SOAPBody) this.getBindingOutputElement(output, SOAP_BODY_KEY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getBindingOutputElement(javax.wsdl.BindingOutput,
     *      java.lang.String)
     */
    public Object getBindingOutputElement(BindingOutput output, String className) {
        if (output == null) {
            return null;
        }
        Collection extList = output.getExtensibilityElements();
        for (Iterator extIterator = extList.iterator(); extIterator.hasNext() == true;) {
            Object element = extIterator.next();
            String elementClassName = element.getClass().getName();
            if ((element != null)
                    && (elementClassName.equals(className) == true)) {
                return element;
            }
        }
        return null;
    }

    // //////////////////////////////////////////////
    // -- SERVICE HELPERS --
    // //////////////////////////////////////////////
    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getDocumentation(org.w3c.dom.Element)
     */
    public String getDocumentation(Element element) {

        if (element == null) {
            return null;
        }
        String value = element.getTextContent();
        return value;
    }

    public Service addNewSOAPServiceForBinding(Definition definition, QName newServiceName,
            String newPortName, String newLocationUri, QName bindingName)
            throws WSDLException {
        Service newService = null;
        Port newPort = null;
        Binding binding = null;
        ExtensionRegistry extensionRegistry = null;
        SOAPAddress soapAddress = null;
        WSDLFactory wsdlFactory = null;
        binding = definition.getBinding(bindingName);
        wsdlFactory = WSDLFactory.newInstance();
        extensionRegistry = wsdlFactory.newPopulatedExtensionRegistry();
        soapAddress = (SOAPAddress) extensionRegistry.createExtension(
                Port.class, new QName("http://schemas.xmlsoap.org/wsdl/soap/",
                        "address"));
        soapAddress.setLocationURI(newLocationUri);
        newPort = definition.createPort();
        newPort.setName(newPortName);
        newPort.addExtensibilityElement(soapAddress);
        newPort.setBinding(binding);
        newService = definition.createService();
        newService.setQName(newServiceName);
        newService.addPort(newPort);
        definition.addService(newService);
        return newService;
    }

    // //////////////////////////////////////////////
    // -- PART HELPERS --
    // //////////////////////////////////////////////
    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.binding.userbc.model.WSDLModel#getParameters()
     */
    public Map getParameters() {
        Map parameterMap = new HashMap();

        return parameterMap;

    }

    // //////////////////////////////////////////////
    // -- OTHER HELPERS --
    // //////////////////////////////////////////////
    /**
     * Retrieves all the SOAPBinding elements in the WSDL
     */
    public String[] getAllSOAPLocationURIs() {
        ExtensibilityElement extensibilityElement = null;
        SOAPAddress soapAddress = null;
        String locationURI = null;
        List<String> soapBindingsList = new ArrayList<String>();
        Service[] servicesList = this.retrieveServices();
        Service service = null;
        if (servicesList != null) {
            for (int index = 0; index < servicesList.length; index++) {
                service = servicesList[index];
                if (service != null) {
                    Port[] portsList = this.retrieveServicePorts(service);
                    Port port = null;
                    for (int count = 0; count < portsList.length; count++) {
                        port = portsList[count];
                        if ((port != null)
                                && (this.hasSOAPBindingPort(port) == true)) {
                            List elements = port.getExtensibilityElements();
                            for (Iterator elementIterator = elements.iterator(); elementIterator
                                    .hasNext() == true;) {
                                extensibilityElement = (ExtensibilityElement) elementIterator
                                        .next();
                                if ((extensibilityElement != null)
                                        && (extensibilityElement instanceof SOAPAddress)) {
                                    soapAddress = (SOAPAddress) extensibilityElement;
                                    locationURI = soapAddress.getLocationURI();
                                    if (locationURI != null) {
                                        if (soapBindingsList
                                                .contains(locationURI) == false) {
                                            soapBindingsList.add(locationURI);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return toArray(soapBindingsList, String.class);
    }

    /**
     * Retrieves all the HTTPBinding elements in the WSDL
     */
    public String[] getAllHTTPLocationURIs() {
        ExtensibilityElement extensibilityElement = null;
        HTTPAddress httpAddress = null;
        String locationURI = null;
        List<String> httpBindingsList = new ArrayList<String>();
        Service[] servicesList = this.retrieveServices();
        Service service = null;
        if (servicesList != null) {
            for (int index = 0; index < servicesList.length; index++) {
                service = servicesList[index];
                if (service != null) {
                    Port[] portsList = this.retrieveServicePorts(service);
                    Port port = null;
                    for (int count = 0; count < portsList.length; count++) {
                        port = portsList[count];
                        if (port != null) {
                            List elements = port.getExtensibilityElements();
                            for (Iterator elementIterator = elements.iterator(); elementIterator
                                    .hasNext() == true;) {
                                extensibilityElement = (ExtensibilityElement) elementIterator
                                        .next();
                                if ((extensibilityElement != null)
                                        && (extensibilityElement instanceof HTTPAddress)) {
                                    httpAddress = (HTTPAddress) extensibilityElement;
                                    locationURI = httpAddress.getLocationURI();
                                    if (locationURI != null) {
                                        if (httpBindingsList
                                                .contains(locationURI) == false) {
                                            httpBindingsList.add(locationURI);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return toArray(httpBindingsList, String.class);
    }

    public String getSOAPLocationURI(Port port) {
        String locationURI = null;
        SOAPAddressImpl soapAddress = null;
        List elements = port.getExtensibilityElements();
        for (Iterator elementIterator = elements.iterator(); elementIterator
                .hasNext();) {
            ExtensibilityElement element = (ExtensibilityElement) elementIterator
                    .next();
            if (element != null) {
                if ((true == element.getElementType()
                        .equals(SOAP_ADDRESS_QNAME))
                        && (true == element.getClass().getName().equals(
                                SOAP_ADDRESS_CLASS.getName()))) {
                    soapAddress = (SOAPAddressImpl) element;
                    locationURI = soapAddress.getLocationURI();
                    break;
                }
            }
        }
        return locationURI;
    }

    public String setSOAPLocationURI(Port port, String newLocationURI) {
        String locationURI = null;
        SOAPAddressImpl soapAddress = null;
        List elements = port.getExtensibilityElements();
        for (Iterator elementIterator = elements.iterator(); elementIterator
                .hasNext();) {
            ExtensibilityElement element = (ExtensibilityElement) elementIterator
                    .next();
            if (element != null) {
                if ((true == element.getElementType()
                        .equals(SOAP_ADDRESS_QNAME))
                        && (true == element.getClass().getName().equals(
                                SOAP_ADDRESS_CLASS.getName()))) {
                    soapAddress = (SOAPAddressImpl) element;
                    soapAddress.setLocationURI(newLocationURI);
                    break;
                }
            }
        }
        return locationURI;
    }

    public PortType getSOAPAbstractPortType(QName serviceName, String portName) {
        PortType portType = null;
        portType = this.getSOAPAbstractPortType(serviceName.getLocalPart(),
                portName);
        return portType;
    }

    public PortType getSOAPAbstractPortType(String serviceName, String portName) {
        PortType portType = null;
        Service service = null;
        Port port = null;
        Binding binding = null;
        List<SOAPBinding> soapBindingList = null;
        SOAPBinding soapBinding = null;

        Service[] serviceList = this.retrieveServices();
        for (int index = 0; index < serviceList.length; index++) {
            service = serviceList[index];
            if (service != null) {
                if (service.getQName().getLocalPart().equals(serviceName) == true) {
                    break;
                }
            }
        }
        if (service != null) {
            port = service.getPort(portName);
            if (port != null) {
                binding = port.getBinding();
                if (binding != null) {
                    if (this.isSOAPBinding(binding) == true) {
                        portType = binding.getPortType();
                    }
                }
            }
        }
        return portType;
    }

    public Part[] getInputParts(Operation operation) {
        List<Part> partList = new ArrayList<Part>();
        if (operation != null) {
            Input input = operation.getInput();
            if (input != null) {
                Message message = input.getMessage();
                if (message != null) {
                    Map partMap = message.getParts();
                    Set set = partMap.entrySet();
                    for (Iterator iterator = set.iterator(); iterator.hasNext() == true;) {
                        Entry entry = (Entry) iterator.next();
                        if (entry != null) {
                            String key = (String) entry.getKey();
                            Part value = (Part) entry.getValue();
                            if (value != null) {
                                partList.add(value);
                            }
                        }
                    }
                }
            }
        }
        return toArray(partList, Part.class);
    }

    public Part[] getOutputParts(Operation operation) {
        List<Part> partList = new ArrayList<Part>();
        if (operation != null) {
            Output output = operation.getOutput();
            if (output != null) {
                Message message = output.getMessage();
                if (message != null) {
                    Map partMap = message.getParts();
                    Set set = partMap.entrySet();
                    for (Iterator iterator = set.iterator(); iterator.hasNext() == true;) {
                        Entry entry = (Entry) iterator.next();
                        if (entry != null) {
                            String key = (String) entry.getKey();
                            Part value = (Part) entry.getValue();
                            if (value != null) {
                                partList.add(value);
                            }
                        }
                    }
                }
            }
        }
        return toArray(partList, Part.class);
    }

    // ///////////////////////////////
    // -- OTHER HELPER METHODS --
    // ///////////////////////////////
    public ExtensibilityElement getExtensiblityElement(List list, Class clazz) {
        ExtensibilityElement[] elements = getExtensiblityElements(list, clazz);
        return elements.length > 0 ? elements[0] : null;
    }

    public ExtensibilityElement[] getExtensiblityElements(List list, Class clazz) {
        List<ExtensibilityElement> result = new ArrayList<ExtensibilityElement>();

        for (Iterator<ExtensibilityElement> iterator = list.iterator(); iterator
                .hasNext();) {
            ExtensibilityElement extensibilityElement = (ExtensibilityElement) iterator
                    .next();
            if (clazz.isAssignableFrom(extensibilityElement.getClass())) {
                result.add(extensibilityElement);
            }
        }

        return toArray(result, ExtensibilityElement.class);
    }

    public String getSoapAction(BindingOperation operation) {
        List list = operation.getExtensibilityElements();
        SOAPOperation soapOperation = (SOAPOperation) this
                .getExtensiblityElement(list, SOAPOperation.class);
        String result = null;
        if (soapOperation != null) {
            result = soapOperation.getSoapActionURI();
        }
        return result;
    }

    public String[] getEndpointsForBinding(Definition definition,
            Binding binding) {
        List<String> result = new ArrayList<String>();
        Map map = definition.getServices();
        for (Iterator iterator = map.values().iterator(); iterator.hasNext();) {
            Service service = (Service) iterator.next();
            Map portMap = service.getPorts();
            for (Iterator portIterator = portMap.values().iterator(); portIterator
                    .hasNext();) {
                Port port = (Port) portIterator.next();
                if (port.getBinding() == binding) {
                    SOAPAddress soapAddress = (SOAPAddress) this
                            .getExtensiblityElement(port
                                    .getExtensibilityElements(),
                                    SOAPAddress.class);
                    result.add(soapAddress.getLocationURI());
                }
            }
        }

        return toArray(result, String.class);
    }

    public Binding findBindingForOperation(Definition definition,
            BindingOperation bindingOperation) {
        Map services = definition.getServices();
        Iterator<Service> serviceIterator = services.values().iterator();

        while (serviceIterator.hasNext()) {
            Map ports = serviceIterator.next().getPorts();
            Iterator<Port> portIterator = ports.values().iterator();
            while (portIterator.hasNext()) {
                Binding binding = portIterator.next().getBinding();
                List bindingOperations = binding.getBindingOperations();
                for (Iterator operationsIterator = bindingOperations.iterator(); operationsIterator
                        .hasNext();) {
                    BindingOperation operation = (BindingOperation) operationsIterator
                            .next();
                    if (operation.getName().equals(bindingOperation.getName()))
                        return binding;
                }
            }
        }

        Map bindings = definition.getBindings();
        Iterator<QName> namesIterator = bindings.keySet().iterator();
        while (namesIterator.hasNext()) {
            Binding binding = definition.getBinding(namesIterator.next());
            List bindingOperations = binding.getBindingOperations();
            for (Iterator operationsIterator = bindingOperations.iterator(); operationsIterator
                    .hasNext();) {
                BindingOperation operation = (BindingOperation) operationsIterator
                        .next();
                if (operation.getName().equals(bindingOperation.getName()))
                    return binding;
            }
        }

        return null;
    }

    public boolean isInputSoapEncoded(BindingOperation bindingOperation) {
        SOAPBody body = (SOAPBody) this.getExtensiblityElement(bindingOperation
                .getBindingInput().getExtensibilityElements(), SOAPBody.class);

        return body != null
                && body.getUse() != null
                && body.getUse().equalsIgnoreCase("encoded")
                && (body.getEncodingStyles() == null || body
                        .getEncodingStyles().contains(
                                "http://schemas.xmlsoap.org/soap/encoding/"));
    }

    public boolean isOutputSoapEncoded(BindingOperation bindingOperation) {
        BindingOutput bindingOutput = bindingOperation.getBindingOutput();
        if (bindingOutput == null)
            return false;

        SOAPBody body = (SOAPBody) this.getExtensiblityElement(bindingOutput
                .getExtensibilityElements(), SOAPBody.class);

        return body != null
                && body.getUse() != null
                && body.getUse().equalsIgnoreCase("encoded")
                && (body.getEncodingStyles() == null || body
                        .getEncodingStyles().contains(
                                "http://schemas.xmlsoap.org/soap/encoding/"));
    }

    public boolean isRpc(Definition definition,
            BindingOperation bindingOperation) {
        SOAPOperation soapOperation = (SOAPOperation) this
                .getExtensiblityElement(bindingOperation
                        .getExtensibilityElements(), SOAPOperation.class);

        if (soapOperation != null && soapOperation.getStyle() != null)
            return soapOperation.getStyle().equalsIgnoreCase("rpc");

        Binding binding = findBindingForOperation(definition, bindingOperation);
        if (binding == null) {
            System.out.println("Failed to find binding for operation ["
                    + bindingOperation.getName() + "] in definition ["
                    + definition.getDocumentBaseURI() + "]");
            return false;
        }

        return isRpc(binding);
    }

    public boolean isRpc(Binding binding) {
        SOAPBinding soapBinding = (SOAPBinding) this.getExtensiblityElement(
                binding.getExtensibilityElements(), SOAPBinding.class);

        return (soapBinding != null && "rpc".equalsIgnoreCase(soapBinding
                .getStyle()));
    }

    /**
     * Returns a list of parts for the specifed operation, either as specified
     * in body or all
     */

    public Part[] getInputParts(BindingOperation operation) {
        List<Part> partList = new ArrayList<Part>();
        Message message = operation.getOperation().getInput().getMessage();
        SOAPBody body = (SOAPBody) this.getExtensiblityElement(operation
                .getBindingInput().getExtensibilityElements(), SOAPBody.class);

        if (body == null || body.getParts() == null) {
            if (message != null) {
                partList.addAll(message.getOrderedParts(null));
            }
        } else {
            Iterator iterator = body.getParts().iterator();
            while (iterator.hasNext()) {
                String partName = (String) iterator.next();
                Part part = message.getPart(partName);

                partList.add(part);
            }
        }
        return toArray(partList, Part.class);
    }

    public Part[] getOutputParts(BindingOperation operation) {
        BindingOutput bindingOutput = operation.getBindingOutput();
        if (bindingOutput == null) {
            return new Part[0];
        }

        List<Part> partList = new ArrayList<Part>();
        Message message = operation.getOperation().getOutput().getMessage();
        SOAPBody body = (SOAPBody) this.getExtensiblityElement(bindingOutput
                .getExtensibilityElements(), SOAPBody.class);

        if (body == null || body.getParts() == null) {
            partList.addAll(message.getOrderedParts(null));
        } else {
            Iterator iterator = body.getParts().iterator();
            while (iterator.hasNext()) {
                String partName = (String) iterator.next();
                Part part = message.getPart(partName);
                partList.add(part);
            }
        }
        return toArray(partList, Part.class);
    }

    public Part[] getFaultParts(BindingOperation bindingOperation,
            String faultName) {
        List<Part> partList = new ArrayList<Part>();

        BindingFault bindingFault = bindingOperation.getBindingFault(faultName);
        SOAPFault fault = (SOAPFault) this.getExtensiblityElement(bindingFault
                .getExtensibilityElements(), SOAPFault.class);

        if (fault != null && fault.getName() != null) {
            partList.addAll(bindingOperation.getOperation().getFault(
                    fault.getName()).getMessage().getOrderedParts(null));
        } else {
            partList.addAll(bindingOperation.getOperation().getFault(faultName)
                    .getMessage().getOrderedParts(null));
        }
        return toArray(partList, Part.class);
    }

    public boolean isAttachmentInputPart(Part part, BindingOperation operation) {
        return (getInputMultipartContent(part, operation).length > 0);
    }

    public boolean isAttachmentOutputPart(Part part, BindingOperation operation) {
        return (getOutputMultipartContent(part, operation).length > 0);
    }

    public MIMEContent[] getInputMultipartContent(Part part,
            BindingOperation operation) {
        MIMEMultipartRelated multipartInput = (MIMEMultipartRelated) this
                .getExtensiblityElement(operation.getBindingInput()
                        .getExtensibilityElements(), MIMEMultipartRelated.class);

        return (getContentParts(part, multipartInput));
    }

    public MIMEContent[] getOutputMultipartContent(Part part,
            BindingOperation operation) {
        MIMEMultipartRelated multipartOutput = (MIMEMultipartRelated) this
                .getExtensiblityElement(operation.getBindingOutput()
                        .getExtensibilityElements(), MIMEMultipartRelated.class);

        return (getContentParts(part, multipartOutput));
    }

    public MIMEContent[] getContentParts(Part part,
            MIMEMultipartRelated multipart) {
        List<MIMEContent> result = new ArrayList<MIMEContent>();

        if (multipart != null) {
            List<MIMEPart> parts = multipart.getMIMEParts();

            for (int count = 0; count < parts.size(); count++) {
                ExtensibilityElement[] contentParts = this
                        .getExtensiblityElements(parts.get(count)
                                .getExtensibilityElements(), MIMEContent.class);

                for (ExtensibilityElement elm : contentParts) {
                    MIMEContent content = (MIMEContent) elm;
                    if (content.getPart().equals(part.getName()))
                        result.add(content);
                }
            }
        }
        return toArray(result, MIMEContent.class);
    }

    public boolean isMultipartRequest(Definition definition,
            BindingOperation bindingOperation) {
        return ((MIMEMultipartRelated) this.getExtensiblityElement(
                bindingOperation.getBindingInput().getExtensibilityElements(),
                MIMEMultipartRelated.class) != null);
    }

    // ///////////////////////////////
    // -- GENERIC HELPERS --
    // ///////////////////////////////

    public void writeWSDLToFile(Definition definition, String wsdlFileName)
            throws WSDLException, IOException {
        WSDLWriter wsdlWriter = null;
        WSDLFactory wsdlFactory = null;
        File wsdlFile = null;
        FileWriter outputFileWriter = null;
        wsdlFactory = WSDLFactory.newInstance();
        wsdlWriter = wsdlFactory.newWSDLWriter();
        //wsdlWriter.writeWSDL(definition, System.out);
        wsdlFile = new File(wsdlFileName);
        outputFileWriter = new FileWriter(wsdlFile);
        wsdlWriter.writeWSDL(definition, outputFileWriter);
        outputFileWriter.close();
    }

    /**
     * We can now use this to produce lists of ints or Strings:
     * List<Integer> ints = Lists.toList(1, 2, 3);
     * List<String> names = Lists.toList("Gopalan", "Suresh", "Raj");
     * 
     * @param <T>
     * @param arr
     * @return
     */
    public static <T> List<T> toList(T... array) {
        List<T> list = new ArrayList<T>();
        for (T arrayElement : array) {
        	list.add(arrayElement);
        }
        return list;
    }

    /**
     * 
     * @param <T>
     * @param collection
     * @param componentType
     * @return
     */
    @SuppressWarnings("unchecked")
    static public <T> T[] toArray(Collection<T> collection,
            Class<T> componentType) {
        // unchecked cast
        T[] array = (T[]) java.lang.reflect.Array.newInstance(componentType,
                collection.size());
        int index = 0;
        for (T value : collection) {
            array[index++] = value;
        }
        return array;
    }

    public static void main(String[] args) {
        String wsdlURI = "";
        // CurrencyExchangeService
        // wsdlURI =
        // "http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl";
        // net.xmethods.services.stockquote.StockQuote
        // wsdlURI
        // ="http://services.xmethods.net/soap/urn:xmethods-delayed-quotes.wsdl";
        // DOTSValidateCanada
        // wsdlURI =
        // "http://ws2.serviceobjects.net/avca/ValidateCanada.asmx?WSDL";
        // wsdlURI =
        // System.getProperty("ALASKA_ROOT") + "/jbi/runtime/Sun/AppServer/domains/domain1/applications/j2ee-modules/eManager/deploy/consuming/0F000000-46AF4899080100-0A121528-01-GovernancePolicyGroup/unit/consuming.wsdl";
        // HouseofDev
        // wsdlURI = "http://ws.houseofdev.com/cfcs/ws.cfc?wsdl";
        // ImageExtractor
        // wsdlURI =
        // "http://www.atomic-x.com/xmlservices/imageextractorws.asmx?wsdl";
        // Financial Services
        // wsdlURI = "http://www.xignite.com/xRealTime.asmx?WSDL";
        // Google Search
        // wsdlURI = "http://api.google.com/GoogleSearch.wsdl";
        // Amazon WebService
        // wsdlURI =
        // "http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl";
        // Amazon AWS Alexa WebService
        // wsdlURI = "http://aws-beta.amazon.com/AWSAlexa/AWSAlexa.wsdl";
        // Amazon Simple Queue Service
        // wsdlURI =
        // "http://webservices.amazon.com/AWSSimpleQueueService/AWSSimpleQueueService.wsdl";
        // StrikeIron Address Verification Premium Web Service - US and Canada
        // wsdlURI = "http://ws.strikeiron.com/DataEnhancement?WSDL";
        // StrikeIron Address Verification Premium Web Service - Global
        // wsdlURI = "http://ws.strikeiron.com/GlobalAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - France
        // wsdlURI = "http://ws.strikeiron.com/FrenchAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - UK
        // wsdlURI = "http://ws.strikeiron.com/UKAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - India
        // wsdlURI = "http://ws.strikeiron.com/IndianAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - Email
        // Verification
        // wsdlURI = "http://ws.strikeiron.com/EmailVerify?WSDL";
        // Paypal Development and Test sandbox API
        // wsdlURI = "http://www.sandbox.paypal.com/wsdl/PayPalSvc.wsdl";
        // eBay WSDL
        // wsdlURI =
        // "http://developer.ebay.com/webservices/latest/eBaySvc.wsdl";
        // FedEx WSDL
        // wsdlURI = "http://www.xmethods.com/sd/FedExTrackerService.wsdl";
        // Translate IP addresses to city, state, country.
        // wsdlURI = "http://ws2.serviceobjects.net/gpp/GeoPinpoint.asmx?WSDL";
        // GeoPhone allows you to lookup name and postal address information for
        // virtually every U.S. telephone number.
        // wsdlURI = "http://ws2.serviceobjects.net/gp/GeoPhone.asmx?WSDL";
        // returns bank office locations for a given zip and mile radius
        // wsdlURI =
        // "http://www.hosca.com/webservices/bankfinder/bankfinder.asmx?WSDL";
        // This web service allows you to validate partial United States mailing
        // addresses against the U.S. Postal Service database. You can enter
        // street, city, and state information and get various return values
        // including: five-digit standard ZIP code (NNNN), ZIP+
        // wsdlURI =
        // "http://webservices.eraserver.net/zipcoderesolver/zipcoderesolver.asmx?wsdl";
        // Converts Section, Township, Range to Lat Long
        // wsdlURI =
        // "http://sylvanmaps.net/SectionTownshipCalculatorWebService/SectionTownshipToLatLong.asmx?WSDL";
        // Monster WSDL
        // wsdlURI =
        // "http://schemas.monster.com/current/wsdl/MonsterBusinessGateway.wsdl";
        // wsdlURI =
        // "http://schemas.monster.com/BGW3.4/WSDL/MonsterBusinessGateway.wsdl";
        // wsdlURI =
        // "http://schemas.monster.com/BGW3.4/WSDL/MonsterBusinessGateway-wsi.wsdl";
        // CA Traffic Service
        // wsdlURI = "http://www.xmethods.net/sd/2001/CATrafficService.wsdl";
        // Simple Autoloan calculator
        // wsdlURI = "http://upload.eraserver.net/circle24/autoloan.asmx?wsdl";
        // Yahoo Search WSDL simulated
        // wsdlURI =
        // "http://www.pacificspirit.com/Authoring/wsdl/YahooV1Search.wsdl";
        // DHL WSDL
        // wsdlURI = "http://dhl-usa.expediteship.com/DHLShipment.asmx?WSDL";
        // wsdlURI = "http://uddi.org/wsdl/uddi_api_v3_portType.wsdl";
        // wsdlURI =
        // "http://egee-jra1-wm.mi.infn.it/egee-jra1-wm/allocation/wsdl/agreementService.wsdl";
        WSDLModel helper = new WSDLModelHelper();
        XSDModel xsdHelper = helper.getXsdHelper();
        try {
            helper.populate(wsdlURI);
            Service[] servicesList = helper.retrieveServices();
            Service service = null;
            if (servicesList != null) {
                for (int index = 0; index < servicesList.length; index++) {
                    service = servicesList[index];
                    if (service != null) {
                        System.out.println("");
                        System.out.println("Service LocalPart: "
                                + service.getQName().getLocalPart());
                        System.out.println("Service Namespace: "
                                + service.getQName().getNamespaceURI());
                        System.out.println("Service Prefix: "
                                + service.getQName().getPrefix());
                        System.out.println("Service Documentation: "
                                + helper.getDocumentation(service
                                        .getDocumentationElement()));
                        System.out.println("========== End Service =========");
                        Port[] portsList = helper.retrieveServicePorts(service);
                        Port port = null;
                        for (int count = 0; count < portsList.length; count++) {
                            port = portsList[count];
                            if ((port != null)
                                    && (helper.hasSOAPBindingPort(port) == true)) {
                                System.out.println(" ServicePort Name: "
                                        + port.getName());
                                System.out
                                        .println(" ServicePort Documentation: "
                                                + helper
                                                        .getDocumentation(port
                                                                .getDocumentationElement()));
                                List elements = port.getExtensibilityElements();
                                System.out
                                        .println(" ========== End Service Port =========");
                            }
                            PortType portType = helper.getSOAPAbstractPortType(
                                    service.getQName(), port.getName());
                            if (portType != null) {
                                List operationsList = portType.getOperations();
                                for (Iterator operationsIterator = operationsList
                                        .iterator(); operationsIterator
                                        .hasNext() == true;) {
                                    Operation operation = (Operation) operationsIterator
                                            .next();
                                    System.out.println("OperationName = "
                                            + operation.getName());
                                    Part[] inputPartList = helper
                                            .getInputParts(operation);
                                    System.out.println("Input is:");
                                    for (int inputIndex = 0; inputIndex < inputPartList.length; inputIndex++) {
                                        Part part = inputPartList[inputIndex];
                                        System.out.println("Input Part Name = "
                                                + part.getName());
                                        if (part.getElementName() != null) {
                                            System.out
                                                    .println("Input Part Element Name = "
                                                            + part
                                                                    .getElementName());
                                        }
                                        if (part.getTypeName() != null) {
                                            System.out
                                                    .println("Input Part Type Name = "
                                                            + part
                                                                    .getTypeName());
                                        }
                                    }
                                    Part[] outputPartList = helper
                                            .getOutputParts(operation);
                                    System.out.println("Output is:");
                                    for (int outputIndex = 0; outputIndex < outputPartList.length; outputIndex++) {
                                        Part part = outputPartList[outputIndex];
                                        System.out
                                                .println("Output Part Name = "
                                                        + part.getName());
                                        if (part.getElementName() != null) {
                                            System.out
                                                    .println("Output Part Element Name = "
                                                            + part
                                                                    .getElementName());
                                        }
                                        if (part.getTypeName() != null) {
                                            System.out
                                                    .println("Output Part Type Name = "
                                                            + part
                                                                    .getTypeName());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (WSDLException e) {
            // TODO Auto-generated catch block
            // e.printStackTrace();
            System.out.println("Exception parsing WSDL: " + e.getMessage());
        } catch (XmlException e) {
            // TODO Auto-generated catch block
            // e.printStackTrace();
            System.out.println("Exception parsing XSD: " + e.getMessage());
        }

    }

    /**
     * @param args
     */
    public static void test(String[] args) {
        String wsdlURI = "";
        // CurrencyExchangeService
        wsdlURI = "http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl";
        // net.xmethods.services.stockquote.StockQuote
        // wsdlURI
        // ="http://services.xmethods.net/soap/urn:xmethods-delayed-quotes.wsdl";
        // DOTSValidateCanada
        // wsdlURI =
        // "http://ws2.serviceobjects.net/avca/ValidateCanada.asmx?WSDL";
        // wsdlURI =
        // System.getProperty("ALASKA_ROOT") + "/jbi/runtime/Sun/AppServer/domains/domain1/applications/j2ee-modules/eManager/deploy/consuming/0F000000-46AF4899080100-0A121528-01-GovernancePolicyGroup/unit/consuming.wsdl";
        // HouseofDev
        // wsdlURI = "http://ws.houseofdev.com/cfcs/ws.cfc?wsdl";
        // ImageExtractor
        // wsdlURI =
        // "http://www.atomic-x.com/xmlservices/imageextractorws.asmx?wsdl";
        // Financial Services
        // wsdlURI = "http://www.xignite.com/xRealTime.asmx?WSDL";
        // Google Search
        // wsdlURI = "http://api.google.com/GoogleSearch.wsdl";
        // Amazon WebService
        // wsdlURI =
        // "http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl";
        // Amazon AWS Alexa WebService
        // wsdlURI = "http://aws-beta.amazon.com/AWSAlexa/AWSAlexa.wsdl";
        // Amazon Simple Queue Service
        // wsdlURI =
        // "http://webservices.amazon.com/AWSSimpleQueueService/AWSSimpleQueueService.wsdl";
        // StrikeIron Address Verification Premium Web Service - US and Canada
        // wsdlURI = "http://ws.strikeiron.com/DataEnhancement?WSDL";
        // StrikeIron Address Verification Premium Web Service - Global
        // wsdlURI = "http://ws.strikeiron.com/GlobalAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - France
        // wsdlURI = "http://ws.strikeiron.com/FrenchAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - UK
        // wsdlURI = "http://ws.strikeiron.com/UKAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - India
        // wsdlURI = "http://ws.strikeiron.com/IndianAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - Email
        // Verification
        // wsdlURI = "http://ws.strikeiron.com/EmailVerify?WSDL";
        // Paypal Development and Test sandbox API
        // wsdlURI = "http://www.sandbox.paypal.com/wsdl/PayPalSvc.wsdl";
        // eBay WSDL
        // wsdlURI =
        // "http://developer.ebay.com/webservices/latest/eBaySvc.wsdl";
        // FedEx WSDL
        // wsdlURI = "http://www.xmethods.com/sd/FedExTrackerService.wsdl";
        // Translate IP addresses to city, state, country.
        // wsdlURI = "http://ws2.serviceobjects.net/gpp/GeoPinpoint.asmx?WSDL";
        // GeoPhone allows you to lookup name and postal address information for
        // virtually every U.S. telephone number.
        // wsdlURI = "http://ws2.serviceobjects.net/gp/GeoPhone.asmx?WSDL";
        // returns bank office locations for a given zip and mile radius
        // wsdlURI =
        // "http://www.hosca.com/webservices/bankfinder/bankfinder.asmx?WSDL";
        // This web service allows you to validate partial United States mailing
        // addresses against the U.S. Postal Service database. You can enter
        // street, city, and state information and get various return values
        // including: five-digit standard ZIP code (NNNN), ZIP+
        // wsdlURI =
        // "http://webservices.eraserver.net/zipcoderesolver/zipcoderesolver.asmx?wsdl";
        // Converts Section, Township, Range to Lat Long
        // wsdlURI =
        // "http://sylvanmaps.net/SectionTownshipCalculatorWebService/SectionTownshipToLatLong.asmx?WSDL";
        // Monster WSDL
        // wsdlURI =
        // "http://schemas.monster.com/current/wsdl/MonsterBusinessGateway.wsdl";
        // wsdlURI =
        // "http://schemas.monster.com/BGW3.4/WSDL/MonsterBusinessGateway.wsdl";
        // wsdlURI =
        // "http://schemas.monster.com/BGW3.4/WSDL/MonsterBusinessGateway-wsi.wsdl";
        // CA Traffic Service
        // wsdlURI = "http://www.xmethods.net/sd/2001/CATrafficService.wsdl";
        // Simple Autoloan calculator
        // wsdlURI = "http://upload.eraserver.net/circle24/autoloan.asmx?wsdl";
        // Yahoo Search WSDL simulated
        // wsdlURI =
        // "http://www.pacificspirit.com/Authoring/wsdl/YahooV1Search.wsdl";
        // DHL WSDL
        // wsdlURI = "http://dhl-usa.expediteship.com/DHLShipment.asmx?WSDL";
        // wsdlURI = "http://uddi.org/wsdl/uddi_api_v3_portType.wsdl";
        // wsdlURI =
        // "http://egee-jra1-wm.mi.infn.it/egee-jra1-wm/allocation/wsdl/agreementService.wsdl";
        WSDLModel helper = new WSDLModelHelper();
        XSDModel xsdHelper = helper.getXsdHelper();
        try {
            helper.populate(wsdlURI);
            Service[] servicesList = helper.retrieveServices();
            Service service = null;
            if (servicesList != null) {
                for (int serviceIndex = 0; serviceIndex < servicesList.length; serviceIndex++) {
                    service = servicesList[serviceIndex];
                    if (service != null) {
                        System.out.println("");
                        System.out.println("Service LocalPart: "
                                + service.getQName().getLocalPart());
                        System.out.println("Service Namespace: "
                                + service.getQName().getNamespaceURI());
                        System.out.println("Service Prefix: "
                                + service.getQName().getPrefix());
                        System.out.println("Service Documentation: "
                                + helper.getDocumentation(service
                                        .getDocumentationElement()));
                        System.out.println("========== End Service =========");
                        Port[] portsList = helper.retrieveServicePorts(service);
                        Port port = null;
                        for (int count = 0; count < portsList.length; count++) {
                            port = portsList[count];
                            if ((port != null)
                                    && (helper.hasSOAPBindingPort(port) == true)) {
                                System.out.println(" ServicePort Name: "
                                        + port.getName());
                                System.out
                                        .println(" ServicePort Documentation: "
                                                + helper
                                                        .getDocumentation(port
                                                                .getDocumentationElement()));
                                List elements = port.getExtensibilityElements();
                                System.out
                                        .println(" ========== End Service Port =========");
                                Binding binding = port.getBinding();
                                if ((binding != null)
                                        && (helper.isSOAPBinding(binding) == true)) {
                                    System.out
                                            .println("  Binding LocalPart: "
                                                    + binding.getQName()
                                                            .getLocalPart());
                                    System.out.println("  Binding Namespace: "
                                            + binding.getQName()
                                                    .getNamespaceURI());
                                    System.out.println("  Binding Prefix: "
                                            + binding.getQName().getPrefix());
                                    System.out
                                            .println("  Binding Documentation: "
                                                    + helper
                                                            .getDocumentation(binding
                                                                    .getDocumentationElement()));
                                    System.out
                                            .println("  ========== End Binding =========");
                                    SOAPBinding soapBinding = null;
                                    SOAPBinding[] soapBindingList = helper
                                            .getSOAPBindings(binding);
                                    for (int soapIndex = 0; soapIndex < soapBindingList.length; soapIndex++) {
                                        soapBinding = soapBindingList[soapIndex];
                                        if (soapBinding != null) {

                                            System.out
                                                    .println("  SOAP Binding Type LocalPart: "
                                                            + soapBinding
                                                                    .getElementType()
                                                                    .getLocalPart());
                                            System.out
                                                    .println("  SOAP Binding Type Namespace: "
                                                            + soapBinding
                                                                    .getElementType()
                                                                    .getNamespaceURI());
                                            System.out
                                                    .println("  SOAP Binding Type Prefix: "
                                                            + soapBinding
                                                                    .getElementType()
                                                                    .getPrefix());
                                            System.out
                                                    .println("  SOAP Binding Style: "
                                                            + soapBinding
                                                                    .getStyle());
                                            System.out
                                                    .println("  SOAP Binding Transport URI: "
                                                            + soapBinding
                                                                    .getTransportURI());
                                            System.out
                                                    .println("  SOAP Binding isRequired: "
                                                            + soapBinding
                                                                    .getRequired());
                                            System.out
                                                    .println("  ========== End SOAP Binding =========");
                                        }
                                    }
                                    HTTPBinding httpBinding = null;
                                    HTTPBinding[] httpBindingList = helper
                                            .getHTTPBindings(binding);
                                    for (int httpIndex = 0; httpIndex < httpBindingList.length; httpIndex++) {
                                        httpBinding = httpBindingList[httpIndex];
                                        if (httpBinding != null) {

                                            System.out
                                                    .println("  HTTP Binding Type LocalPart: "
                                                            + httpBinding
                                                                    .getElementType()
                                                                    .getLocalPart());
                                            System.out
                                                    .println("  HTTP Binding Type Namespace: "
                                                            + httpBinding
                                                                    .getElementType()
                                                                    .getNamespaceURI());
                                            System.out
                                                    .println("  HTTP Binding Type Prefix: "
                                                            + httpBinding
                                                                    .getElementType()
                                                                    .getPrefix());
                                            System.out
                                                    .println("  HTTP Binding Verb: "
                                                            + httpBinding
                                                                    .getVerb());
                                            System.out
                                                    .println("  HTTP Binding isRequired: "
                                                            + httpBinding
                                                                    .getRequired());
                                            System.out
                                                    .println("  ========== End HTTP Binding =========");
                                        }
                                    }
                                    PortType portType = binding.getPortType();
                                    if (portType != null) {
                                        System.out
                                                .println("   PortType LocalPart: "
                                                        + binding.getQName()
                                                                .getLocalPart());
                                        System.out
                                                .println("   PortType Namespace: "
                                                        + binding
                                                                .getQName()
                                                                .getNamespaceURI());
                                        System.out
                                                .println("   PortType Prefix: "
                                                        + binding.getQName()
                                                                .getPrefix());
                                        System.out
                                                .println("   PortType Documentation: "
                                                        + helper
                                                                .getDocumentation(binding
                                                                        .getDocumentationElement()));
                                        System.out
                                                .println("   ========== End PortType =========");
                                        Operation operation = null;
                                        List operationsList = portType
                                                .getOperations();
                                        for (Iterator operationsIterator = operationsList
                                                .iterator(); operationsIterator
                                                .hasNext() == true;) {
                                            operation = (Operation) operationsIterator
                                                    .next();
                                            if (operation != null) {
                                                BindingOperation bindingOperation = helper
                                                        .findBindingOperation(
                                                                binding,
                                                                operation);
                                                if (bindingOperation != null) {
                                                    BindingInput bindingInput = bindingOperation
                                                            .getBindingInput();
                                                    BindingOutput bindingOutput = bindingOperation
                                                            .getBindingOutput();
                                                    if (bindingInput != null) {
                                                        System.out
                                                                .println("     binding Input Name: "
                                                                        + bindingInput
                                                                                .getName());
                                                        SOAPHeader header = helper
                                                                .getSOAPBindingInputHeader(bindingInput);
                                                        SOAPBody body = helper
                                                                .getSOAPBindingInputBody(bindingInput);
                                                        if (header != null) {
                                                            System.out
                                                                    .println("      binding Input Header namespace URI: "
                                                                            + header
                                                                                    .getNamespaceURI());
                                                            System.out
                                                                    .println("      binding Input Header Part: "
                                                                            + header
                                                                                    .getPart());
                                                            String partString = header
                                                                    .getPart();
                                                            if (partString != null) {
                                                                SchemaType type = xsdHelper
                                                                        .retrieveSchemaType(partString);
                                                                if (type != null) {
                                                                    System.out
                                                                            .println("      binding Input Header Element: "
                                                                                    + type);
                                                                    SchemaParticle schemaParticle = null;
                                                                    switch (type
                                                                            .getContentType()) {
                                                                    case SchemaType.NOT_COMPLEX_TYPE:
                                                                        System.out
                                                                                .println("      "
                                                                                        + type
                                                                                                .getName()
                                                                                                .getLocalPart());
                                                                        System.out
                                                                                .println("      =>NOT Complex Type");
                                                                        break;
                                                                    case SchemaType.EMPTY_CONTENT:
                                                                        System.out
                                                                                .println("       "
                                                                                        + type
                                                                                                .getName()
                                                                                                .getLocalPart());
                                                                        System.out
                                                                                .println("       =>Empty Content");
                                                                        break;
                                                                    case SchemaType.SIMPLE_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("       "
                                                                                            + schemaParticle
                                                                                                    .getType()
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("       =>Simple Content");
                                                                        break;
                                                                    case SchemaType.MIXED_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("       "
                                                                                            + schemaParticle
                                                                                                    .getType()
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("       =>Mixed Content");
                                                                        break;
                                                                    case SchemaType.ELEMENT_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("       "
                                                                                            + schemaParticle
                                                                                                    .getType()
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("       =>Element Content");
                                                                        break;
                                                                    default:
                                                                        System.out
                                                                                .println("@@@ WARNING: UNKNOWN TYPE !!!! @@@");
                                                                        break;
                                                                    }
                                                                }
                                                            }
                                                            System.out
                                                                    .println("      binding Input Header Use: "
                                                                            + header
                                                                                    .getUse());
                                                            List encodingStyles = header
                                                                    .getEncodingStyles();
                                                            System.out
                                                                    .println("      binding Input Header Encoding Styles: "
                                                                            + encodingStyles);
                                                            List faults = header
                                                                    .getSOAPHeaderFaults();
                                                            System.out
                                                                    .println("      binding Input Header Header Faults: "
                                                                            + faults);
                                                            for (Iterator faultsIterator = faults
                                                                    .iterator(); faultsIterator
                                                                    .hasNext() == true;) {
                                                                Fault faultImpl = (Fault) faultsIterator
                                                                        .next();
                                                                if (faultImpl != null) {
                                                                    System.out
                                                                            .println("       binding Input Fault Name: "
                                                                                    + faultImpl
                                                                                            .getName());
                                                                    System.out
                                                                            .println("       binding Input Fault Documentation: "
                                                                                    + helper
                                                                                            .getDocumentation(faultImpl
                                                                                                    .getDocumentationElement()));
                                                                    Message message = faultImpl
                                                                            .getMessage();
                                                                    if (message != null) {
                                                                        System.out
                                                                                .println("       binding Input Fault Message QName: "
                                                                                        + message
                                                                                                .getQName());
                                                                        System.out
                                                                                .println("       binding Input Fault Message Documentation: "
                                                                                        + helper
                                                                                                .getDocumentation(message
                                                                                                        .getDocumentationElement()));
                                                                        List elementExtensiblesList = message
                                                                                .getExtensibilityElements();
                                                                        System.out
                                                                                .println("       binding Input Fault elementExtensibleList: "
                                                                                        + elementExtensiblesList);
                                                                        Map partsMap = message
                                                                                .getParts();
                                                                        System.out
                                                                                .println("       binding Input Fault partsMap: "
                                                                                        + partsMap);
                                                                        System.out
                                                                                .println("       ========== End binding Input Fault Message =========");
                                                                    }
                                                                }
                                                            }
                                                            System.out
                                                                    .println("      binding Input Header Element Type: "
                                                                            + header
                                                                                    .getElementType());
                                                            SchemaType type = null;
                                                            QName elementQName = header
                                                                    .getElementType();
                                                            if (elementQName != null) {
                                                                type = xsdHelper
                                                                        .retrieveSchemaType(elementQName);
                                                                if (type != null) {
                                                                    System.out
                                                                            .println("      binding Input Header Element: "
                                                                                    + type);
                                                                    SchemaParticle schemaParticle = null;
                                                                    switch (type
                                                                            .getContentType()) {
                                                                    case SchemaType.NOT_COMPLEX_TYPE:
                                                                        System.out
                                                                                .println("      "
                                                                                        + type
                                                                                                .getName()
                                                                                                .getLocalPart());
                                                                        System.out
                                                                                .println("      =>NOT Complex Type");
                                                                        break;
                                                                    case SchemaType.EMPTY_CONTENT:
                                                                        System.out
                                                                                .println("       "
                                                                                        + type
                                                                                                .getName()
                                                                                                .getLocalPart());
                                                                        System.out
                                                                                .println("       =>Empty Content");
                                                                        break;
                                                                    case SchemaType.SIMPLE_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("       "
                                                                                            + schemaParticle
                                                                                                    .getType()
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("       =>Simple Content");
                                                                        break;
                                                                    case SchemaType.MIXED_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("       "
                                                                                            + schemaParticle
                                                                                                    .getType()
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("       =>Mixed Content");
                                                                        break;
                                                                    case SchemaType.ELEMENT_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("       "
                                                                                            + schemaParticle
                                                                                                    .getType()
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("       =>Element Content");
                                                                        break;
                                                                    default:
                                                                        System.out
                                                                                .println("@@@ WARNING: UNKNOWN TYPE !!!! @@@");
                                                                        break;
                                                                    }
                                                                }
                                                            }
                                                            System.out
                                                                    .println("      ========== End binding Input header =========");
                                                        }
                                                        if (body != null) {
                                                            System.out
                                                                    .println("      binding Input Body namespace URI: "
                                                                            + body
                                                                                    .getNamespaceURI());
                                                            System.out
                                                                    .println("      binding Input body Use: "
                                                                            + body
                                                                                    .getUse());
                                                            System.out
                                                                    .println("      binding Input body Element Type: "
                                                                            + body
                                                                                    .getElementType());
                                                            SchemaType type = null;
                                                            QName elementQName = body
                                                                    .getElementType();
                                                            type = xsdHelper
                                                                    .getExpandedSchemaType(elementQName);
                                                            if (type != null) {
                                                                System.out
                                                                        .println("        "
                                                                                + type
                                                                                        .getName()
                                                                                        .getLocalPart());
                                                            } else {
                                                                System.out
                                                                        .println("@@@ WARNING: UNKNOWN TYPE !!!! @@@");
                                                            }
                                                            List encodingStyles = body
                                                                    .getEncodingStyles();
                                                            System.out
                                                                    .println("      binding Input Encoding Styles: "
                                                                            + encodingStyles);
                                                            List mimeParts = body
                                                                    .getParts();
                                                            System.out
                                                                    .println("      binding Input body Parts: "
                                                                            + mimeParts);
                                                            System.out
                                                                    .println("      binding Input body isRequired?: "
                                                                            + body
                                                                                    .getRequired());
                                                            System.out
                                                                    .println("      ========== End binding Input body =========");
                                                        }
                                                    }
                                                    if (bindingOutput != null) {
                                                        System.out
                                                                .println("     binding Output Name: "
                                                                        + bindingOutput
                                                                                .getName());
                                                        System.out
                                                                .println("     binding Output Documentation: "
                                                                        + helper
                                                                                .getDocumentation(bindingOutput
                                                                                        .getDocumentationElement()));
                                                        System.out
                                                                .println("     ========== End binding Output =========");
                                                        SOAPHeader header = helper
                                                                .getSOAPBindingOutputHeader(bindingOutput);
                                                        SOAPBody body = helper
                                                                .getSOAPBindingOutputBody(bindingOutput);
                                                        if (header != null) {
                                                            System.out
                                                                    .println("      binding Output Header namespace URI: "
                                                                            + header
                                                                                    .getNamespaceURI());
                                                            System.out
                                                                    .println("      binding Output Header Part: "
                                                                            + header
                                                                                    .getPart());
                                                            System.out
                                                                    .println("      binding Output Header Use: "
                                                                            + header
                                                                                    .getUse());
                                                            List encodingStyles = header
                                                                    .getEncodingStyles();
                                                            System.out
                                                                    .println("      binding Output Header Encoding Styles: "
                                                                            + encodingStyles);
                                                            List faults = header
                                                                    .getSOAPHeaderFaults();
                                                            System.out
                                                                    .println("      binding Output Header Header Faults: "
                                                                            + faults);
                                                            for (Iterator faultsIterator = faults
                                                                    .iterator(); faultsIterator
                                                                    .hasNext() == true;) {
                                                                Fault faultImpl = (Fault) faultsIterator
                                                                        .next();
                                                                if (faultImpl != null) {
                                                                    System.out
                                                                            .println("       binding Output Fault Name: "
                                                                                    + faultImpl
                                                                                            .getName());
                                                                    System.out
                                                                            .println("       binding Output Fault Documentation: "
                                                                                    + helper
                                                                                            .getDocumentation(faultImpl
                                                                                                    .getDocumentationElement()));
                                                                    Message message = faultImpl
                                                                            .getMessage();
                                                                    if (message != null) {
                                                                        System.out
                                                                                .println("       binding Output Fault Message QName: "
                                                                                        + message
                                                                                                .getQName());
                                                                        System.out
                                                                                .println("       binding Output Fault Message Documentation: "
                                                                                        + helper
                                                                                                .getDocumentation(message
                                                                                                        .getDocumentationElement()));
                                                                        List elementExtensiblesList = message
                                                                                .getExtensibilityElements();
                                                                        System.out
                                                                                .println("       binding Output Fault elementExtensibleList: "
                                                                                        + elementExtensiblesList);
                                                                        Map partsMap = message
                                                                                .getParts();
                                                                        System.out
                                                                                .println("       binding Output Fault partsMap: "
                                                                                        + partsMap);
                                                                        System.out
                                                                                .println("       ========== End binding Output Fault Message =========");
                                                                    }
                                                                }
                                                            }
                                                            System.out
                                                                    .println("      binding Output Header Element Type: "
                                                                            + header
                                                                                    .getElementType());
                                                            System.out
                                                                    .println("      ========== End binding Output header =========");
                                                        }
                                                        if (body != null) {
                                                            System.out
                                                                    .println("      binding Output Body namespace URI: "
                                                                            + body
                                                                                    .getNamespaceURI());
                                                            System.out
                                                                    .println("      binding Output body Use: "
                                                                            + body
                                                                                    .getUse());
                                                            System.out
                                                                    .println("      binding Output body Element Type: "
                                                                            + body
                                                                                    .getElementType());
                                                            List encodingStyles = body
                                                                    .getEncodingStyles();
                                                            System.out
                                                                    .println("      binding Output Encoding Stles: "
                                                                            + encodingStyles);
                                                            List parts = body
                                                                    .getParts();
                                                            System.out
                                                                    .println("      binding Output body Parts: "
                                                                            + parts);
                                                            System.out
                                                                    .println("      binding Output body isRequired?: "
                                                                            + body
                                                                                    .getRequired());
                                                            System.out
                                                                    .println("      ========== End binding Output body =========");
                                                        }
                                                    }
                                                }

                                                System.out
                                                        .println("     Operation Name: "
                                                                + operation
                                                                        .getName());
                                                System.out
                                                        .println("     Operation Documentation: "
                                                                + helper
                                                                        .getDocumentation(operation
                                                                                .getDocumentationElement()));
                                                List elementExtensibleList = operation
                                                        .getExtensibilityElements();
                                                System.out
                                                        .println("     Operation elementExtensibleList: "
                                                                + elementExtensibleList);
                                                Map faultMap = operation
                                                        .getFaults();
                                                if (faultMap != null) {
                                                    for (Iterator faultsIterator = faultMap
                                                            .keySet()
                                                            .iterator(); faultsIterator
                                                            .hasNext() == true;) {
                                                        String key = (String) faultsIterator
                                                                .next();
                                                        Fault faultImpl = null;
                                                        if (key != null) {
                                                            faultImpl = (Fault) faultMap
                                                                    .get(key);
                                                            if (faultImpl != null) {
                                                                System.out
                                                                        .println("       Operation Fault Name: "
                                                                                + faultImpl
                                                                                        .getName());
                                                                System.out
                                                                        .println("       Operation Fault Documentation: "
                                                                                + helper
                                                                                        .getDocumentation(faultImpl
                                                                                                .getDocumentationElement()));
                                                                Message message = faultImpl
                                                                        .getMessage();
                                                                if (message != null) {
                                                                    System.out
                                                                            .println("       Operation Fault Message QName: "
                                                                                    + message
                                                                                            .getQName());
                                                                    System.out
                                                                            .println("       Operation Fault Message Documentation: "
                                                                                    + helper
                                                                                            .getDocumentation(message
                                                                                                    .getDocumentationElement()));
                                                                    List elementExtensiblesList = message
                                                                            .getExtensibilityElements();
                                                                    System.out
                                                                            .println("       Operation Fault elementExtensibleList: "
                                                                                    + elementExtensiblesList);
                                                                    Map partsMap = message
                                                                            .getParts();
                                                                    System.out
                                                                            .println("       Operation Fault partsMap: "
                                                                                    + partsMap);
                                                                    if (partsMap != null) {
                                                                        for (Iterator partsIterator = partsMap
                                                                                .keySet()
                                                                                .iterator(); partsIterator
                                                                                .hasNext() == true;) {
                                                                            String partKey = (String) partsIterator
                                                                                    .next();
                                                                            if (partKey != null) {
                                                                                Part part = (Part) partsMap
                                                                                        .get(partKey);
                                                                                if (part != null) {
                                                                                    System.out
                                                                                            .println("       Operation Fault part Name: "
                                                                                                    + part
                                                                                                            .getName());
                                                                                    SchemaType type = xsdHelper
                                                                                            .retrieveSchemaType(part
                                                                                                    .getElementName());
                                                                                    if (type != null) {
                                                                                        SchemaParticle schemaParticle = null;
                                                                                        switch (type
                                                                                                .getContentType()) {
                                                                                        case SchemaType.NOT_COMPLEX_TYPE:
                                                                                            System.out
                                                                                                    .println("        "
                                                                                                            + type
                                                                                                                    .getName()
                                                                                                                    .getLocalPart());
                                                                                            System.out
                                                                                                    .println("        =>NOT Complex Type");
                                                                                            break;
                                                                                        case SchemaType.EMPTY_CONTENT:
                                                                                            System.out
                                                                                                    .println("        ");
                                                                                            System.out
                                                                                                    .println("        =>Empty Content");
                                                                                            break;
                                                                                        case SchemaType.SIMPLE_CONTENT:
                                                                                            schemaParticle = type
                                                                                                    .getContentModel();
                                                                                            if (schemaParticle != null) {
                                                                                                System.out
                                                                                                        .println("        "
                                                                                                                + schemaParticle
                                                                                                                        .getName()
                                                                                                                        .getLocalPart());
                                                                                            }
                                                                                            System.out
                                                                                                    .println("        =>Simple Content");
                                                                                            break;
                                                                                        case SchemaType.MIXED_CONTENT:
                                                                                            schemaParticle = type
                                                                                                    .getContentModel();
                                                                                            if (schemaParticle != null) {
                                                                                                System.out
                                                                                                        .println("        "
                                                                                                                + schemaParticle
                                                                                                                        .getName()
                                                                                                                        .getLocalPart());
                                                                                            }
                                                                                            System.out
                                                                                                    .println("        =>Mixed Content");
                                                                                            break;
                                                                                        case SchemaType.ELEMENT_CONTENT:
                                                                                            schemaParticle = type
                                                                                                    .getContentModel();
                                                                                            if (schemaParticle != null) {
                                                                                                System.out
                                                                                                        .println("        "
                                                                                                                + schemaParticle
                                                                                                                        .getName()
                                                                                                                        .getLocalPart());
                                                                                            }
                                                                                            System.out
                                                                                                    .println("        =>Element Content");
                                                                                            break;
                                                                                        default:
                                                                                            System.out
                                                                                                    .println("@@@ WARNING: UNKNOWN TYPE !!!! @@@");
                                                                                            break;
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                    System.out
                                                                            .println("       ========== End binding Input Fault Message =========");
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                                List parameterOrderingList = operation
                                                        .getParameterOrdering();
                                                if (parameterOrderingList != null) {
                                                    for (Iterator orderingIterator = parameterOrderingList
                                                            .iterator(); orderingIterator
                                                            .hasNext() == true;) {
                                                        String parameterName = (String) orderingIterator
                                                                .next();
                                                        if (parameterName != null) {
                                                            System.out
                                                                    .println("       Parameter Order: "
                                                                            + parameterName);
                                                        }
                                                    }
                                                }
                                                OperationType operationType = operation
                                                        .getStyle();
                                                System.out
                                                        .println("     Operation operationType: "
                                                                + operationType);
                                                System.out
                                                        .println("     ========== End Operation =========");
                                                Input input = operation
                                                        .getInput();
                                                if (input != null) {
                                                    System.out
                                                            .println("      Operation Input Name: "
                                                                    + input
                                                                            .getName());
                                                    System.out
                                                            .println("      Operation Input Documentation: "
                                                                    + helper
                                                                            .getDocumentation(input
                                                                                    .getDocumentationElement()));
                                                    Map extensionAttributeMap = input
                                                            .getExtensionAttributes();
                                                    System.out
                                                            .println("      Operation Input Extension Attribute Map: "
                                                                    + extensionAttributeMap);
                                                    List nativeAttributeNames = input
                                                            .getNativeAttributeNames();
                                                    System.out
                                                            .println("      Operation Input Native Attribute Names: "
                                                                    + nativeAttributeNames);
                                                    System.out
                                                            .println("      ========== End Operation Input =========");
                                                    Message message = input
                                                            .getMessage();
                                                    if (message != null) {
                                                        System.out
                                                                .println("       Operation Input Message QName: "
                                                                        + message
                                                                                .getQName());
                                                        System.out
                                                                .println("       Operation Input Message Documentation: "
                                                                        + helper
                                                                                .getDocumentation(message
                                                                                        .getDocumentationElement()));
                                                        List elementExtensiblesList = message
                                                                .getExtensibilityElements();
                                                        System.out
                                                                .println("       Operation Input elementExtensibleList: "
                                                                        + elementExtensiblesList);
                                                        System.out
                                                                .println("       ========== End Operation Input Message =========");
                                                        Map partsMap = message
                                                                .getParts();
                                                        String key = null;
                                                        Part partImpl = null;
                                                        for (Iterator partsIterator = partsMap
                                                                .keySet()
                                                                .iterator(); partsIterator
                                                                .hasNext();) {
                                                            key = (String) partsIterator
                                                                    .next();
                                                            if (key != null) {
                                                                partImpl = (Part) partsMap
                                                                        .get(key);
                                                                if (partImpl != null) {
                                                                    System.out
                                                                            .println("        Part Name: "
                                                                                    + partImpl
                                                                                            .getName());
                                                                    System.out
                                                                            .println("        Part Documentation: "
                                                                                    + helper
                                                                                            .getDocumentation(partImpl
                                                                                                    .getDocumentationElement()));
                                                                    SchemaType type = null;
                                                                    QName elementQName = partImpl
                                                                            .getElementName();
                                                                    if (elementQName != null) {
                                                                        type = xsdHelper
                                                                                .retrieveSchemaType(elementQName);
                                                                        if (type != null) {
                                                                            System.out
                                                                                    .println("        Element: "
                                                                                            + type);
                                                                        }
                                                                    }
                                                                    QName typeQName = partImpl
                                                                            .getTypeName();
                                                                    if (typeQName != null) {
                                                                        type = xsdHelper
                                                                                .retrieveSchemaType(typeQName);
                                                                        if (type != null) {
                                                                            System.out
                                                                                    .println("        Type: "
                                                                                            + type);
                                                                        }
                                                                    }
                                                                    SchemaParticle schemaParticle = null;
                                                                    if ((type
                                                                            .getContentType() == SchemaType.MIXED_CONTENT)
                                                                            || (type
                                                                                    .getContentType() == SchemaType.ELEMENT_CONTENT)) {
                                                                        type = xsdHelper
                                                                                .getDataStructure(type);
                                                                    }
                                                                    System.out
                                                                            .println(type);
                                                                    switch (type
                                                                            .getContentType()) {
                                                                    case SchemaType.NOT_COMPLEX_TYPE:
                                                                        System.out
                                                                                .println("        "
                                                                                        + type
                                                                                                .getName()
                                                                                                .getLocalPart());
                                                                        System.out
                                                                                .println("        =>NOT Complex Type");
                                                                        break;
                                                                    case SchemaType.EMPTY_CONTENT:
                                                                        System.out
                                                                                .println("        No arguments");
                                                                        System.out
                                                                                .println("        =>Empty Content");
                                                                        break;
                                                                    case SchemaType.SIMPLE_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("        "
                                                                                            + schemaParticle
                                                                                                    .getType()
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("        =>Simple Content");
                                                                        break;
                                                                    case SchemaType.MIXED_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            SchemaParticle[] array = schemaParticle
                                                                                    .getParticleChildren();
                                                                            if (array != null) {
                                                                                for (int index = 0; index < array.length; index++) {
                                                                                    System.out
                                                                                            .println("        "
                                                                                                    + array[index]
                                                                                                            .getType()
                                                                                                            .getName()
                                                                                                            .getLocalPart()
                                                                                                    + " "
                                                                                                    + array[index]
                                                                                                            .getName()
                                                                                                            .getLocalPart());
                                                                                }
                                                                            } else {
                                                                                System.out
                                                                                        .println("        "
                                                                                                + schemaParticle
                                                                                                        .getName()
                                                                                                        .getLocalPart());
                                                                            }
                                                                        }
                                                                        System.out
                                                                                .println("        =>Mixed Content");
                                                                        break;
                                                                    case SchemaType.ELEMENT_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            SchemaParticle[] array = schemaParticle
                                                                                    .getParticleChildren();
                                                                            if (array != null) {
                                                                                for (int index = 0; index < array.length; index++) {
                                                                                    if (array[index] != null) {
                                                                                        if (array[index] instanceof SchemaLocalElement) {
                                                                                            SchemaLocalElement localElement = (SchemaLocalElement) array[index];
                                                                                            SchemaType schemaType = localElement
                                                                                                    .getType();
                                                                                            QName qName = schemaType
                                                                                                    .getName();
                                                                                            if (qName != null) {
                                                                                                System.out
                                                                                                        .println("        "
                                                                                                                + array[index]
                                                                                                                        .getType()
                                                                                                                        .getName()
                                                                                                                        .getLocalPart()
                                                                                                                + " "
                                                                                                                + array[index]
                                                                                                                        .getName()
                                                                                                                        .getLocalPart());
                                                                                            } else {
                                                                                                System.out
                                                                                                        .println("        "
                                                                                                                + array[index]
                                                                                                                        .getName()
                                                                                                                        .getLocalPart()
                                                                                                                + " "
                                                                                                                + array[index]
                                                                                                                        .getName()
                                                                                                                        .getLocalPart());
                                                                                            }
                                                                                        } else if (array[index] instanceof SchemaParticle) {
                                                                                            SchemaParticle particle = (SchemaParticle) array[index];
                                                                                            if (particle
                                                                                                    .getParticleType() == SchemaParticle.WILDCARD) {
                                                                                                System.out
                                                                                                        .println("Wildcard: Any Attribute");
                                                                                            }
                                                                                        } else {
                                                                                            System.out
                                                                                                    .println("        "
                                                                                                            + array[index]
                                                                                                                    .getType()
                                                                                                                    .getName()
                                                                                                                    .getLocalPart()
                                                                                                            + " "
                                                                                                            + array[index]
                                                                                                                    .getName()
                                                                                                                    .getLocalPart());
                                                                                        }
                                                                                    }

                                                                                }
                                                                            } else {
                                                                                System.out
                                                                                        .println("        "
                                                                                                + schemaParticle
                                                                                                        .getName()
                                                                                                        .getLocalPart());
                                                                            }
                                                                        }
                                                                        System.out
                                                                                .println("        =>Element Content");
                                                                        break;
                                                                    default:
                                                                        System.out
                                                                                .println("@@@ WARNING: UNKNOWN TYPE !!!! @@@");
                                                                        break;
                                                                    }
                                                                    System.out
                                                                            .println("        ========== End Part Type =========");
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                                Output output = operation
                                                        .getOutput();
                                                if (output != null) {
                                                    System.out
                                                            .println("      Operation Output Name: "
                                                                    + output
                                                                            .getName());
                                                    System.out
                                                            .println("      Operation Output Documentation: "
                                                                    + helper
                                                                            .getDocumentation(output
                                                                                    .getDocumentationElement()));
                                                    Map extensionAttributeMap = output
                                                            .getExtensionAttributes();
                                                    System.out
                                                            .println("      Operation Output Extension Attribute Map: "
                                                                    + extensionAttributeMap);
                                                    List nativeAttributeNames = output
                                                            .getNativeAttributeNames();
                                                    System.out
                                                            .println("      Operation Output Native Attribute Names: "
                                                                    + nativeAttributeNames);
                                                    System.out
                                                            .println("      ========== End Operation Output =========");
                                                    Message message = output
                                                            .getMessage();
                                                    if (message != null) {
                                                        System.out
                                                                .println("       Operation Output Message QName: "
                                                                        + message
                                                                                .getQName());
                                                        System.out
                                                                .println("       Operation Output Message Documentation: "
                                                                        + helper
                                                                                .getDocumentation(message
                                                                                        .getDocumentationElement()));
                                                        List elementExtensiblesList = message
                                                                .getExtensibilityElements();
                                                        System.out
                                                                .println("       Operation Output elementExtensibleList: "
                                                                        + elementExtensiblesList);
                                                        System.out
                                                                .println("       ========== End Operation Output Message =========");
                                                        Map partsMap = message
                                                                .getParts();
                                                        String key = null;
                                                        Part partImpl = null;
                                                        for (Iterator partsIterator = partsMap
                                                                .keySet()
                                                                .iterator(); partsIterator
                                                                .hasNext();) {
                                                            key = (String) partsIterator
                                                                    .next();
                                                            if (key != null) {
                                                                partImpl = (Part) partsMap
                                                                        .get(key);
                                                                if (partImpl != null) {
                                                                    System.out
                                                                            .println("        Part Name: "
                                                                                    + partImpl
                                                                                            .getName());
                                                                    System.out
                                                                            .println("        Part Documentation: "
                                                                                    + helper
                                                                                            .getDocumentation(partImpl
                                                                                                    .getDocumentationElement()));
                                                                    SchemaType type = null;
                                                                    QName elementQName = partImpl
                                                                            .getElementName();
                                                                    if (elementQName != null) {
                                                                        type = xsdHelper
                                                                                .retrieveSchemaType(elementQName);
                                                                        if (type != null) {
                                                                            System.out
                                                                                    .println("        Element: "
                                                                                            + type);
                                                                        }
                                                                    }
                                                                    QName typeQName = partImpl
                                                                            .getTypeName();
                                                                    if (typeQName != null) {
                                                                        type = xsdHelper
                                                                                .retrieveSchemaType(typeQName);
                                                                        if (type != null) {
                                                                            System.out
                                                                                    .println("        Type: "
                                                                                            + type);
                                                                        }
                                                                    }
                                                                    SchemaParticle schemaParticle = null;
                                                                    switch (type
                                                                            .getContentType()) {
                                                                    case SchemaType.NOT_COMPLEX_TYPE:
                                                                        System.out
                                                                                .println("        "
                                                                                        + type
                                                                                                .getName()
                                                                                                .getLocalPart());
                                                                        System.out
                                                                                .println("        =>NOT Complex Type");
                                                                        break;
                                                                    case SchemaType.EMPTY_CONTENT:
                                                                        System.out
                                                                                .println("        ");
                                                                        System.out
                                                                                .println("        =>Empty Content");
                                                                        break;
                                                                    case SchemaType.SIMPLE_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("        "
                                                                                            + schemaParticle
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("        =>Simple Content");
                                                                        break;
                                                                    case SchemaType.MIXED_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("        "
                                                                                            + schemaParticle
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("        =>Mixed Content");
                                                                        break;
                                                                    case SchemaType.ELEMENT_CONTENT:
                                                                        schemaParticle = type
                                                                                .getContentModel();
                                                                        if (schemaParticle != null) {
                                                                            System.out
                                                                                    .println("        "
                                                                                            + schemaParticle
                                                                                                    .getName()
                                                                                                    .getLocalPart());
                                                                        }
                                                                        System.out
                                                                                .println("        =>Element Content");
                                                                        break;
                                                                    default:
                                                                        System.out
                                                                                .println("@@@ WARNING: UNKNOWN TYPE !!!! @@@");
                                                                        break;
                                                                    }
                                                                    System.out
                                                                            .println("        ========== End Part Type =========");
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                System.out.println("");
                            }
                        }
                    }
                }
            }
        } catch (WSDLException e) {
            // TODO Auto-generated catch block
            //          e.printStackTrace();
            System.out.println("Exception parsing WSDL: " + e.getMessage());
        } catch (XmlException e) {
            // TODO Auto-generated catch block
            //          e.printStackTrace();
            System.out.println("Exception parsing XSD: " + e.getMessage());
        }
    }

}
