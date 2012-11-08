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
 * @(#)WSDLConfigurations.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.packaging;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.net.URL;

import com.sun.xsd.model.FastSchemaFactory;
import com.sun.xsd.model.FastSchema;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;
import org.exolab.castor.xml.schema.reader.SchemaReader;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.ElementDecl;
import org.w3c.dom.Document;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.swiftbc.Endpoint;
import com.sun.jbi.swiftbc.EndpointImpl;
import com.sun.jbi.swiftbc.extensions.SwiftAddress;
import com.sun.jbi.swiftbc.extensions.SwiftBinding;
import com.sun.jbi.swiftbc.extensions.SwiftExtensionRegistry;
import com.sun.jbi.swiftbc.extensions.SwiftExtPreprocessDeserializer;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties;
import com.sun.jbi.swiftbc.validator.EndpointValidator;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.extensions.SwiftInput;
import com.sun.jbi.swiftbc.extensions.SwiftInput;
import com.sun.jbi.swiftbc.extensions.SwiftOutput;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints based on
 * the portmap list.
 *
 * @author S. Nageswara Rao
 */
/**
 * @author stcuser
 *
 */
public class WSDLConfigurations {
    
    private static final Messages mMessages = Messages
            .getMessages(WSDLConfigurations.class);
    
    private static Logger mLogger = Messages
            .getLogger(WSDLConfigurations.class);
    
    private String mRootPath;
    
    private List mXsds = new ArrayList();
    
    private Map mEncoderMap = new HashMap();
    
    public WSDLConfigurations(String rootPath) {
        mRootPath = rootPath;
    }
    
    public List parse(List portMaps, Map envVariableMap) throws Exception {
        Map envVariables = (Map) ((HashMap) envVariableMap).clone();
        File dir = new File(mRootPath);
        
        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() + File.separator
                + "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);
        
        ArrayList endpoints = new ArrayList();
        // Iterator wsdls = listWSDLFiles(dir).iterator();
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        mXsds = listResourceFiles(dir, ".xsd");
        while (wsdls.hasNext()) {
            endpoints.addAll(parseWSDL((File) wsdls.next(), resolver, portMaps,
                    envVariables));
        }
        
        return endpoints;
    }
    
    public Map parseForEnvironmentVariables(List portMaps, Map envVariableMap)
            throws Exception {
        Map envVariables = (Map) ((HashMap) envVariableMap).clone();
        File dir = new File(mRootPath);
        
        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() + File.separator
                + "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        while (wsdls.hasNext()) {
            envVariables.putAll(readWSDLForEnvVariables((File) wsdls.next(),
                    resolver, envVariables));
        }
        
        return envVariables;
    }
    
    public List parseWSDL(File wsdlFile, EntityResolver resolver,
            List portMaps, Map envVariableMap) throws Exception {
        
        Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData) it.next();
            
            // Check the Definition if it has an swiftBinding. If not,
            // continue
            SwiftBinding binding = getSwiftBinding(def, pm.getService(), pm
                    .getEndpoint());
            if (binding == null) {
                continue;
            }
            
            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint. If we find one,
            // extract file details
            
            // If we have an swiftBinding, we must have an swiftAddress.
            SwiftAddress address = getSwiftAddress(def, pm.getService(), pm
                    .getEndpoint());
            if (address == null) {
                String msg = mMessages.getString("WCF_Missing_swift_addr")
                        + pm.getService();
                throw new Exception(msg);
            }

            SwiftProtocolProperties protocolproperties = getSwiftProtocolProperties(
                    def, pm.getService(), pm.getEndpoint());
            /*
            if (protocolproperties == null) {
                String msg = mMessages
                        .getString("WCF_Missing_swift_protocolproperties")
                        + pm.getService();
                throw new Exception(msg);
            }
            */

            // If we have an swiftBinding, we must have operations
            Map swiftOperations = getSwiftOperations(def, pm.getService(), pm
                    .getEndpoint());
            
            if (swiftOperations == null || swiftOperations.size() == 0) {
                String msg = mMessages.getString("WCF_Missing_swift_op")
                        + pm.getService();
                throw new Exception(msg);
            }
            
            Map opMeps = getSwiftOperationMepType(def, pm.getService(), pm
                    .getEndpoint(), pm.getDirection());
            
            // Map partMappings = getMessagePartXSDMapping(def, pm.getService(),
            // pm.getEndpoint(),
            // swiftOperations);
            
            // Create an Endpoint for each Port. The endpoint should have the
            // correct swiftBinding, the associated Operations, and the
            // associated OperationInput and OperationOutput
            Endpoint endpoint = new EndpointImpl();
            
            // Store the endpoint name
            endpoint.setEndpointName(pm.getEndpoint());
            
            // Store the Service name
            endpoint.setServiceName(QName.valueOf(pm.getService()));
            
            // Store the Definition
            endpoint.setDefinition(def);
            
            // Store the type of Endpoint
            endpoint.setEndpointType(pm.getDirection());
            
            // Set the description
            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory
                    .newInstance();
            DocumentBuilder documentBuilder = docBuilderFactory
                    .newDocumentBuilder();
            Document result = documentBuilder.parse(wsdlFile);
            endpoint.setServiceDescription(result);
            endpoint.setServiceUnitPath(mRootPath);
            
            // Store our extensibility elements
            endpoint.setSwiftAddress(address);
            endpoint.setSwiftProtocolProperties(protocolproperties);
            endpoint.setSwiftBinding(binding);
            endpoint.setSwiftOperations(swiftOperations);
            
            endpoint.setOperationMsgExchangePattern(opMeps);
            setInputsOutputs(def, pm.getService(), pm.getEndpoint(),
                    swiftOperations.values(), endpoint);
            // This list gets utilized while generating NAK/ACK from swift BC
            endpoint.setXsdsList(mXsds);
            
            EndpointValidator.validateEndpointForUniqueness(endPoints,
                    endpoint, false);
            endPoints.add(endpoint);
        }
        
        return endPoints;
    }
    
    /**
     * List all wsdl files in the currentDir and below
     */
    protected List listResourceFiles(File currentDir, String extension) {
        List cumulativeResults = new ArrayList();
        File[] filesInCurrentDir = currentDir.listFiles();
        for (int fileCount = 0; fileCount < filesInCurrentDir.length; fileCount++) {
            
            if (filesInCurrentDir[fileCount].isFile()) {
                if (filesInCurrentDir[fileCount].getName().toLowerCase()
                        .endsWith(extension)) {
                    cumulativeResults.add(filesInCurrentDir[fileCount]);
                }
            } else if (filesInCurrentDir[fileCount].isDirectory()) {
                List wsdlsInSubDirectories = listResourceFiles(
                        filesInCurrentDir[fileCount], extension);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }
    
    protected Definition readWSDL(File f, EntityResolver resolver,
            Map envVariableMap) throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory)
                .newWSDLReader(resolver);
        reader.setExtensionRegistry(new SwiftExtensionRegistry(envVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
    
    protected Map readWSDLForEnvVariables(File f, EntityResolver resolver,
            Map envVariableMap) throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory)
                .newWSDLReader(resolver);
        SwiftExtPreprocessDeserializer preProcessDeserializer = new SwiftExtPreprocessDeserializer(
                envVariableMap);
        reader.setExtensionRegistry(new SwiftExtensionRegistry(
                preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getEnvVariableMap();
    }
    
    protected Binding getBinding(Definition def, String serviceName,
            String endpointName) {
        
        // It checks all imported WSDLs.
        Map services = def.getServices();
        Service svc = (Service) services.get(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }
    
    protected SwiftAddress getSwiftAddress(Definition def, String serviceName,
            String endpointName) {
        SwiftAddress address = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();
            
            // Look for swift:address
            
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SwiftAddress.class.isInstance(ee)) {
                    address = (SwiftAddress) ee;
                }
            }
        }
        return address;
    }
    
    protected SwiftProtocolProperties getSwiftProtocolProperties(
            Definition def, String serviceName, String endpointName) {
        SwiftProtocolProperties protocolProperties = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();
            
            // Look for swift:protocolproperties
            
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()
                    && protocolProperties == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SwiftProtocolProperties.class.isInstance(ee)) {
                    protocolProperties = (SwiftProtocolProperties) ee;
                }
            }
        }
        return protocolProperties;
        
    }
    
    protected SwiftBinding getSwiftBinding(Definition def, String serviceName,
            String endpointName) {
        SwiftBinding swiftBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
            
            // Look for swift:binding
            
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && swiftBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                QName qn = ee.getElementType();
                qn.getLocalPart();
                if (SwiftBinding.class.isInstance(ee)) {
                    swiftBinding = (SwiftBinding) ee;
                }
            }
        }
        return swiftBinding;
    }
    
    protected Map getSwiftOperations(Definition def, String serviceName,
            String endpointName) {
        Map swiftOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null
                    : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for swift:operation entries
                
                Iterator extIter = extElems == null ? null : extElems
                        .iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter
                            .next();
                    if (SwiftOperation.class.isInstance(ee)) {
                        SwiftOperation swiftOperation = (SwiftOperation) ee;
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput
                                    .getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter
                                        .next();
                                SwiftInput swiftInput = null;
                                
                                if (inputExt instanceof SwiftMessage) {
                                    swiftInput = new SwiftInput();
                                    SwiftMessage swiftMessage = (SwiftMessage) inputExt;
                                    swiftInput.setSwiftMessage(swiftMessage);
                                }
                                if (swiftInput != null) {
                                    swiftOperation
                                            .setSwiftOperationInput(swiftInput);
                                }
                            }
                        }
                        
                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator outputIter = bindingOutput
                                    .getExtensibilityElements().iterator();
                            while (outputIter.hasNext()) {
                                ExtensibilityElement outputExt = (ExtensibilityElement) outputIter
                                        .next();
                                SwiftOutput swiftOutput = null;
                                if (outputExt instanceof SwiftMessage) {
                                    swiftOutput = new SwiftOutput();
                                    SwiftMessage swiftMessage = (SwiftMessage) outputExt;
                                    swiftOutput.setSwiftMessage(swiftMessage);
                                }
                                if (swiftOutput != null) {
                                    swiftOperation
                                            .setSwiftOperationOutput(swiftOutput);
                                }
                            }
                        }
                        swiftOperations.put(QName.valueOf(oper.getName()),
                                swiftOperation);
                    }
                }
            }
        }
        return swiftOperations;
    }
    
    protected void setInputsOutputs(Definition def, String serviceName,
            String endpointName, Collection operations, Endpoint endpoint) {
        
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null
                    : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for swift:operation entries
                
                SwiftOperation swiftOperation = null;
                Iterator extIter = extElems == null ? null : extElems
                        .iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter
                            .next();
                    if (operations.contains(ee)) {
                        swiftOperation = (SwiftOperation) ee;
                    }
                }
                
                if (swiftOperation != null) {
                    SwiftInput swiftInput = swiftOperation
                            .getSwiftOperationInput();
                    if (swiftInput != null) {
                        endpoint.setSwiftOperationInput(swiftOperation,
                                swiftInput);
                    }
                    SwiftOutput swiftOutput = swiftOperation
                            .getSwiftOperationOutput();
                    if (swiftOutput != null) {
                        endpoint.setSwiftOperationOutput(swiftOperation,
                                swiftOutput);
                    }
                    
                }
            }
        }
    }
    
    protected Map getSwiftOperationMepType(Definition def, String serviceName,
            String endpointName, int direction) {
        Map mepTypes = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null
                    : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                
                Iterator extIter = extElems == null ? null : extElems
                        .iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter
                            .next();
                    if (SwiftOperation.class.isInstance(ee)) {
                        String mep = Endpoint.EndpointMessageType.UNSUPPORTED;
                        if (oper.getOperation().getStyle() != null) {
                            mep = determineMEP(direction, oper);
                        } else {
                            // for some reason, sometimes the operation type is
                            // not set
                            // so the BC will populate it based on the WSDL
                            // definition
                            // for now, we only handle Request-response and
                            // one-way.
                            // anything else is not supported and should be
                            // caught in WSDL
                            // validation
                            Operation operation = oper.getOperation();
                            OperationType type = null;
                            if (operation.getInput() != null
                                    && operation.getOutput() != null) {
                                type = OperationType.REQUEST_RESPONSE;
                            } else if (operation.getInput() != null) {
                                type = OperationType.ONE_WAY;
                            }
                            if (type != null) {
                                mep = determineMEP(direction, type);
                            }
                        }
                        mepTypes.put(QName.valueOf(oper.getName()), mep);
                    }
                }
            }
        }
        return mepTypes;
    }
    
    public Map getPartEncoderMapping(Definition def, String serviceName,
            String endpointName, int direction, Map swiftOperations,
            Map mepTypes) throws Exception {
        Map partMapping = new HashMap();
        
        // Don't support inline encoder schemas
        if (mXsds.size() <= 0) {
            return partMapping;
        }
        
        Service service = def.getService(QName.valueOf(serviceName));
        Port port = service.getPort(endpointName);
        PortType portType = port.getBinding().getPortType();
        
        /**
         * Locate the operation we are interested in. There may be multiple
         * operations by the same name (operation overloading) and the WSDL spec
         * does not allow it. The uniqueness should be guaranteed by the
         * examination of input and/or output names. The WSDL validation
         * should've been enforced the uniqueness at design time. For the time
         * being, we will assume that we don't have operation overloading.
         */
        javax.wsdl.Message wsdlMessage = null;
        Map parts = new HashMap();
        for (Iterator opnameIter = swiftOperations.keySet().iterator(); opnameIter
                .hasNext();) {
            String mep = null;
            String encodingStyle = null;
            
            QName operationName = (QName) opnameIter.next();
            SwiftOperation swiftOperation = (SwiftOperation) swiftOperations
                    .get(operationName);
            mep = (String) mepTypes.get(operationName);
            
            for (Iterator operIter = portType.getOperations().iterator(); operIter
                    .hasNext();) {
                Operation op = (Operation) operIter.next();
                if (op.getName().equals(operationName.toString())
                        || op.getName().equals(operationName.getLocalPart())) {
                    /**
                     * There is nothing in the WSDL spec that says that the part
                     * name has to be unique within the WSDL document, so we
                     * need to prefix the part name with the message name.
                     */
                    
                    Input input = op.getInput();
                    if (input != null) {
                        SwiftInput swiftInput = swiftOperation
                                .getSwiftOperationInput();
                        if (swiftInput == null) {
                            throw new Exception(mMessages.getString(
                                    "WCF_No_swiftInput", operationName
                                    .toString()));
                        }
                        wsdlMessage = input.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter
                                .hasNext();) {
                            Part aPart = (Part) partIter.next();
                            QName type = (aPart.getElementName() != null) ? aPart
                                    .getElementName()
                                    : aPart.getTypeName();
                            String partName = aPart.getName();
                            
                            // locate the XSD file based on the part type
                            // namespace
                            String namespace = type.getNamespaceURI();
                            String xsdFileLoc = getXsdFileLocation(namespace,
                                    type);
                            if (xsdFileLoc != null) {
                                // Determine on which extensibility element the
                                // encoding style is
                                // defined
                                if (direction == Endpoint.EndpointType.INBOUND) { // Consumer
                                    SwiftMessage swiftMessage = swiftInput
                                            .getSwiftMessage();
                                    if (swiftMessage == null) {
                                        throw new Exception(mMessages
                                                .getString(
                                                "WCF_No_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                    if (swiftMessage.getUseType().equals(
                                            SwiftMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = swiftMessage
                                                .getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null
                                            || encodingStyle.equals("")) {
                                        throw new Exception(
                                                mMessages
                                                .getString(
                                                "WCF_No_EncodingStyle_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                } else { // Provider
                                    SwiftMessage swiftMessage = swiftInput
                                            .getSwiftMessage();
                                    if (swiftMessage == null) {
                                        throw new Exception(mMessages
                                                .getString(
                                                "WCF_No_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                    if (swiftMessage.getUseType().equals(
                                            SwiftMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = swiftMessage
                                                .getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    
                                    if (encodingStyle == null
                                            || encodingStyle.equals("")) {
                                        throw new Exception(
                                                mMessages
                                                .getString(
                                                "WCF_No_EncodingStyle_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                }
                                
                                Encoder encoder = null;
                                MetaRef metaRef = new MyMetaRef(xsdFileLoc,
                                        type);
                                if (mEncoderMap.get(metaRef) != null) {
                                    encoder = (Encoder) mEncoderMap
                                            .get(metaRef);
                                } else {
                                    EncoderFactory encoderFactory = EncoderFactory
                                            .newInstance();
                                    encoder = encoderFactory.newEncoder(
                                            encoderFactory
                                            .makeType(encodingStyle),
                                            metaRef);
                                    mEncoderMap.put(metaRef, encoder);
                                }
                                partMapping.put(wsdlMessage.getQName()
                                        + partName, encoder);
                            }
                        }
                    }
                    
                    Output output = op.getOutput();
                    if (output != null) {
                        SwiftOutput swiftOutput = swiftOperation
                                .getSwiftOperationOutput();
                        if (swiftOutput == null) {
                            throw new Exception(mMessages.getString(
                                    "WCF_No_swiftOutput", operationName
                                    .toString()));
                        }
                        
                        wsdlMessage = output.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter
                                .hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName type = (aPart.getElementName() != null) ? aPart
                                    .getElementName()
                                    : aPart.getTypeName();
                            
                            // locate the XSD file based on the part type
                            // namespace
                            String namespace = type.getNamespaceURI();
                            String xsdFileLoc = getXsdFileLocation(namespace,
                                    type);
                            if (xsdFileLoc != null) {
                                if (direction == Endpoint.EndpointType.INBOUND
                                        && mep
                                        .equals(Endpoint.EndpointMessageType.IN_OUT)) {
                                    // Consumer request-response operations
                                    SwiftMessage swiftMessage = swiftOutput
                                            .getSwiftMessage();
                                    if (swiftOutput == null) {
                                        throw new Exception(mMessages
                                                .getString(
                                                "WCF_No_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                    
                                    if (swiftMessage.getUseType().equals(
                                            SwiftMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = swiftMessage
                                                .getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null
                                            || encodingStyle.equals("")) {
                                        throw new Exception(
                                                mMessages
                                                .getString(
                                                "WCF_No_EncodingStyle_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                } else { // Provider
                                    SwiftMessage swiftMessage = swiftOutput
                                            .getSwiftMessage();
                                    if (swiftMessage == null) {
                                        throw new Exception(mMessages
                                                .getString(
                                                "WCF_No_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                    
                                    if (swiftMessage.getUseType().equals(
                                            SwiftMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = swiftMessage
                                                .getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null
                                            || encodingStyle.equals("")) {
                                        throw new Exception(
                                                mMessages
                                                .getString(
                                                "WCF_No_EncodingStyle_swiftMessage",
                                                operationName
                                                .toString()));
                                    }
                                }
                                
                                Encoder encoder = null;
                                MetaRef metaRef = new MyMetaRef(xsdFileLoc,
                                        type);
                                if (mEncoderMap.get(metaRef) != null) {
                                    encoder = (Encoder) mEncoderMap
                                            .get(metaRef);
                                } else {
                                    EncoderFactory encoderFactory = EncoderFactory
                                            .newInstance();
                                    encoder = encoderFactory.newEncoder(
                                            encoderFactory
                                            .makeType(encodingStyle),
                                            metaRef);
                                    mEncoderMap.put(metaRef, encoder);
                                }
                                partMapping.put(wsdlMessage.getQName()
                                        + partName, encoder);
                            }
                        }
                        
                    }
                    break;
                }
            }
        }
        
        return partMapping;
    }
    
    public void clearEncoderCache() {
        mEncoderMap.clear();
    }
    
    private String getXsdFileLocation(String aNamespace) {
        String xsdFileLoc = null;
        File aXsdFile = null;
        
        try {
            for (int ii = 0; ii < mXsds.size(); ii++) {
                aXsdFile = (File) mXsds.get(ii);
                FastSchemaFactory factory = FastSchemaFactory.getInstance();
                FastSchema schema = factory.newFastSchema(aXsdFile
                        .getAbsolutePath());
                if (aNamespace.equals(schema.getTargetNamespace())) {
                    xsdFileLoc = aXsdFile.getAbsolutePath();
                    break;
                }
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, "WCF_cannot_parse_xsd", aXsdFile
                    .getName());
        }
        
        return xsdFileLoc;
    }
    
    private String getXsdFileLocation(String aNamespace, QName elementName) {
        String xsdFileLoc = null;
        File aXsdFile = null;
        try {
            for (int ii = 0; ii < mXsds.size(); ii++) {
                aXsdFile = (File) mXsds.get(ii);
                SchemaReader sr = new SchemaReader(aXsdFile);
                Schema schema = sr.read();
                if (aNamespace.equals(schema.getTargetNamespace())) {
                    ElementDecl element = schema.getElementDecl(elementName
                            .getLocalPart());
                    if (element != null) {
                        xsdFileLoc = aXsdFile.getAbsolutePath();
                        break;
                    }
                    continue;
                }
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, "WCF_cannot_parse_xsd", aXsdFile
                    .getName());
        }
        return xsdFileLoc;
    }
    
    /**
     * Given the list of xsds, return exact xsd file where inputted element is
     * present
     *
     * @param aNamespace
     *            TargetNamespace of xsd
     * @param elementName
     *            The element for which the schemas are scanned for existence
     * @param xsds
     *            List of xsds
     * @return String the schema location
     */
    public static String getXsdFileLocation(String aNamespace,
            QName elementName, List xsds) {
        String xsdFileLoc = null;
        File aXsdFile = null;
        try {
            for (int ii = 0; ii < xsds.size(); ii++) {
                aXsdFile = (File) xsds.get(ii);
                SchemaReader sr = new SchemaReader(aXsdFile);
                Schema schema = sr.read();
                if (aNamespace.equals(schema.getTargetNamespace())) {
                    ElementDecl element = schema.getElementDecl(elementName
                            .getLocalPart());
                    if (element != null) {
                        xsdFileLoc = aXsdFile.getAbsolutePath();
                        break;
                    }
                    continue;
                }
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, "WCF_cannot_parse_xsd", aXsdFile
                    .getName());
        }
        return xsdFileLoc;
    }
    
    /**
     * Determine the message exchange pattern. For handling 1.1 wsdls, map
     * transmission primitives to the closest message exchange pattern, taking
     * into account the endpoint direction direction inbound: request-response
     * and solicit-response -> in-out one-way and notification -> in-only
     * direction outbound: request-response and solicit-response -> out-in
     * one-way and notification -> out-only
     *
     * @param pm
     *            the endpoint configuration from the portmap
     * @param po
     *            the binding operation definition from the wsdl
     * @return the message exchange pattern, null if no mapping could be
     *         determined.
     */
    protected String determineMEP(int direction, BindingOperation bo) {
        String mep = null;
        OperationType type = bo.getOperation().getStyle();
        
        if (direction == Endpoint.EndpointType.INBOUND) {
            if (type.equals(OperationType.ONE_WAY)) {
                mep = Endpoint.EndpointMessageType.IN_ONLY;
            } else if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            } else {
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            }
        } else {
            if (type == null) {
                return null; // Not sure why type is not populated for some
                // outbound binding
                // operations.
            }
            if (type.equals(OperationType.ONE_WAY)) {
                mep = Endpoint.EndpointMessageType.OUT_ONLY;
            } else if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            }else {
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            }
        }
        return mep;
    }
    
    protected String determineMEP(int direction, OperationType type) {
        String mep = null;
        
        if (direction == Endpoint.EndpointType.INBOUND) {
            if (type.equals(OperationType.ONE_WAY)) {
                mep = Endpoint.EndpointMessageType.IN_ONLY;
            } else if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            } else {
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            }
        } else {
            if (type.equals(OperationType.ONE_WAY)) {
                mep = Endpoint.EndpointMessageType.OUT_ONLY;
            } else if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            } else {
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            }
        }
        return mep;
    }
    
    /**
     * An implementation of the MetaRef interface
     */
    private class MyMetaRef implements MetaRef {
        private final String mXsdPath;
        
        private final QName mRootElemName;
        
        private final String mToString;
        
        /**
         * Constructor
         */
        protected MyMetaRef(String xsdLoc) {
            this(xsdLoc, null);
        }
        
        /**
         * Alternative constructor that constructs a MetaRef object with the
         * file path location of the main XSD and qualified name of the root
         * element
         */
        protected MyMetaRef(String xsdLoc, QName rootElemName) {
            mXsdPath = xsdLoc;
            mRootElemName = rootElemName;
            mToString = toString();
        }
        
        /**
         * Return the file path location of the main XSD
         *
         * @return the path of the main meta file
         */
        public String getPath() {
            return mXsdPath;
        }
        
        public URL getURL() {
            return null;
        }
        
        /**
         * Return the QName of the root element.
         *
         * @return the QName of the root element
         */
        public QName getRootElemName() {
            return mRootElemName;
        }
        
        public String toString() {
            return mXsdPath + mRootElemName.toString();
        }
        
        public boolean equals(Object obj) {
            if (!(obj instanceof MyMetaRef)) {
                return false;
            }
            return mToString.equals(((MyMetaRef) obj).mToString);
        }
        
        public int hashCode() {
            return mToString.hashCode();
        }
    }
    
}
