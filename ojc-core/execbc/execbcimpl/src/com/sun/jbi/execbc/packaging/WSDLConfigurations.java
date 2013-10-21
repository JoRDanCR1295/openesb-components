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

package com.sun.jbi.execbc.packaging;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.packaging.EndpointData;
import com.sun.jbi.execbc.EndpointImpl;
import com.sun.jbi.execbc.validator.EndpointValidator;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecBinding;
import com.sun.jbi.execbc.extensions.ExecExtPreprocessDeserializer;
import com.sun.jbi.execbc.extensions.ExecExtensionRegistry;
import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.extensions.ExecOutput;
import com.sun.jbi.execbc.packaging.WSDLConfigurations;
import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl4j.ext.WSDL4JExt;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.w3c.dom.Document;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints
 * based on the portmap list.
 *
 * @author Sherry Weng
 */
public class WSDLConfigurations {
    private static final Messages mMessages =
            Messages.getMessages(WSDLConfigurations.class);
    private static Logger mLogger = Messages.getLogger(WSDLConfigurations.class);
    
    private String mRootPath;
    private List mXsds = new ArrayList();
    private Map mEncoderMap = new HashMap();
    
    public WSDLConfigurations(String rootPath) {
        mRootPath = rootPath;
    }
    
    public List parse(List portMaps, Map envVariableMap) throws Exception {
        Map envVariables = (Map)((HashMap)envVariableMap).clone();

        File dir = new File(mRootPath);
        
        File catalog = new File(dir.getAbsolutePath() +
            File.separator + "META-INF" + File.separator +
            "catalog.xml");
        
        EntityResolver resolver = null;

        if ( catalog.exists() ) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            resolver = new CatalogResolver(catalogManager);
        }
        
        
        ArrayList endpoints = new ArrayList();
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        mXsds = listResourceFiles(dir, ".xsd");
        while (wsdls.hasNext()) {
            endpoints.addAll(parseWSDL((File)wsdls.next(), resolver, portMaps, envVariables));
        }
        
        return endpoints;
    }
    
    public Map parseForEnvironmentVariables(List portMaps, Map envVariableMap) throws Exception {
        Map envVariables = (Map)((HashMap)envVariableMap).clone();
        File dir = new File(mRootPath);

        File catalog = new File(dir.getAbsolutePath() +
            File.separator + "META-INF" + File.separator +
            "catalog.xml");

        EntityResolver resolver = null;
        
        if ( catalog.exists() ) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);        
            resolver = new CatalogResolver(catalogManager);
        }
        
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();

        while (wsdls.hasNext()) {
            envVariables.putAll(readWSDLForEnvVariables((File)wsdls.next(), resolver, envVariables));
        }
        
        return envVariables;
    }
    
    public List parseWSDL(File wsdlFile,
            EntityResolver resolver,
            List portMaps,
            Map envVariableMap) throws Exception {
        Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData)it.next();
            
            // Check the Definition if it has a ExecBinding.  If not,
            // continue
            ExecBinding binding = getExecBinding(def,
                    pm.getService(),
                    pm.getEndpoint());
            if (binding == null) {
                continue;
            }
            
            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // get file details
            
            // If we have a ExecBinding, we must have an ExecAddress.
            ExecAddress address =
                    getExecAddress(def, pm.getService(), pm.getEndpoint());
            if (address == null) {
                String msg = mMessages.getString(
                        "WCF_Missing_exec_addr") +
                        pm.getService();
                throw new Exception(msg);
            }
            
            // If we have an ExecBinding, we must have operations
            Map execOps = getExecOperations(def, pm.getService(), pm.getEndpoint());
            if (execOps == null || execOps.size() == 0) {
                String msg =
                        mMessages.getString("WCF_Missing_exec_op") +
                        pm.getService();
                throw new Exception(msg);
            }
            
            Map opMeps =
                    getExecOperationMepType(def, pm.getService(), pm.getEndpoint(), pm.getDirection());
            
            // Create an Endpoint for each Port. The endpoint should have the
            // correct ExecBinding, the associated Operations, and the
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
            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder documentBuilder = docBuilderFactory.newDocumentBuilder();
            Document result = documentBuilder.parse(wsdlFile);
            endpoint.setServiceDescription(result);
            
            // Store our extensibility elements
            endpoint.setExecAddress(address);
            endpoint.setExecBinding(binding);
            endpoint.setExecOperations(execOps);
            endpoint.setOperationMsgExchangePattern(opMeps);
            
            EndpointValidator.validateEndpointForUniqueness(endPoints, endpoint, false);
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
        for (int fileCount = 0; fileCount < filesInCurrentDir.length;
        fileCount++) {
            
            if (filesInCurrentDir[fileCount].isFile()) {
                if (filesInCurrentDir[fileCount].getName().toLowerCase().endsWith(extension)) {
                    cumulativeResults.add(filesInCurrentDir[fileCount]);
                }
            } else if (filesInCurrentDir[fileCount].isDirectory()) {
                List wsdlsInSubDirectories =
                        listResourceFiles(filesInCurrentDir[fileCount], extension);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }
    
    protected Definition readWSDL(File f, EntityResolver resolver, Map envVariableMap) throws WSDLException {
        WSDLReader reader = WSDL4JExt.newWSDLReader(resolver);
        reader.setExtensionRegistry(new ExecExtensionRegistry(envVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
    
    protected Map readWSDLForEnvVariables(File f, EntityResolver resolver, Map envVariableMap)
            throws WSDLException {
        WSDLReader reader = WSDL4JExt.newWSDLReader(resolver);
        ExecExtPreprocessDeserializer preProcessDeserializer = new ExecExtPreprocessDeserializer(envVariableMap);
        reader.setExtensionRegistry(new ExecExtensionRegistry(preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getEnvVariableMap();
    }
    
    
    protected Binding getBinding(Definition def, String serviceName,
            String endpointName) {
        // DO NOT use the getService() method.
        // It checks all imported WSDLs.
        Map services = def.getServices();
        Service svc = (Service)services.get(QName.valueOf(serviceName));
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
    
    protected ExecAddress getExecAddress(Definition def, String serviceName,
            String endpointName) {
        ExecAddress address = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();
            
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (ExecAddress.class.isInstance(ee)) {
                    address = (ExecAddress) ee;
                }
            }
        }
        return address;
    }
    
    protected ExecBinding getExecBinding(Definition def, String serviceName, String endpointName) {
        ExecBinding execBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
            
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && execBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (ExecBinding.class.isInstance(ee)) {
                    execBinding = (ExecBinding) ee;
                }
            }
        }
        return execBinding;
    }
    
    protected Map getExecOperations(Definition def,
            String serviceName,
            String endpointName) {
        Map execOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            final PortType portType = binding.getPortType();
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement operExt = (ExtensibilityElement) extIter.next();
                    if (ExecOperation.class.isInstance(operExt)) {
                        ExecOperation execOperation = (ExecOperation)operExt;
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement)inputIter.next();
                                ExecInput execInput = null;
                                if (inputExt instanceof ExecMessage) {
                                    execInput = new ExecInput();
                                    ExecMessage execMessage = (ExecMessage) inputExt;
                                    execInput.setExecMessage(execMessage);
                                }
                                if (execInput != null) {
                                    execOperation.setExecOperationInput(execInput);
                                }
                            }
                        }
                        
                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator outputIter = bindingOutput.getExtensibilityElements().iterator();
                            while (outputIter.hasNext()) {
                                ExtensibilityElement outputExt = (ExtensibilityElement)outputIter.next();
                                ExecOutput execOutput = null;
                                if (outputExt instanceof ExecMessage) {
                                    execOutput = new ExecOutput();
                                    ExecMessage execMessage = (ExecMessage) outputExt;
                                    execOutput.setExecMessage(execMessage);
                                }
                                if (execOutput != null) {
                                    execOperation.setExecOperationOutput(execOutput);
                                }
                            }
                        }
                        execOperations.put(new QName(portType.getQName().getNamespaceURI(), oper.getName()), execOperation);
                    }
                }
            }
        }
        return execOperations;
    }
    
    protected Map getExecOperationMepType(Definition def,
            String serviceName,
            String endpointName,
            int direction) {
        Map mepTypes = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            final PortType portType = binding.getPortType();
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (ExecOperation.class.isInstance(ee)) {
                        String mep = Endpoint.EndpointMessageType.UNSUPPORTED;
                        if (oper.getOperation().getStyle() != null) {
                            mep = determineMEP(direction, oper);
                        } else {
                            // for some reason, sometimes the operation type is not set
                            // so the BC will populate it based on the WSDL definition
                            // for now, we only handle Request-response and one-way.
                            // anything else is not supported and will be caught in WSDL validation
                            Operation operation = oper.getOperation();
                            OperationType type = null;
                            if (operation.getInput() != null && operation.getOutput() != null) {
                                type = OperationType.REQUEST_RESPONSE;
                            } else if (operation.getInput() != null) {
                                type = OperationType.ONE_WAY;
                            }
                            if (type != null) {
                                mep = determineMEP(direction, type);
                            }
                        }
                        mepTypes.put(new QName(portType.getQName().getNamespaceURI(), oper.getName()), mep);
                    }
                }
            }
        }
        return mepTypes;
    }
    
    public Map getPartEncoderMapping(Definition def, String serviceName, String endpointName,
            int direction, Map execOperations, Map mepTypes) throws Exception {
        Map partMapping = new HashMap();
        
        // Don't support inline encoder schemas
        if (mXsds.size() <= 0) {
            return partMapping;
        }
        
        Service service  = def.getService(QName.valueOf(serviceName));
        Port port = service.getPort(endpointName);
        PortType portType = port.getBinding().getPortType();
        
        /**
         * Locate the operation we are interested in.
         * There may be multiple operations by the same name (operation overloading)
         * and the WSDL spec does allow it. The uniqueness
         * should be guaranteed by the examination of input and/or output names.
         * For the time being, we will assume that we don't have operation overloading.
         */
        javax.wsdl.Message wsdlMessage = null;
        Map parts = new HashMap();
        for (Iterator opnameIter = execOperations.keySet().iterator(); opnameIter.hasNext(); ) {
            String mep = null;
            String encodingStyle = null;
            
            QName operationName = (QName)opnameIter.next();
            ExecOperation execOperation = (ExecOperation)execOperations.get(operationName);
            mep = (String) mepTypes.get(operationName);
            
            for (Iterator operIter = portType.getOperations().iterator(); operIter.hasNext();) {
                Operation op = (Operation)operIter.next();
                if (op.getName().equals(operationName.toString()) ||
                        op.getName().equals(operationName.getLocalPart())) {
                    /**
                     * There is nothing in the WSDL spec that says
                     * that the part name has to be unique within the
                     * WSDL document, so we need to prefix the part name
                     * with the message name.
                     */
                    
                    Input input = op.getInput();
                    if (input != null) {
                        ExecInput execInput = execOperation.getExecOperationInput();
                        if (execInput == null) {
                            throw new Exception(mMessages.getString("WCF_No_ExecInput", operationName.toString()));
                        }
                        wsdlMessage = input.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName type = (aPart.getElementName() != null)? aPart.getElementName():aPart.getTypeName();
                            
                            // locate the XSD file based on the part type namespace
                            String namespace = type.getNamespaceURI();
                            String xsdFileLoc = getXsdFileLocation(namespace);
                            if (xsdFileLoc != null) {
                                ExecMessage execMessage = execInput.getExecMessage();
                                if (execMessage == null) {
                                    throw new Exception(mMessages.getString("WCF_No_ExecMessage", operationName.toString()));
                                }
                                if (execMessage.getExecUseType().equals(ExecMessage.EXEC_USE_TYPE_ENCODED)) {
                                    encodingStyle = execMessage.getExecEncodingStyle();
                                } else {
                                    continue;
                                }
                                
                                if (encodingStyle == null || encodingStyle.equals("")) {
                                    throw new Exception(mMessages.getString("WCF_No_EncodingStyle_ExecMessage", operationName.toString()));
                                }
                                
                                Encoder encoder = null;
                                MetaRef metaRef = new MyMetaRef(xsdFileLoc, type);
                                if (mEncoderMap.get(metaRef) != null) {
                                    encoder = (Encoder)mEncoderMap.get(metaRef);
                                } else {
                                    EncoderFactory encoderFactory = EncoderFactory.newInstance();
                                    encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle),
                                            metaRef);
                                    mEncoderMap.put(metaRef, encoder);
                                }
                                partMapping.put(wsdlMessage.getQName() + partName, encoder);
                            }
                        }
                    }
                    
                    Output output = op.getOutput();
                    if (output != null) {
                        ExecOutput execOutput = execOperation.getExecOperationOutput();
                        if (execOutput == null) {
                            throw new Exception(mMessages.getString("WCF_No_ExecOutput", operationName.toString()));
                        }
                        
                        wsdlMessage = output.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName type = (aPart.getElementName() != null)? aPart.getElementName():aPart.getTypeName();
                            
                            // locate the XSD file based on the part type namespace
                            String namespace = type.getNamespaceURI();
                            String xsdFileLoc = getXsdFileLocation(namespace);
                            if (xsdFileLoc != null) {
                                if (direction == Endpoint.EndpointType.OUTBOUND &&
                                        mep.equals(Endpoint.EndpointMessageType.OUT_IN)) {
                                    // Provider solicit-response operations
                                    ExecMessage execMessage = execOutput.getExecMessage();
                                    if (execMessage == null) {
                                        throw new Exception(mMessages.getString("WCF_No_ExecMessage", operationName.toString()));
                                    }
                                    
                                    if (execMessage.getExecUseType().equals(ExecMessage.EXEC_USE_TYPE_ENCODED)) {
                                        encodingStyle = execMessage.getExecEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString("WCF_No_EncodingStyle_ExecMessage", operationName.toString()));
                                    }
                                    
                                    Encoder encoder = null;
                                    MetaRef metaRef = new MyMetaRef(xsdFileLoc, type);
                                    if (mEncoderMap.get(metaRef) != null) {
                                        encoder = (Encoder)mEncoderMap.get(metaRef);
                                    } else {
                                        EncoderFactory encoderFactory = EncoderFactory.newInstance();
                                        encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle),
                                                metaRef);
                                        mEncoderMap.put(metaRef, encoder);
                                    }
                                    partMapping.put(wsdlMessage.getQName() + partName, encoder);
                                }
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
    
    /**
     * Determine the message exchange pattern.
     * For handling 1.1 wsdls, map transmission primitives to the
     * closest message exchange pattern, taking into account the endpoint direction
     * direction inbound:
     *      request-response and solicit-response -> in-out
     *      one-way and notification -> in-only
     * direction outbound:
     *      request-response and solicit-response -> out-in
     *      one-way and notification -> out-only
     * @param pm the endpoint configuration from the portmap
     * @param po the binding operation definition from the wsdl
     * @return the message exchange pattern, null if no mapping could be determined.
     */
    protected String determineMEP(int direction, BindingOperation bo) {
        String mep = null;
        OperationType type = bo.getOperation().getStyle();
        
        if (direction == Endpoint.EndpointType.INBOUND) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                //IN_OUT is not supported.
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            } else if (type.equals(OperationType.ONE_WAY)) {
                mep = Endpoint.EndpointMessageType.IN_ONLY;
            } else {
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            }
        } else {
            if (type == null) {
                // Not sure why type is not always populated for outbound binding operations.
                // Returning UNSUPPORTED
                return Endpoint.EndpointMessageType.UNSUPPORTED;
            }
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.OUT_IN;
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = Endpoint.EndpointMessageType.OUT_ONLY;
            }
        }
        return mep;
    }
    
    protected String determineMEP(int direction, OperationType type) {
        String mep = null;
        
        if (direction == Endpoint.EndpointType.INBOUND) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                //IN_OUT is not supported.
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            } else if (type.equals(OperationType.ONE_WAY)) {
                mep = Endpoint.EndpointMessageType.IN_ONLY;
            } else {
                mep = Endpoint.EndpointMessageType.UNSUPPORTED;
            }
        } else {
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.OUT_IN;
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = Endpoint.EndpointMessageType.OUT_ONLY;
            }
        }
        return mep;
    }
    
    protected String getXsdFileLocation(String aNamespace) {
        String xsdFileLoc = null;
        File aXsdFile = null;
        
        try {
            for (int ii = 0; ii < mXsds.size(); ii++) {
                aXsdFile = (File)mXsds.get(ii);
                SchemaDocument schemaDoc = SchemaDocument.Factory.parse(aXsdFile);
                if (aNamespace.equals(schemaDoc.getSchema().getTargetNamespace())) {
                    xsdFileLoc = aXsdFile.getAbsolutePath();
                    break;
                }
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, "WCF_cannot_parse_xsd", aXsdFile.getName());
        }
        
        return xsdFileLoc;
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
         * Alternative constructor that constructs a MetaRef object
         * with the file path location of the main XSD and
         * qualified name of the root element
         */
        protected MyMetaRef(String xsdLoc, QName rootElemName) {
            mXsdPath = xsdLoc;
            mRootElemName = rootElemName;
            mToString = toString();
        }
        
        /**
         * Return the file path location of the main XSD
         * @return    the path of the main meta file
         */
        public String getPath() {
            return mXsdPath;
        }
        
        /**
         * Return the QName of the root element.
         * @return the QName of the root element
         */
        public QName getRootElemName() {
            return mRootElemName;
        }
        
        /**
         * Gets the URL of the main metadata file.  This URL should point to an
         * XSD file somewhere.  If this method returns a value other than
         * <code>null</code>, the return value of <code>getPath()</code> will
         * be ignored.  To load encoder metadata from a jar file, a URL in form
         * "jar:&lt;url&gt;!/{entry}" can be used.
         *
         * @return the URL of the main meta file
         */
        public URL getURL() {
            return null;
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
