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
package com.sun.jbi.filebc.packaging;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.encoder.tools.xml.XsdLocator;
import com.sun.jbi.filebc.Endpoint;
import com.sun.jbi.filebc.EndpointImpl;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileBinding;
import com.sun.jbi.filebc.extensions.FileExtensionRegistry;
import com.sun.jbi.filebc.extensions.FileExtPreprocessDeserializer;
import com.sun.jbi.filebc.extensions.FileInput;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.extensions.FileOutput;
import com.sun.jbi.filebc.validator.EndpointValidator;
import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import java.util.Vector;
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
        Map envVariables = (Map) ((HashMap) envVariableMap).clone();

        File dir = new File(mRootPath);

        File catalog = new File(dir.getAbsolutePath() +
                File.separator + "meta-inf" + File.separator +
                "catalog.xml");

        EntityResolver resolver = null;

        if (catalog.exists()) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);
            resolver = new CatalogResolver(catalogManager);
        }


        ArrayList endpoints = new ArrayList();
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        mXsds = listResourceFiles(dir, ".xsd");
        Vector<EndpointData> epResolved = new Vector<EndpointData>();
        while (wsdls.hasNext()) {
            endpoints.addAll(parseWSDL((File) wsdls.next(), resolver, portMaps, epResolved, envVariables));
        }
        if (!epResolved.containsAll(portMaps)) {
            // there is ep that is not found among the definitions from all the WSDLs
            Iterator it = portMaps.iterator();
            while (it.hasNext()) {
                EndpointData epData = (EndpointData) it.next();
                if (!epResolved.contains(epData)) {
                    // report this ep as unresolved
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("FILEBC-W00801.WCF_UNRESOLVED_EP",
                                new Object[]{
                                    epData.getEndpoint(),
                                    epData.getService(),
                                    epData.getInterface(),
                                    epData.getDirection()
                                }));

                    }
                }
            }
        }
        return endpoints;
    }

    public Map parseForEnvironmentVariables(List portMaps, Map envVariableMap) throws Exception {
        Map envVariables = (Map) ((HashMap) envVariableMap).clone();
        File dir = new File(mRootPath);

        File catalog = new File(dir.getAbsolutePath() +
                File.separator + "meta-inf" + File.separator +
                "catalog.xml");

        EntityResolver resolver = null;

        if (catalog.exists()) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);
            resolver = new CatalogResolver(catalogManager);
        }

        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();

        while (wsdls.hasNext()) {
            envVariables.putAll(readWSDLForEnvVariables((File) wsdls.next(), resolver, envVariables));
        }

        return envVariables;
    }

    public List parseWSDL(File wsdlFile,
            EntityResolver resolver,
            List portMaps,
            List<EndpointData> epResolved,
            Map envVariableMap) throws Exception {
        Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData) it.next();

            // Check the Definition if it has a FileBinding.  If not,
            // continue
            FileBinding binding = getFileBinding(def,
                    pm.getService(),
                    pm.getEndpoint());

            if (binding == null) {
                continue;
            }

            epResolved.add(pm);

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // get file details

            // If we have a FileBinding, we must have an FileAddress.
            FileAddress address =
                    getFileAddress(def, pm.getService(), pm.getEndpoint());
            if (address == null) {
                String msg = mMessages.getString(
                        "FILEBC-E00801.WCF_Missing_file_addr") +
                        pm.getService();
                throw new Exception(msg);
            }

            // If we have an FileBinding, we must have operations
            Map fileOps = getFileOperations(def, pm.getService(), pm.getEndpoint());
            if (fileOps == null || fileOps.size() == 0) {
                String msg =
                        mMessages.getString("FILEBC-E00802.WCF_Missing_file_op") +
                        pm.getService();
                throw new Exception(msg);
            }

            Map opMeps =
                    getFileOperationMepType(def, pm.getService(), pm.getEndpoint(), pm.getDirection());

            // Create an Endpoint for each Port. The endpoint should have the
            // correct FileBinding, the associated Operations, and the
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
            endpoint.setFileAddress(address);
            endpoint.setFileBinding(binding);
            endpoint.setFileOperations(fileOps);
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
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        // Suppress WSDL4j System.out logs and control the logging based on the current
        // logger logging level setting
        if (mLogger.isLoggable(Level.FINE)) {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);
        } else {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
        }
        reader.setExtensionRegistry(new FileExtensionRegistry(envVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    protected Map readWSDLForEnvVariables(File f, EntityResolver resolver, Map envVariableMap)
            throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        // Suppress WSDL4j System.out logs and control the logging based on the current
        // logger logging level setting
        if (mLogger.isLoggable(Level.FINE)) {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);
        } else {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
        }
        FileExtPreprocessDeserializer preProcessDeserializer = new FileExtPreprocessDeserializer(envVariableMap);
        reader.setExtensionRegistry(new FileExtensionRegistry(preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getEnvVariableMap();
    }

    protected Binding getBinding(Definition def, String serviceName,
            String endpointName) {
        // DO NOT use the getService() method.
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

    protected FileAddress getFileAddress(Definition def, String serviceName,
            String endpointName) {
        FileAddress address = null;
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
                if (FileAddress.class.isInstance(ee)) {
                    address = (FileAddress) ee;
                }
            }
        }
        return address;
    }

    protected FileBinding getFileBinding(Definition def, String serviceName, String endpointName) {
        FileBinding fileBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && fileBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (FileBinding.class.isInstance(ee)) {
                    fileBinding = (FileBinding) ee;
                }
            }
        }
        return fileBinding;
    }

    protected Map getFileOperations(Definition def,
            String serviceName,
            String endpointName) {
        Map fileOperations = new HashMap();
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
                    if (FileOperation.class.isInstance(operExt)) {
                        FileOperation fileOperation = (FileOperation) operExt;
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                FileInput fileInput = null;
                                if (inputExt instanceof FileMessage) {
                                    fileInput = new FileInput();
                                    FileMessage fileMessage = (FileMessage) inputExt;
                                    fileInput.setFileMessage(fileMessage);
                                }
                                if (fileInput != null) {
                                    fileOperation.setFileOperationInput(fileInput);
                                }
                            }
                        }

                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator outputIter = bindingOutput.getExtensibilityElements().iterator();
                            while (outputIter.hasNext()) {
                                ExtensibilityElement outputExt = (ExtensibilityElement) outputIter.next();
                                FileOutput fileOutput = null;
                                if (outputExt instanceof FileMessage) {
                                    fileOutput = new FileOutput();
                                    FileMessage fileMessage = (FileMessage) outputExt;
                                    fileOutput.setFileMessage(fileMessage);
                                }
                                if (fileOutput != null) {
                                    fileOperation.setFileOperationOutput(fileOutput);
                                }
                            }
                        }
                        fileOperations.put(new QName(portType.getQName().getNamespaceURI(), oper.getName()), fileOperation);
                    }
                }
            }
        }
        return fileOperations;
    }

    protected Map getFileOperationMepType(Definition def,
            String serviceName,
            String endpointName,
            int direction) throws Exception {
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
                    if (FileOperation.class.isInstance(ee)) {
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

                        String verb = ((FileOperation) ee).getVerb();
                        if (Endpoint.EndpointMessageType.OUT_IN.equals(mep) &&
                                (!FileOperation.VERB_READ.equals(verb))) {
                            // For a outbound WSDL request-response operation type is supported for
                            // SyncRead (aka Solicited Read operation) only.
                            String serviceEPOper = serviceName + ":" + endpointName;
                            String msg = mMessages.getString("FILEBC-E00808.WCF_Outbound_REQ_RES", new Object[]{serviceEPOper, oper.getName()});
                            throw new Exception(msg);
                        }

                        mepTypes.put(new QName(portType.getQName().getNamespaceURI(), oper.getName()), mep);
                    }
                }
            }
        }
        return mepTypes;
    }

    public Map getPartEncoderMapping(Definition def, String serviceName, String endpointName,
            int direction, Map fileOperations, Map mepTypes) throws Exception {
        Map partMapping = new HashMap();

        // Don't support inline encoder schemas
        if (mXsds.size() <= 0) {
            return partMapping;
        }

        Service service = def.getService(QName.valueOf(serviceName));
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
        for (Iterator opnameIter = fileOperations.keySet().iterator(); opnameIter.hasNext();) {
            String mep = null;
            String encodingStyle = null;

            QName operationName = (QName) opnameIter.next();
            FileOperation fileOperation = (FileOperation) fileOperations.get(operationName);
            mep = (String) mepTypes.get(operationName);

            for (Iterator operIter = portType.getOperations().iterator(); operIter.hasNext();) {
                Operation op = (Operation) operIter.next();
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
                        FileInput fileInput = fileOperation.getFileOperationInput();
                        // for solicited read, there is no file extension on the Input message
                        if (fileInput == null && !fileOperation.getVerb().equals(FileOperation.VERB_READ)) {
                            throw new Exception(mMessages.getString("FILEBC-E00803.WCF_No_FileInput", operationName.toString()));
                        }
                        wsdlMessage = input.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName elem = (aPart.getElementName() != null) ? aPart.getElementName() : null;

                            // locate the XSD file based on the part type namespace
                            String xsdFileLoc = getXsdFileLocation(elem);
                            if (xsdFileLoc != null) {
                                // Determine on which extensibility element the encoding style is defined
                                if (direction == Endpoint.EndpointType.INBOUND) {  // Consumer
                                    FileMessage fileMessage = fileInput.getFileMessage();
                                    if (fileMessage == null) {
                                        throw new Exception(mMessages.getString("FILEBC-E00804.WCF_No_FileMessage", operationName.toString()));
                                    }
                                    if (fileMessage.getFileUseType().equals(FileMessage.FILE_USE_TYPE_ENCODED)) {
                                        encodingStyle = fileMessage.getFileEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString("FILEBC-E00805.WCF_No_EncodingStyle_FileMessage", operationName.toString()));
                                    }
                                } else {					   // Provider
                                    FileMessage fileMessage = fileInput.getFileMessage();
                                    if (fileMessage == null) {
                                        throw new Exception(mMessages.getString("FILEBC-E00804.WCF_No_FileMessage", operationName.toString()));
                                    }
                                    if (fileMessage.getFileUseType().equals(FileMessage.FILE_USE_TYPE_ENCODED)) {
                                        encodingStyle = fileMessage.getFileEncodingStyle();
                                    } else {
                                        continue;
                                    }

                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString("FILEBC-E00805.WCF_No_EncodingStyle_FileMessage", operationName.toString()));
                                    }
                                }

                                Encoder encoder = null;
                                MetaRef metaRef = new MyMetaRef(xsdFileLoc, elem);
                                if (mEncoderMap.get(metaRef) != null) {
                                    encoder = (Encoder) mEncoderMap.get(metaRef);
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
                        FileOutput fileOutput = fileOperation.getFileOperationOutput();
                        if (fileOutput == null) {
                            throw new Exception(mMessages.getString("FILEBC-E00806.WCF_No_FileOutput", operationName.toString()));
                        }

                        wsdlMessage = output.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName elem = (aPart.getElementName() != null) ? aPart.getElementName() : null;

                            // locate the XSD file based on the part type namespace
                            String xsdFileLoc = getXsdFileLocation(elem);
                            if (xsdFileLoc != null) {
                                if ((direction == Endpoint.EndpointType.INBOUND &&
                                        mep.equals(Endpoint.EndpointMessageType.IN_OUT)) ||
                                        (direction == Endpoint.EndpointType.OUTBOUND &&
                                        mep.equals(Endpoint.EndpointMessageType.OUT_IN))) {
                                    // Consumer request-response operations
                                    FileMessage fileMessage = fileOutput.getFileMessage();
                                    if (fileMessage == null) {
                                        throw new Exception(mMessages.getString("FILEBC-E00804.WCF_No_FileMessage", operationName.toString()));
                                    }

                                    if (fileMessage.getFileUseType().equals(FileMessage.FILE_USE_TYPE_ENCODED)) {
                                        encodingStyle = fileMessage.getFileEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString("FILEBC-E00805.WCF_No_EncodingStyle_FileMessage", operationName.toString()));
                                    }

                                    Encoder encoder = null;
                                    MetaRef metaRef = new MyMetaRef(xsdFileLoc, elem);
                                    if (mEncoderMap.get(metaRef) != null) {
                                        encoder = (Encoder) mEncoderMap.get(metaRef);
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
                mep = Endpoint.EndpointMessageType.IN_OUT;
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
                mep = Endpoint.EndpointMessageType.IN_OUT;
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

    protected String getXsdFileLocation(QName elemName) {
        if (elemName == null) {
            return null;
        }

        File aXsdFile = null;
        try {
            aXsdFile = XsdLocator.findXsdByElement(mXsds, elemName);
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,
                    mMessages.getString("FILEBC-E00807.WCF_Error_getting_xsd",
                    new Object[]{aXsdFile.getName(), e.getLocalizedMessage()}),
                    e);
        }
        return aXsdFile != null ? aXsdFile.getAbsolutePath() : null;
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
