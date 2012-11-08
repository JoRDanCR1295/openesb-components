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
package com.sun.jbi.ftpbc.packaging;

import com.sun.jbi.ftpbc.Endpoint;
import com.sun.jbi.ftpbc.EndpointImpl;
import com.sun.jbi.ftpbc.extensions.FTPAddress;
import com.sun.jbi.ftpbc.extensions.FTPBCExtPreprocessDeserializer;
import com.sun.jbi.ftpbc.extensions.FTPBinding;
import com.sun.jbi.ftpbc.extensions.FTPConstants;
import com.sun.jbi.ftpbc.extensions.FTPExtensionRegistry;
import com.sun.jbi.ftpbc.extensions.FTPInput;
import com.sun.jbi.ftpbc.extensions.FTPOperation;
import com.sun.jbi.ftpbc.extensions.FTPOutput;
import com.sun.jbi.ftpbc.extensions.FTPTransferExtension;

import com.sun.jbi.internationalization.Messages;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.encoder.tools.xml.XsdLocator;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;

import com.ibm.wsdl.factory.WSDLFactoryImpl;

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

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints
 * based on the portmap list.
 *
 *
 * @author jim.fu@sun.com
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

    public List parse(List portMaps, Map appVariableMap) throws Exception {
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
            endpoints.addAll(parseWSDL((File) wsdls.next(), resolver, portMaps, epResolved, appVariableMap));
        }

        if (!epResolved.containsAll(portMaps)) {
            // there is ep that is not found among the definitions from all the WSDLs
            Iterator it = portMaps.iterator();
            while (it.hasNext()) {
                EndpointData epData = (EndpointData) it.next();
                if (!epResolved.contains(epData)) {
                    // report this ep as unresolved
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W003002.WCF_UNRESOLVED_EP",
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

    public Map parseForApplicationVariables(List portMaps, Map appVariableMap) throws Exception {
        Map appVariables = (Map) ((HashMap) appVariableMap).clone();

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
            appVariables.putAll(readWSDLForAppVariables((File) wsdls.next(), resolver, appVariables));
        }

        return appVariables;
    }

    public List parseWSDL(File wsdlFile,
            EntityResolver resolver,
            List portMaps,
            List<EndpointData> epResolved,
            Map appVariableMap) throws Exception {
        Definition def = readWSDL(wsdlFile, resolver, appVariableMap);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData) it.next();

            // Check the Definition if it has a FTPBinding.  If not,
            // continue
            FTPBinding binding = getFTPBinding(def,
                    pm.getService(),
                    pm.getEndpoint());
            if (binding == null) {
                continue;
            }

            epResolved.add(pm);

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // extract file details

            // If we have a FTPBinding, we must have an FTPAddress.
            FTPAddress address =
                    getFTPAddress(def, pm.getService(), pm.getEndpoint());
            if (address == null) {
                throw new Exception(mMessages.getString("FTPBC-E003001.WCF_Missing_ftp_addr", pm.getService()));
            }

            // If we have an FTPBinding, we must have operations
            Map fileOps = getFTPOperations(def, pm.getService(), pm.getEndpoint());
            if (fileOps == null || fileOps.size() == 0) {
                throw new Exception(mMessages.getString("FTPBC-E003002.WCF_Missing_ftp_op", pm.getService()));
            }

            Map opMeps =
                    getFTPOperationMepType(def, pm.getService(), pm.getEndpoint(), pm.getDirection());

            // Create an Endpoint for each Port. The endpoint should have the
            // correct FTPBinding, the associated Operations, and the
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
            endpoint.setAddress(address);
            endpoint.setBinding(binding);
            endpoint.setOperations(fileOps);
            endpoint.setOperationMsgExchangePattern(opMeps);

            // Now add the Endpoint to our list of Endpoints
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

    protected Definition readWSDL(File f, EntityResolver resolver, Map appVariableMap) throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader =
                ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        if (mLogger.isLoggable(Level.FINE)) {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);
        } else {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
        }
        reader.setExtensionRegistry(new FTPExtensionRegistry(appVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    protected Map readWSDLForAppVariables(File f, EntityResolver resolver, Map appVariableMap)
            throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader =
                ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        if (mLogger.isLoggable(Level.FINE)) {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);
        } else {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
        }
        FTPBCExtPreprocessDeserializer preProcessDeserializer = new FTPBCExtPreprocessDeserializer(appVariableMap);
        reader.setExtensionRegistry(new FTPExtensionRegistry(preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getAppVariableMap();
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

    protected FTPAddress getFTPAddress(Definition def, String serviceName,
            String endpointName) {
        FTPAddress address = null;
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
                if (FTPAddress.class.isInstance(ee)) {
                    address = (FTPAddress) ee;
                }
            }
        }
        return address;
    }

    protected FTPBinding getFTPBinding(Definition def, String serviceName, String endpointName) {
        FTPBinding ftpBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && ftpBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (FTPBinding.class.isInstance(ee)) {
                    ftpBinding = (FTPBinding) ee;
                }
            }
        }
        return ftpBinding;
    }

    protected Map getFTPOperations(Definition def,
            String serviceName,
            String endpointName) {
        Map ftpOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();

                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement operExt = (ExtensibilityElement) extIter.next();
                    if (FTPOperation.class.isInstance(operExt)) {
                        FTPOperation ftpOperation = (FTPOperation) operExt;
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            ExtensibilityElement inputExt = null;
                            while (inputIter.hasNext()) {
                                inputExt = (ExtensibilityElement) inputIter.next();
                                FTPInput ftpInput = null;
                                if (inputExt instanceof FTPTransferExtension) {
                                    ftpInput = new FTPInput();
                                    ftpInput.setExtension((FTPTransferExtension) inputExt);
                                } else {
                                    // invalid child ext elements
                                }

                                if (ftpInput != null) {
                                    ftpOperation.setFTPOperationInput(ftpInput);
                                }
                            }
                        }

                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator outputIter = bindingOutput.getExtensibilityElements().iterator();
                            while (outputIter.hasNext()) {
                                ExtensibilityElement outputExt = (ExtensibilityElement) outputIter.next();
                                FTPOutput ftpOutput = null;
                                if (outputExt instanceof FTPTransferExtension) {
                                    ftpOutput = new FTPOutput();
                                    ftpOutput.setExtension((FTPTransferExtension) outputExt);
                                }

                                if (ftpOutput != null) {
                                    ftpOperation.setFTPOperationOutput(ftpOutput);
                                }
                            }
                        }
                        // use qualified name for ftp operation binding
                        //ftpOperations.put(QName.valueOf(oper.getName()), ftpOperation);
                        ftpOperations.put(new QName(binding.getQName().getNamespaceURI(), oper.getName()), ftpOperation);
                    }
                }
            }
        }
        return ftpOperations;
    }

    protected Map getFTPOperationMepType(Definition def,
            String serviceName,
            String endpointName,
            int direction) {
        Map mepTypes = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();

                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (FTPOperation.class.isInstance(ee)) {
                        String mep = Endpoint.EndpointMessageType.UNSUPPORTED;
                        if (oper.getOperation().getStyle() != null) {
                            mep = determineMEP(direction, oper);
                        } else {
                            // for some reason, sometimes the operation type is not set
                            // so the BC will populate it based on the WSDL definition
                            // for now, we only handle Request-response and one-way.
                            // anything else is not supported and should be caught in WSDL validation
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
                        // use qualified name for ftp operation binding
                        //mepTypes.put(QName.valueOf(oper.getName()), mep);
                        mepTypes.put(new QName(binding.getQName().getNamespaceURI(), oper.getName()), mep);
                    }
                }
            }
        }
        return mepTypes;
    }

    public Map getPartEncoderMapping(Definition def, String serviceName, String endpointName,
            int direction, Map ftpOperations, Map mepTypes) throws Exception {
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
        for (Iterator opnameIter = ftpOperations.keySet().iterator(); opnameIter.hasNext();) {
            String mep = null;
            String encodingStyle = null;

            QName operationName = (QName) opnameIter.next();
            FTPOperation ftpOperation = (FTPOperation) ftpOperations.get(operationName);
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
                    Output output = op.getOutput();
                    FTPInput ftpInput = null;

                    if (input != null) {
                        ftpInput = ftpOperation.getFTPOperationInput();
                        if (ftpInput == null && (output == null || output.getMessage() == null || !mep.equals(Endpoint.EndpointMessageType.OUT_IN))) {
                            throw new Exception(mMessages.getString("FTPBC-E003003.WCF_No_FtpInput", operationName.toString()));
                        }
                        if (ftpInput != null) {
                            wsdlMessage = input.getMessage();
                            parts = wsdlMessage.getParts();
                            for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                                Part aPart = (Part) partIter.next();
                                QName type = (aPart.getElementName() != null) ? aPart.getElementName() : aPart.getTypeName();
                                QName elem = (aPart.getElementName() != null) ? aPart.getElementName() : null;
                                String partName = aPart.getName();

                                // locate the XSD file based on the part type namespace
                                //String namespace = type.getNamespaceURI();
                                String xsdFileLoc = getXsdFileLocation(elem);
                                if (xsdFileLoc != null) {
                                    // Determine on which extensibility element the encoding style is defined
                                    FTPTransferExtension extElem = ftpInput.getExtension();
                                    // direction == INBOUND ===> consumer, otherwise, provider
                                    if (extElem == null) {
                                        throw new Exception(mMessages.getString("FTPBC-E003004.WCF_No_FTPTranserExtension", operationName.toString()));
                                    }
                                    if (!(extElem instanceof FTPTransferExtension)) {
                                        // report invalid
                                    }
                                    if (((FTPTransferExtension) extElem).getUse().equals(FTPConstants.EXT_ELEM_ATTR_USE_ENCODED)) {
                                        encodingStyle = ((FTPTransferExtension) extElem).getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString("FTPBC-E003005.WCF_No_EncodingStyle_FTPTranserExtension", operationName.toString()));
                                    }

                                    Encoder encoder = null;
                                    MetaRef metaRef = new MyMetaRef(xsdFileLoc, type);
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

                    if (output != null) {
                        FTPOutput ftpOutput = ftpOperation.getFTPOperationOutput();
                        if (ftpOutput == null) {
                            throw new Exception(mMessages.getString("FTPBC-E003006.WCF_No_FtpOutput", operationName.toString()));
                        }

                        wsdlMessage = output.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName type = (aPart.getElementName() != null) ? aPart.getElementName() : aPart.getTypeName();
                            QName elem = (aPart.getElementName() != null) ? aPart.getElementName() : null;

                            // locate the XSD file based on the part type namespace
                            //String namespace = type.getNamespaceURI();
                            //String xsdFileLoc = getXsdFileLocation(namespace);
                            String xsdFileLoc = getXsdFileLocation(elem);
                            if (xsdFileLoc != null) {
                                // OpenESB issue #
                                // the checking has to be modified to accomodate
                                // on-demand operation where
                                // input leg is empty
                                // and output leg actually hold info
                                // for doing a solicit GET
                                //
                                if ((direction == Endpoint.EndpointType.INBOUND &&
                                        mep.equals(Endpoint.EndpointMessageType.IN_OUT)) ||
                                        (direction == Endpoint.EndpointType.OUTBOUND &&
                                        mep.equals(Endpoint.EndpointMessageType.OUT_IN) /*&& ftpInput == null*/)) {
                                    // Consumer request-response operations
                                    FTPTransferExtension extElem = ftpOutput.getExtension();
                                    if (extElem == null) {
                                        throw new Exception(mMessages.getString("FTPBC-E003004.WCF_No_FTPTranserExtension", operationName.toString()));
                                    }

                                    if (!(extElem instanceof FTPTransferExtension)) {
                                        // report invalid
                                    }

                                    if (((FTPTransferExtension) extElem).getUse().equals(FTPConstants.EXT_ELEM_ATTR_USE_ENCODED)) {
                                        encodingStyle = ((FTPTransferExtension) extElem).getEncodingStyle(); // need encoding style attr
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString("FTPBC-E003005.WCF_No_EncodingStyle_FTPTranserExtension", operationName.toString()));
                                    }

                                    Encoder encoder = null;
                                    MetaRef metaRef = new MyMetaRef(xsdFileLoc, type);
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
     * @param direction the direction of the message exchange
     * @param bo the binding operation definition from the wsdl
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
                return null;   // Not sure why type is not populated for some outbound binding operations.
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
        File aXsdFile = null;

        if (elemName != null) {
            try {
                aXsdFile = XsdLocator.findXsdByElement(mXsds, elemName);
            } catch (Exception e) {
                mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E001011.WCF_cannot_parse_xsd", aXsdFile.getName()));
            }
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
