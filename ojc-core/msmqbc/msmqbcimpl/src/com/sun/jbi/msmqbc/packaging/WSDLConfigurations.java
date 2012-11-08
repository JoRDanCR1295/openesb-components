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

package com.sun.jbi.msmqbc.packaging;

import java.io.File;

import java.net.URL;

import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Collection;

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.wsdl.Port;
import javax.wsdl.Part;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.Binding;
import javax.wsdl.Operation;
import javax.wsdl.Definition;
import javax.wsdl.OperationType;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.BindingOperation;
import javax.wsdl.WSDLException;

import javax.wsdl.Service;
import javax.wsdl.PortType;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.EntityResolver;

import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;

import org.w3c.dom.Document;

import com.sun.jbi.internationalization.Messages;

import com.ibm.wsdl.factory.WSDLFactoryImpl;

import com.sun.xsd.model.FastSchemaFactory;
import com.sun.xsd.model.FastSchema;

import com.sun.encoder.MetaRef;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.EndpointImpl;
import com.sun.jbi.msmqbc.Endpoint.EndpointType;
import com.sun.jbi.msmqbc.Endpoint.EndpointState;
import com.sun.jbi.msmqbc.Endpoint.EndpointMessageType;
import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQOutput;
import com.sun.jbi.msmqbc.extensions.MSMQMessage;
import com.sun.jbi.msmqbc.extensions.MSMQAddress;
import com.sun.jbi.msmqbc.extensions.MSMQBinding;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;
import com.sun.jbi.msmqbc.extensions.MSMQExtensionRegistry;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints based on the portmap list.
 * 
 * @author Sun Microsystems
 */
public class WSDLConfigurations {
    private static Logger mLogger = Messages.getLogger(WSDLConfigurations.class);

    private static final Messages mMessages = Messages.getMessages(WSDLConfigurations.class);

    private String mRootPath;

    private List mXsds = new ArrayList();

    private Map mEncoderMap = new HashMap();

    public WSDLConfigurations(String rootPath) {
        mRootPath = rootPath;
    }

    public List parse(List portMaps, Map envVariableMap) throws Exception {
    	Map envVariables = (Map)((HashMap)envVariableMap).clone();
        File dir = new File(mRootPath);

        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() + File.separator + "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);

        List endpoints = new ArrayList();
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        mXsds = listResourceFiles(dir, ".xsd");
        while (wsdls.hasNext()) {
            endpoints.addAll(parseWSDL((File) wsdls.next(), resolver, portMaps, envVariables));
        }

        return endpoints;
    }

/*    public Map parseForEnvironmentVariables(List portMaps, Map envVariableMap) throws Exception {
        Map envVariables = (Map)((HashMap)envVariableMap).clone();
        File dir = new File(mRootPath);

        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() +
                File.separator +
                "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        while (wsdls.hasNext()) {
            envVariables.putAll(readWSDLForEnvVariables((File)wsdls.next(), resolver, envVariables));
        }

        return envVariables;
    }*/
    
    protected Definition readWSDL(File f, EntityResolver resolver, Map envVariableMap) throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new MSMQExtensionRegistry(envVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }
    public List parseWSDL(File wsdlFile, EntityResolver resolver, List portMaps, Map envVariableMap) throws Exception {

        Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
        //Definition def = readWSDL(wsdlFile, resolver);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData) it.next();

            // Check the Definition if it has a MSMQBinding. If not,
            // continue
            MSMQBinding binding = getMSMQBinding(def, pm.getService(), pm.getEndpoint());
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint. If we find one,
            // extract file details

            // If we have a MSMQBinding, we must have an MSMQAddress.
            MSMQAddress address = getMSMQAddress(def, pm.getService(), pm.getEndpoint());
            if (address == null) {
                String msg = "Missing msmq:address in wsdl for this service " + pm.getService();
                throw new Exception(msg);
            }

            // If we have an MSMQBinding, we must have operations
            Map msmqOperations = getMSMQOperations(def, pm.getService(), pm.getEndpoint(), pm.getDirection());
            if (msmqOperations == null || msmqOperations.size() == 0) {
                String msg = "Missing msmq:operation definition(s) in wsdl for this service " + pm.getService();
                throw new Exception(msg);
            }

            Map opMsgExchangePatterns = getMSMQOperationMepTypes(def, pm.getService(), pm.getEndpoint(),
                    pm.getDirection());

            // Create an Endpoint for each Port. The endpoint should have the
            // correct MSMQBinding, the associated Operations, and the
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
            endpoint.setMSMQAddress(address);
            endpoint.setMSMQBinding(binding);
            endpoint.setMSMQOperations(msmqOperations);
            endpoint.setOperationMsgExchangePatterns(opMsgExchangePatterns);

            setInputsOutputs(def, pm.getService(), pm.getEndpoint(), msmqOperations.values(), endpoint);

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
        for (int fileCount = 0; fileCount < filesInCurrentDir.length; fileCount++) {

            if (filesInCurrentDir[fileCount].isFile()) {
                if (filesInCurrentDir[fileCount].getName().toLowerCase().endsWith(extension)) {
                    cumulativeResults.add(filesInCurrentDir[fileCount]);
                }
            } else if (filesInCurrentDir[fileCount].isDirectory()) {
                List wsdlsInSubDirectories = listResourceFiles(filesInCurrentDir[fileCount], extension);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }

    protected Definition readWSDL(File f, EntityResolver resolver) throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new MSMQExtensionRegistry());
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    protected Binding getBinding(Definition def, String serviceName, String endpointName) {
        String location = null;
        Service svc = def.getService(QName.valueOf(serviceName));
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

    protected MSMQAddress getMSMQAddress(Definition def, String serviceName, String endpointName) {
        MSMQAddress address = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();

            // Look for msmq:address

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (MSMQAddress.class.isInstance(ee)) {
                    address = (MSMQAddress) ee;
                }
            }
        }
        return address;
    }

    protected MSMQBinding getMSMQBinding(Definition def, String serviceName, String endpointName) {
        MSMQBinding msmqBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            // Look for msmq:binding

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && msmqBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (MSMQBinding.class.isInstance(ee)) {
                    msmqBinding = (MSMQBinding) ee;
                }
            }
        }
        return msmqBinding;
    }

    protected Map getMSMQOperations(Definition def, String serviceName, String endpointName, int isInbound) {
        Map msmqOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();

            // Look for msmq:operation

            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement operExt = (ExtensibilityElement) extIter.next();
                    if (MSMQOperation.class.isInstance(operExt)) {
                        MSMQOperation msmqOperation = (MSMQOperation) operExt;
                        msmqOperation.setBindingOperation(oper);
                        msmqOperation.setMEP(determineMEP(isInbound, oper));
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                MSMQInput msmqInput = null;
                                if (inputExt instanceof MSMQMessage) {
                                    msmqInput = new MSMQInput();
                                    MSMQMessage msmqMessage = (MSMQMessage) inputExt;
                                    msmqInput.setMsmqMessage(msmqMessage);
                                }
                                if (msmqInput != null) {
                                    msmqOperation.setMSMQOperationInput(msmqInput);
                                }
                            }
                        }

                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator outputIter = bindingOutput.getExtensibilityElements().iterator();
                            while (outputIter.hasNext()) {
                                ExtensibilityElement outputExt = (ExtensibilityElement) outputIter.next();
                                MSMQOutput msmqOutput = null;
                                if (outputExt instanceof MSMQMessage) {
                                    msmqOutput = new MSMQOutput();
                                    MSMQMessage msmqMessage = (MSMQMessage) outputExt;
                                    msmqOutput.setMsmqMessage(msmqMessage);
                                }

                                if (msmqOutput != null) {
                                    msmqOperation.setMSMQOperationOutput(msmqOutput);
                                }
                            }
                        }
                        msmqOperations.put(QName.valueOf(oper.getName()), msmqOperation);
                    }
                }
            }
        }
        return msmqOperations;
    }

    protected void setInputsOutputs(Definition def,
                                    String serviceName,
                                    String endpointName,
                                    Collection operations,
                                    Endpoint endpoint) {

        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for msmq:operation entries

                MSMQOperation msmqOperation = null;
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (operations.contains(ee)) {
                        msmqOperation = (MSMQOperation) ee;
                    }
                }

                if (msmqOperation != null) {
                    BindingInput bindingInput = oper.getBindingInput();
                    if (bindingInput != null) {
                        Iterator it = bindingInput.getExtensibilityElements().iterator();
                        while (it.hasNext()) {
                            ExtensibilityElement ee = (ExtensibilityElement) it.next();
                            if (ee instanceof MSMQMessage) {
                                MSMQInput msmqInput = new MSMQInput();
                                MSMQMessage msmqMessage = (MSMQMessage) ee;
                                msmqInput.setMsmqMessage(msmqMessage);
                                endpoint.setMSMQOperationInput(msmqOperation, msmqInput);
                            }
                        }
                    }
                }

                BindingOutput bindingOutput = oper.getBindingOutput();
                if (bindingOutput != null) {
                    Iterator it2 = bindingOutput.getExtensibilityElements().iterator();
                    while (it2.hasNext()) {
                        ExtensibilityElement ee = (ExtensibilityElement) it2.next();
                        if (ee instanceof MSMQMessage) {
                            MSMQOutput msmqOutput = new MSMQOutput();
                            MSMQMessage msmqMessage = (MSMQMessage) ee;
                            msmqOutput.setMsmqMessage(msmqMessage);
                            endpoint.setMSMQOperationOutput(msmqOperation, msmqOutput);
                        }
                    }
                }

            }
        }
    }

    protected Map getMSMQOperationMepTypes(Definition def, String serviceName, String endpointName, int direction) {
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
                    if (MSMQOperation.class.isInstance(ee)) {
                        String mep = EndpointMessageType.UNSUPPORTED;
                        if (oper.getOperation().getStyle() != null) {
                            mep = determineMEP(direction, oper);
                        } else {
                            // for some reason, sometimes the operation type is not set
                            // so the BC will populate it based on the WSDL definition
                            // for now, we only handle Request-response and one-way.
                            // anything else is not supported and should be caught in WSDL
                            // validation
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
                        mepTypes.put(QName.valueOf(oper.getName()), mep);
                    }
                }
            }
        }
        return mepTypes;
    }

    public Map getPartEncoderMapping(Endpoint currEndpoint) throws Exception {

        Definition def = currEndpoint.getDefinition();
        String serviceName = currEndpoint.getServiceName().toString();
        String endpointName = currEndpoint.getEndpointName();
        int direction = currEndpoint.getEndpointType();
        Map msmqOperations = currEndpoint.getMSMQOperations();
        Map mepTypes = currEndpoint.getOperationMsgExchangePatterns();
        Map partMapping = new HashMap();

        // Don't support inline encoder schemas
        if (mXsds.size() <= 0) {
            return partMapping;
        }

        Service service = def.getService(QName.valueOf(serviceName));
        Port port = service.getPort(endpointName);
        PortType portType = port.getBinding().getPortType();

        /**
         * Locate the operation we are interested in. There may be multiple operations by the same
         * name (operation overloading) and the WSDL spec does not allow it. The uniqueness should
         * be guaranteed by the examination of input and/or output names. The WSDL validation
         * should've been enforced the uniqueness at design time. For the time being, we will assume
         * that we don't have operation overloading.
         */
        javax.wsdl.Message wsdlMessage = null;
        Map parts = new HashMap();
        for (Iterator opnameIter = msmqOperations.keySet().iterator(); opnameIter.hasNext();) {
            String mep = null;
            String encodingStyle = null;

            QName operationName = (QName) opnameIter.next();
            MSMQOperation msmqOperation = (MSMQOperation) msmqOperations.get(operationName);
            mep = (String) mepTypes.get(operationName);

            for (Iterator operIter = portType.getOperations().iterator(); operIter.hasNext();) {
                Operation op = (Operation) operIter.next();
                if (op.getName().equals(operationName.toString()) || op.getName().equals(operationName.getLocalPart())) {
                    /**
                     * There is nothing in the WSDL spec that says that the part name has to be
                     * unique within the WSDL document, so we need to prefix the part name with the
                     * message name.
                     */

                    Input input = op.getInput();
                    if (input != null) {
                        MSMQInput msmqInput = msmqOperation.getMSMQOperationInput();
                        if (msmqInput == null) {
                            throw new Exception(mMessages.getString("WSDLConfigurations_No_MSMQInput",
                                    operationName.toString()));
                        }
                        wsdlMessage = input.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName type = (aPart.getElementName() != null) ? aPart.getElementName()
                                    : aPart.getTypeName();

                            // locate the XSD file based on the part type namespace
                            String namespace = type.getNamespaceURI();
                            String xsdFileLoc = getXsdFileLocation(namespace);
                            if (xsdFileLoc != null) {
                                // Determine on which extensibility element the encoding style is
                                // defined
                                if (direction == EndpointType.INBOUND) { // Consumer
                                    MSMQMessage msmqMessage = msmqInput.getMsmqMessage();
                                    if (msmqMessage == null) {
                                        throw new Exception(mMessages.getString("WSDLConfigurations_No_MSMQMessage",
                                                operationName.toString()));
                                    }
                                    if (msmqMessage.getUseType().equals(MSMQMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = msmqMessage.getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString(
                                                "WSDLConfigurations_No_EncodingStyle_MSMQMessage",
                                                operationName.toString()));
                                    }
                                } else { // Provider
                                    MSMQMessage msmqMessage = msmqInput.getMsmqMessage();
                                    if (msmqMessage == null) {
                                        throw new Exception(mMessages.getString("WSDLConfigurations_No_MSMQOutput",
                                                operationName.toString()));
                                    }
                                    if (msmqMessage.getUseType().equals(MSMQMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = msmqMessage.getEncodingStyle();
                                    } else {
                                        continue;
                                    }

                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString(
                                                "WSDLConfigurations_No_EncodingStyle_MSMQMessage",
                                                operationName.toString()));
                                    }
                                }

                                Encoder encoder = null;
                                MetaRef metaRef = new MyMetaRef(xsdFileLoc, type);
                                if (mEncoderMap.get(metaRef) != null) {
                                    encoder = (Encoder) mEncoderMap.get(metaRef);
                                } else {
                                    EncoderFactory encoderFactory = EncoderFactory.newInstance();
                                    encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle), metaRef);
                                    mEncoderMap.put(metaRef, encoder);
                                }
                                partMapping.put(wsdlMessage.getQName() + partName, encoder);
                            }
                        }
                    }

                    Output output = op.getOutput();
                    if (output != null) {
                        MSMQOutput msmqOutput = msmqOperation.getMSMQOperationOutput();
                        if (msmqOutput == null) {
                            throw new Exception(mMessages.getString("WSDLConfigurations_No_MSMQOutput",
                                    operationName.toString()));
                        }

                        wsdlMessage = output.getMessage();
                        parts = wsdlMessage.getParts();
                        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
                            Part aPart = (Part) partIter.next();
                            String partName = aPart.getName();
                            QName type = (aPart.getElementName() != null) ? aPart.getElementName()
                                    : aPart.getTypeName();

                            // locate the XSD file based on the part type namespace
                            String namespace = type.getNamespaceURI();
                            String xsdFileLoc = getXsdFileLocation(namespace);
                            if (xsdFileLoc != null) {
                                if (direction == EndpointType.INBOUND && mep.equals(EndpointMessageType.IN_OUT)) {
                                    // Consumer request-response operations
                                    MSMQMessage msmqMessage = msmqOutput.getMsmqMessage();
                                    if (msmqMessage == null) {
                                        throw new Exception(mMessages.getString("WSDLConfigurations_No_MSMQMessage",
                                                operationName.toString()));
                                    }

                                    if (msmqMessage.getUseType().equals(MSMQMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = msmqMessage.getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString(
                                                "WSDLConfigurations_No_EncodingStyle_MSMQMessage",
                                                operationName.toString()));
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
     * Determine the message exchange pattern. For handling 1.1 wsdls, map transmission primitives
     * to the closest message exchange pattern, taking into account the endpoint direction direction
     * inbound: request-response and solicit-response -> in-out one-way and notification -> in-only
     * direction outbound: request-response and solicit-response -> out-in one-way and notification ->
     * out-only
     * 
     * @param pm the endpoint configuration from the portmap
     * @param po the binding operation definition from the wsdl
     * @return the message exchange pattern, null if no mapping could be determined.
     */
    protected String determineMEP(int direction, BindingOperation bo) {
        String mep = null;
        OperationType type = bo.getOperation().getStyle();

        if (direction == EndpointType.INBOUND) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = EndpointMessageType.IN_OUT;
            } else if (type.equals(OperationType.ONE_WAY)) {
                mep = EndpointMessageType.IN_ONLY;
            } else {
                mep = EndpointMessageType.UNSUPPORTED;
            }
        } else {
            if (type == null) {
                return null; // Not sure why type is not populated for some outbound binding
                // operations.
            }
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = EndpointMessageType.OUT_IN;
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = EndpointMessageType.OUT_ONLY;
            }
        }
        return mep;
    }

    protected String determineMEP(int direction, OperationType type) {
        String mep = null;

        if (direction == EndpointType.INBOUND) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = EndpointMessageType.IN_OUT;
            } else if (type.equals(OperationType.ONE_WAY)) {
                mep = EndpointMessageType.IN_ONLY;
            } else {
                mep = EndpointMessageType.UNSUPPORTED;
            }
        } else {
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = EndpointMessageType.OUT_IN;
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = EndpointMessageType.OUT_ONLY;
            }
        }
        return mep;
    }

    protected String getXsdFileLocation(String aNamespace) {
        String xsdFileLoc = null;
        File aXsdFile = null;

        try {
            for (int ii = 0; ii < mXsds.size(); ii++) {
                aXsdFile = (File) mXsds.get(ii);
                FastSchemaFactory factory = FastSchemaFactory.getInstance();
                FastSchema schema = factory.newFastSchema(aXsdFile.getAbsolutePath());
                if (aNamespace.equals(schema.getTargetNamespace())) {
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
		 */
		public URL getURL(){
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
