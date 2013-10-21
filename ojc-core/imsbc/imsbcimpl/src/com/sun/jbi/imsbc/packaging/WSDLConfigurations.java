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

package com.sun.jbi.imsbc.packaging;

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


import org.w3c.dom.Document;

import com.sun.jbi.internationalization.Messages;

import com.ibm.wsdl.factory.WSDLFactoryImpl;

import com.sun.xsd.model.FastSchemaFactory;
import com.sun.xsd.model.FastSchema;

import com.sun.encoder.MetaRef;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;

import com.sun.jbi.imsbc.Endpoint;
import com.sun.jbi.imsbc.EndpointImpl;
import com.sun.jbi.imsbc.Endpoint.EndpointType;
import com.sun.jbi.imsbc.Endpoint.EndpointState;
import com.sun.jbi.imsbc.Endpoint.EndpointMessageType;
import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSOutput;
import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.extensions.IMSAddress;
import com.sun.jbi.imsbc.extensions.IMSBinding;
import com.sun.jbi.imsbc.extensions.IMSOperation;
import com.sun.jbi.imsbc.extensions.IMSExtensionRegistry;
import com.sun.jbi.imsbc.extensions.IMSExtPreprocessDeserializer;
import com.sun.jbi.imsbc.mbeans.ApplicationConfigurationField;
import com.sun.jbi.imsbc.mbeans.AppConfigAddressVisitor;
import com.sun.jbi.imsbc.mbeans.Visitor;

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

    private final File mRootPath;

    private List mXsds = new ArrayList();

    private Map mEncoderMap = new HashMap();

    public WSDLConfigurations(String rootPath) {
        mRootPath = new File(rootPath);
    }

    public Collection<Endpoint> createEndpoints(
            Collection portMaps,
            Map<String, String[]> envVariableMap,
            Map<String, Collection<ApplicationConfigurationField>> appConfMap)
            throws Exception {
        
        File catalog = new File(mRootPath.getAbsolutePath() +
            File.separator + "META-INF" + File.separator +
            "catalog.xml");
        
        EntityResolver resolver = null;
        
        if (catalog.exists()) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            resolver = new CatalogResolver(catalogManager);
        }
         
        ArrayList<Endpoint> endpoints = new ArrayList<Endpoint>();
        List<File> wsdLs = listResourceFiles(mRootPath, ".wsdl");
        for (File file : wsdLs) {
            endpoints.addAll(
                    parseWSDL(file,
                            resolver,
                            portMaps,
                            envVariableMap,
                            appConfMap));
        }
        
        return endpoints;
    }
   
    public Map<String, String[]> parseForEnvironmentVariables(List portMaps, Map<String, String[]> envVariableMap) throws Exception {
        Map<String, String[]> envVariables =
                new HashMap<String, String[]>(envVariableMap);
        
        File catalog = new File(mRootPath.getAbsolutePath() +
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

        for (File file : listResourceFiles(mRootPath, ".wsdl")) {
            envVariables.putAll(
                    readWSDLForEnvVariables(file,
                            resolver,
                            envVariables));
        }
        
        return envVariables;
    }
        
    
    protected Definition readWSDL(File f, EntityResolver resolver, Map envVariableMap) throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new IMSExtensionRegistry(envVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }
    
     private Map readWSDLForEnvVariables(File f, EntityResolver resolver, Map<String, String[]> envVariableMap) 
        throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() :
                ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        IMSExtPreprocessDeserializer preProcessDeserializer = new IMSExtPreprocessDeserializer(envVariableMap);
        reader.setExtensionRegistry(new IMSExtensionRegistry(preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getEnvVariableMap();
    }

    public List parseWSDL(File wsdlFile, EntityResolver resolver, 
		                  Collection portMaps, Map<String, String[]> envVariableMap,
						  Map<String, Collection<ApplicationConfigurationField>> appConfMap)
						  throws Exception {

    	Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData) it.next();

            // Check the Definition if it has a IMSBinding. If not,
            // continue
            IMSBinding binding = getIMSBinding(def, pm.getService(), pm.getEndpoint());
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint. If we find one,
            // extract file details

            // If we have a IMSBinding, we must have an IMSAddress.
            IMSAddress address = getIMSAddress(def, pm.getService(), pm.getEndpoint());
            if (address == null) {
                String msg = mMessages.getString("IMSBC-E00910.Missing_Ims_Address") + pm.getService();
                throw new Exception(msg);
            }

			 // if an application configuration object is defined for this
            // endpoint, apply it.
            String appConfName = pm.getApplicationConfigurationObjectName();
            Collection<ApplicationConfigurationField> appConfObj = appConfMap.get(appConfName);
            if (appConfObj != null) {
                applyApplicationConfiguration(address, appConfObj);
            }

            // If we have an IMSBinding, we must have operations
            Map imsOperations = getIMSOperations(def, pm.getService(), pm.getEndpoint(), pm.getDirection());
            if (imsOperations == null || imsOperations.size() == 0) {
                String msg = mMessages.getString("IMSBC-E00911.Missing_Ims_Operation_Definition") + pm.getService();
                throw new Exception(msg);
            }

            Map opMsgExchangePatterns = getIMSOperationMepTypes(def, pm.getService(), pm.getEndpoint(),
                    pm.getDirection());

            // Create an Endpoint for each Port. The endpoint should have the
            // correct IMSBinding, the associated Operations, and the
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
            endpoint.setIMSAddress(address);
            endpoint.setIMSBinding(binding);
            endpoint.setIMSOperations(imsOperations);
            endpoint.setOperationMsgExchangePatterns(opMsgExchangePatterns);

            setInputsOutputs(def, pm.getService(), pm.getEndpoint(), imsOperations.values(), endpoint);

            // Now add the Endpoint to our list of Endpoints
            endPoints.add(endpoint);
        }

        return endPoints;
    }

    /**
     * Applies the configuration values represented by an application
     * configuration object, to an IMSBCAddress object.
     * 
     * @param address    The IMSBCAddress to which to apply the configuration
     * @param appConfObj The application configuration object to apply
     */
    private void applyApplicationConfiguration(IMSAddress address,
                                               Collection<ApplicationConfigurationField> appConfObj) {
        assert address != null;
        assert appConfObj != null;
        if (!appConfObj.isEmpty()) {
            Visitor addressVisitor = new AppConfigAddressVisitor(address);
            for (ApplicationConfigurationField field: appConfObj) {
                field.accept(addressVisitor);
            }
        }
    }

    /**
     * List all wsdl files in the currentDir and below
     */
    protected List<File> listResourceFiles(File currentDir, String extension) {
        List<File> cumulativeResults = new ArrayList();
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

 /*   protected Definition readWSDL(File f, EntityResolver resolver) throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new IMSExtensionRegistry());
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }*/

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

    protected IMSAddress getIMSAddress(Definition def, String serviceName, String endpointName) {
        IMSAddress address = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();

            // Look for ims:address

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (IMSAddress.class.isInstance(ee)) {
                    address = (IMSAddress) ee;
                }
            }
        }
        return address;
    }

    protected IMSBinding getIMSBinding(Definition def, String serviceName, String endpointName) {
        IMSBinding imsBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            // Look for ims:binding

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && imsBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (IMSBinding.class.isInstance(ee)) {
                    imsBinding = (IMSBinding) ee;
                }
            }
        }
        return imsBinding;
    }

    protected Map getIMSOperations(Definition def, String serviceName, String endpointName, int isInbound) {
        Map imsOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();

            // Look for ims:operation

            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement operExt = (ExtensibilityElement) extIter.next();
                    if (IMSOperation.class.isInstance(operExt)) {
                        IMSOperation imsOperation = (IMSOperation) operExt;
                        imsOperation.setBindingOperation(oper);
                        imsOperation.setMEP(determineMEP(isInbound, oper));
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                IMSInput imsInput = null;
                                if (inputExt instanceof IMSMessage) {
                                    imsInput = new IMSInput();
                                    IMSMessage imsMessage = (IMSMessage) inputExt;
                                    imsInput.setImsMessage(imsMessage);
                                }
                                if (imsInput != null) {
                                    imsOperation.setIMSOperationInput(imsInput);
                                }
                            }
                        }

                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator outputIter = bindingOutput.getExtensibilityElements().iterator();
                            while (outputIter.hasNext()) {
                                ExtensibilityElement outputExt = (ExtensibilityElement) outputIter.next();
                                IMSOutput imsOutput = null;
                                if (outputExt instanceof IMSMessage) {
                                    imsOutput = new IMSOutput();
                                    IMSMessage imsMessage = (IMSMessage) outputExt;
                                    imsOutput.setImsMessage(imsMessage);
                                }

                                if (imsOutput != null) {
                                    imsOperation.setIMSOperationOutput(imsOutput);
                                }
                            }
                        }
                        imsOperations.put(QName.valueOf(oper.getName()), imsOperation);
                    }
                }
            }
        }
        return imsOperations;
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

				// Look for ims:operation entries
                IMSOperation imsOperation = null;
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (operations.contains(ee)) {
                        imsOperation = (IMSOperation) ee;
                    }
                }

                if (imsOperation != null) {
                    BindingInput bindingInput = oper.getBindingInput();
                    if (bindingInput != null) {
                        Iterator it = bindingInput.getExtensibilityElements().iterator();
                        while (it.hasNext()) {
                            ExtensibilityElement ee = (ExtensibilityElement) it.next();
                            if (ee instanceof IMSMessage) {
                                IMSInput imsInput = new IMSInput();
                                IMSMessage imsMessage = (IMSMessage) ee;
                                imsInput.setImsMessage(imsMessage);
                                endpoint.setIMSOperationInput(imsOperation, imsInput);
                            }
                        }
                    }
                }

                BindingOutput bindingOutput = oper.getBindingOutput();
                if (bindingOutput != null) {
                    Iterator it2 = bindingOutput.getExtensibilityElements().iterator();
                    while (it2.hasNext()) {
                        ExtensibilityElement ee = (ExtensibilityElement) it2.next();
                        if (ee instanceof IMSMessage) {
                            IMSOutput imsOutput = new IMSOutput();
                            IMSMessage imsMessage = (IMSMessage) ee;
                            imsOutput.setImsMessage(imsMessage);
                            endpoint.setIMSOperationOutput(imsOperation, imsOutput);
                        }
                    }
                }

            }
        }
    }

    protected Map getIMSOperationMepTypes(Definition def, String serviceName, String endpointName, int direction) {
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
                    if (IMSOperation.class.isInstance(ee)) {
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
        Map imsOperations = currEndpoint.getIMSOperations();
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
        for (Iterator opnameIter = imsOperations.keySet().iterator(); opnameIter.hasNext();) {
            String mep = null;
            String encodingStyle = null;

            QName operationName = (QName) opnameIter.next();
            IMSOperation imsOperation = (IMSOperation) imsOperations.get(operationName);
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
                        IMSInput imsInput = imsOperation.getIMSOperationInput();
                        if (imsInput == null) {
                            throw new Exception(mMessages.getString("IMSBC-E00904.No_Ims_Input",
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
                                    IMSMessage imsMessage = imsInput.getImsMessage();
                                    if (imsMessage == null) {
                                        throw new Exception(mMessages.getString("IMSBC-E00905.No_Ims_Message",
                                                operationName.toString()));
                                    }
                                    if (imsMessage.getUseType().equals(IMSMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = imsMessage.getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString(
                                                "IMSBC-E00906.No_Encoding_Style",
                                                operationName.toString()));
                                    }
                                } else { // Provider
                                    IMSMessage imsMessage = imsInput.getImsMessage();
                                    if (imsMessage == null) {
                                        throw new Exception(mMessages.getString("IMSBC-E00907.No_Ims_Ouput",
                                                operationName.toString()));
                                    }
                                    if (imsMessage.getUseType().equals(IMSMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = imsMessage.getEncodingStyle();
                                    } else {
                                        continue;
                                    }

                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString(
                                                "IMSBC-E00906.No_Encoding_Style",
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
                        IMSOutput imsOutput = imsOperation.getIMSOperationOutput();
                        if (imsOutput == null) {
                            throw new Exception(mMessages.getString("IMSBC-E00907.No_Ims_Ouput",
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
                                    IMSMessage imsMessage = imsOutput.getImsMessage();
                                    if (imsMessage == null) {
                                        throw new Exception(mMessages.getString("IMSBC-E00905.No_Ims_Message",
                                                operationName.toString()));
                                    }

                                    if (imsMessage.getUseType().equals(IMSMessage.USE_TYPE_ENCODED)) {
                                        encodingStyle = imsMessage.getEncodingStyle();
                                    } else {
                                        continue;
                                    }
                                    if (encodingStyle == null || encodingStyle.equals("")) {
                                        throw new Exception(mMessages.getString(
                                                "IMSBC-E00906.No_Encoding_Style",
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
                return null;
				// Not sure why type is not populated for some outbound binding operations.
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
            mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00908.Wcf_Cannot_Parse_Xsd", aXsdFile.getName()));
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
