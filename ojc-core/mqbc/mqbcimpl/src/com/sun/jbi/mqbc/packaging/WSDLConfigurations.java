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
 */

/*
 * @(#)WSDLConfigurations.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.packaging;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.mqbc.Endpoint;
import com.sun.jbi.mqbc.EndpointImpl;
import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.extensions.MQBCAddress;
import com.sun.jbi.mqbc.extensions.MQBCBinding;
import com.sun.jbi.mqbc.extensions.MQBCBody;
import com.sun.jbi.mqbc.extensions.MQBCExtPreprocessDeserializer;
import com.sun.jbi.mqbc.extensions.MQBCHeader;
import com.sun.jbi.mqbc.extensions.MQBCOperation;
import com.sun.jbi.mqbc.extensions.MQBCRedelivery;
import com.sun.jbi.mqbc.extensions.MQExtensionRegistry;
import com.sun.jbi.mqbc.extensions.MQFault;
import com.sun.jbi.mqbc.extensions.MQInput;
import com.sun.jbi.mqbc.extensions.MQOutput;
import com.sun.jbi.mqbc.mbeans.AppConfigAddressVisitor;
import com.sun.jbi.mqbc.mbeans.ApplicationConfigurationField;
import com.sun.jbi.mqbc.mbeans.Visitor;
import com.sun.jbi.mqbc.validator.EndpointValidator;
import com.sun.xsd.model.FastSchema;
import com.sun.xsd.model.FastSchemaFactory;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;

/**
 *
 * @author rchen
 */
public final class WSDLConfigurations {
    private static final Messages mMessages =
            Messages.getMessages(WSDLConfigurations.class);
    private static Logger mLogger = Messages.getLogger(WSDLConfigurations.class);
    
    private final File mRootPath;
    private final Map mEncoderMap = new HashMap();
    
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
            catalogManager.setUseStaticCatalog(false);
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

    public Map<String, String[]> parseForEnvironmentVariables(Map<String, String[]> envVariableMap)
            throws Exception {
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
    
    public Map getPartEncoderMapping(Endpoint endpoint) throws Exception {
        
        Definition def = endpoint.getDefinition();
        String serviceName = endpoint.getServiceName().toString();
        String endpointName = endpoint.getEndpointName();
        int direction = endpoint.getEndpointType();
        Map mqOperations = endpoint.getMQOperations();
        Map partMapping = new HashMap();
        
        // Don't support inline encoder schemas
        List<File> xsds = listResourceFiles(mRootPath, ".xsd");
        if (xsds.size() < 1) {
            return partMapping;
        }
        
        Service service  = def.getService(QName.valueOf(serviceName));
        Port port = service.getPort(endpointName);
        PortType portType = port.getBinding().getPortType();
        
        javax.wsdl.Message wsdlMessage = null;
        Map parts = new HashMap();
        for (Iterator opnameIter = mqOperations.keySet().iterator(); opnameIter.hasNext(); ) {
            String mep = null;
            String encodingStyle = null;
            
            QName operationName = (QName)opnameIter.next();
            MQBCOperation mqOperation = (MQBCOperation)mqOperations.get(operationName);
            
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
                        MQInput mqInput =mqOperation.getMQOperationInput();
                        if (mqInput == null) {
                            throw new Exception(mMessages.getString("WCF_No_MQBody", operationName.toString()));
                        }
                        MQBCBody mqBody = mqInput.getMQMessage();
                        if (mqBody == null) {
                            throw new Exception(mMessages.getString("WCF_No_MQBody", operationName.toString()));
                        }
                        
                        wsdlMessage = input.getMessage();
                        
                        
                        addEncoderMapping(operationName,
                                wsdlMessage,
                                mqBody,
                                partMapping);
                    }
                    
                    break;
                }
            }
        }
        return partMapping;
    }
    
    private Collection<Endpoint> parseWSDL(File wsdlFile,
            EntityResolver resolver,
            Collection portMaps,
            Map<String, String[]> envVariableMap,
            Map<String, Collection<ApplicationConfigurationField>> appConfMap)
            throws Exception {
        
        assert envVariableMap != null;
        assert appConfMap != null;
        assert portMaps != null;
        assert resolver != null;
        assert wsdlFile != null;
        
        Definition def = readWSDL(wsdlFile, resolver,envVariableMap);
        ArrayList<Endpoint> endPoints = new ArrayList<Endpoint>();
        for (Object portMap : portMaps) {
            EndpointData pm = (EndpointData) portMap;
            String serviceName = pm.getService();
            String endpointName = pm.getEndpoint();

            // Interested only in Definitions with MQ binding
            MQBCBinding binding = getMQBinding(def, serviceName, endpointName);
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // extract mq:address details

            MQBCAddress address = getMQAddress(def, serviceName, endpointName);
            if (address == null) {
                String msg = "Missing mq:address in wsdl for this service "
                        + serviceName
                        + " "
                        + wsdlFile.getName();
                throw new Exception(msg);
            }
            
            // if an application configuration object is defined for this
            // endpoint, apply it.
            String appConfName = pm.getApplicationConfigurationObjectName();
            Collection<ApplicationConfigurationField> appConfObj = appConfMap.get(appConfName);
            if (appConfObj != null) {
                applyApplicationConfiguration(address, appConfObj);
            }

            // Locate operations for the endpoint.
            Map mqOperations = getMQOperations(def,
                    serviceName,
                    endpointName);
            if (mqOperations == null || mqOperations.size() == 0) {
                String msg =
                        "Missing mq:operation definition(s) in wsdl for this service "
                                + pm.getService();
                throw new Exception(msg);
            }
            Map<QName, String> opMeps = getMQOperationMepType(def,
                    serviceName,
                    endpointName,
                    pm.getDirection());

            // Redelivery parameters for the endpoint
            Map<QName, MQBCRedelivery> redeliverySpec = getMQRedelivery(def,
                    serviceName,
                    endpointName);

            // Create an Endpoint for each port
            Endpoint endpoint = new EndpointImpl(pm);
            endpoint.setDefinition(def);
            endpoint.setEndpointType(pm.getDirection());

            // Set the description
            DocumentBuilderFactory docBuilderFactory =
                    DocumentBuilderFactory.newInstance();
            DocumentBuilder documentBuilder =
                    docBuilderFactory.newDocumentBuilder();
            Document result = documentBuilder.parse(wsdlFile);
            endpoint.setServiceDescription(result);

            // Store our extensibility elements
            endpoint.setMQAddress(address);
            endpoint.setMQBinding(binding);
            endpoint.setMQOperations(mqOperations);
            endpoint.setOperationMsgExchangePattern(opMeps);
            endpoint.setRedelivery(redeliverySpec);

            // Now add the Endpoint to our list of Endpoints
            EndpointValidator.validateEndpointForUniqueness(endPoints,
                    endpoint,
                    false);
            endPoints.add(endpoint);
        }
        
        return endPoints;
    }

    /**
     * Applies the configuration values represented by an application
     * configuration object, to an MQBCAddress object.
     * 
     * @param address    The MQBCAddress to which to apply the configuration
     * @param appConfObj The application configuration object to apply
     */
    private void applyApplicationConfiguration(MQBCAddress address,
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
    private List<File> listResourceFiles(final File currentDir, final String extension) {
        final List<File> cumulativeResults = new ArrayList<File>();
        final File[] filesInCurrentDir = currentDir.listFiles();
        for (final File element : filesInCurrentDir) {
            
            if (element.isFile()) {
                if (element.getName().toLowerCase().endsWith(extension)) {
                    cumulativeResults.add(element);
                }
            } else if (element.isDirectory()) {
                final List<File> filesInSubdirectories =
                        listResourceFiles(element, extension);
                cumulativeResults.addAll(filesInSubdirectories);
            }
        }
        return cumulativeResults;
    }
    
    
    
    private Definition readWSDL(File f, EntityResolver resolver,Map<String, String[]> envVariableMap)
            throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader =
                ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new MQExtensionRegistry(envVariableMap));
        reader.setFeature("javax.wsdl.verbose", mLogger.isLoggable(Level.FINE)); // NO i18n
        Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
    
    private Map readWSDLForEnvVariables(File f,
                                        EntityResolver resolver,
                                        Map<String, String[]> envVariableMap)
            throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() :
                ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        MQBCExtPreprocessDeserializer preProcessDeserializer = new MQBCExtPreprocessDeserializer(envVariableMap);
        reader.setFeature("javax.wsdl.verbose", mLogger.isLoggable(Level.FINE));
        reader.setExtensionRegistry(new MQExtensionRegistry(preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getEnvVariableMap();
    }
     
    private Binding getBinding(Definition def, String serviceName,
            String endpointName) {
        
        // DO NOT use the getService() method.
        // It checks all imported WSDLs.
        Map services = def.getServices();
        
        Service svc =(Service)services.get(QName.valueOf(serviceName));
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
    
    private MQBCAddress getMQAddress(Definition def, String serviceName,
            String endpointName) {
        MQBCAddress address = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();
            
            //Look for mq:address
            
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (MQBCAddress.class.isInstance(ee)) {
                    address = (MQBCAddress) ee;
                }
            }
        }
        return address;
    }
    
    private MQBCBinding getMQBinding(Definition def, String serviceName, String endpointName) {
        MQBCBinding mqBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
            
            //Look for mq:binding
            
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && mqBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (MQBCBinding.class.isInstance(ee)) {
                    mqBinding = (MQBCBinding) ee;
                }
            }
        }
        return mqBinding;
    }

    private Map getMQOperations(Definition def,
                                String serviceName,
                                String endpointName) throws Exception {
        Map mqOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for mq:operation entries
                
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement operExt = (ExtensibilityElement) extIter.next();
                    if (MQBCOperation.class.isInstance(operExt)) {
                        MQBCOperation mqOperation = (MQBCOperation)operExt;
                        
                        // Input
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            MQInput mqInput = new MQInput();
                            boolean gotBody = false;
                            boolean gotMqBinding = false;
                            for (Object extObj : bindingInput.getExtensibilityElements()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement)extObj;
                                if (inputExt instanceof MQBCBody) {
                                    gotMqBinding = true;
                                    if (!gotBody) {
                                        MQBCBody mqbody = (MQBCBody)inputExt;
                                        mqInput.setMQMessage(mqbody);
                                        gotBody = true;
                                    } else {
                                        throw new Exception(I18n.msg(
                                                "0951: Invalid operation {0} -" 
                                                        + " input has multiple" 
                                                        + " MQ body bindings.",
                                                oper.getOperation().getName()));
                                    }
                                } else if (inputExt instanceof MQBCHeader) {
                                    gotMqBinding = true;
                                    MQBCHeader mqheader =  (MQBCHeader)inputExt;
                                    mqInput.addMQHeader(mqheader);
                                }
                            }
                            if (gotMqBinding) {
                                mqOperation.setMQOperationInput(mqInput);
                            }
                        }
                        
                        // Output
                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            MQOutput mqOutput = new MQOutput();
                            boolean gotMqBinding = false;
                            boolean gotBody = false;
                            for (Object extObj: bindingOutput.getExtensibilityElements()) {
                                ExtensibilityElement outputExt =
                                        (ExtensibilityElement) extObj;
                                if (outputExt instanceof MQBCBody) {
                                    gotMqBinding = true;
                                    if (!gotBody) {
                                        MQBCBody mqbody = (MQBCBody) outputExt;
                                        mqOutput.setMQMessage(mqbody);
                                        gotBody = true;
                                    } else {
                                        throw new Exception(I18n.msg(
                                                "0952: Invalid operation {0} -" 
                                                        + " output has multiple" 
                                                        + " MQ body bindings.",
                                                oper.getOperation().getName()));
                                    }
                                } else if (outputExt instanceof MQBCHeader) {
                                    gotMqBinding = true;
                                    MQBCHeader mqheader = (MQBCHeader) outputExt;
                                    mqOutput.addMQHeader(mqheader);
                                }
                            }
                            if (gotMqBinding && !gotBody) {
                                throw new Exception(I18n.msg(
                                        "0950: Invalid operation {0} -" 
                                                + " output lacks required" 
                                                + " MQ body binding.",
                                        oper.getOperation().getName()));
                            }
                            if (gotMqBinding) {
                                mqOperation.setMQOperationOutput(mqOutput);
                            }
                        }
                        
                        // Fault
                        Map<String, BindingFault> faults = oper.getBindingFaults();
                        if (faults != null) {
                            MQFault mqFault = null;
                            for (BindingFault fault : faults.values()) {
                                for (Object faultObj : fault.getExtensibilityElements()) {
                                    ExtensibilityElement faultExt =
                                            (ExtensibilityElement) faultObj;
                                    if (faultExt instanceof MQFault) {
                                        if (mqFault == null) {
                                            mqFault = (MQFault) faultExt;
                                        } else {
                                            throw new Exception(I18n.msg(
                                                    "0953: Invalid operation {0} -"
                                                            + " multiple MQ fault" 
                                                            + " bindings found.",
                                                    oper.getOperation().getName()));
                                        }
                                    }
                                }
                            }
                            if (mqFault != null) {
                                mqOperation.setMQOperationFault(mqFault);
                            }
                        }
                        
                        mqOperations.put(new QName(binding.getPortType()
                                .getQName().getNamespaceURI(), oper.getName()),
                                mqOperation);
                    }
                }
            }
        }
        return mqOperations;
    }

    private Map<QName, MQBCRedelivery> getMQRedelivery(Definition def,
                                                       String serviceName,
                                                       String endpointName) {

        Map<QName, MQBCRedelivery> mqRedeliveries = new HashMap<QName, MQBCRedelivery>();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            if (bindingOperations != null) {
                for (Object operObj : bindingOperations) {
                    BindingOperation oper = (BindingOperation) operObj;
                    if (oper != null) {
                        // Look for mq:redelivery entries
                        for (Object elemObj : oper.getExtensibilityElements()) {
                            ExtensibilityElement elem
                                    = (ExtensibilityElement) elemObj;
                            if (MQBCRedelivery.class.isInstance(elem)) {
                                MQBCRedelivery mqbcRedelivery = (MQBCRedelivery) elem;
                                mqRedeliveries.put(new QName(binding.getPortType()
                                        .getQName().getNamespaceURI(),
                                        oper.getName()), mqbcRedelivery);
                            }
                        }
                    }
                }
            }
        }
        return mqRedeliveries;
    }

    private Map<QName, String> getMQOperationMepType(Definition def,
            String serviceName,
            String endpointName,
            int direction) {
        Map<QName, String> mepTypes = new HashMap<QName, String>();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for mq:operation entries
                
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (MQBCOperation.class.isInstance(ee)) {
                        OperationType opType = oper.getOperation().getStyle();
                        if (opType != null) {
                            Operation operation = oper.getOperation();
                            if (operation.getInput() != null) {
                                if (operation.getOutput() != null) {
                                    opType = OperationType.REQUEST_RESPONSE;
                                } else {
                                    opType = OperationType.ONE_WAY;
                                }
                            }
                        }
                        String mep = determineMEP(direction, opType);
                        mepTypes.put(new QName(binding.getQName().getNamespaceURI(), oper.getName()), mep);
                    }
                }
            }
        }
        return mepTypes;
    }
    
    private String determineMEP(int direction, OperationType type) {
        String mep = Endpoint.EndpointMessageType.UNSUPPORTED;
        
        // Endpoint direction is irrelevant to exchange type,
        // for this component.
/*        
        if (direction == EndpointType.INBOUND) {
            if (OperationType.REQUEST_RESPONSE.equals(type)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            } else if (OperationType.ONE_WAY.equals(type)) {
                mep = Endpoint.EndpointMessageType.IN_ONLY;
            }
        } else {
            if (OperationType.REQUEST_RESPONSE.equals(type)) {
                mep = Endpoint.EndpointMessageType.OUT_IN;
            } else if (OperationType.ONE_WAY.equals(type)) {
                mep = Endpoint.EndpointMessageType.OUT_ONLY;
            }
        }
*/
        if (OperationType.REQUEST_RESPONSE.equals(type)) {
            mep = Endpoint.EndpointMessageType.IN_OUT;
        } else if (OperationType.ONE_WAY.equals(type)) {
            mep = Endpoint.EndpointMessageType.IN_ONLY;
        }
        return mep;
    }
        
    private void addEncoderMapping(QName operationName,
            javax.wsdl.Message wsdlMessage,
            MQBCBody mqBcBody,
            Map partMapping) throws Exception {
        String encodingStyle = null;
        Map parts = wsdlMessage.getParts();
        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
            Part aPart = (Part) partIter.next();
            String partName = aPart.getName();
            QName type = (aPart.getElementName() != null)? aPart.getElementName():aPart.getTypeName();
            
            // locate the XSD file based on the part type namespace
            String namespace = type.getNamespaceURI();
            String xsdFileLoc = getXsdFileLocation(namespace);
            if (xsdFileLoc != null) {
                if (mqBcBody.getMQMessageUse().equals(MQBCBody.MESSAGE_USE_TYPE_ENCODED)) {
                    encodingStyle = mqBcBody.getMQMsgBodyEncodingStyle();
                } else {
                    continue;
                }
                if (encodingStyle == null || encodingStyle.equals("")) {
                    throw new Exception(mMessages.getString("WCF_No_EncodingStyle_MQBCBody", operationName.toString()));
                }
                Encoder encoder = null;
                EncoderFactory encoderFactory = EncoderFactory.newInstance();
                MetaRef metaRef = encoderFactory.makeMeta(xsdFileLoc, type);
                if (mEncoderMap.get(metaRef) != null) {
                    encoder = (Encoder)mEncoderMap.get(metaRef);
                } else {
                    encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle),
                            metaRef);
                    mEncoderMap.put(metaRef, encoder);
                }
                partMapping.put(wsdlMessage.getQName().toString() + partName, encoder);
            }
        }
    }
    
    
    
    private String getXsdFileLocation(String aNamespace) {
        String xsdFileLoc = null;
        File aXsdFile = null;
        
        try {
            List<File> xsds = listResourceFiles(mRootPath, ".xsd");
            for (File xsdFile : xsds) {
                FastSchemaFactory factory = FastSchemaFactory.getInstance();
                FastSchema schema =
                        factory.newFastSchema(xsdFile.getAbsolutePath());
                if (aNamespace.equals(schema.getTargetNamespace())) {
                    xsdFileLoc = xsdFile.getAbsolutePath();
                    break;
                }
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, "WCF_cannot_parse_xsd", aXsdFile.getName());
        }
        
        return xsdFileLoc;
    }
    
}
