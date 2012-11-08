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

package com.sun.jbi.sapbc.packaging;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Endpoint;
import com.sun.jbi.sapbc.Endpoint.EndpointMessageType;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.EndpointImpl;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.SAPAddressClient;
import com.sun.jbi.sapbc.extensions.SAPAddressServer;
import com.sun.jbi.sapbc.extensions.SAPBinding;
import com.sun.jbi.sapbc.extensions.SAPEnvironmentalVars;
import com.sun.jbi.sapbc.extensions.SAPFmOperation;
import com.sun.jbi.sapbc.extensions.SAPIDocOperation;
import com.sun.jbi.sapbc.extensions.WSDLInput;
import com.sun.jbi.sapbc.extensions.WSDLOutput;
import com.sun.jbi.sapbc.extensions.SAPMessage;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingInput;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.BindingOutput;
import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLDocumentParserFactory;
import com.sun.wsdl.model.WSDLExtensibleElement;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.impl.WSDLDocumentParserFactoryImpl;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.Map;
import java.util.logging.Level;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import java.io.FileReader;
import java.net.URI;
import java.util.Collection;

import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints
 * based on the portmap list.
 *
 * @author Noel Ang (noel.ang@sun.com)
 */
public final class WSDLConfigurations {
    
    public WSDLConfigurations(final String rootPath) {
        if (rootPath == null) {
        //    OK if only using parsWSDL
        } else {
            File dir = new File(rootPath);
            if (!dir.isDirectory()) {
                throw new IllegalArgumentException(
                        mMessages.getString(
                        "WSDLConfigurations.Illegal_arg_not_directory",
                        rootPath)
                        );
            }
            mRootPath = dir;
        }
        
        //mXsds.addAll(SAPWSDLUtilities.listResourceFiles(mRootPath, XSDEXT));
    }
    
    //From ServiceUnitImpl
    public List<Endpoint> parseEndpoints(final List<EndpointData> portMaps, Map envVariableMap, boolean preprocess) 
            throws Exception {
        mPreprocess = preprocess;
        Map envVariables = (Map)((HashMap)envVariableMap).clone();
        setSAPEnvVars(new SAPEnvironmentalVars(envVariables));
        
        final CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(mRootPath.getAbsolutePath()
          + File.separator
          + "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        
        final EntityResolver resolver = new CatalogResolver(catalogManager);
        final ArrayList<Endpoint> endpoints = new ArrayList<Endpoint>();
        final List<File> wsdls = SAPWSDLUtilities.listResourceFiles(mRootPath, WSDLEXT);
        
        for (Iterator<File> wsdl = wsdls.iterator(); wsdl.hasNext(); ) {
            File wsdlFile = wsdl.next();
            mLogger.log(Level.INFO, "WSDLConfigurations.Processing_WSDL", wsdlFile.getAbsolutePath());
            endpoints.addAll(parseWSDL(wsdlFile, resolver, portMaps));
        }
        
        return endpoints;
    }
    
    public List<Endpoint> parseWSDL(final File wsdlFile,
            final EntityResolver resolver,
            final List<EndpointData> portMaps)
            throws Exception {
        
        final WSDLDefinitions wsdlDef = readWSDL(wsdlFile);
        wsdlDef.setName(wsdlFile.getName());
        final String tns = wsdlDef.getTargetNamespace();
        
        final ArrayList<Endpoint> endPoints = new ArrayList<Endpoint>();
        
        // Document object representation of the WSDL file.
        // This object is attached to every endpoint definition created in
        // the following process.
        final Document wsdlDoc;
        {
            DocumentBuilder documentBuilder =
                    DocumentBuilderFactory.newInstance().newDocumentBuilder();
            wsdlDoc = documentBuilder.parse(wsdlFile);
        }
        
        //debug int counter = 0;
        for (Iterator<EndpointData> portIter = portMaps.iterator(); portIter.hasNext(); ) {
            
            final EndpointData pm = portIter.next();
            mInterfaceQName = pm.getInterface(); //wsdl:portType
            mServiceQName = pm.getService(); //wsdl:service
            mEndpointQName = new QName("",pm.getEndpoint()); //wsdl:port
            
            //debug mLogger.info("##########debug looking for service binding in ["+wsdlFile.getName()+"] to match portMap endpoint ["+pm.getEndpoint()+"] service ["+pm.getService().toString()+"] interface ["+pm.getInterface().toString()+"]");
            //debug mLogger.info("##########debug counter ["+counter+"] wsdlFile ["+wsdlFile.getName()+"]");
            //debug mLogger.info("##########debug serviceq ["+mServiceQName.toString()+"] endpointQ ["+mEndpointQName.toString()+"] interface ["+pm.getInterface()+"]");
            //debug counter++;
            
            // Look through all the definitions to see if I can find a
            // service binding that matches this endpoint.
            
            // Skip endpoints without sap:binding elements
            final Binding wsdlBinding = SAPWSDLUtilities.findBindingDefinition(wsdlDef, mServiceQName, mEndpointQName);
            if (wsdlBinding == null) {
                // This is not a SAP WSDL
                mLogger.log(Level.WARNING, "WSDLConfigurations.Could_not_find_service", new Object[] {mServiceQName.toString(),pm.getEndpoint(), wsdlFile.getAbsolutePath()});
                mLogger.log(Level.WARNING, "WSDLConfigurations.Next_endpoint");
                continue;
            }
            final SAPBinding binding = findSAPBinding(wsdlBinding, mServiceQName, pm.getEndpoint());
            if (binding == null) {
                // This is not a SAP WSDL
                mLogger.log(Level.WARNING, "WSDLConfigurations.No_SAP_binding", new Object[] {wsdlBinding.getName()});
                mLogger.log(Level.WARNING, "WSDLConfigurations.Next_endpoint");
                continue;
            }
            
            
            // Find sap:address element
            final SAPAddress address = findSAPAddress(wsdlDef, mServiceQName, pm.getEndpoint());
            if (address == null) {
                throw new Exception(
                        mMessages.getString(
                        "WSDLConfigurations.Failed_parse_missing_sapaddress",
                        mServiceQName
                        )
                        );
            }
            
            // Find sap:fmoperation & sap:idocoperation elements
            //final Map<QName, SAPFmOperation> ops = findSAPOperations(wsdlDef, mServiceQName, pm.getEndpoint());
            //Allow for both SAPFmOperation and SAPIDocOperation
            final Map ops = findSAPOperations(wsdlBinding);
            if (ops == null || ops.size() == 0) {
                throw new Exception(
                        mMessages.getString(
                        "WSDLConfigurations.Failed_parse_missing_sapops",
                        mServiceQName
                        )
                        );
            }
            
            // Discover message exchange patterns used in the description
            final PortType wsdlPortType = wsdlBinding.getWSDLPortType();
            final Map opMeps = findOperationMepTypes(wsdlPortType,pm.getDirection());
            
            
            // This port passes muster: it refers to the services of the SAP BC.
            // Create an endpoint definition for it.
            Endpoint endpoint = new EndpointImpl(
                    pm.getEndpoint(),
                    mServiceQName,
                    wsdlDef,
                    pm.getDirection());
            endpoint.setSAPAddress(address);
            endpoint.setSAPBinding(binding);
            endpoint.setSAPOperations(ops);
            endpoint.setOperationMsgExchangePattern(opMeps);
            endpoint.setServiceDescription(wsdlDoc);
            //debug mLogger.severe("############debug adding endpoint ["+endpoint.getEndpointName()+"]");
            endPoints.add(endpoint);
        }
        
        return endPoints;
    }
    
    /**
     * Return the SAP binding extension information from the WSDL definition.
     *
     * @return SAPBinding object or null
     *
     * @throws IllegalArgumentException if serviceName or endpointName is null.
     */
    protected SAPBinding findSAPBinding(
            final Binding binding,
            final QName serviceName,
            final String endpointName)
            throws WSDLException {
        
        SAPBinding sapBinding = null;
        
        if (binding != null) {
            sapBinding = (SAPBinding) findExtensibilityElement(binding, SAPBinding.class);
        } else {
            mLogger.log(Level.WARNING, "WSDLConfigurations.Failed_find_X_Definition", new Object[] {"Binding",serviceName.toString(), endpointName});
        }
        return sapBinding;
    }
    
    /**
     * Return the SAP address extension information from the WSDL definition.
     *
     * @return SAPAddress object or null
     *
     * @throws NullPointerException if wsdlDef is null.
     * @throws IllegalArgumentException if serviceName or endpointName is null.
     */
    private SAPAddress findSAPAddress(
            final WSDLDefinitions def,
            final QName serviceName,
            final String endpointName)
            throws WSDLException {
        
        final Service service = SAPWSDLUtilities.findServiceDefinition(def, mServiceQName);
        
        SAPAddress address = null;
        SAPAddressClient client = null;
        SAPAddressServer server = null;
        
        if (service != null) {
            final ServicePort port = SAPWSDLUtilities.findServicePortDefinition(service, mEndpointQName);
            if (port != null) {
                address = (SAPAddress) findExtensibilityElement(port, SAPAddress.class);
            }
        }
        return address;
    }
    
    protected Map findSAPOperations(final Binding binding)
    throws WSDLException{
        
        final Map operations = new HashMap();
        
        if (binding != null) {
            
            // Find all operations that:
            // 1. are sap:fmoperation operations
            // 2. possess operations with sap:message input bindings
            // 3. possess operations with sap:message output bindings
            //
            // Return a <SAP Operation> --> <Binding Operation> map of them
            
            // Find all sap:fmoperation operations
            Map<SAPFmOperation, BindingOperation> sapFmOps =
                    makeSapFmOperationsMap(binding.getBindingOperations());
            
            // Initialize all sap:fmoperation operations with their
            // sap:message and sap:message input/output bindings
            for (Iterator<Map.Entry<SAPFmOperation, BindingOperation>> iter = sapFmOps.entrySet().iterator()
            ; iter.hasNext()
            ; ) {
                Map.Entry<SAPFmOperation, BindingOperation> opEntry = iter.next();
                SAPFmOperation sapOp = opEntry.getKey();
                BindingOperation bindOp = opEntry.getValue();
                BindingInput bindIn = bindOp.getBindingInput();
                BindingOutput bindOut = bindOp.getBindingOutput();
                
                if (bindIn != null) {
                    SAPMessage sapMessage = (SAPMessage) findExtensibilityElement(bindIn, SAPMessage.class);
                    if (sapMessage != null) {
                        WSDLInput wsdlInput = new WSDLInput();
                        wsdlInput.setSAPMessage(sapMessage);
                        sapOp.setSAPOperationInput(wsdlInput);
                    }
                }
                
                if (bindOut != null) {
                    SAPMessage sapMessage = (SAPMessage) findExtensibilityElement(bindOut, SAPMessage.class);
                    if (sapMessage != null) {
                        WSDLOutput wsdlOutput = new WSDLOutput();
                        wsdlOutput.setSAPMessage(sapMessage);
                        sapOp.setSAPOperationOutput(wsdlOutput);
                    }
                }
                
                operations.put(QName.valueOf(bindOp.getName()), sapOp);
            }
            
            // Find all operations that:
            // 1. are sap:idocoperation operations
            // 2. possess operations with sap:message input bindings
            // 3. possess operations with sap:message output bindings
            //
            // Return a <SAP Operation> --> <Binding Operation> map of them
            
            // Find all sap:idocoperation operations
            Map<SAPIDocOperation, BindingOperation> sapIDocOps =
                    makeSapIDocOperationsMap(binding.getBindingOperations());
            
            // Initialize all sap:idocoperation operations with their
            // sap:message and sap:message input/output bindings
            for (Iterator<Map.Entry<SAPIDocOperation, BindingOperation>> iter = sapIDocOps.entrySet().iterator()
            ; iter.hasNext()
            ; ) {
                Map.Entry<SAPIDocOperation, BindingOperation> opEntry = iter.next();
                SAPIDocOperation sapOp = opEntry.getKey();
                BindingOperation bindOp = opEntry.getValue();
                BindingInput bindIn = bindOp.getBindingInput();
                BindingOutput bindOut = bindOp.getBindingOutput();
                
                if (bindIn != null) {
                    SAPMessage sapMessage = (SAPMessage) findExtensibilityElement(bindIn, SAPMessage.class);
                    if (sapMessage != null) {
                        WSDLInput wsdlInput = new WSDLInput();
                        wsdlInput.setSAPMessage(sapMessage);
                        sapOp.setSAPOperationInput(wsdlInput);
                    }
                }
                
                if (bindOut != null) {
                    SAPMessage sapMessage = (SAPMessage) findExtensibilityElement(bindOut, SAPMessage.class);
                    if (sapMessage != null) {
                        WSDLOutput wsdlOutput = new WSDLOutput();
                        wsdlOutput.setSAPMessage(sapMessage);
                        sapOp.setSAPOperationOutput(wsdlOutput);
                    }
                }
                
                operations.put(QName.valueOf(bindOp.getName()), sapOp);
            }
        }
        return operations;
    }
    
    
    /**
     * Determine the message exchange pattern of each specified operation,
     * in the context of an endpoint's type/direction.
     *
     * @param def          The source WSDL definition to examine
     * @param endpointType Endpoint type/direction to use as orientation context
     *                     to qualify determination of message exchange patterns
     *
     * @return Mapping between each operation of the specified endpoint and
     *         their message exchange pattern.
     *         Map keys: Operationas as QName.
     *         Map values: Message exchange patterns as Strings.
     *
     * @see {@link com.sun.jbi.sapbc.Endpoint.EndpointMessageType}
     */
    private Map<QName, String> findOperationMepTypes(final PortType portType,
            final EndpointType endpointType) {
        
        final Map<QName, String> mepTypes = new HashMap<QName, String>();
        
        //For function module operations
        for (Iterator<Operation> iter = portType.getOperations().iterator(); iter.hasNext(); ) {
            Operation portTypeOp = (Operation) iter.next();
            String mep = EndpointMessageType.UNSUPPORTED;
            int opType = Operation.UNKNOWN_OPERATION;
            if (portTypeOp != null) {
                opType = portTypeOp.getOperationType();
                if (opType == Operation.UNKNOWN_OPERATION) {
                    // For some reason, sometimes the operation type is not set,
                    // so the BC will determine the exchange pattern implicitly,
                    // based on the presence of specific elements in the WSDL
                    // definition.
                    //
                    boolean haveInput = portTypeOp.getInput() != null;
                    boolean haveOutput = portTypeOp.getOutput() != null;
                    boolean oneWayExchange = haveInput && !haveOutput;
                    
                    if (oneWayExchange) {
                        opType = Operation.ONE_WAY_OPERATION;
                    } else if (haveInput && haveOutput) {
                        opType = Operation.REQUEST_RESPONSE_OPERATION;
                    }
                }
                mep = determineMep(endpointType, opType);
            }
            mepTypes.put(QName.valueOf(portTypeOp.getName()), mep);
        }
        
        return mepTypes;
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
     * @see com.sun.wsdl.model.Operation
     */
    protected String determineMep(EndpointType direction, int type) {
        String mep = EndpointMessageType.UNSUPPORTED;
        
/*
UNKNOWN_OPERATION = -1;
ONE_WAY_OPERATION = 1;
REQUEST_RESPONSE_OPERATION = 2;
REQUEST_RESPONSE_ONE_WAY_OPERATION = 3;
SOLICIT_RESPONSE_OPERATION = 4;
NOTIFICATION_OPERATION = 8;
SOLICIT_RESPONSE_NOTIFICATION_OPERATION = 12;
 */
        
        switch (direction) {
            case INBOUND:
                if (Operation.REQUEST_RESPONSE_ONE_WAY_OPERATION == type) {
                    mep = EndpointMessageType.IN_OUT;
                } else if (Operation.ONE_WAY_OPERATION == type) {
                    mep = EndpointMessageType.IN_ONLY;
                } else {
                    mep = EndpointMessageType.UNSUPPORTED;
                }
                break;
            case OUTBOUND:
                if (Operation.REQUEST_RESPONSE_ONE_WAY_OPERATION == type) {
                    mep = EndpointMessageType.OUT_IN;
                } else if (Operation.SOLICIT_RESPONSE_OPERATION == type) {
                    mep = EndpointMessageType.OUT_ONLY;
                } else if (Operation.ONE_WAY_OPERATION == type) {
                    mep = EndpointMessageType.OUT_ONLY;
                } else if (Operation.NOTIFICATION_OPERATION == type) {
                    mep = EndpointMessageType.OUT_ONLY;
                } else {
                    mep = EndpointMessageType.UNSUPPORTED;
                }
                break;
        }
        
        return mep;
    }
    
    /**
     * Creates a map of sap:operation extensibility elements found in the
     * specified set of binding operations.
     *
     *
     * @param ops List of operations
     * @return A mapping of sap:fmoperation extensibility elements to the
     *         binding operations under which they were discovered.
     *         Map key type: com.sun.jbi.sapbc.extensions.SAPFmOperation.
     *         Map value type: com.sun.wsdl.model.BindingOperation.
     */
    private Map<SAPFmOperation, BindingOperation> makeSapFmOperationsMap(final Collection<BindingOperation> ops)
    throws WSDLException {
        final Map<SAPFmOperation, BindingOperation> map = new HashMap<SAPFmOperation, BindingOperation>();
        
        for (Iterator<BindingOperation> iter = ops.iterator(); iter.hasNext(); ) {
            BindingOperation op = iter.next();
            SAPFmOperation sapOp = (SAPFmOperation) findExtensibilityElement(op, SAPFmOperation.class);
            if (sapOp != null) {
                map.put(sapOp, op);
            }
        }
        
        return map;
    }
    
    private Map<SAPIDocOperation, BindingOperation> makeSapIDocOperationsMap(final Collection<BindingOperation> ops)
    throws WSDLException {
        final Map<SAPIDocOperation, BindingOperation> map = new HashMap<SAPIDocOperation, BindingOperation>();
        
        for (Iterator<BindingOperation> iter = ops.iterator(); iter.hasNext(); ) {
            BindingOperation op = iter.next();
            SAPIDocOperation sapOp = (SAPIDocOperation) findExtensibilityElement(op, SAPIDocOperation.class);
            if (sapOp != null) {
                map.put(sapOp, op);
            }
        }
        
        return map;
    }
    
    /**
     * Searches an extensible element's extensions for one that is of the
     * specified type.
     *
     * @param extensible The element whose extensibility elements are searched
     * @param type       The class of the qualifying extensibility element
     *
     * @return The first extensibility element that matches the specified type,
     *         or null if none is found.
     *
     * @throws NullPointerException if extensible is null.
     */
    private Object findExtensibilityElement(final WSDLExtensibleElement extensible, final Class type)
    throws WSDLException{
        Object extElem = null;
        
        Collection<ExtensibilityElement> extElems = extensible.getExtensibilityElements();
        if (extElems != null) {
            //debug mLogger.info("#####debug Found ["+extElems.size()+"] extElems for ["+extensible.getQualifiedName()+"] for type ["+type.getName()+"]");
            for (Iterator<ExtensibilityElement> iter = extElems.iterator();iter.hasNext();) {
                Object obj = iter.next();
                ExtensibilityElement e = (ExtensibilityElement) obj;
                Map attrMap = e.getOtherAttributes();
                if (type.getName().equals("com.sun.jbi.sapbc.extensions.SAPBinding") && e.getElementType().equals(SAPBinding.TYPE)) {
                    if (mPreprocess)
                        getSAPEnvVars().preProcessEnvVars(SAPBinding.TYPE, attrMap);
                    else
                        extElem =  new com.sun.jbi.sapbc.extensions.SAPBinding(e, getSAPEnvVars());
                } else if (type.getName().equals("com.sun.jbi.sapbc.extensions.SAPAddress") && e.getElementType().equals(SAPAddress.TYPE)) {
                    if (mPreprocess) {
                        getSAPEnvVars().preProcessEnvVars(SAPAddress.TYPE, attrMap);
                        getSAPEnvVars().preProcessEnvVars(SAPAddressClient.TYPE, attrMap);
                        getSAPEnvVars().preProcessEnvVars(SAPAddressServer.TYPE, attrMap);
                    } else
                        extElem =  new com.sun.jbi.sapbc.extensions.SAPAddress(e, getSAPEnvVars());
                } else if (type.getName().equals("com.sun.jbi.sapbc.extensions.SAPMessage") && e.getElementType().equals(SAPMessage.TYPE)) {
                    extElem =  new com.sun.jbi.sapbc.extensions.SAPMessage(e);
                } else if (type.getName().equals("com.sun.jbi.sapbc.extensions.SAPFmOperation") && e.getElementType().equals(SAPFmOperation.TYPE)) {
                    extElem =  new com.sun.jbi.sapbc.extensions.SAPFmOperation(e);
                } else if (type.getName().equals("com.sun.jbi.sapbc.extensions.SAPIDocOperation") && e.getElementType().equals(SAPIDocOperation.TYPE)) {
                    extElem =  new com.sun.jbi.sapbc.extensions.SAPIDocOperation(e);
                } else {
                    //debug mLogger.info("#############debug NOT EQUAL");
                }
                
            }
        } else {
            //debug mLogger.info("#############debug extElems == null");
        }
        if (extElem == null)
            mLogger.log(Level.WARNING, "WSDLConfigurations.Failed_find_Extensibility_Element", type.getName());
        return extElem;
    }
    
    
    /**
     * Parses the contents of a file as a WSDL document.
     *
     * @return WSDLDefinitions representation of the WSDL document
     */
    public WSDLDefinitions readWSDL(final File file)
    throws java.lang.Exception {
        mLogger.finer("Reading from WSDL file ["+file.getAbsolutePath()+"]");
        WSDLDocument doc = loadWSDL(file);
        return doc.getDocumentDefinitions();
    }
    
    /* Seems like what needs to be added is the processing of the envVariableMap against the WSDL
     * does the Definition contain the envVariableMap, what is the envVarMap's relationship to the 
     * Definition?  What is FileExtPreprocessDeserializer's relationship to FileExtRegistry?
     * If I can get the info from WSDLDocument am I good to go?
     * /
  
    /*  From file
    protected Definition readWSDL(File f, EntityResolver resolver, Map envVariableMap) throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader =
                ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        //THIS IS WHAT IS NEW
        reader.setExtensionRegistry(new FileExtensionRegistry(envVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
    
     */
    
    private WSDLDocument loadWSDL(final File file) throws Exception {
        URI uri = file.toURI();
        WSDLDocumentParserFactory factory = new WSDLDocumentParserFactoryImpl();
        WSDLDocument doc = factory.load( new FileReader(new File(uri)), uri.toString());
        return doc;
    }
    
    /**
     * Retrieves the SAPEnvironmentalVars object.
     *
     * @return SAPEnvironmentalVars holds information on the environmental
     * variable map
     */
    public SAPEnvironmentalVars getSAPEnvVars() {
        return mEnvVars;
    }
    
    /**
     * Sets the SAPEnvironmentalVars object.
     *
     * @p WSDLDefinitions representation of the WSDL document
     */
    public void setSAPEnvVars(SAPEnvironmentalVars ev) {
        mEnvVars = ev;
    }
    
    private QName mInterfaceQName;
    private QName mServiceQName;
    private QName mEndpointQName;
    
    private static final Messages mMessages =
            Messages.getMessages(WSDLConfigurations.class);
    
    private static final Logger mLogger =
            Messages.getLogger(WSDLConfigurations.class);
    
    private static final String WSDLEXT =
            ".".concat(mMessages.getString("WSDLConfigurations.Extension_wsdl"));
    
    private static final String XSDEXT =
            ".".concat(mMessages.getString("WSDLConfigurations.Extension_xsd"));
    
    private File mRootPath = null;
    private final List<File> mXsds = new ArrayList();
    private final Map mEncoderMap = new HashMap();
    
    private SAPEnvironmentalVars mEnvVars = null;
    
    private boolean mPreprocess;
    
}
