/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc.packaging;

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

import com.sun.jbi.dcombc.Endpoint;
import com.sun.jbi.dcombc.EndpointImpl;
import com.sun.jbi.dcombc.Endpoint.EndpointType;
import com.sun.jbi.dcombc.Endpoint.EndpointState;
import com.sun.jbi.dcombc.Endpoint.EndpointMessageType;
import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMOutput;
import com.sun.jbi.dcombc.extensions.DCOMMessage;
import com.sun.jbi.dcombc.extensions.DCOMAddress;
import com.sun.jbi.dcombc.extensions.DCOMBinding;
import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.dcombc.extensions.DCOMExtensionRegistry;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints based on the portmap list.
 * 
 * @author Chandrakanth Belde
 */
public class WSDLConfigurations {
	/**
	 *
	 */
    private static Logger mLogger = Messages.getLogger(WSDLConfigurations.class);

    private static final Messages mMessages = Messages.getMessages(WSDLConfigurations.class);

    private String mRootPath;

    private List mXsds = new ArrayList();

    private Map mEncoderMap = new HashMap();
	
	/**
	 * Constructor
	 */
    public WSDLConfigurations(String rootPath) {
        mRootPath = rootPath;
    }
    
    /**
     * 
     * @param portMaps
	 * @envVariableMap
     * 
     * @return
     * @throws Exception
     */
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

	/*public Map parseForEnvironmentVariables(List portMaps, Map envVariableMap) throws Exception {
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
        reader.setExtensionRegistry(new DCOMExtensionRegistry(envVariableMap));
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    /**
     * 
     * @param wsdlFile
     * @param resolver
     * @param portMaps
     * 
     * @return
     * @throws Exception
     */
    public List parseWSDL(File wsdlFile, 
    					  EntityResolver resolver, 
    					  List portMaps,
						  Map envVariableMap) throws Exception {

        Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData) it.next();

            // Check the Definition if it has a DCOMBinding. If not,
            // continue
            DCOMBinding binding = getDCOMBinding(def, pm.getService(), pm.getEndpoint());
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint. If we find one,
            // extract file details

            // If we have a DCOMBinding, we must have an DCOMAddress.
            DCOMAddress address = getDCOMAddress(def, pm.getService(), pm.getEndpoint());
            if (address == null) {
                String msg = "Missing dcom:address in wsdl for this service " + pm.getService();
                throw new Exception(msg);
            }

            // If we have an DCOMBinding, we must have operations
            Map dcomOperations = getDCOMOperations(def, pm.getService(), pm.getEndpoint(), pm.getDirection());
            if (dcomOperations == null || dcomOperations.size() == 0) {
                String msg = "Missing dcom:operation definition(s) in wsdl for this service " + pm.getService();
                throw new Exception(msg);
            }

            Map opMsgExchangePatterns = getDCOMOperationMepTypes(def, pm.getService(), pm.getEndpoint(),
                    pm.getDirection());

            // Create an Endpoint for each Port. The endpoint should have the
            // correct DCOMBinding, the associated Operations, and the
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
            endpoint.setDCOMAddress(address);
            endpoint.setDCOMBinding(binding);
            endpoint.setDCOMOperations(dcomOperations);
            endpoint.setOperationMsgExchangePatterns(opMsgExchangePatterns);

            setInputsOutputs(def, pm.getService(), pm.getEndpoint(), dcomOperations.values(), endpoint);

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
    
    /**
     * 
     * @param f
     * @param resolver
     * 
     * @return
     * @throws javax.wsdl.WSDLException
     */
    protected Definition readWSDL(File f, EntityResolver resolver) 
    							throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new DCOMExtensionRegistry());
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }
    
    /**
     * 
     * @param def
     * @param serviceName
     * @param endpointName
     * 
     * @return
     */
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
    
    /**
     * 
     * @param def
     * @param serviceName
     * @param endpointName
     * 
     * @return
     */
    protected DCOMAddress getDCOMAddress(Definition def, String serviceName, String endpointName) {
        DCOMAddress address = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();

            // Look for dcom:address

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (DCOMAddress.class.isInstance(ee)) {
                    address = (DCOMAddress) ee;
                }
            }
        }
        return address;
    }
    
    /**
     * 
     * @param def
     * @param serviceName
     * @param endpointName
     * 
     * @return
     */
    protected DCOMBinding getDCOMBinding(Definition def, String serviceName, String endpointName) {
        DCOMBinding dcomBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            // Look for dcom:binding

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && dcomBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (DCOMBinding.class.isInstance(ee)) {
                    dcomBinding = (DCOMBinding) ee;
                }
            }
        }
        return dcomBinding;
    }

    /**
     * 
     * @param def
     * @param serviceName
     * @param endpointName
     * @param isInbound
     * 
     * @return
     */
    protected Map getDCOMOperations(Definition def, String serviceName, String endpointName, int isInbound) {
        Map dcomOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();

            // Look for dcom:operation

            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement operExt = (ExtensibilityElement) extIter.next();
                    if (DCOMOperation.class.isInstance(operExt)) {
                        DCOMOperation dcomOperation = (DCOMOperation) operExt;
                        dcomOperation.setBindingOperation(oper);
                        dcomOperation.setMEP(determineMEP(isInbound, oper));
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator inputIter = bindingInput.getExtensibilityElements().iterator();
                            while (inputIter.hasNext()) {
                                ExtensibilityElement inputExt = (ExtensibilityElement) inputIter.next();
                                DCOMInput dcomInput = null;
                                if (inputExt instanceof DCOMMessage) {
                                    dcomInput = new DCOMInput();
                                    DCOMMessage dcomMessage = (DCOMMessage) inputExt;
                                    dcomInput.setDCOMMessage(dcomMessage);
                                }
                                if (dcomInput != null) {
                                    dcomOperation.setDCOMOperationInput(dcomInput);
                                }
                            }
                        }

                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator outputIter = bindingOutput.getExtensibilityElements().iterator();
                            while (outputIter.hasNext()) {
                                ExtensibilityElement outputExt = (ExtensibilityElement) outputIter.next();
                                DCOMOutput dcomOutput = null;
                                if (outputExt instanceof DCOMMessage) {
                                    dcomOutput = new DCOMOutput();
                                    DCOMMessage dcomMessage = (DCOMMessage) outputExt;
                                    dcomOutput.setDCOMMessage(dcomMessage);
                                }

                                if (dcomOutput != null) {
                                    dcomOperation.setDCOMOperationOutput(dcomOutput);
                                }
                            }
                        }
                        dcomOperations.put(QName.valueOf(oper.getName()), dcomOperation);
                    }
                }
            }
        }
        return dcomOperations;
    }
    
    /**
     * 
     * @param def
     * @param serviceName
     * @param endpointName
     * @param operations
     * @param endpoint
     */
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
                // Look for dcom:operation entries

                DCOMOperation dcomOperation = null;
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (operations.contains(ee)) {
                        dcomOperation = (DCOMOperation) ee;
                    }
                }

                if (dcomOperation != null) {
                    BindingInput bindingInput = oper.getBindingInput();
                    if (bindingInput != null) {
                        Iterator it = bindingInput.getExtensibilityElements().iterator();
                        while (it.hasNext()) {
                            ExtensibilityElement ee = (ExtensibilityElement) it.next();
                            if (ee instanceof DCOMMessage) {
                                DCOMInput dcomInput = new DCOMInput();
                                DCOMMessage dcomMessage = (DCOMMessage) ee;
                                dcomInput.setDCOMMessage(dcomMessage);
                                endpoint.setDCOMOperationInput(dcomOperation, dcomInput);
                            }
                        }
                    }
                }

                BindingOutput bindingOutput = oper.getBindingOutput();
                if (bindingOutput != null) {
                    Iterator it2 = bindingOutput.getExtensibilityElements().iterator();
                    while (it2.hasNext()) {
                        ExtensibilityElement ee = (ExtensibilityElement) it2.next();
                        if (ee instanceof DCOMMessage) {
                            DCOMOutput dcomOutput = new DCOMOutput();
                            DCOMMessage dcomMessage = (DCOMMessage) ee;
                            dcomOutput.setDCOMMessage(dcomMessage);
                            endpoint.setDCOMOperationOutput(dcomOperation, dcomOutput);
                        }
                    }
                }

            }
        }
    }

    /**
     * 
     * @param def
     * @param serviceName
     * @param endpointName
     * @param direction
     * 
     * @return
     */
    protected Map getDCOMOperationMepTypes(Definition def, 
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
                    if (DCOMOperation.class.isInstance(ee)) {
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
    
    /**
     * 
     * @param direction
     * @param type
     * 
     * @return
     */
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

    /**
     * 
     * @param aNamespace
     * 
     * @return
     */
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
}
