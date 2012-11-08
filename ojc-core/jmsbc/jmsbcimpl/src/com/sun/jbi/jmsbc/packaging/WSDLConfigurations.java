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

package com.sun.jbi.jmsbc.packaging;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Definition;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
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

import com.ibm.wsdl.factory.WSDLFactoryImpl;

import com.sun.jbi.jmsbc.Endpoint;
import com.sun.jbi.jmsbc.EndpointImpl;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSBinding;
import com.sun.jbi.jmsbc.extensions.JMSExtensionRegistry;
import com.sun.jbi.jmsbc.extensions.JMSJNDIEnv;
import com.sun.jbi.jmsbc.extensions.JMSJNDIEnvEntry;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSMessage;
import com.sun.jbi.jmsbc.extensions.JMSInput;
import com.sun.jbi.jmsbc.extensions.JMSOutput;
import com.sun.jbi.jmsbc.extensions.JMSExtSerializer;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfigurationMBean;
import com.sun.jbi.jmsbc.validator.EndpointValidator;

import com.sun.jbi.internationalization.Messages;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.encoder.tools.xml.XsdLocator;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints
 * based on the portmap list.
 *
 */
public class WSDLConfigurations {

    private static final Messages mMessages =
        Messages.getMessages(WSDLConfigurations.class);
    private static final Logger mLogger =
        Messages.getLogger(WSDLConfigurations.class);
    
    private String mSUID;
    
    private String mRootPath;
    private Map mEncoderMap = new HashMap();
    private List mWSDLs = new ArrayList();
    private List mXSDs = new ArrayList();
    private JMSBCRuntimeConfigurationMBean mRuntimeConfig;

    public WSDLConfigurations (String suID, String rootPath, JMSBCRuntimeConfigurationMBean runtimeConfig) {
        mRootPath = rootPath;
        mSUID = suID;
        mRuntimeConfig = runtimeConfig;
    }

    public List parse(List portMaps, Map envVariableMap, Map configMap)
        throws Exception {
    	Map envVariables = (Map)((HashMap)envVariableMap).clone();

        File dir = new File(mRootPath);
        File catalog = new File(dir.getAbsolutePath() +
            File.separator + "meta-inf" + File.separator +
            "catalog.xml");
        
        EntityResolver resolver = null;

        if ( catalog.exists() ) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);
            resolver = new CatalogResolver(catalogManager);
        }

        ArrayList endpoints = new ArrayList();
        Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
        mXSDs = listResourceFiles(dir, ".xsd");
        while (wsdls.hasNext()) {
            endpoints.addAll(parseWSDL((File)wsdls.next(), resolver, portMaps, envVariables, configMap));
        }

        return endpoints;
    }

    public Map parseForEnvironmentVariables(List portMaps, Map envVariableMap) throws Exception {
    	Map envVariables = (Map)((HashMap)envVariableMap).clone();
        File dir = new File(mRootPath);
        File catalog = new File(dir.getAbsolutePath() +
            File.separator + "meta-inf" + File.separator +
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
                          Map envVariableMap,
                          Map configMap) 
        throws Exception {

        Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData)it.next();

            // Check the Definition if it has a JMSBinding.  If not,
            // continue
            JMSBinding binding = getJMSBinding(def,
                                               pm.getService(),
                                               pm.getEndpoint());
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // extract file details

            // If we have a JMSBinding, we must have an JMSAddress.
            JMSAddress address =
                getJMSAddress(def, pm.getService(), pm.getEndpoint());
            
            if (address == null) {                
                String errMsg = mMessages.getString("JMSBC-E0401.MissingJMSAddress",
                            new Object[]{binding.getBinding().getQName(),
                                         pm.getEndpoint(),
                                         pm.getService(),
                                         wsdlFile.getName()});

                throw new InvalidConfigurationException (errMsg);
            }
                                
            // Create an Endpoint for each Port.  The endpoint should have the
            // correct JMSBinding, the associated Operations, and the
            // associated OperationInput and OperationOutput
            Endpoint endpoint = new EndpointImpl();

            // Store the su ID
            endpoint.setServiceUnitID(mSUID);
            
            // Store the endpoint name
            endpoint.setEndpointName(pm.getEndpoint());
            
            // Store the Service name
            endpoint.setServiceName(QName.valueOf(pm.getService()));

            // Store the Definition
            endpoint.setDefinition(def);
                        
            // Store the type of Endpoint
            endpoint.setEndpointType(pm.getDirection());
            //Store application configuration name
            endpoint.setApplicationConfigurationName(pm.getApplicationConfigurationName());
            if(pm.getApplicationConfigurationName() != null && pm.getApplicationConfigurationName().trim().length()!=0){
            	String config = pm.getApplicationConfigurationName().trim();
            	Object[] objArr = (Object[])configMap.get(config);
            	if(objArr != null){
            		JMSAddress newAddress = new JMSAddress();
            		newAddress.setConnectionURL((String)objArr[0]);
            		newAddress.setConnectionFactoryName((String)objArr[1]);
            		newAddress.setInitialContextFactory((String)objArr[2]);
            		newAddress.setProviderURL((String)objArr[3]);
            		newAddress.setSecurityPrincipal((String)objArr[4]);
            		newAddress.setSecurityCredentials((String)objArr[5]);
            		newAddress.setUsername((String)objArr[6]);
            		newAddress.setPassword((String)objArr[7]);
            		
            		JMSJNDIEnv env = new JMSJNDIEnv();
            		newAddress.setJndiEnv(env);
            		
            		String entries = (String)objArr[8];
            		if(entries!=null){
            			HashMap<String, JMSJNDIEnvEntry> jndiMap = new HashMap<String, JMSJNDIEnvEntry>();
            			for(StringTokenizer tokenizer = new StringTokenizer(entries, "&"); tokenizer.hasMoreTokens();){
            				String nameValue = tokenizer.nextToken().trim();
            				if(nameValue.length() == 0 || nameValue.equals("="))
            					continue;
            				String name = "";
            				String value = "";
            				int index = nameValue.indexOf('=');
            				if(index == -1){
            					name = nameValue;
            				}else{
            					name = nameValue.substring(0, index).trim();
            					if(index < (nameValue.length() - 1)){
            						value = nameValue.substring(index + 1, nameValue.length()).trim();
            					}
            				}
                            JMSJNDIEnvEntry jndienventry = new JMSJNDIEnvEntry();
                            jndienventry.setName(name.replaceAll("%26", "&"));
                            jndienventry.setValue(value.replaceAll("%26", "&"));
        					jndiMap.put(jndienventry.getName(),jndienventry);
            			}
            			if(jndiMap.size() > 0){
            				env.setJNDIEnvEntries(jndiMap);
            			}
            		}
            		//set address to the new value
            		address = newAddress;
            	}else{
            		//Throw an exception
                    throw new InvalidConfigurationException (mMessages.getString("JMSBC-E0409.ApplicationConfigurationIsNotAvailable",
                            new Object[]{config}));
            	}
            }

            // Set the description
            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder documentBuilder = docBuilderFactory.newDocumentBuilder();
            Document result = documentBuilder.parse(wsdlFile);
            endpoint.setServiceDescription(result);

            // Store our extensibility elements
            endpoint.setJMSAddress(address);
            endpoint.setJMSBinding(binding);
            
            // Traverse operations and their inputs and outputs and store them
            setJMSOperationsAndInputsOutputs(def,
                                             pm.getService(),
                                             pm.getEndpoint(),
                                             pm.getDirection()==Endpoint.EndpointType.INBOUND,
                                             endpoint,
                                             wsdlFile.getName());

            EndpointValidator.validateEndpointForUniqueness(endPoints, endpoint, false);
            
            // Now add the Endpoint to our list of Endpoints
            endPoints.add(endpoint);
        }

        return endPoints;
    }

    public void clearEncoderCache() {
        mEncoderMap.clear();
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
    
    protected Definition readWSDL(File f, EntityResolver resolver, Map envVariableMap)
        throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() :
            ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        // Suppress WSDL4j System.out logs and control the logging based on the current
        // logger logging level setting
        if (mLogger.isLoggable(Level.FINE)) {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);                         
        } else {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
        }
        JMSExtSerializer jmsExtSerializer = new JMSExtSerializer(envVariableMap, true, mRuntimeConfig);
        JMSExtensionRegistry jmsExtRegistry = new JMSExtensionRegistry(jmsExtSerializer);
        reader.setExtensionRegistry(jmsExtRegistry);
        Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
    
    protected Map readWSDLForEnvVariables(File f, EntityResolver resolver, Map envVariableMap) 
        throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() :
                ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        // Suppress WSDL4j System.out logs and control the logging based on the current
        // logger logging level setting
        if (mLogger.isLoggable(Level.FINE)) {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);                         
        } else {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
        }
        JMSExtSerializer jmsExtSerializer = new JMSExtSerializer(envVariableMap, false, mRuntimeConfig);
        JMSExtensionRegistry jmsExtRegistry = new JMSExtensionRegistry(jmsExtSerializer);
        reader.setExtensionRegistry(jmsExtRegistry);
        reader.readWSDL(f.getAbsolutePath());
        return jmsExtSerializer.getEnvVariableMap();
    }
    
    protected Binding getBinding(Definition def, String serviceName,
                                 String endpointName) {
        String location = null;
        Service svc = getService(def,serviceName);
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
        
    protected JMSAddress getJMSAddress(Definition def, String serviceName,
                                       String endpointName) {
        JMSAddress address = null;
        Service svc = getService(def,serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();
                
            //Look for jms:address                
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (JMSAddress.class.isInstance(ee)) {
                    address = (JMSAddress) ee;
                    address.setService(serviceName);
                    address.setEndpoint(endpointName);
                }
            }
        }
        return address;
    }
        
    protected JMSBinding getJMSBinding(Definition def, String serviceName, String endpointName) {
        JMSBinding jmsBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
                
            //Look for jms:binding                
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && jmsBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (JMSBinding.class.isInstance(ee)) {
                    jmsBinding = (JMSBinding) ee;
                }
            }
        }
        return jmsBinding;
    }
        
    protected void setJMSOperationsAndInputsOutputs (Definition def,
                                                     String serviceName,
                                                     String endpointName,
                                                     boolean isInbound,
                                                     Endpoint endpoint,
                                                     String wsdlFile) throws Exception {
        Map jmsOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for jms:operation entries
                    
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (JMSOperation.class.isInstance(ee)) {
                        // We got a jms operation binding
                                                
                        //
                        // Collect the binding operation info
                        //
                        JMSOperation jmsOp = (JMSOperation)ee;
                        jmsOp.setBindingOperation(oper);
                        String mep = null;
                        OperationType opType = null;
                    	if (oper.getOperation().getStyle() != null) {
                            opType = oper.getOperation().getStyle();
                    	} else {
                    	    // for some reason, sometimes the operation type is not set
                    	    // so the BC will populate it based on the WSDL definition
                    	    // for now, we only handle Request-response and one-way.
                    	    // anything else is not supported and should be caught in WSDL validation
                    	    Operation operation = oper.getOperation();
                    	    if (operation.getInput() != null && operation.getOutput() != null) {
                    	        opType = OperationType.REQUEST_RESPONSE;
                    	    } else if (operation.getInput() != null) {
                    	        opType = OperationType.ONE_WAY;
                    	    }
                    	}    
                        mep = determineMEP(opType);
                        
                        jmsOp.setMEP(mep);
                        jmsOperations.put(endpoint.createOperationAddress(oper.getName()), jmsOp);
                        //jmsOperations.put(QName.valueOf(oper.getName()),jmsOp);
                        
                        //
                        // Collect the binding operation input info
                        //
                        BindingInput bindingInput = oper.getBindingInput();
                        if (bindingInput != null) {
                            Iterator it = 
                                bindingInput.getExtensibilityElements().iterator();
                            while (it.hasNext()) {
                                ExtensibilityElement ee2 = 
                                        (ExtensibilityElement)it.next();
                                if (ee2 instanceof JMSMessage) {
                                    JMSInput jmsInput = new JMSInput();
                                    jmsInput.setJMSMessage((JMSMessage)ee2);
                                    endpoint.setJMSOperationInput(jmsOp,jmsInput);
                                    break;
                                }
                            }
                        }
                        
                        //
                        // Collect the binding operation output info
                        //
                        BindingOutput bindingOutput = oper.getBindingOutput();
                        if (bindingOutput != null) {
                            Iterator it2 = 
                                bindingOutput.getExtensibilityElements().iterator();
                            while (it2.hasNext()) {
                                ExtensibilityElement ee2 = 
                                        (ExtensibilityElement)it2.next();
                                if (ee2 instanceof JMSMessage) {
                                    JMSOutput jmsOutput = new JMSOutput();
                                    jmsOutput.setJMSMessage((JMSMessage)ee2);
                                    endpoint.setJMSOperationOutput(jmsOp,jmsOutput);
                                    break;
                                }
                            }
                        } 
                    
                        // Validate input/output
                        /*
                        if (endpoint.getJMSOperationInput(jmsOp)==null) {
                            String errMsg = mMessages.getString("JMSBC-E0403.MissingJMSInput",
                                        new Object[]{binding.getQName(),
                                                     oper.getName(),
                                                     endpointName,
                                                     serviceName,
                                                     wsdlFile});
                            throw new InvalidConfigurationException (errMsg);
                        }                        
                        */                       
                    }
                }
            }            
        }

        // Check for JMS Operations!
        if (jmsOperations == null || jmsOperations.size() == 0) {
            String errMsg = mMessages.getString("JMSBC-E0402.MissingJMSOperation",
                        new Object[]{binding.getQName(),
                                     endpointName,
                                     serviceName,
                                     wsdlFile});
            throw new InvalidConfigurationException (errMsg);
       } else {
            // Store away the JMSOperation(s) for the endpoint
            endpoint.setJMSOperations(jmsOperations);
       }
    }
            
    /** 
     * Map OperationType to message exchange pattern type
     * @param type The OperationType 
     * @returns The mep string
     */
    protected String determineMEP(OperationType type) {
        String mep = Endpoint.EndpointMessageType.UNSUPPORTED;
        
        if (type != null) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            } else if (type.equals(OperationType.ONE_WAY)) {
                mep = Endpoint.EndpointMessageType.IN_ONLY;            
            }
        }
                
        return mep;
    }
    
    protected Service getService(Definition def,
                                String serviceName) {
        Map services = def.getServices();
        Service svc = (Service)services.get(QName.valueOf(serviceName));
        return svc;
    }
 
    public Map getPartEncoderMapping(Endpoint endpoint) throws Exception {
        
        Definition def = endpoint.getDefinition();
        String serviceName = endpoint.getServiceName().toString();
        String endpointName = endpoint.getEndpointName();
        int direction = endpoint.getEndpointType();
        Map jmsOperations = endpoint.getJMSOperations();
        Map partMapping = new HashMap();
        
        // Don't support inline encoder schemas
    	if (mXSDs.size() <= 0) {
    	    return partMapping;
    	}
    	
        Service service  = def.getService(QName.valueOf(serviceName));
        Port port = service.getPort(endpointName);
        PortType portType = port.getBinding().getPortType();
         
        javax.wsdl.Message wsdlMessage = null;
        Map parts = new HashMap();
        for (Iterator opnameIter = jmsOperations.keySet().iterator(); opnameIter.hasNext(); ) {
            QName operationName = (QName)opnameIter.next();
            JMSOperation jmsOperation = (JMSOperation)jmsOperations.get(operationName);            

            for (Iterator operIter = portType.getOperations().iterator(); operIter.hasNext();) {
                Operation op = (Operation)operIter.next();
                
                if (op.getName().equals(operationName.toString()) ||
                    op.getName().equals(operationName.getLocalPart())) {
                    Input input = op.getInput();
                    if (input != null) {
                        JMSInput jmsInput = endpoint.getJMSOperationInput(jmsOperation);
                        if (jmsInput == null) {
			    if (jmsOperation.getVerb().equalsIgnoreCase("read")) {
				// do nothing
			    } else {
				throw new EncoderException(mMessages.getString("JMSBC-E0406.EncodingMissingJMSInput", new String[] { operationName.toString() }));
			    }
			} else {
			    JMSMessage jmsMessage = jmsInput.getJMSMessage();
			    if (jmsMessage == null) {
				throw new EncoderException(mMessages.getString("JMSBC-E0407.EncodingMissingJMSMessage", new String[] { "input", operationName.toString() }));
			    }
			    wsdlMessage = input.getMessage();
			    if(wsdlMessage==null){
				throw new WSDLException(WSDLException.INVALID_WSDL,mMessages.getString("JMSBC-E0410.MissingInputMessage"));
			    }
			    addEncoderMapping(operationName, wsdlMessage, jmsMessage, partMapping);
			}
                    }
                    
                    Output output = op.getOutput();
                    if (output != null) {                        
                        JMSOutput jmsOutput = endpoint.getJMSOperationOutput(jmsOperation);
                        if (jmsOutput == null) {
                            throw new EncoderException(mMessages.getString("JMSBC-E0408.EncodingMissingJMSOutput", 
                                    new String [] {operationName.toString()}));
                        }
                        JMSMessage jmsMessage = jmsOutput.getJMSMessage();
                        if (jmsMessage == null) {
                            throw new EncoderException(mMessages.getString("JMSBC-E0407.EncodingMissingJMSMessage", 
                                    new String [] {"output", operationName.toString()}));
                        }
                        wsdlMessage = output.getMessage();
                        addEncoderMapping(operationName,
                                          wsdlMessage,                              
                                          jmsMessage,
                                          partMapping);
                    }
                    break;
                }
            }
        }        
        return partMapping;
    }

    private void addEncoderMapping(QName operationName,
                                   javax.wsdl.Message wsdlMessage,
                                   JMSMessage jmsMessage,
                                   Map partMapping) throws Exception {
        String encodingStyle = null;
        Map parts = wsdlMessage.getParts();
        for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
            Part aPart = (Part) partIter.next();
            String partName = aPart.getName();
            QName elem = (aPart.getElementName() != null) ? aPart.getElementName() : null;
            
            String xsdFileLoc = getXsdFileLocation(elem);
            if (xsdFileLoc != null) {
                if (jmsMessage.getUse().equals(JMSMessage.ATTR_USE_TYPE_ENCODED)) {
                    encodingStyle = jmsMessage.getJMSEncodingStyle();
                } else {
                    continue;
                }
                if (encodingStyle == null || encodingStyle.equals("")) {
                    throw new EncoderException(mMessages.getString("JMSBC-E0405.EncodingMissingEncodingStyle", operationName.toString()));
                }
                Encoder encoder = null;
                //MetaRef metaRef = new MyMetaRef(xsdFileLoc, type);
                EncoderFactory encoderFactory = EncoderFactory.newInstance();
                MetaRef metaRef = encoderFactory.makeMeta(xsdFileLoc, elem);
                if (mEncoderMap.get(metaRef) != null) {
                    encoder = (Encoder)mEncoderMap.get(metaRef);
                } else {                                        
                    encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle),
                                                        metaRef);
                    mEncoderMap.put(metaRef, encoder);
                }
                partMapping.put(wsdlMessage.getQName() + partName, encoder);                            	
            }
        }
    }
    
    protected String getXsdFileLocation(QName elemName) throws Exception {
        if (elemName == null) {
            return null;
        }
        
        File aXsdFile = null;
        try {
            aXsdFile = XsdLocator.findXsdByElement(mXSDs, elemName);
        } catch (Exception e) {
            throw new EncoderException (
                    mMessages.getString("JMSBC-E0404.XSDParseFailed",
                          new Object [] {elemName}),
                    e);
        }
        return aXsdFile != null ? aXsdFile.getAbsolutePath() : null;
    }
}
