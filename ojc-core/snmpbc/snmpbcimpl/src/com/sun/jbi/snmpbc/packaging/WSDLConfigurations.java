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

package com.sun.jbi.snmpbc.packaging;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.snmpbc.Endpoint;
import com.sun.jbi.snmpbc.extensions.SNMPAddress;
import com.sun.jbi.snmpbc.extensions.SNMPBinding;
import com.sun.jbi.snmpbc.extensions.SNMPExtensionRegistry;
import com.sun.jbi.snmpbc.extensions.SNMPInput;
import com.sun.jbi.snmpbc.extensions.SNMPMessage;
import com.sun.jbi.snmpbc.extensions.SNMPOperation;
import com.sun.jbi.snmpbc.extensions.SNMPOutput;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints
 * based on the portmap list.
 *
 */
public class WSDLConfigurations {

    private static final Messages mMessages =
        Messages.getMessages(WSDLConfigurations.class);
    
    private String mRootPath;
    
    // <String, File>
    private Map<String, File> mWSDLs = new HashMap<String, File> ();
    private Map<String, File> mXSDs = new HashMap<String, File> ();

    public WSDLConfigurations (String rootPath) {
        mRootPath = rootPath;
    }

    public List parse(List portMaps)
        throws Exception {

        File dir = new File(mRootPath);

        CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() +
                                       File.separator +
                                       "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        EntityResolver resolver = new CatalogResolver(catalogManager);


        ArrayList endpoints = new ArrayList();
        fillFileLists(dir);
        Iterator<File> wsdls = mWSDLs.values().iterator();
        while (wsdls.hasNext()) {
            endpoints.addAll(parseWSDL(wsdls.next(), resolver, portMaps));
        }

        return endpoints;
    }

    public List parseWSDL(File wsdlFile,
                          EntityResolver resolver,
                          List portMaps) 
        throws Exception {

        Definition def = readWSDL(wsdlFile, resolver);
        ArrayList endPoints = new ArrayList();
        Iterator it = portMaps.iterator();
        while (it.hasNext()) {
            EndpointData pm = (EndpointData)it.next();

            // Check the Definition if it has a SNMPBinding.  If not,
            // continue
            SNMPBinding binding = getSNMPBinding(def,
                                               pm.getService(),
                                               pm.getEndpoint());
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // extract file details

            // If we have a SNMPBinding, we must have an SNMPAddress.
            SNMPAddress address =
                getSNMPAddress(def, pm.getService(), pm.getEndpoint());
            
                                
            // Create an Endpoint for each Port.  The endpoint should have the
            // correct SNMPBinding, the associated Operations, and the
            // associated OperationInput and OperationOutput
            Endpoint endpoint = new Endpoint();

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
            endpoint.setSNMPAddress(address);
            endpoint.setSNMPBinding(binding);
            
            // Traverse operations and their inputs and outputs and store them
            setSNMPOperationsAndInputsOutputs(def,
                                             pm.getService(),
                                             pm.getEndpoint(),
                                             pm.getDirection()==Endpoint.INBOUND,
                                             endpoint,
                                             wsdlFile.getName());

            // Now add the Endpoint to our list of Endpoints
            endPoints.add(endpoint);
        }

        return endPoints;
    }
    
    /**
     * Get all WSDL and XSD files in the currentDir and below
     * and store in internal lists
     * @param currentDir 
     */
    protected void fillFileLists(File currentDir) {
        File[] filesInCurrentDir = currentDir.listFiles();
        for (int i = 0; i < filesInCurrentDir.length; i++) {

            if (filesInCurrentDir[i].isFile()) {
                if (filesInCurrentDir[i].getName().toLowerCase().endsWith(".wsdl")) {
                    if (!mWSDLs.containsKey(filesInCurrentDir[i].getName())) {
                        mWSDLs.put(filesInCurrentDir[i].getName(), filesInCurrentDir[i]);
                    }
                } else if (filesInCurrentDir[i].getName().toLowerCase().endsWith(".xsd")) {
                    if (!mXSDs.containsKey(filesInCurrentDir[i].getName())) {
                        mXSDs.put(filesInCurrentDir[i].getName(), filesInCurrentDir[i]);
                    }
                }
            } else if (filesInCurrentDir[i].isDirectory()) {
                fillFileLists(filesInCurrentDir[i]);                
            }
        }
    }

    
    protected Definition readWSDL(File f, EntityResolver resolver)
        throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader =
            ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new SNMPExtensionRegistry());
        Definition def = reader.readWSDL(f.getAbsolutePath());
        
        return def;
    }
 
        
    protected Binding getBinding(Definition def, String serviceName,
                                 String endpointName) {
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
        
    protected SNMPAddress getSNMPAddress(Definition def, String serviceName,
                                       String endpointName) {
        SNMPAddress address = null;
        Service svc = getService(def,serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();
                
            //Look for snmp:address                
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SNMPAddress.class.isInstance(ee)) {
                    address = (SNMPAddress) ee;
                }
            }
        }
        return address;
    }
        
    protected SNMPBinding getSNMPBinding(Definition def, String serviceName, String endpointName) {
        SNMPBinding snmpBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
                
            //Look for snmp:binding                
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && snmpBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SNMPBinding.class.isInstance(ee)) {
                    snmpBinding = (SNMPBinding) ee;
                }
            }
        }
        return snmpBinding;
    }
        
    protected void setSNMPOperationsAndInputsOutputs (Definition def,
                                                     String serviceName,
                                                     String endpointName,
                                                     boolean isInbound,
                                                     Endpoint endpoint,
                                                     String wsdlFile) throws Exception {
        Map snmpOperations = new HashMap();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for snmp:operation entries
                    
                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (SNMPOperation.class.isInstance(ee)) {
                        // We got a snmp operation binding
                                                
                        //
                        // Collect the binding operation info
                        //
                        SNMPOperation snmpOp = (SNMPOperation)ee;
                        snmpOp.setBindingOperation(oper);
                        String mep = null;
                    	if (oper.getOperation().getStyle() != null) {
                    	    mep = determineMEP(isInbound, oper);
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
                    	        mep = determineMEP(isInbound, type);
                    	    }
                    	}                        
                        snmpOp.setMEP(mep);
                        snmpOperations.put(new QName(binding.getQName().getNamespaceURI(), oper.getName()),snmpOp);
                        
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
                                if (ee2 instanceof SNMPMessage) {
                                    SNMPInput snmpInput = new SNMPInput();
                                    snmpInput.setSNMPMessage((SNMPMessage)ee2);
                                    endpoint.setSNMPOperationInput(snmpOp,snmpInput);
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
                                if (ee2 instanceof SNMPMessage) {
                                    SNMPOutput snmpOutput = new SNMPOutput();
                                    snmpOutput.setSNMPMessage((SNMPMessage)ee2);
                                    endpoint.setSNMPOperationOutput(snmpOp,snmpOutput);
                                    break;
                                }
                            }
                        } 
                    
                        // Validate input/output
                        if (endpoint.getSNMPOperationInput(snmpOp)==null) {
//                             mLogger.log(Level.SEVERE,
//                                         "WSDLConfigurations_MISSING_SNMP_INPUT",
//                                         new Object[]{binding.getQName(),
//                                                      oper.getName(),
//                                                      endpointName,
//                                                      serviceName,
//                                                      wsdlFile});
                            String errMsg = mMessages.getString("SNMPBC_C00103.MISSING_SNMP_OPERATIONS",
                                        new Object[]{binding.getQName(),
                                                     oper.getName(),
                                                     endpointName,
                                                     serviceName,
                                                     wsdlFile});
                            throw new Exception (errMsg);
                        }                        
                    }
                }
            }            
        }

        // Check for SNMP Operations!
        if (snmpOperations == null || snmpOperations.size() == 0) {
//             mLogger.log(Level.SEVERE,
//                         "WSDLConfigurations_MISSING_SNMP_OPERATIONS",
//                         new Object[]{binding.getQName(),
//                                      endpointName,
//                                      serviceName,
//                                      wsdlFile});

            String errMsg = mMessages.getString("SNMPBC_C00104.MISSING_SNMP_INPUT",
                        new Object[]{binding.getQName(),
                                     endpointName,
                                     serviceName,
                                     wsdlFile});
            throw new Exception (errMsg);
       } else {
            // Store away the SNMPOperation(s) for the endpoint
            endpoint.setSNMPOperations(snmpOperations);
       }
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
     * @param isDirectionInbound 
     * @param bo 
     * @return the message exchange pattern, null if no mapping could be determined.
     */
    protected String determineMEP(boolean isDirectionInbound, BindingOperation bo) {
        String mep = null;
        OperationType type = bo.getOperation().getStyle();
        if (isDirectionInbound) {
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = SNMPOperation.IN_OUT;
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = SNMPOperation.IN_ONLY;
            }
        } else {
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = SNMPOperation.OUT_IN;
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = SNMPOperation.OUT_ONLY;
            }
        }
        return mep;
    }    
    
    protected String determineMEP(boolean isDirectionInbound, OperationType type) {
        String mep = null;
        
        if (isDirectionInbound) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = SNMPOperation.IN_OUT;
            } else if (type.equals(OperationType.ONE_WAY)) {
                mep = SNMPOperation.IN_ONLY;
            } 
        } else {
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = SNMPOperation.OUT_IN;
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = SNMPOperation.OUT_ONLY;
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
    
}
