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
 * @(#)EndpointValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.validator;

import com.sun.jbi.execbc.util.EPUtil;
import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.validator.EndpointValidator;
import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.extensions.ExecOutput;
import com.sun.jbi.execbc.Endpoint.EndpointType;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

/**
 * This class performs validation for endpoints.
 *
 * @author Sherry Weng
 */
public class EndpointValidator {
    private static final Messages mMessages =
            Messages.getMessages(EndpointValidator.class);
    private static Logger mLogger = Messages.getLogger(EndpointValidator.class);
    
    public static void validateEndpointForUniqueness(Collection<Endpoint> endpoints, Endpoint aEndpoint, boolean endpointValidated) throws Exception {
        if (!endpointValidated) {
            validateEndpoint(aEndpoint);
        }
        
        int endpointType = aEndpoint.getEndpointType();
        String serviceEndpointRef = aEndpoint.getServiceName().toString() + aEndpoint.getEndpointName();
        
        for (Endpoint aDeployedEndpoint : endpoints) {
            if (aDeployedEndpoint.getEndpointType() != endpointType) {
                continue;
            } else {
                String aDeployedServiceEPRef = aDeployedEndpoint.getServiceName().toString() + aDeployedEndpoint.getEndpointName();
                if (serviceEndpointRef.equals(aDeployedServiceEPRef)) {
                    throw new Exception(mMessages.getString("EPV_endpoint_already_exists",
                            new String[] {aEndpoint.getServiceName().toString(), aEndpoint.getEndpointName()}));
                }
//Not applicable to Exec BC
//                String aEndpointFileDir = EPUtil.getFileDirectory(aEndpoint);
//                String aDeployedFileDir = EPUtil.getFileDirectory(aDeployedEndpoint);
//                
//                if (!aEndpointFileDir.equals(aDeployedFileDir)) {
//                    continue;
//                }
//                
//                Set<Map.Entry<QName, ExecOperation>> opEntries = aEndpoint.getExecOperations().entrySet();
//                for (Map.Entry<QName, ExecOperation> aEntry : opEntries) {
//                    String opname = aEntry.getKey().toString();
//                    ExecOperation aOperation = aEntry.getValue();
//                    String aFileInputResource = null;
//                    String aFileOutputResource = null;
//                    try {
//                        aFileInputResource = 
//                                (aOperation.getExecOperationInput() != null)? 
//                                    new File(aEndpointFileDir, aOperation.getExecOperationInput().getExecMessage().getFileName()).getCanonicalPath() : null;
//                        aFileOutputResource = 
//                                (aOperation.getFileOperationOutput() != null)? 
//                                    new File(aEndpointFileDir, aOperation.getFileOperationOutput().getExecMessage().getFileName()).getCanonicalPath() : null;
//                    }
//                    catch (Exception ex) {
//                        throw new Exception(mMessages.getString("EPV_invalid_file_resource", new String [] {opname, serviceEndpointRef, ex.getMessage()}));
//                    }
//                    
//                    Set<Map.Entry<QName, ExecOperation>> deployedOpEntries = aDeployedEndpoint.getExecOperations().entrySet();
//                    
//                    for (Map.Entry<QName, ExecOperation> aDeployedEntry : deployedOpEntries) {
//                        String aDeployedOpname = aDeployedEntry.getKey().toString();
//                        ExecOperation aDeployeOperation = aDeployedEntry.getValue();
//                        
//                        String aDeployedFileInputResource = null;
//                        String aDeployedFileOutputResource = null;
//
//                        try {
//                            aDeployedFileInputResource = 
//                                    (aDeployeOperation.getFileOperationInput() != null)? 
//                                        new File(aDeployedFileDir, aDeployeOperation.getFileOperationInput().getExecMessage().getFileName()).getCanonicalPath() : null;
//                            aDeployedFileOutputResource = (aDeployeOperation.getFileOperationOutput() != null)? 
//                                        new File(aDeployedFileDir, aDeployeOperation.getFileOperationOutput().getExecMessage().getFileName()).getCanonicalPath() : null;
//                        }
//                        catch (Exception ex) {
//                            throw new Exception(mMessages.getString("EPV_invalid_file_resource", new String [] {aDeployedOpname, aDeployedServiceEPRef, ex.getMessage()}));
//                        }
//                        
//                        if (aFileInputResource.equals(aDeployedFileInputResource)) {
//                            throw new Exception(mMessages.getString("EPV_Invalid_duplicate_file_resource",
//                                    new String [] {opname, serviceEndpointRef, aDeployedFileInputResource} ));
//                        }
//                        
//                        if (aFileOutputResource != null         &&
//                                aDeployedFileOutputResource != null &&
//                                aFileOutputResource.equals(aDeployedFileOutputResource)) {
//                            throw new Exception(mMessages.getString("EPV_Invalid_duplicate_file_resource",
//                                    new String [] {opname, serviceEndpointRef, aDeployedFileOutputResource} ));
//                        }
//                    }
//                }
            }
        }
    }
    
    
    public static void validateEndpoint(Endpoint aEndpoint) throws Exception {
        String serviceEndpointRef = aEndpoint.getServiceName().toString() + aEndpoint.getEndpointName();
        
        Set<Map.Entry<QName, ExecOperation>> opEntries = aEndpoint.getExecOperations().entrySet();
        Map<QName, String> operationMeps = aEndpoint.getOperationMsgExchangePattern();
        
        if (aEndpoint.getEndpointType() == EndpointType.INBOUND) {
            for (Map.Entry<QName, ExecOperation> aEntry : opEntries) {
                QName opname = aEntry.getKey();
                ExecOperation aOperation = aEntry.getValue();
                
                String mep = (String) operationMeps.get(opname);
                if (mep == null) {
                    throw new Exception(mMessages.getString("EPV_Invalid_operation_mep", new String [] {opname.toString(), serviceEndpointRef} ));
                }

                if (Endpoint.EndpointMessageType.OUT_IN.equals(mep)) {
                    ExecInput input = aOperation.getExecOperationInput();
                    ExecOutput output = aOperation.getExecOperationOutput();
                    
                    if (input == null) {
                        throw new Exception(mMessages.getString("EPV_Invalid_missing_input", new String [] {opname.toString(), serviceEndpointRef}));
                    }
                    if (output == null) {
                        throw new Exception(mMessages.getString("EPV_Invalid_missing_output", new String [] {opname.toString(), serviceEndpointRef}));
                    }
                } else if (Endpoint.EndpointMessageType.IN_ONLY.equals(mep)
                        || Endpoint.EndpointMessageType.OUT_ONLY.equals(mep)) {
                    ExecInput input = aOperation.getExecOperationInput();
                    
                    if (input == null) {
                        throw new Exception(mMessages.getString("EPV_Invalid_missing_input", new String [] {opname.toString(), serviceEndpointRef}));
                    }
                } else {
                    throw new Exception(mMessages.getString("EPV_Invalid_MEP", new String [] {opname.toString(), serviceEndpointRef}));
                }
            }
        }
    }
}
