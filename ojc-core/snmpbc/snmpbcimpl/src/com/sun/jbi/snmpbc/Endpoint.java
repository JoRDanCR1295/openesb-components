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
 * @(#)Endpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.snmpbc.extensions.SNMPAddress;
import com.sun.jbi.snmpbc.extensions.SNMPBinding;
import com.sun.jbi.snmpbc.extensions.SNMPInput;
import com.sun.jbi.snmpbc.extensions.SNMPOperation;
import com.sun.jbi.snmpbc.extensions.SNMPOutput;

import org.w3c.dom.Document;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import java.util.HashMap;
import java.util.Map;

/**
 * Encapsulate endpoint information
 *
 * @author echou
 */
public class Endpoint {

    public static final int INBOUND = 0;    // BC is consumer (external proxy consumer)
    public static final int OUTBOUND = 1;   // BC is provider (external proxy provider)    
    public static final String INBOUND_STR  = "INBOUND";
    public static final String OUTBOUND_STR = "OUTBOUND";
    
    /**
     * Defines the different states an Endpoint may be in
     */
    public static final int SHUTDOWN = 0;
    public static final int STOPPED = 1;
    public static final int RUNNING = 2;
    
    
    private QName mServiceName;
    private String mEndpointName;
    private Definition mDefinition;
    
    private int mEndpointState;
    private EndpointStatus mEndpointStatus;
        
    private int mEndpointType;
    
    private ServiceEndpoint mServiceEndpoint;
    private Document mServiceDescription;
    
    private SNMPAddress mSNMPAddress;
    private SNMPBinding mSNMPBinding;
    private Map mOperations;      // <QName, SNMPOperation>
    private Map mOperationInput;  // <SNMPOperation, SNMPInput>
    private Map mOperationOutput; // <SNMPOperation, SNMPOutput>
    
    public Endpoint() {    
        mOperations = new HashMap();
        mOperationInput = new HashMap();
        mOperationOutput = new HashMap();
    }

    ////////
    //
    //  Binding and Port Information Methods
    //
    ////////

    public QName getServiceName() {
        return mServiceName;
    }

    public void setServiceName(QName serviceName) {
        mServiceName = serviceName;
    }

    public String getEndpointName() {
        return mEndpointName;
    }

    public void setEndpointName(String endpointName) {
        mEndpointName = endpointName;
    }

    public Definition getDefinition() {
        return mDefinition;
    }

    public void setDefinition(Definition definition) {
        mDefinition = definition;
    }

    ////////
    //
    //  State Information Methods
    //
    ////////

    public int getState() {
        return mEndpointState;
    }

    public void setState(int state) {
        mEndpointState = state;
    }

    public void setEndpointStatus(EndpointStatus val) {
        mEndpointStatus = val;
    }

    public EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }

    ////////
    //
    //  Outbound (provider) or inbound (consumer) - with respect to SE
    //
    ////////

    public int getEndpointType() {
        return mEndpointType;
    }

    public void setEndpointType(int type) {
        mEndpointType = type;
    }

    ////////
    //
    //  JBI-representation of Endpoint
    //
    ////////

    public ServiceEndpoint getServiceEndpoint() {
        return mServiceEndpoint;
    }

    public void setServiceEndpoint(ServiceEndpoint serviceEndpoint) {
        mServiceEndpoint = serviceEndpoint;
    }

    public Document getServiceDescription() {
        return mServiceDescription;
    }

    public void setServiceDescription(Document serviceDescription) {
        mServiceDescription = serviceDescription;
    }
    

    public SNMPAddress getSNMPAddress() {
        return mSNMPAddress;
    }
    
    public void setSNMPAddress(SNMPAddress address) {
        mSNMPAddress = address;
    }

    public SNMPBinding getSNMPBinding() {
        return mSNMPBinding;
    }

    public void setSNMPBinding(SNMPBinding binding) {
        mSNMPBinding = binding;
    }

    public Map getSNMPOperations() {
        return mOperations;
    }

    public void setSNMPOperations(Map operations) {
        mOperations = operations;
    }

    public SNMPInput getSNMPOperationInput(SNMPOperation operation) {
        return (SNMPInput) mOperationInput.get(operation);
    }

    public void setSNMPOperationInput(SNMPOperation operation,
                                     SNMPInput operationInput) {
        mOperationInput.put(operation, operationInput);
    }

    public SNMPOutput getSNMPOperationOutput(SNMPOperation operation) {
        return (SNMPOutput) mOperationOutput.get(operation);
    }

    public void setSNMPOperationOutput(SNMPOperation operation,
                                      SNMPOutput operationOutput) {
        mOperationOutput.put(operation, operationOutput);
    }
  
    public static String endpointTypeToString (int endpointType) {
        if (endpointType == INBOUND) {
            return INBOUND_STR;            
        } else {
            return OUTBOUND_STR;
        }
    }
    
}
