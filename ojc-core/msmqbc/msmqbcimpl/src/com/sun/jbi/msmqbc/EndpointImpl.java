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
 * @(#)EndpointImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import javax.jbi.servicedesc.ServiceEndpoint;

import org.w3c.dom.Document;

import com.sun.jbi.eManager.provider.EndpointStatus;

import com.sun.jbi.msmqbc.extensions.MSMQInput;
import com.sun.jbi.msmqbc.extensions.MSMQOutput;
import com.sun.jbi.msmqbc.extensions.MSMQAddress;
import com.sun.jbi.msmqbc.extensions.MSMQBinding;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;

/**
 * @author Sun Microsystems
 */

public class EndpointImpl implements Endpoint, Serializable {

    private static final long serialVersionUID = 3256727264572813369L;

    private QName mServiceName;

    private String mEndpointName;

    private Definition mDefinition;

    private int mEndpointState;

    private EndpointStatus mEndpointStatus;

    private int mEndpointType;

    private ServiceEndpoint mServiceEndpoint;

    private Document mServiceDescription;

    private MSMQAddress mMSMQAddress;

    private MSMQBinding mMSMQBinding;

    private Map mOperations; // <QName, MSMQOperation>

    private Map mOperationInputs; // <MSMQOperation, MSMQInput>

    private Map mOperationOutputs; // <MSMQOperation, MSMQOutput>

    private Map mOperationMEPs;

    private Map mPartMapping;

    public EndpointImpl() {
        mOperations = new HashMap();
        mOperationInputs = new HashMap();
        mOperationOutputs = new HashMap();
        mOperationMEPs = new HashMap();
    }

    // //////
    //
    // Binding and Port Information Methods
    //
    // //////

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

    // //////
    //
    // State Information Methods
    //
    // //////

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

    // //////
    //
    // Outbound (provider) or inbound (consumer) - with respect to SE
    //
    // //////

    public int getEndpointType() {
        return mEndpointType;
    }

    public void setEndpointType(int type) {
        mEndpointType = type;
    }

    // //////
    //
    // JBI-representation of Endpoint
    //
    // //////

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

    // //////
    //
    // MSMQ-specific Endpoint. These should probably be refactored
    // out in a better way so as to maintain their original
    // hierarchical structure
    //
    // //////

    public MSMQAddress getMSMQAddress() {
        return mMSMQAddress;
    }

    public void setMSMQAddress(MSMQAddress address) {
        mMSMQAddress = address;
    }

    public MSMQBinding getMSMQBinding() {
        return mMSMQBinding;
    }

    public void setMSMQBinding(MSMQBinding binding) {
        mMSMQBinding = binding;
    }

    public Map getMSMQOperations() {
        return mOperations;
    }

    public void setMSMQOperations(Map operations) {
        mOperations = operations;
    }

    public MSMQInput getMSMQOperationInput(MSMQOperation operation) {
        return (MSMQInput) mOperationInputs.get(operation);
    }

    public void setMSMQOperationInput(MSMQOperation operation, MSMQInput operationInput) {
        mOperationInputs.put(operation, operationInput);
    }

    public MSMQOutput getMSMQOperationOutput(MSMQOperation operation) {
        return (MSMQOutput) mOperationOutputs.get(operation);
    }

    public void setMSMQOperationOutput(MSMQOperation operation, MSMQOutput operationOutput) {
        mOperationOutputs.put(operation, operationOutput);
    }

    public void setOperationMsgExchangePatterns(Map opMEPs) {
        mOperationMEPs = opMEPs;
    }

    public Map getOperationMsgExchangePatterns() {
        return mOperationMEPs;
    }

    // //////
    //
    // Encoder mappings
    //
    // //////

    public void setMessagePartEncoderMapping(Map partMapping) {
        mPartMapping = partMapping;
    }

    public Map getMessagePartEncoderMapping() {
        return mPartMapping;
    }

    public static String endpointTypeToString(int endpointType) {
        if (endpointType == EndpointType.INBOUND) {
            return EndpointType.INBOUND_STR;
        } else {
            return EndpointType.OUTBOUND_STR;
        }
    }

}
