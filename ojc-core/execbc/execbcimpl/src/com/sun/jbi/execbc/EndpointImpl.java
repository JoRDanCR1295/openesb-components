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

package com.sun.jbi.execbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecBinding;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

/**
 * This class implements the EndPoint interface for
 * for File Binding component.
 */
public class EndpointImpl implements Endpoint, Serializable {
    private QName mServiceName;
    private String mEndpointName;
    private Definition mDefinition;
    
    private int mEndpointState;
    private EndpointStatus mEndpointStatus;
    
    private int mEndpointType;
    
    private ServiceEndpoint mServiceEndpoint;
    private transient Document mServiceDescription;
    
    private ExecAddress mExecAddress;
    private ExecBinding mExecBinding;
    private Map mOperations;
    private Map mOperationMEPs;
    private Map mPartMapping;
    
    public EndpointImpl() {
        mOperations = new HashMap();
        mOperationMEPs = new HashMap();
        mPartMapping = new HashMap();
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
    
    ////////
    //
    //  File-specific Endpoint.
    //
    ////////
    
    public ExecAddress getExecAddress() {
        return mExecAddress;
    }
    
    public void setExecAddress(ExecAddress address) {
        mExecAddress = address;
    }
    
    public ExecBinding getExecBinding() {
        return mExecBinding;
    }
    
    public void setExecBinding(ExecBinding binding) {
        mExecBinding = binding;
    }
    
    public Map getExecOperations() {
        return mOperations;
    }
    
    public void setExecOperations(Map operations) {
        mOperations = operations;
    }
    
    public void setOperationMsgExchangePattern(Map opMEPs) {
        mOperationMEPs = opMEPs;
    }
    
    public Map getOperationMsgExchangePattern() {
        return mOperationMEPs;
    }
    
    public void setMessagePartEncoderMapping(Map partMapping) {
        mPartMapping = partMapping;
    }
    
    public Map getMessagePartEncoderMapping() {
        return mPartMapping;
    }
}
