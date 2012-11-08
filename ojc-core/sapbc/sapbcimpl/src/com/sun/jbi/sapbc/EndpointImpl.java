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

package com.sun.jbi.sapbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.Endpoint.EndpointState;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.SAPBinding;

import java.io.Serializable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.wsdl.model.WSDLDefinitions;

import org.w3c.dom.Document;

public class EndpointImpl implements Endpoint, Serializable {
    
    public EndpointImpl(
            String name,
            QName serviceName,
            WSDLDefinitions wsdlDef,
            EndpointType direction) {
    
        mServiceName = QName.valueOf(serviceName.toString());
        mEndpointName = name;
        mDefinition = wsdlDef;
        mEndpointType = direction;
        
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
        if (!mServiceName.equals(serviceName)) {
            mServiceName = QName.valueOf(serviceName.toString());
        }
    }

    public String getEndpointName() {
        return mEndpointName;
    }

    public void setEndpointName(String endpointName) {
        mEndpointName = (endpointName != null ? endpointName : "");
    }

    public WSDLDefinitions getDefinition() {
        return mDefinition;
    }

    public void setDefinition(WSDLDefinitions definition) {
        mDefinition = definition;
    }

    ////////
    //
    //  State Information Methods
    //
    ////////

    public EndpointState getState() {
        return mEndpointState;
    }

    public void setState(EndpointState state) {
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

    public EndpointType getEndpointType() {
        return mEndpointType;
    }

    public void setEndpointType(EndpointType type) {
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
    //  SAP BC-specific information 
    //
    ////////

    public SAPAddress getSAPAddress() {
        return mAddress;
    }
    
    public void setSAPAddress(SAPAddress address) {
        mAddress = address;
    }

    public SAPBinding getSAPBinding() {
        return mBinding;
    }

    public void setSAPBinding(SAPBinding binding) {
        mBinding = binding;
    }

    public Map getSAPOperations() {
        return mOperations;
    }

    public void setSAPOperations(Map operations) {
        mOperations = operations;
    }
    
    public void setOperationMsgExchangePattern(Map opMeps) {
        mOperationMeps = opMeps;
    }
    
    public Map getOperationMsgExchangePattern() {
        return mOperationMeps;
    }
    
    public String toString() {
        String retStr = "Endpoint Name ["+getEndpointName()+
                        "] Service Name ["+getServiceName().toString()+
                        "] WSDL name ["+getDefinition().getName()+"]";
        return retStr;
    }
    
    private Map mOperations;
    private Map mOperationMeps;
    private transient Document mServiceDescription;
    private QName mServiceName;
    private String mEndpointName = "";
    private WSDLDefinitions mDefinition;
    private EndpointState mEndpointState;
    private EndpointStatus mEndpointStatus;
    private EndpointType mEndpointType;
    private ServiceEndpoint mServiceEndpoint;
    private SAPAddress mAddress;
    private SAPBinding mBinding;
}
