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

package com.sun.jbi.mqbc;


import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.mqbc.extensions.MQBCAddress;
import com.sun.jbi.mqbc.extensions.MQBCBinding;
import com.sun.jbi.mqbc.extensions.MQBCRedelivery;
import com.sun.jbi.mqbc.packaging.EndpointData;
import org.w3c.dom.Document;


public class EndpointImpl
    implements Endpoint, Serializable {

    private final QName mServiceName;
    private final String mEndpointName;
    private Definition mDefinition;
    
    private EndpointStatus mEndpointStatus;
        
    private int mEndpointType;
    
    private QName mServiceEndpointName;
    private ServiceEndpoint mServiceEndpoint;
    private Document mServiceDescription;
    private EndpointData mEndpointData;
    
    private MQBCAddress mMQAddress;
    private MQBCBinding mMQBinding;
    private Map mOperations;      
    
    private final Map<QName, String> mOperationMEPs;
    private Map mPartMapping;     // <QName+partname, encoder>
    private final Map<QName, MQBCRedelivery> mOpToRedeliveryMap =
            new HashMap<QName, MQBCRedelivery>();
    
    public static String endpointTypeToString (int endpointType) {
        if (endpointType == Endpoint.EndpointType.INBOUND) {
            return Endpoint.EndpointType.INBOUND_STR;            
        } else {
            return Endpoint.EndpointType.OUTBOUND_STR;
        }
    }

    public EndpointImpl(EndpointData data) {    
        mOperations = new HashMap();
        mServiceName = mServiceEndpointName = QName.valueOf(data.getService());
        mEndpointName = data.getEndpoint();
        mEndpointData = data;
        mOperationMEPs = new HashMap<QName, String>();
    }

    ////////
    //
    //  Binding and Port Information Methods
    //
    ////////

    public QName getServiceName() {
        return mServiceName;
    }

    public String getEndpointName() {
        return mEndpointName;
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

    public QName getServiceEndpointName() {
        return mServiceEndpointName;
    }

    public ServiceEndpoint getServiceEndpoint() {
        return mServiceEndpoint;
    }

    public void setServiceEndpoint(ServiceEndpoint serviceEndpoint) {
        mServiceEndpoint = serviceEndpoint;
        mServiceEndpointName = (serviceEndpoint != null
                ? serviceEndpoint.getServiceName() : mServiceName);
    }

    public Document getServiceDescription() {
        return mServiceDescription;
    }

    public void setServiceDescription(Document serviceDescription) {
        mServiceDescription = serviceDescription;
    }
    
    ////////
    //
    //  JMS-specific Endpoint.  These should probably be refactored
    //  out in a better way so as to maintain their original
    //  hierarchical structure
    //
    ////////

    public MQBCAddress getMQAddress() {
        return mMQAddress;
    }
    
    public void setMQAddress(MQBCAddress address) {
        mMQAddress = address;
    }

    public MQBCBinding getMQBinding() {
        return mMQBinding;
    }

    public void setMQBinding(MQBCBinding binding) {
        mMQBinding = binding;
    }

    public Map getMQOperations() {
        return mOperations;
    }

    public void setMQOperations(Map operations) {
        mOperations.putAll(operations);
    }

    public MQBCRedelivery getRedelivery(QName operationName) {
        synchronized (mOpToRedeliveryMap) {
            return mOpToRedeliveryMap.get(operationName);
        }
    }
    
    public void setRedelivery(Map<QName, MQBCRedelivery> opToRedeliveryMap) {
        synchronized (mOpToRedeliveryMap) {
            mOpToRedeliveryMap.clear();
            mOpToRedeliveryMap.putAll(opToRedeliveryMap);
        }
    }
    
    public Map<QName, String> getOperationMsgExchangePattern() {
        synchronized (mOperationMEPs) {
            return Collections.unmodifiableMap(mOperationMEPs);
        }
    }
    public void setOperationMsgExchangePattern(Map<QName, String> opMEPs) {
        synchronized (mOperationMEPs) {
            mOperationMEPs.clear();
            mOperationMEPs.putAll(opMEPs);
        }
    }
     ////////
    //
    // Encoder mappings
    //
    ////////
        
    public void setMessagePartEncoderMapping(Map partMapping) {
        mPartMapping = partMapping;
    }
    
    public Map getMessagePartEncoderMapping() {
        return mPartMapping;
    }    
    
    public EndpointData toEndpointData() {
        return mEndpointData;
    }
}
