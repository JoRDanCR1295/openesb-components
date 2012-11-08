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

package com.sun.jbi.jmsbc;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.jmsbc.Endpoint.EndpointType;
import com.sun.jbi.jmsbc.Endpoint.EndpointState;

import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSBinding;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSInput;
import com.sun.jbi.jmsbc.extensions.JMSOutput;


public class EndpointImpl
    implements Endpoint, Serializable {

    private QName mServiceName;
    private String mEndpointName;
    private Definition mDefinition;
    private PortType mPortType; // lazily initialized
    
    private int mEndpointState;
    private EndpointStatus mEndpointStatus;
        
    private int mEndpointType;
    
    private ServiceEndpoint mServiceEndpoint;
    private Document mServiceDescription;
    
    private JMSAddress mJMSAddress;
    private JMSBinding mJMSBinding;
    private Map mOperations;      // <QName, JMSOperation>
    private Map mOperationInput;  // <JMSOperation, JMSInput>
    private Map mOperationOutput; // <JMSOperation, JMSOutput>
    private Map mPartMapping;     // <QName+partname, encoder>
    
    private String mServiceUnitID;
    private String mConfig;
    private ServiceQuality[] mQos;
    private ServiceUnit serviceUnit;
    
    public EndpointImpl() {    
        mOperations = new HashMap();
        mOperationInput = new HashMap();
        mOperationOutput = new HashMap();
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
    //  JMS-specific Endpoint.  These should probably be refactored
    //  out in a better way so as to maintain their original
    //  hierarchical structure
    //
    ////////

    public JMSAddress getJMSAddress() {
        return mJMSAddress;
    }
    
    public void setJMSAddress(JMSAddress address) {
        mJMSAddress = address;
    }

    public JMSBinding getJMSBinding() {
        return mJMSBinding;
    }

    public void setJMSBinding(JMSBinding binding) {
        mJMSBinding = binding;
    }

    public Map getJMSOperations() {
        return mOperations;
    }

    public void setJMSOperations(Map operations) {
        mOperations = operations;
    }

    public JMSInput getJMSOperationInput(JMSOperation operation) {
        return (JMSInput)mOperationInput.get(operation);
    }

    public void setJMSOperationInput(JMSOperation operation,
                                     JMSInput operationInput) {
        mOperationInput.put(operation, operationInput);
    }

    public JMSOutput getJMSOperationOutput(JMSOperation operation) {
        return (JMSOutput) mOperationOutput.get(operation);
    }

    public void setJMSOperationOutput(JMSOperation operation,
                                      JMSOutput operationOutput) {
        mOperationOutput.put(operation, operationOutput);
    }
  
    public static String endpointTypeToString (int endpointType) {
        if (endpointType == Endpoint.EndpointType.INBOUND) {
            return Endpoint.EndpointType.INBOUND_STR;            
        } else {
            return Endpoint.EndpointType.OUTBOUND_STR;
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
    
    public QName createOperationAddress(String operationName) {
        if (mPortType == null) {
            Map services = mDefinition.getServices();
            
            // DO NOT use the getService() method.
            // It checks all imported WSDLs.
            Service svc = (Service)services.get(mServiceName);
            Port port = svc.getPort(QName.valueOf(mEndpointName).getLocalPart());
            mPortType = port.getBinding().getPortType();
        }
        return new QName(mPortType.getQName().getNamespaceURI(), operationName);
    }
    
    public void setServiceUnitID (String serviceUnitID) {
        this.mServiceUnitID = serviceUnitID;
    }
    
    public String getServiceUnitID () {
        return mServiceUnitID;
    }

	public String getKey() {
        return mServiceUnitID + 
        getServiceName().toString() + 
        getEndpointName() +
        endpointTypeToString(getEndpointType());               
	}

	public String getApplicationConfigurationName() {
		return mConfig;
	}

	public void setApplicationConfigurationName(String config) {
		mConfig = config;
	}

	public ServiceQuality[] getServiceQualities() {
		return mQos;
	}

	public void setServiceQualities(ServiceQuality[] qos) {
		mQos = qos;
	}

	public ServiceUnit getServiceUnit() {
		return serviceUnit;
	}

	public void setServiceUnit(ServiceUnit su) {
		serviceUnit = su;
	}
	
	public String toString(){
		return getKey();
	}
}
