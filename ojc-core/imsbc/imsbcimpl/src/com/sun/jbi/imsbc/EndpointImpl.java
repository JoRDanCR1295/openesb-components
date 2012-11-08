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

package com.sun.jbi.imsbc;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import javax.jbi.servicedesc.ServiceEndpoint;

import org.w3c.dom.Document;

import com.sun.jbi.eManager.provider.EndpointStatus;

import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSOutput;
import com.sun.jbi.imsbc.extensions.IMSAddress;
import com.sun.jbi.imsbc.extensions.IMSBinding;
import com.sun.jbi.imsbc.extensions.IMSOperation;

/**
 * This class implements the EndPoint interface for
 * for IMS Binding component.
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

    private IMSAddress mIMSAddress;

    private IMSBinding mIMSBinding;

    private Map mOperations; // <QName, IMSOperation>

    private Map mOperationInputs; // <IMSOperation, IMSInput>

    private Map mOperationOutputs; // <IMSOperation, IMSOutput>

    private Map mOperationMEPs;

    private Map mPartMapping;

    private ServiceUnit serviceUnit;

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
    // Outbound (provider) or inbound (consumer) - with respect to SE Methods
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
    // JBI-representation of Endpoint methods
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
    // IMS Binding specific Endpoint methods
    //
    // //////

    public IMSAddress getIMSAddress() {
        return mIMSAddress;
    }

    public void setIMSAddress(IMSAddress address) {
        mIMSAddress = address;
    }

    public IMSBinding getIMSBinding() {
        return mIMSBinding;
    }

    public void setIMSBinding(IMSBinding binding) {
        mIMSBinding = binding;
    }

    public Map getIMSOperations() {
        return mOperations;
    }

    public void setIMSOperations(Map operations) {
        mOperations = operations;
    }

    public IMSInput getIMSOperationInput(IMSOperation operation) {
        return (IMSInput) mOperationInputs.get(operation);
    }

    public void setIMSOperationInput(IMSOperation operation, IMSInput operationInput) {
        mOperationInputs.put(operation, operationInput);
    }

    public IMSOutput getIMSOperationOutput(IMSOperation operation) {
        return (IMSOutput) mOperationOutputs.get(operation);
    }

    public void setIMSOperationOutput(IMSOperation operation, IMSOutput operationOutput) {
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
    // Encoder mappings Methods
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

	public ServiceUnit getServiceUnit() {
		return serviceUnit;
	}

	public void setServiceUnit(ServiceUnit su) {
		serviceUnit = su;
	}

}
