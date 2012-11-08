/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import javax.jbi.servicedesc.ServiceEndpoint;

import org.w3c.dom.Document;

import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMOutput;
import com.sun.jbi.dcombc.extensions.DCOMAddress;
import com.sun.jbi.dcombc.extensions.DCOMBinding;
import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.eManager.provider.EndpointStatus;

/**
 *
 * @author Chandrakanth Belde
 */
public class EndpointImpl implements Endpoint, Serializable {
	/**
	 *
	 */
    private static final long serialVersionUID = 3256727264572813369L;

    private QName mServiceName;

    private String mEndpointName;

    private Definition mDefinition;

    private int mEndpointState;

    private EndpointStatus mEndpointStatus;

    private int mEndpointType;

    private ServiceEndpoint mServiceEndpoint;

    private Document mServiceDescription;

    private DCOMAddress mDCOMAddress;

    private DCOMBinding mDCOMBinding;

    private Map mOperations; // <QName, DCOMOperation>

    private Map mOperationInputs; // <DCOMOperation, DCOMInput>

    private Map mOperationOutputs; // <DCOMOperation, DCOMOutput>

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
    // DCOM-specific Endpoint. These should probably be refactored
    // out in a better way so as to maintain their original
    // hierarchical structure
    //
    // //////

    public DCOMAddress getDCOMAddress() {
        return mDCOMAddress;
    }

    public void setDCOMAddress(DCOMAddress address) {
        mDCOMAddress = address;
    }

    public DCOMBinding getDCOMBinding() {
        return mDCOMBinding;
    }

    public void setDCOMBinding(DCOMBinding binding) {
        mDCOMBinding = binding;
    }

    public Map getDCOMOperations() {
        return mOperations;
    }

    public void setDCOMOperations(Map operations) {
        mOperations = operations;
    }

    public DCOMInput getDCOMOperationInput(DCOMOperation operation) {
        return (DCOMInput) mOperationInputs.get(operation);
    }

    public void setDCOMOperationInput(DCOMOperation operation, DCOMInput operationInput) {
        mOperationInputs.put(operation, operationInput);
    }

    public DCOMOutput getDCOMOperationOutput(DCOMOperation operation) {
        return (DCOMOutput) mOperationOutputs.get(operation);
    }

    public void setDCOMOperationOutput(DCOMOperation operation, DCOMOutput operationOutput) {
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
