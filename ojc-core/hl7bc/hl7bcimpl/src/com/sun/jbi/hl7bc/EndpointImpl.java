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

package com.sun.jbi.hl7bc;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7Binding;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.extensions.HL7Input;
import com.sun.jbi.hl7bc.extensions.HL7Output;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extensions.HL7CommunicationControls;

public class EndpointImpl implements Endpoint, Serializable {

    private QName mServiceName;

    private String mEndpointName;

    private Definition mDefinition;

    private int mEndpointState;

    private EndpointStatus mEndpointStatus;

    private int mEndpointType;

    private ServiceEndpoint mServiceEndpoint;

    private Document mServiceDescription;

    private String mServiceUnitPath;

    private HL7Address mHL7Address;

    private HL7Binding mHL7Binding;

    private HL7ProtocolProperties mHL7ProtocolProperties;

    private HL7CommunicationControls mHL7CommunicationControls;

    private Map mOperations; // <QName, HL7Operation>

    private Map mOperationInput; // <HL7Operation, HL7Input>

    private Map mOperationOutput; // <HL7Operation, HL7Output>

    private Map mPartMapping;

    private Map mOperationMEPs;
    
    private ServiceUnit mServiceUnit;

    private RedeliveryConfig mRedeliveryConfig;

    private List mXsds;

    private int maxConcurrencyLimit = -1;

    private Semaphore mThrottleSema = null;
    
    private long mMsgSentTimeStamp = -1;
    
    private long mACKMsgReceivedTimeStamp = -1;

    public EndpointImpl() {
        mOperations = new HashMap();
        mOperationInput = new HashMap();
        mOperationOutput = new HashMap();
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
    // Message Exchange Pattern Methods
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

    public void setServiceUnitPath(String serviceUnitPath) {
        mServiceUnitPath = serviceUnitPath;
    }

    public String getServiceUnitPath() {
        return mServiceUnitPath;
    }

    // //////
    //
    // HL7-specific Endpoint. These should probably be refactored
    // out in a better way so as to maintain their original
    // hierarchical structure
    //
    // //////

    public HL7Address getHL7Address() {
        return mHL7Address;
    }

    public void setHL7Address(HL7Address address) {
        mHL7Address = address;
    }

    public HL7ProtocolProperties getHL7ProtocolProperties() {
        return mHL7ProtocolProperties;
    }

    public void setHL7ProtocolProperties(HL7ProtocolProperties protocolProperties) {
        mHL7ProtocolProperties = protocolProperties;
    }

    public HL7CommunicationControls getHL7CommunicationControls() {
        return mHL7CommunicationControls;
    }

    public void setHL7CommunicationControls(HL7CommunicationControls communicationControls) {
        mHL7CommunicationControls = communicationControls;
    }

    public HL7Binding getHL7Binding() {
        return mHL7Binding;
    }

    public void setHL7Binding(HL7Binding binding) {
        mHL7Binding = binding;
    }

    public Map getHL7Operations() {
        return mOperations;
    }

    public void setHL7Operations(Map operations) {
        mOperations = operations;
    }

    public HL7Input getHL7OperationInput(HL7Operation operation) {
        return (HL7Input) mOperationInput.get(operation);
    }

    public void setHL7OperationInput(HL7Operation operation, HL7Input operationInput) {
        mOperationInput.put(operation, operationInput);
    }

    public HL7Output getHL7OperationOutput(HL7Operation operation) {
        return (HL7Output) mOperationOutput.get(operation);
    }

    public void setHL7OperationOutput(HL7Operation operation, HL7Output operationOutput) {
        mOperationOutput.put(operation, operationOutput);
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

    public void setXsdsList(List xsds) {
        mXsds = xsds;
    }

    public List getXsdsList() {
        return mXsds;
    }

    public RedeliveryConfig getRedeliveryConfiguration() {
        return mRedeliveryConfig;
    }

    public void setRedeliveryConfiguration(RedeliveryConfig redeliveryConfig) {
        mRedeliveryConfig = redeliveryConfig;
    }

    public int getMaxConcurrencyLimit() {
        return maxConcurrencyLimit;
    }

    public void setMaxConcurrencyLimit(int maxConcurrencyLimit) {
        if (mThrottleSema == null && maxConcurrencyLimit > 0) {
            mThrottleSema = new Semaphore(maxConcurrencyLimit);
        }
        this.maxConcurrencyLimit = maxConcurrencyLimit;
    }

    public void acquireThrottle() throws InterruptedException {
        if (maxConcurrencyLimit <= 0) // no throttling
            return;
        if (mThrottleSema == null && maxConcurrencyLimit > 0)
            throw new IllegalStateException(
                    "Endpoint.acquireThrottle() called when the throttling semaphore is not initialized yet.");
        mThrottleSema.acquire();
    }

    public void releaseThrottle() {
        if (maxConcurrencyLimit <= 0) // no throttling
            return;
        if (mThrottleSema == null && maxConcurrencyLimit > 0)
            throw new IllegalStateException(
                    "Endpoint.releaseThrottle() called when the throttling semaphore is not initialized yet.");
        mThrottleSema.release();
    }

    public boolean tryAcquireThrottle() {
        if (mThrottleSema == null)
            throw new IllegalStateException(
                    "Endpoint.tryAcquireThrottle() called when the throttling semaphore is not initialized yet.");
        return mThrottleSema.tryAcquire();
    }

    public boolean tryAcquireThrottle(long timeout, TimeUnit tu) throws InterruptedException {
        if (mThrottleSema == null)
            throw new IllegalStateException(
                    "Endpoint.tryAcquireThrottle(timeout, timeunit) called when the throttling semaphore is not initialized yet.");
        return mThrottleSema.tryAcquire(timeout, tu);
    }

    /**
     * Get the unique name of this endpoint instance
     */
    public String getUniqueName() {
        String serviceNamespaceURI = (getServiceName().getNamespaceURI() == null) ? ""
                : getServiceName().getNamespaceURI();
        String serviceName = getServiceName().getLocalPart();
        String endpointName = getEndpointName();

        return serviceNamespaceURI + ":" + serviceName + ":" + endpointName;
    }
    
    public ServiceUnit getServiceUnit() {
        return mServiceUnit;
    }

    public void setServiceUnit(ServiceUnit su) {
        mServiceUnit = su;
    }
    
    public void setLastHL7MsgSentTimeStamp(long ts){
        mMsgSentTimeStamp = ts;
    }
    
    public long getLastHL7MsgSentTimeStamp(){
        return mMsgSentTimeStamp;
    }
    
    public void setLastACKMsgReceivedTimeStamp(long ts){
        mACKMsgReceivedTimeStamp =  ts;
    }
    
    public long getLastACKMsgReceivedTimeStamp(){
        return mACKMsgReceivedTimeStamp;
    }
}
