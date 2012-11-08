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
package com.sun.jbi.filebc;

import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileBinding;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import java.util.UUID;
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
    private FileAddress mFileAddress;
    private String mWorkAreaDir;
    private FileBinding mFileBinding;
    private Map mOperations;
    private Map mOperationMEPs;
    private Map mPartMapping;
    private String mEPUUID;
    protected long mMessageIdCounter = 0;
    // add service Unit id. Note this has been added later. Its not guaranteed to be set.  
    private String mServiceUnitId;
    protected RedeliveryConfig mRedeliveryConfig;
    protected int maxConcurrencyLimit = -1;

    public EndpointImpl() {
        mOperations = new HashMap();
        mOperationMEPs = new HashMap();
        mPartMapping = new HashMap();
    }

    public String getServiceUnitID() {
        return mServiceUnitId;
    }

    public void setServiceUnitID(String suId) {
        mServiceUnitId = suId;
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
    public FileAddress getFileAddress() {
        return mFileAddress;
    }

    public void setFileAddress(FileAddress address) {
        mFileAddress = address;
    }

    public FileBinding getFileBinding() {
        return mFileBinding;
    }

    public void setFileBinding(FileBinding binding) {
        mFileBinding = binding;
    }

    public Map getFileOperations() {
        return mOperations;
    }

    public void setFileOperations(Map operations) {
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

    public synchronized long getCRMPMessageId() {
        return (++mMessageIdCounter);
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
        this.maxConcurrencyLimit = maxConcurrencyLimit;
    }

    /**
     * Get the unique name of this endpoint instance
     */
    public String getUniqueName() {
        String serviceNamespaceURI =
                (getServiceName().getNamespaceURI() == null) ? "" : getServiceName().getNamespaceURI();
        String serviceName = getServiceName().getLocalPart();
        String endpointName = getEndpointName();

        return serviceNamespaceURI + ":" + serviceName + ":" + endpointName;
    }

    public String getWorkAreaDir() {
        return mWorkAreaDir;
    }

    public void setWorkAreaDir(String workAreaDir) {
        mWorkAreaDir = workAreaDir;
    }

    public String getEPUUID() {
        if (mEPUUID == null) {
            mEPUUID = (mEndpointType == Endpoint.EndpointType.INBOUND ? "IB" : "OB").concat(UUID.nameUUIDFromBytes(getUniqueName().getBytes()).toString());
        }
        return mEPUUID;
    }
}
