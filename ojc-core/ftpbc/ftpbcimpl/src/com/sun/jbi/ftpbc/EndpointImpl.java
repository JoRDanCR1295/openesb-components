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
package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.extensions.FTPAddress;
import com.sun.jbi.ftpbc.extensions.FTPBinding;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.eManager.provider.EndpointStatus;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import javax.xml.namespace.QName;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;

import org.w3c.dom.Document;

/**
 * This class implements the EndPoint interface for
 * for FTP Binding component.
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
    private FTPAddress mFTPAddress;
    private FTPBinding mFTPBinding;
    private Map mOperations;
    private Map mOperationMEPs;
    private Map mPartMapping;
    private Map<String, String> mOperationUUISs;
    private int maxConcurrencyLimit = -1;
    private RedeliveryConfig mRedeliveryConfig;
    private ServiceUnit mSU; // su associated with this ep
    private Semaphore mThrottleSema;
    // add service Unit id. Note this has been added later. Its not guaranteed to be set.  
    private String mServiceUnitId;

    public EndpointImpl() {
        mOperations = new HashMap();
        mOperationMEPs = new HashMap();
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
    //  FTP-specific Endpoint.
    //
    ////////
    public FTPAddress getAddress() {
        return mFTPAddress;
    }

    public void setAddress(FTPAddress address) {
        mFTPAddress = address;
    }

    public FTPBinding getBinding() {
        return mFTPBinding;
    }

    public void setBinding(FTPBinding binding) {
        mFTPBinding = binding;
    }

    public Map getOperations() {
        return mOperations;
    }

    public void setOperations(Map operations) {
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

    /**
     * Get the unique name of this endpoint instance
     */
    public String getUniqueName() {
        String serviceNamespaceURI = getServiceName().getNamespaceURI();
        String serviceName = getServiceName().getLocalPart();
        String endpointName = getEndpointName();
        return getUniqueName(serviceNamespaceURI, serviceName, endpointName, isInbound());
    }

    /**
     * Utility method to create the unique names with explicit arguments
     */
    public static String getUniqueName(String aServiceNamespaceURI,
            String aServiceName,
            String aEndpointName,
            boolean isInbound) {
        String aEndpointType =
                isInbound ? ENDPOINT_TYPE_INBOUND : ENDPOINT_TYPE_OUTBOUND;
        return aServiceNamespaceURI + "," + aServiceName + "," + aEndpointName + "," + aEndpointType;
    }

    public boolean isInbound() {
        return mEndpointType == EndpointType.INBOUND;
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

    public RedeliveryConfig getRedeliveryConfiguration() {
        return mRedeliveryConfig;
    }

    public void setRedeliveryConfiguration(RedeliveryConfig redeliveryConfig) {
        mRedeliveryConfig = redeliveryConfig;
    }

    public void acquireThrottle() throws InterruptedException {
        if (maxConcurrencyLimit <= 0) // no throttling
        {
            return;
        }
        if (mThrottleSema == null && maxConcurrencyLimit > 0) {
            throw new IllegalStateException("Endpoint.acquireThrottle() called when the throttling semaphore is not initialized yet.");
        }
        mThrottleSema.acquire();
    }

    public void releaseThrottle() {
        if (maxConcurrencyLimit <= 0) // no throttling
        {
            return;
        }
        if (mThrottleSema == null && maxConcurrencyLimit > 0) {
            throw new IllegalStateException("Endpoint.releaseThrottle() called when the throttling semaphore is not initialized yet.");
        }
        mThrottleSema.release();
    }

    public boolean tryAcquireThrottle() {
        if (mThrottleSema == null) {
            throw new IllegalStateException("Endpoint.tryAcquireThrottle() called when the throttling semaphore is not initialized yet.");
        }
        return mThrottleSema.tryAcquire();
    }

    public boolean tryAcquireThrottle(long timeout, TimeUnit tu) throws InterruptedException {
        if (mThrottleSema == null) {
            throw new IllegalStateException("Endpoint.tryAcquireThrottle(timeout, timeunit) called when the throttling semaphore is not initialized yet.");
        }
        return mThrottleSema.tryAcquire(timeout, tu);
    }

    public String getServiceUnitID() {
        return mServiceUnitId;
    }

    public void setServiceUnitID(String suId) {
        mServiceUnitId = suId;
    }

    public ServiceUnit getServiceUnit() {
        return mSU;
    }

    public void setServiceUnit(ServiceUnit su) {
        mSU = su;
    }

    /**
     * Get the UUID for operation if it is in the cache
     * otherwise, generate one and put into cache
     * @return UUID for the operation;
     */
    public synchronized String getOperationUUID(QName opName) {
        String uuid = null;
        if (mOperationUUISs == null) {
            mOperationUUISs = new HashMap();
        }
        String key = opName.toString();
        key.concat(":::").concat((String) mOperationMEPs.get(opName)); // also attach MEP
        uuid = mOperationUUISs.get(key);
        if (uuid == null) {
            mOperationUUISs.put(key, (uuid = UUID.nameUUIDFromBytes(key.getBytes()).toString()));
        }
        return uuid;
    }
}
