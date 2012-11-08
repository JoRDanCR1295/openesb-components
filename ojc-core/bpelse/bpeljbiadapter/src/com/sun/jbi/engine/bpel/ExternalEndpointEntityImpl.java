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
package com.sun.jbi.engine.bpel;

import com.sun.jbi.engine.bpel.core.bpel.engine.EndpointEntity;
import com.sun.jbi.engine.bpel.core.bpel.engine.ExtEndpointEntity;
import javax.xml.namespace.QName;

/**
 *
 * @author Vitaly Bychkov
 */
public class ExternalEndpointEntityImpl implements ExtEndpointEntity {

    private QName mServiceName;
    private QName mInterface;
    private String mEndpointName;
    private String mAddress;
    private EndpointEntity mInternalEndpoint;

    /**
     *
     * defines External Endpoint with correspondent ServiceName, EndpointName, Address,
     * implementing the correspondent Interface and having the correspondent Internal Ednpoint.
     * External and Internal Endpoints both implementing the same Interface.
     * @param serviceName Service Name
     * @param epName Endpoint Name
     * @param address Endpoint Address
     * @param intEP Internal Endpoint Entity
     * @param portType Interface QName value
     */
    public ExternalEndpointEntityImpl(QName serviceName, String epName,
            String address, EndpointEntity intEP, QName portType)
    {
        mServiceName = serviceName;
        mEndpointName = epName;
        mAddress = address;
        mInternalEndpoint = intEP;
        setInterface(portType);
    }

    /**
     *
     * defines External Endpoint with correspondent ServiceName, EndpointName, Address
     * and having the correspondent Internal Ednpoint.
     * @param serviceName Service Name
     * @param epName Endpoint Name
     * @param address Endpoint Address
     * @param intEP Internal Endpoint Entity
     */
    public ExternalEndpointEntityImpl(QName serviceName, String epName,
            String address, EndpointEntity intEP)
    {
        this(serviceName, epName, address, intEP, null);
    }

    /**
     * defines any one endpoint implementing the correspondent Interface
     * @param portType Interface QName value
     */
    public ExternalEndpointEntityImpl(QName portType) {
        this(null, null, null, null, portType);
    }

    public EndpointEntity getInternalEndpoint() {
        return mInternalEndpoint;
    }

    public QName getServiceName() {
        return mServiceName;
    }

    public String getEndpointName() {
        return mEndpointName;
    }

    public String getAddress() {
        return mAddress;
    }

    public QName getInterface() {
        return mInterface;
    }

    public void setInterface(QName portType) {
        mInterface = portType;
        EndpointEntity inEP = getInternalEndpoint();
        if (inEP != null) {
            inEP.setInterface(portType);
        }
    }

    @Override
    public String toString() {
        return "Interface: "+getInterface()+"; " +
                "ExternalEndpoint("+(mServiceName == null ? "" : mServiceName.toString())+", " +
                (mEndpointName == null ? "" : mEndpointName)+")"+
                " internalEndpoint: "+(mInternalEndpoint == null ? "" : mInternalEndpoint.toString());
    }

}
