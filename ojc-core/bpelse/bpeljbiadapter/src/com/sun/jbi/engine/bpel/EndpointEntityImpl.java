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
import javax.xml.namespace.QName;

/**
 *
 * @author Vitaly Bychkov
 */
public class EndpointEntityImpl implements EndpointEntity {

    private QName mServiceName;
    private String mEndpointName;
    private String mAddress;
    private QName mInterface;

    /**
     * defines any one endpoint implementing the correspondent Interface
     * @param portType Interface QName value
     */
    public EndpointEntityImpl(QName portType) {
        this(null, null, null, portType);
    }

    /**
     * defines endpoint with correspondent ServiceName and Endpoint Name
     * @param serviceName Service Name
     * @param epName Endpoint Name
     */
    public EndpointEntityImpl(QName serviceName, String epName) {
        this(serviceName, epName, null, null);
    }

    /**
     * defines endpoint with correspondent ServiceName, EndpointName and
     * implementing the correspondent Interface
     * @param serviceName Service Name
     * @param epName Endpoint Name
     * @param portType Interface QName value
     */
    public EndpointEntityImpl(QName serviceName, String epName, QName portType) {
        this(serviceName, epName, null, portType);
    }

    /**
     * defines endpoint with correspondent ServiceName, EndpointName, Address and
     * implementing the correspondent Interface
     * @param serviceName Service Name
     * @param epName Endpoint Name
     * @param address Endpoint Address
     * @param portType Interface QName value
     */
    public EndpointEntityImpl(QName serviceName, String epName, String address, QName portType) {
        mServiceName = serviceName;
        mEndpointName = epName;
        mAddress = address;
        mInterface = portType;
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
    }

    @Override
    public String toString() {
        return "interface: "+getInterface()+"; " +
                "Endpoint("+(mServiceName == null ? "" : mServiceName.toString())+", " +
                (mEndpointName == null ? "" : mEndpointName)+"); Address: "+(mAddress == null ? ""  : mAddress);
    }
}
