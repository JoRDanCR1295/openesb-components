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
package com.sun.jbi.engine.bpel.core.bpel.engine;

import javax.xml.namespace.QName;

/**
 *
 * @author Vitaly Bychkov
 */
public interface EndpointEntity {

    /**
     * returns endpoint's Interface(in terms of wsdl 1.1 PortType) name
     * @return Interface Interface name
     */
    QName getInterface();

    /**
     * sets endpoint's Interface(in terms of wsdl 1.1 PortType) name
     * Interface is the only one endpoint property which can be setted separately
     * @param portType Interface QName value
     */
    void setInterface(QName portType);

    /**
     * returns ServiceName
     * @return ServiceName - ServiceName
     */
    QName getServiceName();

    /**
     * returns EndpointName(in terms of wsdl 1.1 Port name)
     * @return EndpointName EndpointName
     */
    String getEndpointName();

    /**
     * returns endpoint address. Returns null in case internal Endpoint or in case address wasn't setuped
     * @return Address Endpoint Address
     */
    String getAddress();

}
