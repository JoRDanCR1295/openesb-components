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
 * @(#)EndpointDataImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.packaging;

import com.sun.jbi.sapbc.Endpoint.EndpointType;
import javax.xml.namespace.QName;

/**
 * Class which represents the "raw" endpoint 
 * information independent of the format of the endpoint configuration file
 * 
 */
public class EndpointDataImpl implements EndpointData  {

    public EndpointDataImpl(QName interfaceName,
                               QName service,
                               String endPoint,
                               EndpointType direction) {
        mInterface = interfaceName;
        mService = service;
        mEndPoint = endPoint;
        mDirection = direction;
    }

    /**
     * A qualified name for the WSDL PortType
     *
     * @return The interface QName.
     */
    public QName getInterface() {
        return mInterface;
    }
    
    /**
     * A qualified name for the WSDL Service
     *
     * @return The interface QName.
     */
    public QName getService() {
        return mService;
    }

    /**
     * The name for the WSDL Port
     *
     * @return The interface QName.
     */
    public String getEndpoint() {
        return mEndPoint;
    }

    public EndpointType getDirection() {
        return mDirection;
    }

    private QName mInterface;
    private QName mService;
    private String mEndPoint;
    private EndpointType mDirection;
}
