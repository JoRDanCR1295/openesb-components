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

package com.sun.jbi.swiftbc.packaging;

/**
 * Class which represents the "raw" endpoint 
 * information independent of the format of the endpoint configuration file
 * 
 */
public class EndpointDataImpl implements EndpointData {

    private String mInterface;

    private String mService;

    private String mEndPoint;

    private int mDirection;

    protected EndpointDataImpl(String interfaceName, String service, String endPoint, int direction) {
        mInterface = interfaceName;
        mService = service;
        mEndPoint = endPoint;
        mDirection = direction;
    }

    public String getInterface() {
        return mInterface;
    }

    public String getService() {
        return mService;
    }

    public String getEndpoint() {
        return mEndPoint;
    }

    public int getDirection() {
        return mDirection;
    }

}
