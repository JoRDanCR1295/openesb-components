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

package com.sun.jbi.imsbc.packaging;

/**
 * Class which represents the "raw" endpoint 
 * information independent of the format of the endpoint configuration file
 *
 * @author Sun Microsystems
 * 
 */
public class EndpointDataImpl implements EndpointData {

    private String mInterface;

    private String mService;

    private String mEndPoint;

    private int mDirection;

	private String mAppConf;

    protected EndpointDataImpl(String interfaceName, String service, String endPoint,  String appconfName, int direction) {
        mInterface = interfaceName;
        mService = service;
        mEndPoint = endPoint;
        mDirection = direction;
		mAppConf = appconfName;
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

     /**
     * The name of the application configuration object associated with this
     * endpoint, if any.
     *
     * @return The name of the associated application configuration object, if
     *         any, or else a blank string.
     */
    public String getApplicationConfigurationObjectName() {
        return (mAppConf != null ? mAppConf : "");
    }

}
