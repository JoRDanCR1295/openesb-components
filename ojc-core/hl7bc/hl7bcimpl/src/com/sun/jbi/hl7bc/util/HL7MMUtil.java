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


package com.sun.jbi.hl7bc.util;

import com.sun.jbi.hl7bc.Endpoint;
import java.util.Date;

import com.sun.jbi.mm.MMUtil;


/**
 *
 * @author ylee
 */
public class HL7MMUtil extends MMUtil {

    public static String COMPONENT_NAME = "sun-hl7-binding";


    public static String getSolutionGroup(Endpoint endpoint) {
        return getSolutionGroup(COMPONENT_NAME,getServiceInstanceID(endpoint));
    }


    public static String getSolutionGroup(String serviceInstanceId) {
        return getSolutionGroup(COMPONENT_NAME,serviceInstanceId);
    }


    public static boolean isNdcEnabled(Endpoint endpoint) {
        return isNdcEnabled(getServiceInstanceID(endpoint));
    }

    public static boolean isNdcEnabled(String serviceInstanceId) {
        return isNdcEnabled(COMPONENT_NAME, serviceInstanceId);
    }


    public static boolean isMonitorEnabled(String serviceInstanceId) {
        return isMonitorEnabled(COMPONENT_NAME, serviceInstanceId);
    }

    public static boolean isMonitorEnabled(Endpoint endpoint) {
        return isMonitorEnabled(getServiceInstanceID(endpoint));
    }


    public static String getServiceInstanceID(Endpoint endpoint) {
        return endpoint.getServiceName().toString() + "," + endpoint.getEndpointName();
    }


    // checkpoint methods

    /**
     * set a checkpoint
     * @param endpoint - service endpoint where the service instance ID can be derived
     * @param desc - this is use to annotate the checkpoint
     */
    public static String setCheckpoint(Endpoint endpoint, String desc) {
        return setCheckpoint(endpoint, desc, null);
    }

    public static String setCheckpoint(Endpoint endpoint, String desc, String payload) {
        String msgId = getUniqueID();
        String serviceInstanceId = getServiceInstanceID(endpoint);
        String solutionGroup = getSolutionGroup(serviceInstanceId);
        setCheckpoint(solutionGroup, msgId, COMPONENT_NAME, serviceInstanceId, desc, new Date(), payload);
        return msgId;
    }

    public static void setCheckpoint(String msgId, String desc, Endpoint endpoint) {
        String serviceInstanceId = getServiceInstanceID(endpoint);
        String solutionGroup = getSolutionGroup(serviceInstanceId);
        setCheckpoint(solutionGroup, msgId, COMPONENT_NAME, serviceInstanceId, desc, new Date());
    }

    public static void setCheckpoint(Endpoint endpoint, String msgId, String desc, String payload) {
        String serviceInstanceId = getServiceInstanceID(endpoint);
        String solutionGroup = getSolutionGroup(serviceInstanceId);
        setCheckpoint(solutionGroup, msgId, COMPONENT_NAME, serviceInstanceId, desc, new Date(), payload);
    }

    public static void setCheckpoint(String msgId, String serviceInstanceId, String desc,
            Date timestamp, String payload) {
        String solutionGroup = getSolutionGroup(serviceInstanceId);
        setCheckpoint(solutionGroup, msgId, COMPONENT_NAME, serviceInstanceId, desc, timestamp, payload);
    }


}
