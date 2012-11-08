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
 * @(#)NormalizedMessageProperties.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.httpsoapbc;

/**
  * This class captures all the normalized message properties defined
  * in HTTP binding component
  */
public class NormalizedMessageProperties {
    /** Following set of properties are directional, i.e. the "inbound" properties are read-only
      * and propagated to provide binding context info to the application, while
      * "outbound" properties are meant to be used to either dynamically overwrite a static 
      * configuration (e.g. address url), or to set custom protocol info (e.g. SOAP headers)
      */
    public static final String SOAP_HEADER_PROPERTY = "org.glassfish.openesb.headers.soap";
    public static final String SOAP_FAULTCODE_PROPERTY = "org.glassfish.openesb.soap.faultcode";
    public static final String SOAP_FAULTSTRING_PROPERTY = "org.glassfish.openesb.soap.faultstring";
    public static final String SOAP_FAULTACTOR_PROPERTY = "org.glassfish.openesb.soap.faultactor";
    public static final String INBOUND_HTTP_HEADERS_PROPERTY = "org.glassfish.openesb.inbound.http.headers";
    public static final String OUTBOUND_HTTP_HEADERS_PROPERTY = "org.glassfish.openesb.outbound.http.headers";
    public static final String RESPONSE_HTTP_HEADERS_PROPERTY = "org.glassfish.openesb.response.http.headers";
    public static final String OUTBOUND_CUSTOM_PROPERTY = "org.glassfish.openesb.outbound.custom.properties";
    public static final String INBOUND_ADDRESS_URL = "org.glassfish.openesb.inbound.address.url";
    public static final String OUTBOUND_ADDRESS_URL = "org.glassfish.openesb.outbound.address.url";
    public static final String OUTBOUND_BASIC_AUTH_USERNAME = "org.glassfish.openesb.outbound.basicauth.username";
    public static final String OUTBOUND_BASIC_AUTH_PASSWORD = "org.glassfish.openesb.outbound.basicauth.password";
    
    /**
     * Following set includes "generic" properties for message grouping and sequencing
     * (to provide unique message identifier at the application level)
     */
    public static final String NM_GROUP_ID_PROPERTY = "org.glassfish.openesb.messaging.groupid";
    public static final String NM_MSG_ID_PROPERTY = "org.glassfish.openesb.messaging.messageid";
    
    /**
     * Following set has some HTTP header information included in the 
     * org.glassfish.openesb.inbound.http.headers property
     */
    public static final String HTTP_HEADER_ClIENT_HOST_NAME = "ClientHostName";
    public static final String HTTP_HEADER_CLIENT_PORT_NUMBER = "ClientPortNumber";
    
}
