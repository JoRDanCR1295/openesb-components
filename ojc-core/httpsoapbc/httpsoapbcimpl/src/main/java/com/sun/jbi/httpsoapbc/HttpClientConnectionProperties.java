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
 * @(#)EndpointFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
 
package com.sun.jbi.httpsoapbc;

/**
 * This class encapsulates HTTP client connection properties, including
 * Timeout, SSL hostname verifier flag
 */
public class HttpClientConnectionProperties {
    private boolean hostnameVerification;
    private Integer connectTimeout;
    private Integer readTimeout;
    
    // default constructor
    public HttpClientConnectionProperties() {} 
    
    public HttpClientConnectionProperties(boolean hostnameVerification, Integer connectTimeout, Integer readTimeout ) {
        this.hostnameVerification = hostnameVerification;
        this.connectTimeout = connectTimeout;
        this.readTimeout = readTimeout;
    } 
    
    /**
     * returns the hostname verification flag for the HTTPS connection
     */
    public boolean getHostnameVerification() {
        return this.hostnameVerification;
    }
    
    /**
     * sets the hostname verification flag for HTTPS connection
     * @param hostnameVerification  the hostname verification flag for the HTTPS connection
     */
    public void setHostnameVerification(boolean hostnameVerification) {
        this.hostnameVerification = hostnameVerification;
    }
    
    /**
     * returns the HTTP connect timeout configuration
     */
    public Integer getConnectTimeout() {
        return this.connectTimeout;
    }
    
    /**
     * sets the HTTP connect timeout configuration
     * @param connectTimeout the Integer connect timeout configuration
     */
    public void setConnectTimeout(Integer connectTimeout) {
        this.connectTimeout = connectTimeout;
    }
    
    /**
     * returns the HTTP read timeout configuration
     */
    public Integer getReadTimeout() {
        return this.readTimeout;
    }
    
    
    /**
     * sets the HTTP read timeout configuration
     * @param readTimeout the Integer read timeout configuration
     */
    public void setReadTimeout(Integer readTimeout) {
        this.readTimeout = readTimeout;
    }
}