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
 * @(#)Provides.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.descriptors;

import java.util.List;
import javax.xml.namespace.QName;

/**
 *  Represents a service unit descriptor entry for <provides>, for example for
 *  <code>
 *      <provides endpoint-name="port1" interface-name="ns1:portType1" service-name="ns1:service1">
 *          <application-config xmlns="http://www.sun.com/jbi/descriptor/configuration" name="foo"/>
 *          <hostname-verifier xmlns="http://www.sun.com/jbi/httpbc/ssl/hostname_verifier" verification="false"/>
 *      </provides>
 *  </code>
 */
public class HttpSoapProvides implements HttpSoapEndpointIdentifier {
    
    QName interfaceName;
    QName serviceName;
    String endpointName;
    String appConfigName;
    boolean hostnameVerification;
    boolean propagateSoapHeader;
    Integer connectTimeout;
    Integer readTimeout;
    List<HttpSoapHandler> handlers;
    
    /** Creates a new instance of Provides */
    HttpSoapProvides(QName ifName, QName serviceName, String endpointName) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
    }
    
    HttpSoapProvides(QName ifName, QName serviceName, String endpointName, String appConfigName) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.appConfigName = appConfigName;
    }

    HttpSoapProvides(QName ifName, QName serviceName, String endpointName, String appConfigName, 
                     boolean hostnameVerification, Integer connectTimeout, Integer readTimeout, boolean propagateSoapHeader) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.appConfigName = appConfigName;
        this.hostnameVerification = hostnameVerification;
        this.connectTimeout = connectTimeout;
        this.readTimeout = readTimeout;
        this.propagateSoapHeader = propagateSoapHeader;
    }
    
    HttpSoapProvides(QName ifName, QName serviceName, String endpointName, String appConfigName, 
                     boolean hostnameVerification, Integer connectTimeout, Integer readTimeout, boolean propagateSoapHeader, List<HttpSoapHandler> handlers) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.appConfigName = appConfigName;
        this.hostnameVerification = hostnameVerification;
        this.connectTimeout = connectTimeout;
        this.readTimeout = readTimeout;
        this.propagateSoapHeader = propagateSoapHeader;
        this.handlers = handlers;
    }

    /**
     * @return the interface QName declared
     */
    public QName getInterfaceName() {
        return interfaceName;
    }

    /**
     * @return the service QName declared
     */
    public QName getServiceName() {
        return serviceName;        
    }

    /**
     * @return the endpoint name declared
     */
    public String getEndpointName() {
        return endpointName;        
    }
    
    /**
     * @return whether this is a provider endpoint or a consumer endpoint address
     */    
    public boolean isProvider() {
        return true;
    }    
    
    /**
     * @return the application configuration  name associated with the endpoint
     */
    public String getApplicationConfigurationName() {
        return appConfigName;
    }
    
    /**
     * @return the hostname verification flag associated with the endpoint (only applicable to 'providers' and https connections)
     */
    public boolean getHostnameVerification() {
        return hostnameVerification;
    }
    
    /**
     * @return the HTTP connect timeout configuration associated with the endpoint (only applicable to 'providers')
     */
    public Integer getConnectTimeout() {
        return connectTimeout;
    }
    
    /**
     * @return the HTTP read timeout configuration associated with the endpoint (only applicable to 'providers')
     */
    public Integer getReadTimeout() {
        return readTimeout;
    }
    
    /**
     * @return the SOAP header propagation flag
     */
    public boolean getPropagateSoapHeader() {
        return propagateSoapHeader;
    }
    
    
    /**
     * @return the flag to determine whether or not to enable ?wsdl
     */
    public boolean getEnableWsdlQuery() {
        return false;
    }
    
    /**
     * @return the handler list
     */
    public List<HttpSoapHandler> getHandlers () {
        return handlers;
    }
    
}
