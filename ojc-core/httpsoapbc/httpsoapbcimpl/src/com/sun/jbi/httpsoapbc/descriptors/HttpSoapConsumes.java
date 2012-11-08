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
 * @(#)Consumes.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.descriptors;

import java.util.List;
import javax.xml.namespace.QName;

public class HttpSoapConsumes implements HttpSoapEndpointIdentifier {
    QName interfaceName;
    QName serviceName;
    String endpointName;    
    String linkType;
    String appConfigName;
    boolean propagateSoapHeader;
    boolean enableWsdlQuery;
    List<HttpSoapHandler> handlers;
    
    /** Creates a new instance of Consumes */
    HttpSoapConsumes(QName ifName, QName serviceName, String endpointName, String linkType) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.linkType = linkType;
    }

    HttpSoapConsumes(QName ifName, QName serviceName, String endpointName, String linkType, String appConfigName, boolean propagateSoapHeader) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.linkType = linkType;
        this.appConfigName = appConfigName;
        this.propagateSoapHeader = propagateSoapHeader;
    }
    
    HttpSoapConsumes(QName ifName, QName serviceName, String endpointName, String linkType, String appConfigName, 
                     boolean propagateSoapHeader, boolean enableWsdlQuery, List<HttpSoapHandler> handlers) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.linkType = linkType;
        this.appConfigName = appConfigName;
        this.propagateSoapHeader = propagateSoapHeader;
        this.enableWsdlQuery = enableWsdlQuery;
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
     * @return the optional link-type declared, null if not declared
     */
    public String getLinkType() {
        return linkType;        
    }
    

    /**
     * @return whether this is a provider endpoint or a consumer endpoint address
     */    
    public boolean isProvider() {
        return false;
    }
    
    /**
     * @return the application configuration  name associated with the endpoint
     */
    public String getApplicationConfigurationName() {
        return appConfigName;
    }
    
    /**
     * @return false (not applicable to consumers)
     */
    public boolean getHostnameVerification() {
        return false;
    }
    
    /**
     * @return null (not applicable to consumers)
     */
    public Integer getConnectTimeout() {
        return null;
    }
    
    /**
     * @return null (not applicable to consumers)
     */
    public Integer getReadTimeout() {
        return null;
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
        return enableWsdlQuery;
    }
    
    /**
     * @return the handler list
     */
    public List<HttpSoapHandler> getHandlers () {
        return handlers;
    }
}
