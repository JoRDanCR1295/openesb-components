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

package com.sun.jbi.management.descriptor;

import javax.xml.namespace.QName;

/**
 *  Represents a service unit descriptor entry for <provides>, for example for
 *  <code>
 *        <provides xmlns:servicens=”http://www.seebeyond.com/eInsight/test2“
 *                    xmlns:ifns="urn:Test2TargetNamespace"  
 *              interface-name=”ifns:Test2PortType” 
 *              service-name=”servicens:PartnerLink” 
 *              endpoint-name=”Test2ServerRole”/>
 *  </code>
 */
public class Provides implements EndpointIdentifier {
    
    QName interfaceName;
    QName serviceName;
    String endpointName;
    String appConfigName;
    
    /** Creates a new instance of Provides */
    Provides(QName ifName, QName serviceName, String endpointName) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
    }
    
    Provides(QName ifName, QName serviceName, String endpointName, String appConfigName) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.appConfigName = appConfigName;
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
}
