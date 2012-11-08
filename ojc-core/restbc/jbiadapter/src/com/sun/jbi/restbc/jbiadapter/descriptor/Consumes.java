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

package com.sun.jbi.restbc.jbiadapter.descriptor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.namespace.QName;

/**
 *  Represents a service unit descriptor entry for <consumes>, for example for
 *  <code>
 *         <consumes xmlns:servicens=”http://www.seebeyond.com/eInsight/test7”
 *                    xmlns:ifns=”http://www.xmethods.net/sd/CurrencyExchangeService.wsdl” 
 *              interface-name=”ifns:CurrencyExchangePortType” 
 *              service-name=”servicens:Partner2” 
 *              endpoint-name=”Server” 
 *              link-type="standard"/>
 *  </code>
 */
public class Consumes implements EndpointIdentifier {
    
    private QName interfaceName;
    private QName serviceName;
    private String endpointName;    
    private String linkType;
    private String appConfigName;
    private List<Filter> filters = new ArrayList<Filter> ();
    
    /** Creates a new instance of Consumes */
    public Consumes(QName ifName, QName serviceName, String endpointName, String linkType) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.linkType = linkType;
    }

    public Consumes(QName ifName, QName serviceName, String endpointName, String linkType, String appConfigName) {
        this.interfaceName = ifName;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.linkType = linkType;
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
     * @return the filters
     */
    public List<Filter> getFilters() {
        return Collections.unmodifiableList(filters);
    }

    /**
     * @param filters the filters to add
     */
    public void addFilters(List<Filter> filters) {
        filters.addAll(filters);
    }
    
}
