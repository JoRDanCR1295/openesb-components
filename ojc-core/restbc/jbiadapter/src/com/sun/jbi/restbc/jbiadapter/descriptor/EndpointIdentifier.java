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
 * @(#)EndpointIdentifier.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.restbc.jbiadapter.descriptor;

import java.util.List;

import javax.xml.namespace.QName;

/**
 * The information and identifiers for a JBI internal endpoint
 * @author Sun Microsystems
 */
public interface EndpointIdentifier {

    /**
     * @return the interface QName declared
     */    
    public QName getInterfaceName();

    /**
     * @return the service QName declared
     */    
    public QName getServiceName();

    /**
     * @return the endpoints declared
     */    
    public String getEndpointName();

    /**
     * @return whether this is a provider endpoint or a consumer endpoint address
     */    
    public boolean isProvider();    
    
    /**
     * @return the application configuration  name associated with the endpoint
     */
    public String getApplicationConfigurationName();
    
    /**
     * 
     * @return filter chains
     */
    public List<Filter> getFilters();
    
    /**
     * 
     * @param filters filters chains to add
     */
    public void addFilters(List<Filter> filters);
    
    
}
