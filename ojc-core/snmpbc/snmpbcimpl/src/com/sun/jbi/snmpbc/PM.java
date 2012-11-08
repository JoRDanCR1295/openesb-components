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
 * @(#)PM.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

/**
 * represents operation for Performance Manager, used to invoke methods such
 * as Get, GetNext, etc. on the SNMP Engine.
 * 
 * @author echou
 */
public class PM extends GenericOperation {
    
    private String mofIdRef;
    private ServiceEndpoint serviceEndpoint;
    
    /** Creates a new instance of FM 
     * @param mofIdRef 
     * @param operationName 
     * @param endpoint 
     */
    public PM(String mofIdRef,
            QName operationName, 
            Endpoint endpoint) {
        super(operationName, endpoint);
        
        this.mofIdRef = mofIdRef;
    }
    
    public String getMOFIdRef() {
        return mofIdRef;
    }
    
    public void activatePM(ComponentContext componentContext) throws JBIException {
        serviceEndpoint = componentContext.activateEndpoint(
                getServiceName(), getEndpointName());
    }
    
    public void deactivatePM(ComponentContext componentContext) throws JBIException {
        componentContext.deactivateEndpoint(serviceEndpoint);
    }
    
    
    public static String getEndpointKey(QName serviceName, String endpointName) {
        return serviceName.toString() + "#" + endpointName;
    }
}
