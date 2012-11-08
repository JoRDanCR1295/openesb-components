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
 * @(#)JBIServiceUnitInformation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.common;

import java.util.logging.Logger;

/**
 * @author Sun MicrosystemInc.
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class JBIServiceUnitInformation {
    
    private Logger logger = 
            Logger.getLogger(JBIServiceUnitInformation.class.getName());

    private String endpointName;
    private String serviceName;
    private String interfaceName;
    private String fullyQualifiedserviceName;

    /**
     * @param endpointName
     * @param serviceName
     * @param interfaceName
     */
    public JBIServiceUnitInformation(String endpointName, String serviceName,
            String fullyQualifiedserviceName,String interfaceName) {
        super();
        this.endpointName = endpointName;
        this.serviceName = serviceName;
        this.fullyQualifiedserviceName = fullyQualifiedserviceName;
        this.interfaceName = interfaceName;
    }
    /**
     * 
     */
    public JBIServiceUnitInformation() {
        super();
    }

    /**
     * @return Returns the endpointName.
     */
    public String getEndpointName() {
        return endpointName;
    }
    /**
     * @param endpointName The endpointName to set.
     */
    public void setEndpointName(String endpointName) {
        this.endpointName = endpointName;
    }
    /**
     * @return Returns the interfaceName.
     */
    public String getInterfaceName() {
        return interfaceName;
    }
    /**
     * @param interfaceName The interfaceName to set.
     */
    public void setInterfaceName(String interfaceName) {
        this.interfaceName = interfaceName;
    }
    /**
     * @return Returns the serviceName.
     */
    public String getServiceName() {
        return serviceName;
    }
    /**
     * @param serviceName The serviceName to set.
     */
    public void setServiceName(String serviceName) {
        this.serviceName = serviceName;
    }
    public String getFullyQualifiedserviceName() {
        return fullyQualifiedserviceName;
    }
    public void setFullyQualifiedserviceName(String fullyQualifiedserviceName) {
        this.fullyQualifiedserviceName = fullyQualifiedserviceName;
    }
    
    // will uniquely identify this instance
    public String toString() {
        return this.endpointName+ "_" + this.fullyQualifiedserviceName;
    }
}
