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
 * @(#)JBIServiceUnitStatus.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.management;

import java.io.Serializable;
import java.util.logging.Logger;

/**
 * @author ylee
 * @author Graj
 *
 */
public class JBIServiceUnitStatus implements Serializable {

    private static final String SERVICE_UNIT_NAME = "ServiceUnitName";
    private static final String SERVICE_UNIT_DESCRIPTION = "ServiceUnitDescription";
    private static final String STATUS_TYPE = "Status";
    private static final String TARGET_NAME_TYPE = "TargetName";

    protected String serviceUnitName; // 0B000000-DBBABDE9030100-0A12437F-01
    protected String serviceUnitDescription; // This represents the Application Sub-Assembly
    protected String status; // UNKNOWN
    protected String targetName; // xsltserviceengine-9bfbff60-467d-11d9-9669-0800200c9a67

    private Logger logger = Logger.getLogger(JBIServiceUnitStatus.class.getName());
    
    /**
     *
     */
    public JBIServiceUnitStatus() {
        super();
        // TODO Auto-generated constructor stub
    }

    /**
     * @return Returns the serviceUnitDescription.
     */
    public String getServiceUnitDescription() {
        return this.serviceUnitDescription;
    }
    
    /**
     * @param serviceUnitDescription The serviceUnitDescription to set.
     */
    public void setServiceUnitDescription(String serviceUnitDescription) {
        this.serviceUnitDescription = serviceUnitDescription;
    }
    
    /**
     * @return Returns the serviceUnitName.
     */
    public String getServiceUnitName() {
        return this.serviceUnitName;
    }
    
    /**
     * @param serviceUnitName The serviceUnitName to set.
     */
    public void setServiceUnitName(String serviceUnitName) {
        this.serviceUnitName = serviceUnitName;
    }
    
    /**
     * @return Returns the status.
     */
    public String getStatus() {
        return this.status;
    }
    
    /**
     * @param status The status to set.
     */
    public void setStatus(String status) {
        this.status = status;
    }
    
    /**
     * @return Returns the targetName.
     */
    public String getTargetName() {
        return this.targetName;
    }
    
    /**
     * @param targetName The targetName to set.
     */
    public void setTargetName(String targetName) {
        this.targetName = targetName;
    }
    
    public static void main(String[] args) {
    }
}
