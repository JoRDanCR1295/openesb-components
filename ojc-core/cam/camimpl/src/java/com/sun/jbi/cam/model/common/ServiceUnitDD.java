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
 * @(#)ServiceUnitDD.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import com.sun.jbi.cam.model.common.Identification;

import org.w3c.dom.Element;

/**
 *
 * @author ylee
 */

/**
 * Service Unit Deployment Descriptor model
 */
public class ServiceUnitDD {
    /**
     * doc me
     */
    Identification mIdInfo;
    
    /**
     * doc me
     */
    private String mTargetName;
    
    /**
     * constructor
     * @param id id
     * @param name name
     * @param desc description
     * @param targetId target component id
     */
    protected ServiceUnitDD(Identification idInfo, String targetName) {
        this.mIdInfo = idInfo;
        this.mTargetName = targetName;
    }
    
    /**
     * attribute
     * @return value
     */
    public String getName() {
        return this.mIdInfo.getName();
    }
    
    /**
     * attribute
     * @return value
     */
    public String getDescription() {
        return this.mIdInfo.getDescription();
    }
    
    /**
     * attribute
     * @return value
     */
    public String getTargetName() {
        return this.mTargetName;
    }
    
    /**
     * string value of the object
     * @return value
     */
    public String toString() {
        return "Name : " + getName() + "\n" + "Description : "
                + getDescription() + "\n" + "TargetName : "
                + getTargetName();
    }
    
    public static ServiceUnitDD createServiceUnitDD(Identification idInfo,
            String targetName) {
        return new ServiceUnitDD(idInfo, targetName);
    }
    
    public static ServiceUnitDD createServiceUnitDD(Element suEl) {
        Identification idInfo = null;
        String targetName = null;
        
        Element idInfoEl = DOMUtil.util.getElement(suEl, "identification");
        if (idInfoEl != null) {
            idInfo = Identification.createIdentification(idInfoEl);
        }
        
        Element targetNameEl = DOMUtil.util.getElement(suEl,
                "component-name");
        if (targetNameEl != null) {
            targetName = DOMUtil.util.getTextData(targetNameEl);
        }
        
        return createServiceUnitDD(idInfo, targetName);
    }
    
}
