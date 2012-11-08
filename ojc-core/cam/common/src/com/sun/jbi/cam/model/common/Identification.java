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
 * @(#)Identification.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import org.w3c.dom.Element;

/**
 *
 * @author ylee
 */

/**
 * Identification model
 */
public class Identification {
    /**
     * doc me
     */
    private String mName;
    
    /**
     * doc me
     */
    private String mDesc;
    
    /**
     * constructor
     * @param id id
     * @param name name
     * @param desc description
     * @param targetId target component id
     */
    protected Identification(String name, String desc) {
        this.mName = name;
        this.mDesc = desc;
    }
    
    /**
     * attribute
     * @return value
     */
    public String getName() {
        return this.mName;
    }
    
    /**
     * attribute
     * @return value
     */
    public String getDescription() {
        return this.mDesc;
    }
    
    /**
     * string value of the object
     * @return value
     */
    public String toString() {
        return "Name : " + getName() + "\n" + "Description : "
                + getDescription();
    }
    
    public static Identification createIdentification(String name,
            String desc) {
        return new Identification(name, desc);
    }
    
    public static Identification createIdentification(Element idInfoEl) {
        String name = null;
        String desc = null;
        
        Element nameEl = DOMUtil.util.getElement(idInfoEl, "name");
        if (nameEl != null) {
            name = DOMUtil.util.getTextData(nameEl);
        }
        
        Element descEl = DOMUtil.util.getElement(idInfoEl, "description");
        if (descEl != null) {
            desc = DOMUtil.util.getTextData(descEl);
        }
        return createIdentification(name, desc);
    }
    
}
