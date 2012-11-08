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
 * @(#)DisplayInformation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration.model;

import java.io.Serializable;

/**
 * @author graj
 * 
 */
public class DisplayInformation implements Serializable {

    String attributeName;

    String displayName;

    String displayDescription;

    boolean isPasswordField;

    String defaultValue;

    /**
     * 
     */
    public DisplayInformation() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param attributeName
     * @param displayName
     * @param displayDescription
     * @param isPasswordField
     * @param defaultValue
     */
    public DisplayInformation(String attributeName, String displayName,
            String displayDescription, boolean isPasswordField) {
        super();
        this.attributeName = attributeName;
        this.displayName = displayName;
        this.displayDescription = displayDescription;
        this.isPasswordField = isPasswordField;
    }

    /**
     * @return the attributeName
     */
    public String getAttributeName() {
        return attributeName;
    }

    /**
     * @param attributeName
     *            the attributeName to set
     */
    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
    }

    /**
     * @return the defaultValue
     */
    public String getDefaultValue() {
        return defaultValue;
    }

    /**
     * @param defaultValue
     *            the defaultValue to set
     */
    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    /**
     * @return the displayDescription
     */
    public String getDisplayDescription() {
        return displayDescription;
    }

    /**
     * @param displayDescription
     *            the displayDescription to set
     */
    public void setDisplayDescription(String displayDescription) {
        this.displayDescription = displayDescription;
    }

    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * @param displayName
     *            the displayName to set
     */
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    /**
     * @return the isPasswordField
     */
    public boolean isPasswordField() {
        return isPasswordField;
    }

    /**
     * @param isPasswordField
     *            the isPasswordField to set
     */
    public void setPasswordField(boolean isPasswordField) {
        this.isPasswordField = isPasswordField;
    }
    
    public void dump() {
        System.out.println("////////////////////////////");
        System.out.println("//   attributeName: "+attributeName);
        System.out.println("//   displayName: "+displayName);
        System.out.println("//   displayDescription: "+displayDescription);
        System.out.println("//   isPasswordField: "+isPasswordField);
        System.out.println("//   defaultValue: "+defaultValue);
        System.out.println("////////////////////////////");
        System.out.println("");
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
