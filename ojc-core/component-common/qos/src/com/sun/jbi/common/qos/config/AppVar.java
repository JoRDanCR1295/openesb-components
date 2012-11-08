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
 * @(#)AppVar.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import com.sun.jbi.common.util.Util;

/**
 * Represents an application variable.
 * @author Kevan Simpson
 */
public class AppVar {
    public enum VarType { 
        String, Number, Boolean, Password;
    
        /**
         * Case-insensitive {@link VarType#valueOf} method.
         * @param value A string which should resolve to an enum.
         * @return A VarType enum or <code>null</code>.
         */
        public static VarType toVarType(String value) {
            if (!Util.isEmpty(value)) {
                for (VarType type : VarType.values()) {
                    if (value.equalsIgnoreCase(type.toString())) {
                        return type;
                    }
                }
            }
            return null; 
        }
    }
    
    private String mName, mValue;
    private VarType mType;

    public AppVar() {
    }
    
    public AppVar(String name, String value, VarType type) {
        mName = name;
        mValue = value;
        mType = type;
    }
    
    public String toPropertyValue() {
        StringBuffer buff = new StringBuffer();
        if (getValue() != null) {
            buff.append(getValue());
        }
        buff.append("{").append(getType()).append("}");
        return buff.toString();
    }
    
    /**
     * @return the name
     */
    public String getName() {
        return mName;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        mName = name;
    }

    /**
     * @return the type
     */
    public VarType getType() {
        return mType;
    }

    /**
     * @param type the type to set
     */
    public void setType(VarType type) {
        mType = type;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return mValue;
    }

    /**
     * @param value the value to set
     */
    public void setValue(String value) {
        mValue = value;
    }
}
