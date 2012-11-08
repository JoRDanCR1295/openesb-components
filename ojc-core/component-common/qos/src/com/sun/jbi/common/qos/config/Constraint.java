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
 * @(#)Constraint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.config.Property.Type;
import com.sun.jbi.common.util.Util;

/**
 * Represents XSD-like constraints on component configuration {@link Property}
 * instances.
 * 
 * @author Kevan Simpson
 */
public class Constraint {
    public enum Facet {
        enumeration,
        fractionDigits,
        length,
        maxExclusive,
        maxInclusive,
        minExclusive,
        minInclusive,
        minLength,
        pattern,
        totalDigits,
        whiteSpace,
        suggestedValue;
        
        public int bit() {
            return (int) Math.pow(2, ordinal());
        }
    }
    
    private Facet mFacet;
    private String mValue;
    
    public Constraint(Facet facet, String value) {
        mFacet = facet;
        mValue = value;
    }

    /**
     * Tests the specified value to determine if it meets this <code>Constraint</code>.
     * 
     * @param value The value to test.
     * @return <code>true</code> if value is non-<code>null</code> and meets this
     *         <code>Constraint</code>, else <code>false</code>.
     * @throws IllegalStateException if this constraint's {@link Facet} is unsupported.
     */
    public boolean meetsConstraint(Object value) {
        // TODO handle data types other than strings, booleans, integers
        if (value != null) {
            switch (mFacet) {
                case enumeration: {
                    // sufficient for strings, booleans, integers w/o '+' prefix
                    return (Util.equals(mValue, String.valueOf(value)));
                }
                case maxInclusive: {
                    Integer i = toInt(value);
                    return (i == null) ? false 
                            : i.intValue() <= toInt(Type.xsd_int.getValue(mValue)).intValue();
                }
                case minInclusive: {
                    Integer i = toInt(value);
                    return (i == null) ? false 
                            : i.intValue() >= toInt(Type.xsd_int.getValue(mValue)).intValue();
                }
                default: {
                    throw new IllegalStateException(I18n.loc(
                            "QOS-6071: Unsupported Facet: {0}", mFacet.toString()));
                }
            }
        }
        
        return false;
    }
    
    /** 
     * Returns the facet.
     * @return the facet. 
     */
    public Facet getFacet() {
        return mFacet;
    }

    /** 
     * Returns the value.
     * @return the value. 
     */
    public String getValue() {
        return mValue;
    }

    Integer toInt(Object value) {
        return (value instanceof Integer) ? (Integer) value : null;
    }
}
