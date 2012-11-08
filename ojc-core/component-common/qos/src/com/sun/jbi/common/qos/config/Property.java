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
 * @(#)Property.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.config.Constraint.Facet;
import com.sun.jbi.common.util.Util;

/**
 * Represents a component (or application) configuration property,
 * as defined in a component descriptor.
 * 
 * @author Kevan Simpson
 */
public class Property implements Comparable<Property> {
    public enum Type {
        xsd_boolean(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "boolean")),
        xsd_int(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "int")),
        xsd_positiveInteger(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "positiveInteger")),
        xsd_string(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string"));

        private QName mType;

        Type(QName qname) {
            mType = qname;
        }
        
        public QName getQName() {
            return mType;
        }
        
        public static Type toType(QName qname) {
            if (qname != null) {
                for (Type type : values()) {
                    if (type.getQName().equals(qname)) {
                        return type;
                    }
                }
            }
            
            return null;
        }

        public String getJavaType() {
            switch (this) {
                case xsd_boolean: 
                    return Boolean.class.getName();
                case xsd_int: 
                case xsd_positiveInteger: 
                    return Integer.class.getName();
                case xsd_string: 
                    return String.class.getName(); 
                default: 
                    return null;
            }
        }

        public Object getValue(Object val) {
            if (val != null) {
                String str = String.valueOf(val);
                switch (this) {
                    case xsd_boolean: 
                        return Boolean.valueOf(str);
                    case xsd_int: 
                    case xsd_positiveInteger: 
                        return Integer.valueOf(str);
                    case xsd_string: 
                    default: 
                        return str;
                }
            }
            
            return null;
        }
    }
    
    private String mName, mGroup, mDefaultValue;
    private QName mType;
    private Type mEnumType;
    private List<String> mValues;
    private int mMaxOccurs;
    private boolean mRequired;
    private List<Constraint> mConstraints;
    private int mBitConstraints;
    
    public Property(String name, String defaultValue) {
        mName = name;
        mValues = new ArrayList<String>();
        mMaxOccurs = 1;
        mConstraints = new ArrayList<Constraint>();
        mDefaultValue = defaultValue;
        if (defaultValue != null) {
            addValue(defaultValue);
        }
    }
    
    public Property(String name) {
        this(name, null);
    }
    
    public Property(Property meta) {
        this(meta.getName());
        setType(meta.getType());
    }
    
    /** @see java.lang.Comparable#compareTo(java.lang.Object) */
    public int compareTo(Property o) {
        return (o == null) ? -1 : this.getName().compareTo(o.getName());
    }

    public void addValue(String value) {
        insertValue(value, getList().size());
    }

    /** Clears value(s) for this <code>Property</code>. */
    public void clear() {
        getList().clear();
    }

    /**
     * Evaluates specified value and returns <code>true</code> if the value
     * conforms to all of this <code>Property</code>'s {@link Constraint}s.
     * 
     * @param value The specified value.
     * @return <code>true</code> if specified value meets constraints, else <code>false</code>.
     */
    public boolean conforms(Object value) {
        boolean conforms = true;
        for (Constraint c : mConstraints) {
            if (!c.meetsConstraint(value)) {
                switch (c.getFacet()) {
                    case enumeration: {
                        conforms = false;
                        break;
                    }
                    default: {
                        return false;
                    }
                }
            }
        }
        
        return conforms;
    }

    /** Returns the number of values in this <code>Property</code>. */
    public int count() {
        return getList().size();
    }

    public Object getDefaultValue() {
        return (mDefaultValue != null) 
                ? mEnumType.getValue(mDefaultValue) : null;
    }
    
    public int getMaxOccurs() {
        return mMaxOccurs;
    }
    
    public String getName() {
        return mName;
    }
    
    public QName getType() {
        return mType;
    }
    
    public Object getTypedValue() {
        return getTypedValueAt(0);
    }
    
    public Object getTypedValueAt(int index) {
        String val = getValueAt(index);
        if (val != null && mEnumType != null) {
            return mEnumType.getValue(val);
        }
        
        return val;
    }
    
    public String getValue() {
        return getValueAt(0);
    }
    
    public String getValueAt(int index) {
        if (index >= 0 && index < getList().size()) {
            return getList().get(index);
        }
        
        return null;
    }
    
    public void insertValue(String value, int index) {
        if (index >= 0) {
            if (index < getList().size()) {
                getList().set(index, value);
            }
            else {
                getList().add(value);
            }
        }
    }
    
    public boolean isRepeating() {
        return (getMaxOccurs() > 1);
    }
    
    public boolean isRequired() {
        return mRequired;
    }

    public Object suggestValue(Object newValue, boolean useDefault) {
        if (conforms(newValue)) {
            return newValue;
        }
        
        Object suggested = null;
        for (Constraint c : mConstraints) {
            if (!c.meetsConstraint(newValue)) {
                if (useDefault) {
                    return getDefaultValue();
                }

                suggested = mEnumType.getValue(c.getValue());
            }
        }
        
        return (useDefault && suggested == null) 
                ? getDefaultValue() : suggested;
    }
    
    /*      The following two methods are used by ConfigParser.             */
    
    /**
     * Applies {@link Constraint}s to this <code>Property</code>. 
     * @param cons The constraint(s) to apply.
     */
    protected void addConstraints(Constraint... cons) {
        if (cons != null) {
            for (Constraint c : cons) {
                mConstraints.add(c);
                mBitConstraints |= c.getFacet().bit();  
            }
        }
    }
    
    /**
     * Validates the constraints applied to this <code>Property</code>
     * following the definitions of like-named XSD facets.
     * 
     * @throws IllegalStateException if applied constraints are invalid.
     */
    protected void validateConstraints() {
        if (!mConstraints.isEmpty()) {
            Map<Facet, Constraint> map = new HashMap<Facet, Constraint>();
            List<Constraint> enumList = new ArrayList<Constraint>();
            for (Constraint c : mConstraints) {
                Facet f = c.getFacet();
                switch (f) {
                    case enumeration: {
                        // TODO check for duplicates, implement Constraint.hashCode()
                        enumList.add(c);
                        break;
                    }
                    case maxInclusive:
                    case minInclusive: {
                        String value = c.getValue();
                        if (map.get(f) != null) {
                            throw illegal("QOS-6068: Duplicate Constraint: {0}", 
                                          f.toString());
                        }
                        else {
                            // TODO validate better - value by property type
                            Type type = Type.toType(getType());
                            switch (type) {
                                case xsd_positiveInteger:
                                case xsd_int: {
                                    int num = Util.parseInt(value, Integer.MIN_VALUE);
                                    if (!value.equals(String.valueOf(Integer.MIN_VALUE))) {
                                        if (num == Integer.MIN_VALUE) { // non-numeric value
                                            throw illegal(
                                                    "QOS-6069: Numeric value is required for type: {0}", 
                                                    getType());
                                        }
                                    }
                                    
                                    if (type == Type.xsd_positiveInteger && num < 1) {
                                       throw illegal(
                                               "QOS-6070: Positive value is required for type: {0}", 
                                               getType());
                                    }
                                    break;
                                }
                                default: {  
                                    // these facets do not apply to string | boolean
                                    throw illegal(
                                            "QOS-6077: Constraint {0} does not apply to type {1}", 
                                            f.toString(), type.toString());
                                }
                            }
                            // no obvious type issues... cache for further validation
                            map.put(f, c);
                        }
                        break;
                    }
                    default: {
                        throw illegal(
                                "QOS-6071: Unsupported Facet: {0}", f.toString());
                    }
                }
            }
            
            for (Facet f : map.keySet()) {
                // only support enumeration, maxInclusive, minInclusive
                Constraint c = map.get(f);
                switch (f) {
                    case enumeration: {
                        // already validated above w/ duplicate check
                        break;
                    }
                    case maxInclusive: {
                        if (map.get(Facet.maxExclusive) != null) {
                            throw illegal(
                                    "QOS-6072: {0} may not be combined with {1}", 
                                    f.toString(), Facet.maxExclusive.toString());
                        }
                        // TODO compare values against min[In|Ex]clusive values
                        Constraint incl = map.get(Facet.minInclusive);
                        if (incl != null) {
                            int max = Util.parseInt(c.getValue(), Integer.MIN_VALUE),
                                min = Util.parseInt(incl.getValue(), Integer.MAX_VALUE);
                            if (max < min) {
                                throw illegal(
                                        "QOS-6073: {0} value ({1}) must be greater than or equal to {2} value ({3})", 
                                        f.toString(), String.valueOf(max),
                                        Facet.minInclusive.toString(), String.valueOf(min));
                            }
                        }
                        break;
                    }
                    case minInclusive: {
                        if (map.get(Facet.minExclusive) != null) {
                            throw illegal(
                                    "QOS-6072: {0} may not be combined with {1}", 
                                    f.toString(), Facet.minExclusive.toString());
                        }
                        // TODO compare values against max[In|Ex]clusive values
                        Constraint incl = map.get(Facet.maxInclusive);
                        if (incl != null) {
                            int max = Util.parseInt(incl.getValue(), Integer.MIN_VALUE),
                                min = Util.parseInt(c.getValue(), Integer.MAX_VALUE);
                            if (min > max) {
                                throw illegal(
                                        "QOS-6074: {0} value ({1}) must be less than or equal to {2} value ({3})", 
                                        f.toString(), String.valueOf(min),
                                        Facet.maxInclusive.toString(), String.valueOf(max));
                            }
                        }
                        break;
                    }
                    default: {
                        // TODO implement validation when other facets supported
                    }
                }

            }
        }
    }

    private IllegalStateException illegal(String msg, Object... params) {
        return new IllegalStateException(I18n.loc(msg, params));
    }
    
    protected String getGroup() {
        return mGroup;
    }

    protected void setGroup(String group) {
        mGroup = group;
    }

    protected void setMaxOccurs(int maxOccurs) {
        mMaxOccurs = Math.max(1, maxOccurs);
    }
    
    protected void setRequired(boolean required) {
        mRequired = required;
    }
    
    protected void setType(QName type) {
        mType = type;
        mEnumType = Type.toType(type);
        // validate supported type
        if (mEnumType == null) {
            throw new IllegalArgumentException(I18n.loc(
                    "QOS-6065: Property type {0} not supported!", 
                    String.valueOf(type)));
        }
    }
    
    public void setValue(String value) {
        insertValue(value, 0);
    }
    
    public List<String> values() {
        return new ArrayList<String>(getList());
    }
    
    /** @see java.lang.Object#toString() */
    public String toString() {
        StringBuffer buff = new StringBuffer();
        buff.append("Property[").append(getName())
            .append("=").append(String.valueOf(getList())).append("]");
        return buff.toString();
    }

    protected List<String> getList() {
        return mValues;
    }
}
