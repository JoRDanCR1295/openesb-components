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
 * @(#)XMLAttributeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.wsdlmodel.impl;

import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements a XML attribute.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XMLAttributeImpl extends XMLNodeImpl implements XMLAttribute {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 1250789178715422508L;
    
    /** Holds value of property optional. */
    private boolean optional;
    
    /** Holds value of property enumValues. */
    private String[] enumValues;
    
    /** Holds value of property type. */
    private Class type;
    
    /** Creates a new instance of XMLAttributeImpl.
     */
    public XMLAttributeImpl() {
        super();
        setValue(null);
    }
    
    /** Creates a new instance of XMLAttributeImpl.
     * @param   qName   Qualified name.
     * @param   value   Value.
     */
    public XMLAttributeImpl(String qName, String value) {
        setQualifiedName(qName);
        setValue(value);
    }
    
    /** Creates a new instance of XMLAttributeImpl.
     * @param   qName   Qualified name.
     * @param   type    Class type
     * @param   opt     Optional if <tt>true</tt>.
     * @param   eVals   Enumerated values.
     */
    public XMLAttributeImpl(String qName, Class type, boolean opt,
                            String[] eVals) {
        setQualifiedName(qName);
        setType(type);
        setOptional(opt);
        setEnumValues(eVals);
    }
    
    /** Getter for property optional.
     * @return Value of property optional.
     *
     */
    public boolean isOptional() {
        return optional;
    }
    
    /** Setter for property optional.
     * @param optional New value of property optional.
     *
     */
    public void setOptional(boolean optional) {
        this.optional = optional;
    }
    
    /** Getter for property enumValues.
     * @return Value of property enumValues.
     *
     */
    public String[] getEnumValues() {
        return enumValues;
    }
    
    /** Setter for property enumValues.
     * @param enumValues New value of property enumValues.
     *
     */
    public void setEnumValues(String[] enumValues) {
        this.enumValues = enumValues;
    }
    
    /** Determines if a value is within the enumerated values.
     * @param   v   Value to be tested.
     * @return  <tt>true</tt> if the value is within the set.
     */
    public boolean isInEnumValues(String v) {
        if (null == enumValues) {
            return true;
        } else {
        boolean in = false;
            for (int i = 0; i < enumValues.length; i++) {
            if (v.equals(enumValues[i])) {
                in = true;
                break;
            }
        }
        return in;
        }
    }
    
    /** Getter for property type.
     * @return Value of property type.
     *
     */
    public Class getType() {
        return type;
    }
    
    /** Setter for property type.
     * @param type New value of property type.
     *
     */
    public void setType(Class type) {
        this.type = type;
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   v   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor v) {
        return true;
    }
}
