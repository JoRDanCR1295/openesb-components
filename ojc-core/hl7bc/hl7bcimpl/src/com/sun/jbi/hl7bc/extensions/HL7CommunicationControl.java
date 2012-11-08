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
 * @(#)HL7CommunicationControl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.sun.jbi.hl7bc.HL7Constants;

/**
 * @author T.V.A Raghunadh, S. Nageswara Rao
 */
public class HL7CommunicationControl implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    public static final String ATTR_NAME = "name";

    public static final String ATTR_VALUE = "value";

    public static final String ATTR_ENABLED = "enabled";

    public static final String ATTR_RECOURSE_ACTION = "recourseAction";

    private QName mFieldElementType = HL7Constants.QNAME_COMMUNICATIONCONTROL;

    private Boolean mFieldRequired = Boolean.FALSE;

    private volatile String mName;

    private volatile long mValue;
    private volatile String mStringValue;

    private volatile Boolean mEnabled;

    private volatile String mRecourseAction;

    /**
     * Set the extensibility element type
     * 
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        mFieldElementType = elementType;
    }

    /**
     * Get the extensibility element type
     * 
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return mFieldElementType;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        mFieldRequired = required;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return mFieldRequired;
    }

    /**
     * Get value of name attribute
     * 
     * @return The String value of name attribute
     */
    public String getName() {
        return mName;
    }

    /**
     * Sets value of name attribute
     * 
     * @param val The String value of name attribute
     */
    public void setName(String val) {
        mName = val;
    }

    /**
     * Get value of value attribute
     * 
     * @return The long value of value attribute
     */
    public long getValue() {
        return mValue;
    }
    
    public String getValueAsString(){
    	return mStringValue;
    }

    /**
     * Sets value of value attribute
     * 
     * @param val The long value of value attribute
     */
    public void setValue(long val) {
        mValue = val;
    }
    
    public void setValueAsString(String val){
    	mStringValue = val;
    }

    /**
     * Get value of enabled attribute
     * 
     * @return The String value of value attribute
     */
    public Boolean getEnabled() {
        return mEnabled;
    }

    /**
     * Sets value of enabled attribute
     * 
     * @param val The Boolean value of value attribute
     */
    public void setEnabled(Boolean val) {
        mEnabled = val;
    }

    /**
     * Get value of recourseAction attribute
     * 
     * @return The String value of name attribute
     */
    public String getRecourseAction() {
        return mRecourseAction;
    }

    /**
     * Sets value of name attribute
     * 
     * @param val The String value of name attribute
     */
    public void setRecourseAction(String val) {
        mRecourseAction = val;
    }

    /**
     * String format of the object
     * 
     * @return The String format of the object
     */
    public String toString() {
        StringBuilder strBld = new StringBuilder(super.toString());
        strBld.append("\nHL7CommunicationControl(" + getElementType() + "):");
        strBld.append("\nRequired=" + getRequired());
        strBld.append("\nName=" + getName());
        strBld.append("\nValue=" + getValue());
        strBld.append("\nEnabled=" + getEnabled());
        strBld.append("\nRecourseAction" + getRecourseAction());
        strBld.append("\n");

        return strBld.toString();
    }

}
