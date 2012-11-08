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
 * @(#)HL7Message.java 
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
 * @author S. Nageswara Rao
 */
public class HL7Message implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    // Attribute names
    public static final String ATTR_USE_TYPE_ENCODED = "encoded";

    public static final String ATTR_HL7_ENCODER = "hl7encoder-1.0";

    /*
     * TODO: 1) Need to handls MSH validation attributes 2) Recourse Actions
     */

    private QName mFieldElementType = HL7Constants.QNAME_MESSAGE;

    private String mPart = null;

    private Boolean mFieldRequired = null;
    
    private String mEncodingStyle = ATTR_HL7_ENCODER;

	private String mUseType = ATTR_USE_TYPE_ENCODED;
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
     * Set the Encoding Style
     */
    public void setEncodingStyle(String val) {
        mEncodingStyle = val;
    }

    /**
     * Get the Encoding Style
     */
    public String getEncodingStyle() {
        return mEncodingStyle;
    }

    /**
     * Set the Part
     */
    public void setPart(String val) {
        mPart = val;
    }

    /**
     * Get the Part
     */
    public String getPart() {
        return mPart;
    }

    /**
     * Get Use Type
     */
    public String getUseType() {
        return mUseType;
    }
    /**
     * Set Use Type
     */
    public void setUseType(String val) {
        mUseType = val;
    }

    public String toString() {
        StringBuilder strBld = new StringBuilder(super.toString());
        strBld.append("\nHL7 Message (" + getElementType() + "):");
        strBld.append("\nencoderStyle=" + getEncodingStyle());
        strBld.append("\npart=" + getPart());
        return strBld.toString();
    }
}
