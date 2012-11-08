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
 * @(#)HL7CommunicationControls.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import java.io.Serializable;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.sun.jbi.hl7bc.HL7Constants;

/**
 * @author T.V.A Raghunadh, S. Nageswara Rao
 */
public class HL7CommunicationControls implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    private QName mFieldElementType = HL7Constants.QNAME_COMMUNICATIONCONTROLS;

    private volatile Boolean mFieldRequired = Boolean.FALSE;

    private volatile Map<String, HL7CommunicationControl> commControlEntries  = new HashMap<String, HL7CommunicationControl>(); 

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
     * Gets the options
     * 
     * @return The map of HL7CommunicationControl elements
     */
    public Map<String, HL7CommunicationControl> getCommunicationControls() {
        return commControlEntries;
    }

    /**
     * Sets the options
     * 
     * @param val The map of HL7CommunicationControl elements
     */
    public void setCommunicationControls(Map<String, HL7CommunicationControl> val) {
        commControlEntries = val;
    }

    /**
     * String format of the object
     * 
     * @return The String format of the object
     */
    public String toString() {
        StringBuilder strBld = new StringBuilder(super.toString());
        strBld.append("\nHL7CommunicationControls (" + getElementType() + "):");
        strBld.append("\nRequired=" + getRequired());
        strBld.append("\nHL7 Communication Controls list =[[" + communicationControlsMapToString() + "]]");
        strBld.append("\n");
        return strBld.toString();
    }

    private String communicationControlsMapToString() {
        StringBuilder strBld = new StringBuilder();
        if (getCommunicationControls() != null) {
            HL7CommunicationControl commCntrl = null;
            for(String commCntrlName : commControlEntries.keySet()) {
                commCntrl = commControlEntries.get(commCntrlName);
                strBld.append(commCntrl.toString()); 
            }
        }
        return strBld.toString();
    }

}
