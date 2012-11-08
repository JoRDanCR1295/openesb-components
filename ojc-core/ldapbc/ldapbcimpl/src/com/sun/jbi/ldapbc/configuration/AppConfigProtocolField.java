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
 */

package com.sun.jbi.ldapbc.configuration;

import javax.management.openmbean.SimpleType;

/**
 * Application Configuration field for Protocol.
 * 
 * @author Noel.Ang@sun.com
 */
public final class AppConfigProtocolField extends ApplicationConfigurationField {
    private String value = "";

    public AppConfigProtocolField() {
        super("protocol", "Security Protocol", SimpleType.STRING);
    }
    
    public String toString() {
        return value;
    }

    public void fromString(String data) {
        Object[] validation = validate(data);
        if (validation == null) {
            value = data;
        } else if (validation[0] != null) {
            value = validation[0].toString();
        } else {
            throw new IllegalArgumentException(validation[1].toString());
        }
    }

    public Object clone() {
        AppConfigProtocolField clone = new AppConfigProtocolField();
        clone.setValue(getValue());
        return clone;
    }

    /**
     * Validate the data against the field.
     *
     * @return If there are no errors, this validation should return null or
     *         an empty array.  If there are correctable errors, this validation
     *         must return an array with one element that is the corrective
     *         value. Otherwise, an array of one or more strings describing the
     *         validation failure(s) must be returned, with the first element
     *         in array being <code>null</code>.
     */
    public Object[] validate(Object data) {
        boolean corrected = false;
        String value;
        
        // Check against null
        if (data == null) {
            value = "";
            corrected = true;
        } else {
            value = String.valueOf(data);
        }
        
        // Check against whitespace padding
        if (value.startsWith(" ") || value.endsWith(" ")) {
            value = value.trim();
            corrected = true;
        }
        
        // Check against illegal characters
        // If any are found, create corrective value by removing them all
        for (int i = 0, len = value.length(); i < len; ++i) {
            if (!Character.isLetterOrDigit(value.charAt(i))) {
                StringBuffer newValueBuffer = new StringBuffer(value);
                for (int n = 0; n < newValueBuffer.length(); ++n) {
                    if (!Character.isLetterOrDigit(newValueBuffer.charAt(n))) {
                        newValueBuffer.deleteCharAt(n);
                        n--;
                    }
                }
                value = newValueBuffer.toString();
                corrected = true;
                // outer loop only used to find the first indication of
                // illegal characters
                break;
            }
        }
        
        return (corrected ? new String[] { value } : null);
    }

    /**
     * Assign a value to this field. Implements can assume that the supplied
     * value has already been validated thru a {@link #validate} call.
     * 
     * @throws ClassCastException if the runtime type of the value is invalid
     *         for the field. 
     */
    public void setValue(Object value) throws ClassCastException {
        this.value = (value != null ? (String) value : "");
    }

    /**
     * Retrieve the value previously assigned to this field.
     *
     * @return Value previously assigned to this field, or <code>null</code>
     *         if no value has been
     */
    public String getValue() {
        return value;
    }
}