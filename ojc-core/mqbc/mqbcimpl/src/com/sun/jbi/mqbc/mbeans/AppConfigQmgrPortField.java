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

package com.sun.jbi.mqbc.mbeans;

import javax.management.openmbean.SimpleType;

/**
 * Application Configuration field for Queue Manager Port (number).
 * @author Noel.Ang@sun.com
 */
final class AppConfigQmgrPortField extends ApplicationConfigurationField {
    private static final Integer INITIAL_VALUE = -1;
    
    private Integer value = INITIAL_VALUE;

    AppConfigQmgrPortField() {
        super("queuemanagerPort", "Queue Manager Host Port", SimpleType.INTEGER);
    }

    public String toString() {
        return value.toString();
    }

    public void fromString(String data) {
        Integer ivalue;
        try {
            ivalue = Integer.valueOf(data);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(e);
        }
        
        value = ivalue;
    }

    public Object clone() {
        AppConfigQmgrPortField clone = new AppConfigQmgrPortField();
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
    public Object[] validate(Object value) {
        // Check against bad type
        if (value != null && !(value instanceof Integer)) {
            return new String[] {
                    null,
                    "Invalid value type, expected java.lang.Integer, got: "
                            + value.getClass().getName(),
            };
        }
        
        Object[] ret = null;
        
        // Check against null
        if (value == null) {
            ret = new Object[] {INITIAL_VALUE};
        }
        
        if (value != null) {
            Integer val = (Integer) value;
            if (val < INITIAL_VALUE || val > 65535) {
                ret = new String[]{
                        null,
                        "Invalid port value " + val + ". Valid inclusive range is between -1 (denoting system-defined default) and 65535."
                };
            }
        }
        
        return ret;
    }

    /**
     * Assign a value to this field. Implements can assume that the supplied
     * value has already been validated thru a {@link #validate} call.
     * 
     * @throws ClassCastException if the runtime type of the value is invalid
     *         for the field. 
     */
    public void setValue(Object value) throws ClassCastException {
        this.value = (value != null ? (Integer) value : INITIAL_VALUE);
    }

    /**
     * Retrieve the value previously assigned to this field.
     *
     * @return Value previously assigned to this field, or <code>null</code>
     *         if no value has been
     */
    public Integer getValue() {
        return value;
    }
}
