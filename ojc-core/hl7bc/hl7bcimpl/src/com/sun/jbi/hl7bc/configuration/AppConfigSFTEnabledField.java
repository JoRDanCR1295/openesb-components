package com.sun.jbi.hl7bc.configuration;

import javax.management.openmbean.SimpleType;

public class AppConfigSFTEnabledField extends ApplicationConfigurationField {
    private Boolean value = false;

    AppConfigSFTEnabledField() {
        super("enabledSFT", "SFT Enabled", SimpleType.BOOLEAN);
    }
    
    public String toString() {
        return value.toString();
    }

    public void fromString(String data) {
        Object[] validation = validate(data);
        if (validation == null) {
            value = new Boolean(data);
        } else if (validation[0] != null) {
            value = new Boolean(validation[0].toString());
        } else {
            throw new IllegalArgumentException(validation[1].toString());
        }
    }

    public Object clone() {
    	AppConfigSFTEnabledField clone = new AppConfigSFTEnabledField();
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
        Boolean value;
        
        // Check against null
        if (data == null) {
            value = false;
            corrected = true;
        } else {
            value = new Boolean(data.toString());
            corrected = true;
        }
        
        return (corrected ? new Boolean[] { value } : null);
    }

    /**
     * Assign a value to this field. Implements can assume that the supplied
     * value has already been validated thru a {@link #validate} call.
     * 
     * @throws ClassCastException if the runtime type of the value is invalid
     *         for the field. 
     */
    public void setValue(Object value) throws ClassCastException {
        this.value = (value != null ? new Boolean(value.toString()) : new Boolean(false));
    }

    /**
     * Retrieve the value previously assigned to this field.
     *
     * @return Value previously assigned to this field, or <code>null</code>
     *         if no value has been
     */
    public Boolean getValue() {
        return value;
    }
}
