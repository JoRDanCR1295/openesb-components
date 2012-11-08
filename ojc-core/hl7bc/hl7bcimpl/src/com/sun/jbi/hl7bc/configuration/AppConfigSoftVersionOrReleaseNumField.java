package com.sun.jbi.hl7bc.configuration;

import javax.management.openmbean.SimpleType;

import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;

public class AppConfigSoftVersionOrReleaseNumField extends
		ApplicationConfigurationField {
    private String value = HL7ProtocolProperties.SOFTWARE_CERTIFIED_VERSION;

    AppConfigSoftVersionOrReleaseNumField() {
        super("softwareCertifiedVersionOrReleaseNumber", "Software Version or Release Number", SimpleType.STRING);
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
    	AppConfigSoftVersionOrReleaseNumField clone = new AppConfigSoftVersionOrReleaseNumField();
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
            value = HL7ProtocolProperties.SOFTWARE_CERTIFIED_VERSION;
            corrected = true;
        } else {
            value = String.valueOf(data);
        }
        
        // Check against whitespace padding
        if (value.startsWith(" ") || value.endsWith(" ")) {
            value = value.trim();
            corrected = true;
        }
        
        if(value.length() == 0){
        	value = HL7ProtocolProperties.SOFTWARE_CERTIFIED_VERSION;
        	corrected = true;
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
