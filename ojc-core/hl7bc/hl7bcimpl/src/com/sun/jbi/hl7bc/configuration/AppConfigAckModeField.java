package com.sun.jbi.hl7bc.configuration;

import javax.management.openmbean.SimpleType;

import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;

public class AppConfigAckModeField extends ApplicationConfigurationField {
    private String value = "";

    AppConfigAckModeField() {
        super("acknowledgmentMode", "Acknowledgement Mode", SimpleType.STRING);
    }
    
    public String toString() {
        return value.toString();
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
    	AppConfigAckModeField clone = new AppConfigAckModeField();
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
        String value;
        
        // Check against null
        if (data == null) {
            return new String[]{HL7Constants.ACK_MODE_ORIGINAL};
        } else {
            value =data.toString();
        }
        
        // Check against whitespace padding
        if (value.startsWith(" ") || value.endsWith(" ")) {
            value = value.trim();
        }
        
        if(value.length() == 0){
        	value = HL7Constants.ACK_MODE_ORIGINAL;
        	return new String[]{value};
        }
        
        if(value.equals(HL7Constants.ACK_MODE_ORIGINAL) || value.equals(HL7Constants.ACK_MODE_ENHANCED)){
        	return new String[]{value};
        }else{
        	return new String[]{null,"The Acknowledgement Mode should be either \"original\" or \"enhanced\""}; 
        }
        
    }

    /**
     * Assign a value to this field. Implements can assume that the supplied
     * value has already been validated thru a {@link #validate} call.
     * 
     * @throws ClassCastException if the runtime type of the value is invalid
     *         for the field. 
     */
    public void setValue(Object value) throws ClassCastException {
        this.value = (value != null ? value.toString() : HL7Constants.ACK_MODE_ORIGINAL);
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
