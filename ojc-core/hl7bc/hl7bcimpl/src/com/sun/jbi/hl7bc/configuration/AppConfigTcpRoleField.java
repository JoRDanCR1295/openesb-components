package com.sun.jbi.hl7bc.configuration;

import javax.management.openmbean.SimpleType;

import com.sun.jbi.hl7bc.extensions.HL7Address;

public class AppConfigTcpRoleField extends ApplicationConfigurationField {
    private String value = HL7Address.TCP_ROLE_DEFAULT;

    AppConfigTcpRoleField() {
        super("tcpRole", "TCP Role (client/server)", SimpleType.STRING);
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
    	AppConfigTcpRoleField clone = new AppConfigTcpRoleField();
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
        if (HL7Address.TCP_ROLE_DEFAULT.equals(data)) {
        	return null;
        }
        if (HL7Address.TCP_ROLE_CLIENT.equals(data)) {
        	return null;
        }
        if (HL7Address.TCP_ROLE_SERVER.equals(data)) {
        	return null;
        }
        
        return new String[] { HL7Address.TCP_ROLE_DEFAULT };
    }

    /**
     * Assign a value to this field. Implements can assume that the supplied
     * value has already been validated thru a {@link #validate} call.
     * 
     * @throws ClassCastException if the runtime type of the value is invalid
     *         for the field. 
     */
    public void setValue(Object value) throws ClassCastException {
        if (HL7Address.TCP_ROLE_SERVER.equals(value)) {
        	value = HL7Address.TCP_ROLE_SERVER;
        } else if (HL7Address.TCP_ROLE_CLIENT.equals(value)) {
        	value = HL7Address.TCP_ROLE_CLIENT;
        } else {
        	value = HL7Address.TCP_ROLE_DEFAULT;
        }
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
