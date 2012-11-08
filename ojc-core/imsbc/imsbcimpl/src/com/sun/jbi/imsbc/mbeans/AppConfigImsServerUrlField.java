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

package com.sun.jbi.imsbc.mbeans;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.management.openmbean.SimpleType;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.imsbc.IMSException;

/**
 * Application Configuration field for IMS Server URL.
 * 
 * @author Noel.Ang@sun.com
 */
final class AppConfigImsServerUrlField extends ApplicationConfigurationField {
    
	
	private String value = "";

    // Message bundle
    private static final Messages mMessages =
            Messages.getMessages(AppConfigImsServerUrlField.class);
    
    // Logger
    private static Logger mLogger = Messages.getLogger(AppConfigImsServerUrlField.class);

    AppConfigImsServerUrlField() {
        super("imsServerLocation", "IMS Server Location URL", SimpleType.STRING);
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
        AppConfigImsServerUrlField clone = new AppConfigImsServerUrlField();
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

		Object[] ret = null;

		if (value.length() > 0) {
			String[] stArr = value.split(":");
			if(!(stArr.length == 3 && stArr[0].equals("ims") && stArr[1].indexOf("//") 
					== 0 && Integer.parseInt(stArr[2])<= 65535)) {
					ret = new String[]{
							null,
						   mMessages.getString("IMSBC-E01135.Invalid_Server_Url", value)};
					return ret;
			}

		}
        
        // Check against whitespace padding
        if (value.startsWith(" ") || value.endsWith(" ")) {
            value = value.trim();
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
