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
 * @(#)ACKBuilderFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack;

import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.hl7bc.HL7Constants;

import com.sun.jbi.hl7bc.I18n;

/**
 * Factory class for creating ACKBuilder 
 * 
 * @author S. Nageswara Rao, Raghunadh
 */

public class ACKBuilderFactory implements HL7Constants {

    private static final Logger mLog = Logger.getLogger(ACKBuilderFactory.class.getName());

    public enum HL7Version {
        HL7v21, HL7v22, HL7v23, HL7v231, HL7v24, HL7v25, HL7v251, HL7v26
    };

    /**
     * Create and return the ACKBuilder given the HL7 version
     * 
     * @param hl7version The hl7 version number
     */
    public static ACKBuilder createACKBuilder(HL7Version hl7version) throws Exception {
        switch (hl7version) {

        case HL7v21: {
            mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v21ACKBuilder.class.getName() ));
            return new HL7v21ACKBuilder();
        }
        case HL7v231: {
            mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v231ACKBuilder.class.getName() ));
            return new HL7v231ACKBuilder();
        }
		case HL7v22: {
			mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v22ACKBuilder.class.getName() ));
            return new HL7v22ACKBuilder();
		}
        case HL7v23: {
            mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v23ACKBuilder.class.getName()));
            return new HL7v23ACKBuilder();
        }
        case HL7v24: {
            mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v24ACKBuilder.class.getName() ));
            return new HL7v24ACKBuilder();
        }
		case HL7v25: {
            mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v25ACKBuilder.class.getName() ));
            return new HL7v25ACKBuilder();
        }
		case HL7v251: {
            mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v251ACKBuilder.class.getName() ));
            return new HL7v251ACKBuilder();
        }
		case HL7v26: {
            mLog.log(Level.FINE, I18n.msg("I0162: Creating ACK Builder object of class {0}", HL7v26ACKBuilder.class.getName() ));
            return new HL7v26ACKBuilder();
        }
		default: {
            mLog.log(Level.SEVERE, I18n.msg("E0283: The hl7 version {0} is unsupported", hl7version ));

            String errMsg = I18n.msg("E0283: The hl7 version {0} is unsupported", hl7version );
            throw new Exception(errMsg);
        }

        }
    }

    public static class Util {
        public static HL7Version stringToEnumValue(String hl7verStr) {
            HL7Version hl7version = null;
            if (hl7verStr.equals(HL7v21)) {
                hl7version = HL7Version.HL7v21;
            } else if (hl7verStr.equals(HL7v22)) {
                hl7version = HL7Version.HL7v22;
            } else if (hl7verStr.equals(HL7v23)) {
                hl7version = HL7Version.HL7v23;
            } else if (hl7verStr.equals(HL7v231)) {
                hl7version = HL7Version.HL7v231;
            } else if (hl7verStr.equals(HL7v24)) {
                hl7version = HL7Version.HL7v24;
            } else if (hl7verStr.equals(HL7v25)) {
                hl7version = HL7Version.HL7v25;
            } else if (hl7verStr.equals(HL7v251)) {
				hl7version = HL7Version.HL7v251;
            } else if (hl7verStr.equals(HL7v26)) {
				hl7version = HL7Version.HL7v26;
            }
            return hl7version;
        }
    }
}
