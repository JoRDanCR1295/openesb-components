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
 * @(#)MQXDecode.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import java.text.MessageFormat;
import java.util.ResourceBundle;

public class MQXDecode {
    
     protected MQXDecode() {
    }

    /**
     * Obtain a mnemonic string from a MQException reason code. The reason code
     * is matched to a declared MQRC field, and the name of the field is
     * returned.  Only MQRC_ fields are supported.
     * 
     * @param rc Reason code as given by {@link com.ibm.mq.MQException#reasonCode}.
     * 
     * @return Name of the MQRC field whose value matches the given code.
     */
    public static String decode(int rc) {
        String msg = null;
        final String rcS = "MQRC_" + String.valueOf(rc);
        try {
            msg = c_codes.getString(rcS);
        }
        catch (Exception e) {
            msg = c_codes.getString("INFO_MQRC_UNKNOWN");
                                      
        }
        return msg;
    }

    private static final String c_CODE_BUNDLE_RESOURCE =
            "com/sun/jbi/mqbc/extservices/MQXCodes";

    private static final ResourceBundle c_codes =
            ResourceBundle.getBundle(c_CODE_BUNDLE_RESOURCE);
    
}
