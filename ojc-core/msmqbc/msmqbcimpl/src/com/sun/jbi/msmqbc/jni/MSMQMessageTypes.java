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
 * @(#)MSMQMessageTypes.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.jni;

/**
 * This class wraps the messsage types for sending a message to a queue
 * 
 * @author Sun Microsystems
 */

public class MSMQMessageTypes {

    public static String ARRAY_OF_BYTES = "array of bytes";

    public static String ARRAY_OF_BYTES_VAL = "VT_ARRAY | VT_UI1";

    public static String STRING = "string";

    public static String STRING_VAL = "VT_BSTR";

    public static String SHORT_INTEGER = "short integer";

    public static String SHORT_INTEGER_VAL = "VT_UI2";

    public static String LONG_INTEGER = "long integer";

    public static String LONG_INTEGER_VAL = "VT_UI4";

    public static String CHAR = "char";

    public static String CHAR_VAL = "VT_I1";

    public static String FLOAT = "float";

    public static String FLOAT_VAL = "VT_R4";

    public static String DOUBLE = "double";

    public static String DOUBLE_VAL = "VT_R8";

    public static String DATE = "date";

    public static String DATE_VAL = "VT_DATE";

    public static String CURRENCY = "currency";

    public static String CURRENCY_VAL = "VT_CY";

    public static String getMessageType(String val) {
        if (val.equals(ARRAY_OF_BYTES)) {
            return ARRAY_OF_BYTES_VAL;
        } else if (val.equals(STRING)) {
            return STRING_VAL;
        } else if (val.equals(SHORT_INTEGER)) {
            return SHORT_INTEGER_VAL;
        } else if (val.equals(LONG_INTEGER)) {
            return LONG_INTEGER_VAL;
        } else if (val.equals(CHAR)) {
            return CHAR_VAL;
        } else if (val.equals(FLOAT)) {
            return FLOAT_VAL;
        } else if (val.equals(DOUBLE)) {
            return DOUBLE_VAL;
        } else if (val.equals(DATE)) {
            return DATE_VAL;
        } else if (val.equals(CURRENCY)) {
            return CURRENCY_VAL;
        } else {
            return ARRAY_OF_BYTES_VAL;
        }
    }

}
