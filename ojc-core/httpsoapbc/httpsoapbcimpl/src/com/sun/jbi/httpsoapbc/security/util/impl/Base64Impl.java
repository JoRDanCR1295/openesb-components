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
 * @(#)Base64Impl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.util.impl;

import com.sun.jbi.httpsoapbc.security.util.api.Base64;

public class Base64Impl implements Base64 {

    public String decode(String data) {
        return getUTF8String(com.sun.org.apache.xerces.internal.impl.dv.util.Base64.decode(data));
    }

    public String encode(String data) {
        String encoded = com.sun.org.apache.xerces.internal.impl.dv.util.Base64.encode(getUTF8Bytes(data));
        //This encoder adds a new line character at the end of the base64 encoded data, so
        //remove this character before returning.
        if (encoded != null) {
            encoded = encoded.substring(0, encoded.length() - 1);
        }
        
        return encoded;
    }
        
    public static byte[] getUTF8Bytes(String data) {
        if (data == null) {
            return new byte[0];
        }
        
        try {
            return data.getBytes("UTF-8");
        } catch (java.io.UnsupportedEncodingException uee) {
            // Default encoding if UTF-8 is not available.
            return data.getBytes();
        }
    }

    public static String getUTF8String(byte [] data) {
        try {
            return new String(data, "UTF-8");
        } catch (java.io.UnsupportedEncodingException uee) {
            // default encoding ...
            return new String(data);
        }
    }    
}
