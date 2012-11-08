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
 * @(#)Util.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.util;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Contains generic utility methods.
 * 
 * @author Kevan Simpson
 */
public class Util {
	/**
	 * Utility to compare two objects for equality, 
	 * either of which may be <code>null</code>.
	 * @param o1 The first object.
	 * @param o2 The second object.
	 * @return <code>true</code> if both objects are <code>null</code> or equal.
	 */
    public static boolean equals(Object o1, Object o2) {
        if (o1 == null) return (o2 == null);
        else if (o2 != null) return o1.equals(o2);
        else return false;
    }
    
    /**
     * Utility to generate a hashCode for an object, which may be <code>null</code>.
     * @param o1 The object.
     * @return The object's hashCode or 0 if the object is <code>null</code>.
     */
    public static int hashCode(Object o1) {
        return (o1 == null) ? 0 : o1.hashCode();
    }
    
    /**
     * Converts the specified error's stacktrace to a string.
     * @param t The error to convert.
     * @return string-representation of error's stacktrace.
     */
    public static String toString(Throwable t) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        t.printStackTrace(pw);
        pw.flush();
        return sw.toString();
    }
    
    /**
     * Tests the specified string for null-ness and emptiness (zero non-whitespace characters).
     * @param str The test string.
     * @return <code>true</code> if the string is <code>null</code> or zero-length when trimmed.
     */
    public static boolean isEmpty(String str) {
        return (str == null || str.trim().length() == 0);
    }
    
    /**
     * Escapes XML text.
     * @param text The xml to escape.
     * @return Escaped xml.
     */
    public static String escape(String text) {
        if (text == null) return "";
        
        StringBuffer buff = new StringBuffer();
        char[] chars = text.toCharArray();
        for (int i = 0, n = chars.length; i < n; i++) {
            switch (chars[i]) {
                case '<':
                    buff.append("&lt;");    break;
                case '>':
                    buff.append("&gt;");    break;
                case '&':
                    buff.append("&amp;");   break;
                case '"':
                    buff.append("&quot;");  break;
                case '\'':
                    buff.append("&apos;");  break;
                default:
                    buff.append(chars[i]);
            }
        }

        return buff.toString();
    }
}
