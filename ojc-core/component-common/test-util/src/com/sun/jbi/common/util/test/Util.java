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

package com.sun.jbi.common.util.test;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;

/**
 * Contains generic utility methods.
 * 
 * @author Kevan Simpson
 */
public class Util {
    private static TransformerFactory mFactory = TransformerFactory.newInstance();
    
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
    
    public static int parseInt(String str, int defaultValue) {
    	try { return (Util.isEmpty(str)) ? defaultValue : Integer.parseInt(str); }
    	catch (NumberFormatException nfe) { return defaultValue; }
    }

    public static long parseLong(String str, long defaultValue) {
    	try { return (Util.isEmpty(str)) ? defaultValue : Long.parseLong(str); }
    	catch (NumberFormatException nfe) { return defaultValue; }
    }

    /**
     * Generates a string representation of an array.
     * @param objs The array of objects.
     * @return A string representation of the array or an empty string.
     */
    public static String toString(Object[] objs, String delim) {
        if (objs == null) return "";
        
        StringBuffer buff = new StringBuffer();
        String actualDelim = "";
        
        buff.append("[");
        for (Object o : objs) {
            try { buff.append(actualDelim).append(String.valueOf(o)); }
            catch (Exception e) { /* ignore */ }
            actualDelim = (delim == null) ? "," : delim;
        }
        buff.append("]");
        
        return buff.toString();
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
    
    /**
     * Formats the specified xml source and returns an xml string.
     * <b>Note:</b> If the specified <code>Source</code> is an instance of
     * <code>StreamSource</code>, it will be {@link #reset(Source)} after
     * the xml string is generated.
     * 
     * @param src The xml source object.
     * @return Formatted xml or an empty string if formatting fails.
     * @throws Exception if an error occurs printing xml.
     */
    public static String print(Source src) throws Exception {
        StringWriter writer = new StringWriter();
        StreamResult dest = new StreamResult(writer);
        Transformer tr = mFactory.newTransformer();
        tr.transform(src, dest);
        return writer.toString();
    }

    /**
     * Reads the content of the specified file.  This method reads the file
     * a byte at a time, using {@link DataInputStream#readByte()}.
     * 
     * @param file The file to read.
     * @return The content of the file.
     * @throws Exception If an error occurs reading file.
     */
    public static String readFileContent(File file) throws Exception {
        FileInputStream fis = null;
        DataInputStream dis = null;
        try {
            StringBuffer buff = new StringBuffer();
            fis = new FileInputStream(file);
            dis = new DataInputStream(fis);
            
            while (dis.available() != 0) {
                buff.append((char) dis.readByte());
            }
            
            return buff.toString();
        }
        finally {
            if (fis != null) fis.close();
            if (dis != null) dis.close();
        }
    }

    /**
     * Writes the specified content to a file.  This method writes a
     * <code>byte[]</code>, generated from the content parameter, to
     * a file using a {@link DataOutputStream}.
     * 
     * @param file The destination file.
     * @param content The content to write.
     * @throws Exception If an error occurs writing content to file.
     */
    public static void writeFile(File file, String content) throws Exception {
        FileOutputStream fos = null;
        DataOutputStream dos = null;
        try {
             fos = new FileOutputStream(file);
             dos = new DataOutputStream(fos);
             
             byte[] b = content.getBytes();
             dos.write(b, 0, b.length);
        }
        finally {
            if (fos != null) fos.close();
            if (dos != null) dos.close();
        }
    }
}
