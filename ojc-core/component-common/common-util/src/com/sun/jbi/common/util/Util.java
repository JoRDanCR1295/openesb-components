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

package com.sun.jbi.common.util;

import java.io.Closeable;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

/**
 * Contains generic utility methods.
 * 
 * @author Kevan Simpson
 */
public class Util {
    /**
     * Searches for a factory implementation to instantiate an instance of an
     * unknown class, using the specified fallback classname if the factory id
     * is not specified.
     * 
     * @param factoryId A system property whose value is a classname to instantiate.
     * @param fallbackClassName The fallback implementation if factory is absent.
     * @param seeker The class seeking this facotry id or the <code>Util</code> class
     *               if the specified parameter is <code>null</code>.
     * @return an instantiated object.
     * @throws Exception if an error occurs during lookup or instantiation.
     */
    public static Object find(String factoryId, String fallbackClassName, Class<? extends Object> seeker) 
            throws Exception {        
//      Figure out which ClassLoader to use for loading the provider
//      class.  If there is a Context ClassLoader then use it.
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

        if (classLoader == null) {
            // if we have no Context ClassLoader, use the current ClassLoader
            classLoader = (seeker == null) 
                    ? Util.class.getClassLoader() : seeker.getClassLoader();
        }

        // Use the system property first
        try {
            String systemProp = System.getProperty(factoryId);
            if (!Util.isEmpty(systemProp)) {                
                return newInstance(systemProp, classLoader, true );
            }
        } 
        catch (Exception syse) {
            //if first option fails due to any reason we should try next option in the
            //look up algorithm.
        }

        if (Util.isEmpty(fallbackClassName)) {
//          throw new ConfigurationError(
//          "Provider for " + factoryId + " cannot be found", null);
        }

        return newInstance(fallbackClassName, classLoader, true);
    }

    /**
     * Instantiates an instance of the specified class using the specified classloader.
     * 
     * @param className The specified classname.
     * @param cl A classloader or <code>null</code>.
     * @param doFallback when <code>true</code>, the context CL will be used, else
     *                   an {@link Exception} will be thrown if instantiation fails.
     * @return a new instance of the specified class.
     * @throws Exception if an error occurs loading or instantiating class.
     */
    public static Object newInstance(String className, 
                                     ClassLoader cl,
                                     boolean doFallback) throws Exception {
        Class providerClass;
        if (cl == null) {
            // If classloader is null Use the bootstrap ClassLoader.
            // Thus Class.forName(String) will use the current
            // ClassLoader which will be the bootstrap ClassLoader.
            providerClass = Class.forName(className);
        }
        else {
            try {
                providerClass = cl.loadClass(className);
            }
            catch (ClassNotFoundException x) {
                if (doFallback) {
                    // Fall back to current classloader
                    cl = Thread.currentThread().getContextClassLoader();
                    providerClass = Class.forName(className, true, cl);
                }
                else {
                    throw x;
                }
            }
        }

        Object instance = providerClass.newInstance();
        return instance;
    }

    /**
     * Acquires a Logger from the specified {@link ComponentContext}.
     * <p>
     * If a Logger cannot be acquired from the context, a standard
     * Logger will be returned.
     * 
     * @param ctx a component's context.
     * @param logger the logger name.
     * @return a <code>Logger</code>, which will be context-aware if and only if
     *         the specified context is not <code>null</code>.
     */
    public static Logger getLogger(ComponentContext ctx, String logger) {
        try {
            return (ctx == null) ? Logger.getLogger(logger) : ctx.getLogger(logger, null);
        }
        catch (JBIException jbi) {  
            // failed to acquire Logger
            Logger log = Logger.getLogger(logger);
            if (log.isLoggable(Level.FINE)) {
                log.fine("UTIL-3001: Failed to acquire Logger from "+ 
                         ctx.getComponentName() +" component context:"+ jbi.getMessage());
            }
            return log;
        }
    }

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
     * Utility to parse <code>int</code>s.
     * @param str The string to parse into an <code>int</code>.
     * @param defaultValue The value to return if parsing fails.
     * @return The <code>int</code> value of the string or <code>defaultValue</code>.
     */
    public static int parseInt(String str, int defaultValue) {
    	try { return (Util.isEmpty(str)) ? defaultValue : Integer.parseInt(str); }
    	catch (NumberFormatException nfe) { return defaultValue; }
    }

    /**
     * Utility to parse <code>long</code>s.
     * @param str The string to parse into an <code>long</code>.
     * @param defaultValue The value to return if parsing fails.
     * @return The <code>long</code> value of the string or <code>defaultValue</code>.
     */
    public static long parseLong(String str, long defaultValue) {
    	try { return (Util.isEmpty(str)) ? defaultValue : Long.parseLong(str); }
    	catch (NumberFormatException nfe) { return defaultValue; }
    }

    /**
     * Utility method wrapping a {@link StringTokenizer}.
     * @param str a string to be parsed
     * @param delims the delimiter(s)
     * @return array of parsed strings
     */
    public static String[] tokenize(String str, String delims) {
        if (isEmpty(str)) {
            return new String[] { String.valueOf(str) };
        }
        
        StringTokenizer tkns = new StringTokenizer(str, delims);
        List<String> list = new ArrayList<String>();
        while (tkns.hasMoreTokens()) {
            list.add(tkns.nextToken());
        }
        
        return list.toArray(new String[list.size()]);
    }

    /**
     * Generates a string representation of an array.
     * @param objs The array of objects.
     * @param delim The delimiter with which to concatenate the array elements.
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
     * Reads the content of the specified file.  This method reads the file
     * a byte at a time, using {@link DataInputStream#readByte()}.
     * 
     * @param file The file to read.
     * @return The content of the file.
     * @throws IOException If an error occurs reading file.
     */
    public static String readFileContent(File file) throws IOException {
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
            safeClose(fis);
            safeClose(dis);
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
    public static void writeFile(File file, String content) throws IOException {
        if (file == null || content == null) {
            return;
        }
        FileOutputStream fos = null;
        DataOutputStream dos = null;
        try {
             fos = new FileOutputStream(file);
             dos = new DataOutputStream(fos);
             
             byte[] b = content.getBytes();
             dos.write(b, 0, b.length);
        }
        finally {
            safeClose(fos);
            safeClose(dos);
        }
    }
        
    public static void safeClose(Closeable stream) {
        if (stream != null) {
            try { stream.close(); }
            catch (IOException e) { /* ignore */ }
        }
    }
}
