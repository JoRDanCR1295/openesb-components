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
 * @(#)EgateConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Properties;

/**
 * Access to eGate properties.  This checks the system properties first,
 * then the definitions in the user's own ~/.ecrc file (if any), and
 * finally a small list of built-in properties (see static init below).
 * These properties are mainly meant for internal testing purposes,
 * not normally for use by customers.
 * Converted from eGate 4.5.3 Java/com/stc/jcsre/JCSProperties.java.
 *
 * @author Michael Libourel
 * @version
 */
public class EgateConfig {
    private static Properties preProp = new Properties();
    private static Properties jcsProp = new Properties();
    private static File jcsFile =
        new File(System.getProperty("user.home"), ".ecrc");

    /**
     * (Re-)loads all properties from the initialization file.
     */
    public static void reload () {
        try {
            InputStream is = new FileInputStream(jcsFile);
            jcsProp.load(is);
            is.close();
        } catch (java.io.FileNotFoundException fnf) {
            // There is no ".ecrc" file at all; perfectly legal.
            //- System.out.println("no init file \"" + jcsFile.getPath() + '"');
        } catch (java.io.IOException io) {
            // There is a ".ecrc" file, but it's broken or inaccessible.
            System.out.println("can't init from \"" + jcsFile.getPath() + '"');
        }
    }

    static {
        reload();

        // The built-in properties.
        preProp.setProperty("JGen.antlr", "com.stc.antlr");
    }

    /**
     * Gets a property (cached).
     *
     * @param key  the property name
     * @return the property value, or null
     */
    public static String getProperty (String key) {
        return getProperty(key, null, false);
    }

    /**
     * Gets a property (cached), or default.
     *
     * @param key  the property name
     * @param deft  the default value (may be null)
     * @return the property value, or null
     */
    public static String getProperty (String key, String deft) {
        return getProperty(key, deft, false);
    }

    /**
     * Gets a property; optionally make sure the initialization file
     * is current.  If the preperty is not defined, return null.
     *
     * @param key  the property name
     * @param now  flag: relaod initialization file first?
     * @return the property value, or null
     */
    public static String getProperty (String key, boolean now) {
        return getProperty(key, null, now);
    }

    /**
     * Gets a property; optionally make sure the initialization file
     * is current.  If the property is not defined, return the given default.
     *
     * @param key  the property name
     * @param deft  the default value (may be null)
     * @param now  flag: relaod initialization file first?
     * @return the property value, or else the default
     */
    public static String getProperty (String key, String deft, boolean now) {
        // First try system properties (command line).
        String res = System.getProperty(key);
        if (res == null) {
            // Not a system property, try the file.
            if (now) {
                reload();
            }
            res = jcsProp.getProperty(key);
            if (res == null) {
                // Not in the file, try built-ins.
                res = preProp.getProperty(key);
                if (res == null) {
                    // Not a built-in, use default.
                    res = deft;
                }
            }
        }
        return res;
    }

    /**
     * Gets a boolean flag (cached).
     * If the property is not defined, return the default supplied here.
     * The property value must match "0|N|n|no|F|f|false" for false, or
     * "1|Y|y|yes|T|t|true" for true.  Other values will throw a runtime
     * exception.
     *
     * @param key  the property name
     * @param deft  the default value if undefined
     * @return the boolean property value
     */
    public static boolean getFlag (String key, boolean deft) {
        String res = getProperty(key);
        if (res == null) {
            return deft;
        }
        if (res.equals("0") ||
            res.equals("N") ||
            res.equals("n") ||
            res.equals("no") ||
            res.equals("F") ||
            res.equals("f") ||
            res.equals("false")) {
            // Valid "false" value.
            return false;
        }
        if (res.equals("1") ||
            res.equals("Y") ||
            res.equals("y") ||
            res.equals("yes") ||
            res.equals("T") ||
            res.equals("t") ||
            res.equals("true")) {
            // Valid "true" value.
            return true;
        }
        throw new RuntimeException("eGate property [" + key
            + "] has illegal value \"" + res + "\"");
    }

    /**
     * Gets an integer property (cached).
     * If the property is not defined, return the default supplied here.
     * If the value is defined but not a legal integer format, barf.
     *
     * @param key  the property name.
     * @param deft  the default value if undefined.
     * @return the integer property value.
     * @throws NumberFormatException error in number format.
     */
    public static int getInteger (String key, int deft)
        throws NumberFormatException {
        String res = getProperty(key);
        return (res == null || res.equals("")) ? deft : Integer.parseInt(res);
    }

    /**
     * Tests this package.  Given a list of propery names, display the
     * value of each property, and the path of the initialization file.
     * Option "-p" will display the list of built-in properties, option
     * "-j" will display the list of properties read from the ".ecrc"
     * file.
     *
     * @param args  a list of property names
     */
    public static void main(String[] args) {
        System.out.println("ecrc file "
            + (jcsFile.exists() ? "is" : "would be")
            + " <" + jcsFile.getPath() + ">");
        for (int i = 0; i < args.length; i++) {
            if (args[i].equals("-p")) {
                // Option -p: print the predefined properties.
                System.out.println("// Predefined Properties:");
                preProp.list(System.out);
                continue;
            }
            if (args[i].equals("-j")) {
                // Option -j: print the properties in the ".ecrc" file.
                System.out.println("// User-defined Properties:");
                jcsProp.list(System.out);
                continue;
            }
            // Just get the value.
            String v = getProperty(args[i]);
            System.out.println("Value of [" + args[i] + "] is "
                + (v == null ? "undefined" : ('"' + v + '"')));
        }
    }
}
