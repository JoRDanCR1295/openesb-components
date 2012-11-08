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
 * @(#)MessageManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common;

import java.text.MessageFormat;
import java.util.Hashtable;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;


/**
 * An internationalization / localization helper class which reduces the bother
 * of handling ResourceBundles and takes care of the common cases of message
 * formating which otherwise require the creation of Object arrays and such. <p>
 *
 * The MessageManager operates on a package basis. One MessageManager per
 * package can be created and accessed via the getManager method call. <p>
 *
 * The MessageManager will look for a ResourceBundle named by the package name
 * given plus the suffix of "LocalStrings". In practice, this means that the
 * localized information will be contained in a LocalStrings.properties file
 * located in the package directory of the classpath. <p>
 *
 * Please see the documentation for java.util.ResourceBundle for more
 * information.
 *
 * @author Sun Microsystems
 * @version 
 */
public class MessageManager {

    // --------------------------------------------------------------
    // STATIC SUPPORT METHODS
    // --------------------------------------------------------------

    /** Message managers. */
    private static Hashtable managers = new Hashtable();

    /** The ResourceBundle for this MessageManager. */
    private ResourceBundle bundle;


    /**
     * Creates a new MessageManager for a given package. This is a private
     * method and all access to it is arbitrated by the static getManager
     * method call so that only one MessageManager per package will be created.
     *
     * @param packageName Name of package to create MessageManager for.
     */
    private MessageManager(String packageName) {
        String bundleName = packageName + ".Bundle";
        bundle = ResourceBundle.getBundle(bundleName);
    }

    /**
     * Creates a new MessageManager for a given package. This is a private
     * method and all access to it is arbitrated by the static getManager
     * method call so that only one MessageManager per package will be created.
     *
     * @param baseName Base name of resource bundle file.
     * @param clazz Class object for locating package.
     */
    private MessageManager(String baseName, Class clazz) {
        bundle = ResourceBundle.getBundle(baseName, Locale.getDefault(), clazz.getClassLoader());
    }

    /** Finds package name for given class.
     * @param   clazz   Class object.
     */
    private static String findName(Class clazz) {
        String pref = clazz.getName();
        int last = pref.lastIndexOf('.');
        if (last >= 0) {
            return pref.substring(0, last);
        } else {
            // base package, search for bundle
            return ""; // NOI18N
        }
    }

    /**
     * Get the MessageManager for a particular package. If a manager for a
     * package already exists, it will be reused, else a new MessageManager will
     * be created and returned.
     *
     * @param clazz A class object in the package
     * @return The message manager for the package
     */
    public static MessageManager getManager(Class clazz) {
        String packageName = findName(clazz);
        MessageManager mgr = (MessageManager) managers.get(packageName);
        if (mgr == null) {
            String bundleName = (packageName.length() == 0) ? "Bundle" : packageName + ".Bundle";
            mgr = new MessageManager(bundleName, clazz);
            managers.put(packageName, mgr);
        }
        return mgr;
    }

    /**
     * Get the MessageManager for a particular package. If a manager for a
     * package already exists, it will be reused, else a new MessageManager will
     * be created and returned.
     *
     * @param packageName The name of the package
     * @return The message manager for the package
     */
    public static synchronized MessageManager getManager(String packageName) {
        MessageManager mgr = (MessageManager) managers.get(packageName);
        if (mgr == null) {
            mgr = new MessageManager(packageName);
            managers.put(packageName, mgr);
        }
        return mgr;
    }


    /**
     * Get a string from the underlying resource bundle.
     *
     * @param key The name of the string
     * @return  The string
     */
    public String getString(String key) {
        if (key == null) {
            String msg = "key is null";
            throw new NullPointerException(msg);
        }

        String str = null;

        try {
            str = bundle.getString(key);
        } catch (MissingResourceException mre) {
            str = "Cannot find message associated with key '" + key + "'";
        }

        return str;
    }


    /**
     * Get a string from the underlying resource bundle and format it with the
     * given set of arguments.
     *
     * @param key The name of the string
     * @param args Argument to interpolate into the format string
     * @return The formatted string
     */
    public String getString(String key, Object[] args) {
        String iString = null;
        String value = getString(key);

        try {
            Object nonNullArgs[] = args;
            for (int i = 0; i < args.length; i++) {
                if (args[i] == null) {
                    if (nonNullArgs == args) {
                        nonNullArgs = (Object[]) args.clone();
                    }
                    nonNullArgs[i] = "null";
                }
            }

            iString = MessageFormat.format(value, nonNullArgs);
        } catch (IllegalArgumentException iae) {
            StringBuffer buf = new StringBuffer();
            buf.append(value);
            for (int i = 0; i < args.length; i++) {
                buf.append(" arg[" + i + "]=" + args[i]);
            }
            iString = buf.toString();
        }

        return iString;
    }


    /**
     * Get a string from the underlying resource bundle and format it with the
     * given object argument. This argument can of course be a String object.
     *
     * @param key The name of the string
     * @param arg The string to format with
     * @return The formatted string
     */
    public String getString(String key, Object arg) {
        Object[] args = new Object[]{arg};
        return getString(key, args);
    }


    /**
     * Get a string from the underlying resource bundle and format it with the
     * given object arguments. These arguments can of course be String objects.
     *
     * @param key The name of the string
     * @param arg1 The first argument to interpolate into the string
     * @param arg2 The second argument to interpolate into the string
     * @return The formatted string
     */
    public String getString(String key, Object arg1, Object arg2) {
        Object[] args = new Object[]{arg1, arg2};
        return getString(key, args);
    }


    /**
     * Get a string from the underlying resource bundle and format it with the
     * given object arguments. These arguments can of course be String objects.
     *
     * @param key The name of the string
     * @param arg1 The first argument to interpolate into the string
     * @param arg2 The second argument to interpolate into the string
     * @param arg3 The third argument to interpolate into the string
     * @return The formatted string
     */

    public String getString(String key, Object arg1, Object arg2,
                                Object arg3) {
        Object[] args = new Object[]{arg1, arg2, arg3};
        return getString(key, args);
    }


    /**
     * Get a string from the underlying resource bundle and format it with the
     * given object arguments. These arguments can of course be String objects.
     *
     * @param key The name of the string
     * @param arg1 The first argument to interpolate into the string
     * @param arg2 The second argument to interpolate into the string
     * @param arg3 The third argument to interpolate into the string
     * @param arg4 The fourth argument to interpolate into the string
     * @return The formatted string
     */
    public String getString(String key, Object arg1, Object arg2, Object arg3,
                                Object arg4) {
        Object[] args = new Object[]{arg1, arg2, arg3, arg4};
        return getString(key, args);
    }

}
