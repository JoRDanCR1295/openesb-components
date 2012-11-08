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
 * @(#)Messages.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.util;

import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Messages is a support class for i18n message handling.
 * <p>
 * The Messages class is created using the getMessages() factory method.  The
 * key argument to the factory method is a Class object.  The
 * Bundle.properties file is loaded based on the Class object and convention
 * agreed by developers using the Messages class.  Bundle files are assumed
 * to be in a sub-package called "messages" of the Class objects package.  So,
 * if the Class,
 * <p>
 * <blockquote><pre>
 *     com.sun.jbi.foo.SomeClass
 * </pre></blockquote>
 * is passed as the argument to the getMessages() method, then this class will
 * assume the following Bundle file to exist,
 * <p>
 * <blockquote><pre>
 *     com.sun.jbi.foo.messages.Bundle.properties
 * </pre></blockquote>
 * <p>
 * Should a Class not have a package, getMessages() will search for a
 * messages.Bundle.properties class.  Finally, the behavior is currently
 * undefined for Class objects of primitive types or arrays.
 * <p>
 * This class also provides facilities to obtain Logger objects that respect the
 * internationalized Bundle.properties files.  If a call is made to
 * registerContext(), the getLogger() method will return a Logger object from the
 * ComponentContext.  This Logger object will respect the contract provided
 * by the JBI framework for changing log levels.  Should no ComponentContext be
 * available, a normal Logger object will be created through the
 * Logger.getLogger() method provided by the Java APIs.
 */
public class Messages {

    private static final Logger mLogger = Logger.getLogger("com.sun.jbi.engine.iep.core.runtime.util.Messages");
    private static final ResourceBundle mBundle = ResourceBundle.getBundle("com/sun/jbi/engine/iep/core/runtime/util/messages/Bundle");
    private static final String DEFAULT_BUNDLE_NAME = "messages.Bundle";
    private static Logger LOGGER = null;

    public static void setLogger(Logger logger) {
        LOGGER = logger;
    }

    /*
     * The name of the Resource Bundle
     */
    private String mBundleName;
    /*
     * The ResourceBundle object containing all localized strings
     */
    private ResourceBundle mResourceBundle;
    /*
     * Logger to use when LOGGER is not set
     * 
     */
    private Logger mDefaultLogger;

    /**
     * Private constructor for Messages class.  Creates the ResourceBundle
     * used by this class
     *
     * @param        clazz the Class object used to determine the
     * @param        locale the locale 
     */
    private Messages(Class clazz, Locale locale) {
        mBundleName = Messages.getBundleName(clazz);
        mResourceBundle = ResourceBundle.getBundle(mBundleName, locale,
                clazz.getClassLoader());
        mDefaultLogger = getLogger(clazz);
    }

    /**
     * Retrieves the package name for a Class object.  Searches the
     * Package object first to determine the package name.  If the Package
     * object is not present, parses the fully-qualified class name to
     * determine the package name.
     *
     * If the class doesn't have a package, the empty string is returned.
     *
     * @param        clazz the 
     * @return       the package name for clazz if it exists; otherwise
     * an empty string is returned
     */
    private static String getPackageName(Class clazz) {
        Package packageObj = clazz.getPackage();
        if (packageObj != null) {
            return packageObj.getName();
        }

        // We need to do a better job here.  We assume the Class object
        // is just a "normal" class and not an array or the Class
        // representing a primitive.  To do this right, we have to handle
        // all cases defined by the getName() method of Class
        String fullClassName = clazz.getName();
        int lastIndex = fullClassName.lastIndexOf(".");
        if (lastIndex > 0) {
            return fullClassName.substring(0, lastIndex);
        }

        // The fully-qualified class name doesn't have any "." in it.
        // Assume that it doesn't have a package and return the empty
        // String
        return "";
    }

    /**
     * Factory method to create a Messages object from a Class.
     *
     * @param        clazz the Class object used to find the ResourceBundle
     * @return       a Messages object
     * @exception    MissingResourceException if unable to find the appropriate
     * ResourceBundle
     */
    public static Messages getMessages(Class clazz) throws MissingResourceException {
        return new Messages(clazz, Locale.getDefault());
    }

    /**
     * Factory method to create a Messages object from Class and a Locale
     *
     * @param        clazz the Class object used to find the ResourceBundle
     * @param        locale the Locale to find the correct locale-specific
     * ResourceBundle
     * @return       a Messages object
     * @exception    MissingResourceException if unable to find the appropriate
     * ResourceBundle
     */
    public static Messages getMessages(Class clazz, Locale locale) throws MissingResourceException {
        return new Messages(clazz, locale);
    }

    /**
     * Factory method to determine the bundle name for a Class
     *
     * @param        clazz the Class object used to find the ResourceBundle
     * @return       the name of the ResourceBundle
     * @exception    MissingResourceException if unable to find the appropriate
     * ResourceBundle
     */
    public static String getBundleName(Class clazz) throws MissingResourceException {
        String packageName = getPackageName(clazz);
        String bundleName = DEFAULT_BUNDLE_NAME;
        if (!packageName.equals("")) {
            bundleName = packageName + "." + DEFAULT_BUNDLE_NAME;
        }

        return bundleName;
    }

    /**
     * Retrieves a Logger object.  If a ComponentContext is
     * registered with this Messages class, the class name of clazz
     * is used as a suffix.  Otherwise, a Logger is retrieved from
     * the Logger.getLogger() static method using the class name of
     * clazz as the name of the Logger.
     *
     * @param        clazz the class object to retrieve the Logger for
     * @return       a Logger object
     */
    private static Logger getLogger(Class clazz) {
        boolean hasBundle = false;
        try {
            ResourceBundle.getBundle(getBundleName(clazz), Locale.getDefault(),
                    clazz.getClassLoader());
            hasBundle = true;
        } catch (MissingResourceException mre) {
        // ignore on purpose
        }

        if (hasBundle) {
            return Logger.getLogger(clazz.getName(), getBundleName(clazz));
        } else {
            return Logger.getLogger(clazz.getName());
        }
    }

    /**
     * Retrieves the ResourceBundle used by this class
     *
     * @return       the ResourceBundle
     */
    public ResourceBundle getBundle() {
        return mResourceBundle;
    }

    /**
     * Retrieves the name of the Bundle file used by this class
     *
     * @return name of the Bundle file
     */
    public String getBundleName() {
        return mBundleName;
    }

    /**
     * Retrieves the default logger used by this class
     *
     * @return       the ResourceBundle
     */
    public Logger getDefaultLogger() {
        return mDefaultLogger;
    }

    /**
     * Convenience method to retrieve a String from the resource bundle.
     * The main difference being that instead of throwing a
     * MissingResourceException if the key can not be found in the resource
     * bundle, the key iteslf will be used to return a value.
     *
     * This also applies a MessageFormat to the value with the argument
     * provided
     *
     * @param key the resource key
     * @param args the MessageFormat arguments 
     */
    public String getString(String key, Object... args) {
        if (args.length == 0) {
            try {
                return mResourceBundle.getString(key);
            } catch (MissingResourceException e) {
                mLogger.log(Level.WARNING,
                        mBundle.getString("Messages.Resource_entry_is_missing_from_the_resource_bundle") + ": " + key,
                        e);
                return '!' + key + '!';
            }
        }
        String result = null;
        String rawValue = getString(key);
        rawValue = StringUtil.replaceAll(rawValue, "'", "''");
        try {
            result = MessageFormat.format(rawValue, args);
        } catch (IllegalArgumentException e) {
            mLogger.log(Level.WARNING,
                    mBundle.getString("Messages.Resource_string_could_not_be_formatted") + ":(" + key + "," + Arrays.asList(args) + ")",
                    e);
            result = rawValue;
        }
        return result;
    }

    private Logger getLogger() {
        return LOGGER == null ? mDefaultLogger : LOGGER;
    }

    public boolean isLoggable(Level level) {
        return getLogger().isLoggable(level);
    }

    public void logOriginal(Level level, String message) {
        getLogger().log(level, message);
    }

    public void log(Level level, String key, Object... arg) {
        getLogger().log(level, getString(key, arg));
    }

    public void log(Level level, String key, Object arg, Throwable t) {
        getLogger().log(level, getString(key, arg), t);
    }

    public void log(Level level, String key, Object[] args, Throwable t) {
        getLogger().log(level, getString(key, args), t);
    }
    
    public void println(String key, Object... arg) {
        System.out.println(getString(key, arg));
    }
    
    public void printlnOriginal(String message) {
        System.out.println(message);
    }

    public void print(String key, Object... arg) {
        System.out.print(getString(key, arg));
    }
    
    public void printOriginal(String message) {
        System.out.print(message);
    }
}
