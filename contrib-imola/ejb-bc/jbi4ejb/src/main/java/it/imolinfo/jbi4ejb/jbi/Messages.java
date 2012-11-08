/*
 *  Copyright (c) 2005, 2006 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4ejb.jbi;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Class dedicated to the internationalization of application messages.
 * <br>
 *
 * @author  <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class Messages {
    /*
     * DON'T ADD LOGGING TO THIS CLASS, because this class is used by logging
     * itself, so it will cause java.lang.StackOverflowError while initializing
     * the logging system
     */

    /**
     * The bundle name for a class.
     */
    private static final String BUNDLE_NAME = "messages.Bundle";

    /**
     * The suffix added to the package name of a class to identify the correct
     * resource bundle to be used by the class itself.
     */
    private static final String BUNDLE_NAME_SUFFIX = "." + BUNDLE_NAME;

    /**
     * The resource bundle containing all localized strings.
     */
    private final ResourceBundle bundle;

    /**
     * Retrieves the <code>ResourceBundle</code> used by this instance.
     *
     * @param  clazz   the class used to identify the resource bundle. Must not
     *                 be <code>null</code>.
     * @param  locale  the locale to use. Must not be <code>null</code>.
     */
    @SuppressWarnings("unchecked")
    private Messages(final Class clazz, final Locale locale) {
        bundle = ResourceBundle.getBundle(getBundleName(clazz), locale,
                                          clazz.getClassLoader());
    }

    /**
     * Factory method to create a <code>Messages</code> object from a
     * <code>Class</code>.
     *
     * @param   clazz   the class used to find the resource bundle. Must not be
     *                  <code>null</code>.
     * @return  a <code>Messages</code> object related to <code>clazz</code>,
     *          never <code>null</code>. The messages bundle used is related to
     *          the default locale.
     */
    @SuppressWarnings("unchecked")
    public static Messages getMessages(final Class clazz) {
        return new Messages(clazz, Locale.getDefault());
    }

    /**
     * Factory method to create a <code>Messages</code> object from a
     * <code>Class</code> and a <code>Locale</code>.
     *
     * @param   clazz   the class used to find the resource bundle. Must not be
     *                  <code>null</code>.
     * @param   locale  the <code>Locale</code> to find the correct resource
     *                  bundle. If <code>null</code>, the default locale will be
     *                  used.
     * @return  a <code>Messages</code> object related to <code>clazz</code> and
     *          <code>locale</code>, never <code>null</code>.
     */
    @SuppressWarnings("unchecked")
    public static Messages getMessages(final Class clazz, final Locale locale) {
        if (locale == null) {
            return new Messages(clazz, Locale.getDefault());
        }
        return new Messages(clazz, locale);
    }

    /**
     * Retrieves a localized <code>String</code> which may contains parameters.
     * This method applies a <code>MessageFormat</code> to the value with the
     * arguments provided.
     *
     * @param   key   the resource key to retrieve the (localized) message.
     * @param   args  the optional <code>MessageFormat</code> arguments.
     * @return  the localized messaged related to the key <code>key</code> after
     *          the substitution of its parameters with values
     *          <code>args</code>.
     */
    public String getString(final String key, final Object ... args) {
        String rawValue;

        try {
            synchronized (bundle) {
                rawValue = bundle.getString(key);
            }
        } catch (MissingResourceException e) {
            return key;
        }
        try {
            return MessageFormat.format(rawValue, args);
        } catch (IllegalArgumentException e) {
            return rawValue;
        }
    }

    /**
     * Determines the bundle name for a <code>Class</code>.
     *
     * @param   clazz  the <code>Class</code> object used to find the
     *                 <code>ResourceBundle</code>. Must not be
     *                 <code>null</code>.
     * @return  the name of the <code>ResourceBundle</code> related to
     *          <code>clazz</code>, ever different from <code>null</code>.
     */
    @SuppressWarnings("unchecked")
    private static String getBundleName(final Class clazz) {
        String packageName = getPackageName(clazz);

        if (packageName.length() == 0) {
            return BUNDLE_NAME;
        }
        return packageName.concat(BUNDLE_NAME_SUFFIX);
    }

    /**
     * Retrieves the package name for a <code>Class</code> object.
     * If the class doesn't have a package, the empty string is returned.
     *
     * @param   clazz  the class to retrieve its package name. Must not be
     *                 <code>null</code>.
     * @return  the package name for <code>clazz</code> if it exists, otherwise
     *          the empty string.
     */
    @SuppressWarnings("unchecked")
    private static String getPackageName(final Class clazz) {
        Package pack = clazz.getPackage();
        String className;
        int lastDotIndex;

        if (pack != null) {
            return pack.getName();
        }

        if (clazz.isArray()) {
            className = clazz.getComponentType().getName();
        } else {
            className = clazz.getName();
        }
        lastDotIndex = className.lastIndexOf(".");
        if (lastDotIndex > 0) {
            return className.substring(0, lastDotIndex);
        }
        return "";
    }
}
