/*
 * Copyright (c) 2007 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * This program, and all the routines referenced herein,
 * are the proprietary properties and trade secrets of
 * Sun Microsystems.
 *
 * Except as provided for by license agreement, this
 * program shall not be duplicated, used, or disclosed
 * without  written consent signed by an officer of
 * Sun Microsystems.
 */

package com.sun.caps.management.common;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;


/**
 * This class reads the i18n strings from locale specific bundle from the
 * Bundle[locale].properties or bundle[locale].properties file in a specified
 * package. This class has methods for formating the messages.
 *
 * @author graj
 *
 */
public class I18NBundle {
    /** package name */
    private String         mBundlePackageName = null;

    /** resource bundle */
    private ResourceBundle mBundle            = null;

    /**
     * constructor
     * @param packageName packe name ( e.g. com.sun.mypackage ) in which
     * to look for Bundle.properties file
     */
    public I18NBundle(String packageName) {
        this.mBundlePackageName = packageName;
        this.mBundle = null;

    }

    /**
     * loads the bundle
     * @param bundleName bundle name
     * @param packageName packe name ( e.g. com.sun.mypackage ) in which
     * to look for Bundle.properties file
     */
    private void loadBundle(String packageName, String bundleName) {

        String bundleBaseName = packageName + "." + bundleName;
        ResourceBundle resBundle = null;
        try {
            resBundle = ResourceBundle.getBundle(bundleBaseName);
        } catch (MissingResourceException ex) {
            // Try with locale independent defaultBundle
            try {
                resBundle = ResourceBundle.getBundle(bundleBaseName,
                        new Locale(""));
            } catch (Exception anyEx) {
                Util.logDebug(anyEx);
            }
        }

        if (resBundle != null) {
            this.mBundle = resBundle;
        }
    }

    /**
     * gets the loaded resource bundle
     * @return resource bundle
     */
    public ResourceBundle getBundle() {
        // lazzy init
        if (this.mBundle == null) {
            loadBundle(this.mBundlePackageName, "Bundle");
            // try to load the bundle with lower case first letter
            if (this.mBundle == null) {
                loadBundle(this.mBundlePackageName, "bundle");
            }
        }
        return this.mBundle;
    }

    /** gets the i18n message
     * @param aI18NMsg String.
     * @param aArgs Object[]
     * @return formated i18n string.
     */
    public static String getFormattedMessage(String aI18NMsg, Object[] aArgs) {
        String formattedI18NMsg = aI18NMsg;
        try {
            MessageFormat mf = new MessageFormat(aI18NMsg);
            formattedI18NMsg = mf.format(aArgs);
        } catch (Exception ex) {
        }
        return formattedI18NMsg;
    }

    /** gets the i18n message
     * @param aI18NKey i18n key
     * @param anArgsArray array of arguments for the formatted string
     * @return formatted i18n string
     */
    public String getMessage(String aI18NKey, Object[] anArgsArray) {
        String i18nMessage = "";
        ResourceBundle bundle = getBundle();
        if(bundle != null) {
            i18nMessage = getBundle().getString(aI18NKey);
        }
        if ((anArgsArray != null) && (i18nMessage != null) && (i18nMessage.length() > 0)) {
            return getFormattedMessage(i18nMessage, anArgsArray);
        } else {
            return i18nMessage;
        }
    }

    /** gets the i18n message
     * @param aI18NKey i18n key
     * @return i18n string
     */
    public String getMessage(String aI18NKey) {
        return getMessage(aI18NKey, null);
    }

    /** gets the i18n message
     * @param aI18NKey i18n key
     * @param arg1 argrument object to message
     * @return i18n string
     */
    public String getMessage(String aI18NKey, Object arg1) {
        Object[] args = { arg1 };
        return getMessage(aI18NKey, args);
    }

    /** gets the i18n message
     * @param aI18NKey i18n key
     * @param arg1 argrument object to message
     * @param arg2 argrument object to message
     * @return i18n string
     */
    public String getMessage(String aI18NKey, Object arg1, Object arg2) {
        Object[] args = { arg1, arg2 };
        return getMessage(aI18NKey, args);
    }

    /** gets the i18n message
     * @param aI18NKey i18n key
     * @param arg1 argrument object to message
     * @param arg2 argrument object to message
     * @param arg3 argrument object to message
     * @return i18n string
     */
    public String getMessage(String aI18NKey, Object arg1, Object arg2,
            Object arg3) {
        Object[] args = { arg1, arg2, arg3 };
        return getMessage(aI18NKey, args);
    }
}
