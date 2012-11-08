/*
 *
 *          Copyright (c) 2007 Sun Microsystems, Inc.
 *          All Rights Reserved.
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 */

package com.sun.caps.management.common.resources;


import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 *
 * @author ylee
 */
public class Messages {
    private static final String BUNDLE_NAME = "com.sun.caps.management.common.resources.Bundle"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    protected Messages() {
    }

    public static String getString(String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        } catch (MissingResourceException e) {
            return '!' + key + '!';
        }
    }
    public static String getString(String key,String[] arguments) {
        String result =  null;
        try {
            result = RESOURCE_BUNDLE.getString(key);
            for (int index = 0; index < arguments.length; index++) {
                String stringValue = arguments[index];
                String argumentKey = "{" + index +"}";
                result = result.replace(argumentKey,stringValue);
            }

        } catch (MissingResourceException e) {
            return '!' + key + '!';
        }
        
        return result;
    }
    
}
