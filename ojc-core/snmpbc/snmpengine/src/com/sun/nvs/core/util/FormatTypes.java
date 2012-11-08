/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.util;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
abstract public class FormatTypes {
    /**
     * DOCUMENT ME!
     */
    public static String CLI = "CLI";

    /**
     * DOCUMENT ME!
     */
    public static String XML = "XML";
    private static final String[] formatTypes = {CLI, XML};

    /**
     * DOCUMENT ME!
     */
    public static final int CLI_Type = 0;

    /**
     * DOCUMENT ME!
     */
    public static final int XML_Type = 1;

    /**
     * DOCUMENT ME!
     *
     * @param typeStr DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int getValue(String typeStr) {
        for (int i = 0; i < formatTypes.length; i++) {
            if (typeStr.equals(formatTypes[i])) {
                return i;
            }
        }

        throw new UnSupportedFormatTypeException(
            "The Format Type: " + typeStr + " is currently not being supported"
        );
    }

    /**
     * DOCUMENT ME!
     *
     * @param typeVal DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getString(int typeVal) {
        return formatTypes[typeVal];
    }
}
