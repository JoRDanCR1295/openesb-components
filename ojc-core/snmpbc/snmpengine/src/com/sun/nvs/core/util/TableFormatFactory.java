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
abstract public class TableFormatFactory {
    /**
     * DOCUMENT ME!
     *
     * @param formatType DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws UnSupportedFormatTypeException DOCUMENT ME!
     */
    public static TableFormat getTableFormatter(String formatType)
        throws UnSupportedFormatTypeException {
        int format = FormatTypes.getValue(formatType);
        TableFormat tableFormat = null;

        switch (format) {
        case FormatTypes.CLI_Type:
            tableFormat = new TableFormat();

            break;

        case FormatTypes.XML_Type:
            tableFormat = new XMLTableFormat();

            break;

        default:
            throw new UnSupportedFormatTypeException(
                "The Format Type: " + formatType + " is currently not being supported"
            );
        }

        return tableFormat;
    }
}
