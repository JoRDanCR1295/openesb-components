/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.util;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class ParseToken {
    /**
     * DOCUMENT ME!
     */
    public static final int QUOTE_TYPE_NONE = 0;

    /**
     * DOCUMENT ME!
     */
    public static final int QUOTE_TYPE_SINGLE = 1;

    /**
     * DOCUMENT ME!
     */
    public static final int QUOTE_TYPE_DOUBLE = 2;
    private int quoteType = QUOTE_TYPE_NONE;
    private int pos;
    private String value;

    /**
     * Creates a new ParseToken object.
     *
     * @param startPos DOCUMENT ME!
     * @param val DOCUMENT ME!
     */
    public ParseToken(int startPos, String val) {
        this.pos = startPos;
        this.value = val;
    }

    /**
     * Creates a new ParseToken object.
     *
     * @param startPos DOCUMENT ME!
     * @param val DOCUMENT ME!
     * @param quoteType DOCUMENT ME!
     */
    public ParseToken(int startPos, String val, int quoteType) {
        this(startPos, val);
        this.quoteType = quoteType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isQuoted() {
        return (quoteType != QUOTE_TYPE_NONE);
    }

    /**
     * DOCUMENT ME!
     *
     * @param b DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int toQuoteType(byte b) {
        if (b == '\'') {
            return QUOTE_TYPE_SINGLE;
        } else if (b == '\"') {
            return QUOTE_TYPE_DOUBLE;
        } else {
            return QUOTE_TYPE_NONE;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getQuoteType() {
        return quoteType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getStartPos() {
        return pos;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getValue() {
        return value;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return value;
    }
}
