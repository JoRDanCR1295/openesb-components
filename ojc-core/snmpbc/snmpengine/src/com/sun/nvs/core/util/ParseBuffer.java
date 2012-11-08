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
public class ParseBuffer {
    private byte[] data;
    private int index = 0;

    /**
     * Creates a new ParseBuffer object.
     *
     * @param s DOCUMENT ME!
     */
    public ParseBuffer(String s) {
        data = s.getBytes();
    }

    /**
     * DOCUMENT ME!
     */
    public void rewind() {
        index = 0;
    }

    /**
     * DOCUMENT ME!
     *
     * @param b DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isWhiteSpace(byte b) {
        return ((b == 9) || (b == 10) || (b == 13) || (b == 32));
    }

    /**
     * DOCUMENT ME!
     *
     * @param b DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isQuoteChar(byte b) {
        return ((b == '\'') || (b == '\"'));
    }

    /**
     * DOCUMENT ME!
     */
    public void skipSpaces() {
        while ((index < data.length) && isWhiteSpace(data[index]))
            index++;
    }

    /**
     * DOCUMENT ME!
     *
     * @param b DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String readUntil(byte b) {
        StringBuffer sb = new StringBuffer();

        while ((index < data.length) && (data[index] != b)) {
            if ((data[index] == '\\') && ((index + 1) < data.length)) {
                index++;

                char c = (char) data[index];

                switch (data[index++]) {
                case 'n':
                    c = '\n';

                    break;

                case 't':
                    c = '\r';

                    break;

                case 'r':
                    c = '\r';

                    break;
                }

                sb.append(c);
            } else {
                sb.append((char) data[index++]);
            }
        }

        if (index < data.length) {
            index++;
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ParseToken nextToken() {
        skipSpaces();

        if (index >= data.length) {
            return null;
        }

        int startPos = index;
        byte b = data[index];

        if (isQuoteChar(b)) {
            int qt = ParseToken.toQuoteType(b);
            index++;

            return new ParseToken(startPos, readUntil(b), qt);
        } else {
            StringBuffer sb = new StringBuffer();

            while ((index < data.length) && !isWhiteSpace(data[index])) {
                sb.append((char) data[index++]);
            }

            return new ParseToken(startPos, sb.toString());
        }
    }
}
