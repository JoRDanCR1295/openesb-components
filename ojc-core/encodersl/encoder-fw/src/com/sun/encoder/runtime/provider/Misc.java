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
 * @(#)Misc.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

/**
 * Miscellaneous low-level runtime support methods for OTDs.
 * Some parts may be migrated elsewhere.
 * NYI: Should move this to otd/framework...
 *
 * @author Michael Libourel
 * @version
 * Warning: this is a duplicate class of com.sun.encoder.ud1.runtime.Misc
 */
public final class Misc {

    public static final String ECLIPSIS = "...";
    private static final int SHOW_MAX_SIZE = 50;

    /**
     * Converts string with chars in range 00-FF to byte array with same.
     *
     * @param text  the string
     * @return the byte array
     */
    public static byte[] str2bytes(final String text) {
        if (text == null) {
            return null;
        }
        int len = text.length();
        byte[] b = new byte[len];
        while (len-- > 0) {
            b[len] = (byte) text.charAt(len);
        }
        return b;
    }

    /**
     * Converts a byte-array to a string with chars in range 00-FF.
     *
     * @param data  the byte-array
     * @param offset  the index of the first byte
     * @param length  the number of bytes from the offset
     * @return the string
     */
    public static String bytes2str(byte[] data, int offset, int length) {
        StringBuffer sb = new StringBuffer();
        while (length-- > 0) {
            sb.append((char) (data[offset++] & 0xFF));
        }
        return sb.toString();
    }

    /**
     * Converts a byte-array to a string with chars in range 00-FF.
     *
     * @param data  the byte-array
     * @return the string
     */
    public static String bytes2str (byte[] data) {
        if (data == null) {
            return null;
        }
        return bytes2str(data, 0, data.length);
    }

    /**
     * Returns hex value of low nibble.
     *
     * @param n  the value
     * @return a hexadecimal digit.
     */
    public static char hex(int n) {
        return "0123456789abcdef".charAt(n & 0xF);
    }

    /**
     * Converts byte to printable, quoted string.
     *
     * @param b  the byte value
     * @return a string representation
     */
    public static String printable(byte b) {
        switch (b) {
            case '\\':
                return "\\\\";
            case '"':
                return "\\\"";
            case '\b':
                return "\\b";
            case '\n':
                return "\\n";
            case '\r':
                return "\\r";
            case '\t':
                return "\\t";
        }
        if (b < 0x20 || 0x7E < b || b == '\\' || b == '\"') {
            return "\\x" // escaped
                + hex(b >> 4) + hex(b >> 0);
        }
        // Printable.
        return new String(new char[]{(char) b});
    }

    /**
     * Converts byte[] to printable, quoted string.
     *
     * @param data  the byte array, or null
     * @return a string representation
     */
    public static String printable(byte[] data) {
        if (data == null) {
            return "null";
        }
        StringBuffer sb = new StringBuffer("\"");
        for (int i = 0; i < data.length; i++) {
            sb.append(printable(data[i]));
        }
        sb.append('"');
        return sb.toString();
    }

    /**
     * Converts character to printable, quoted string.
     *
     * @param b  the byte value
     * @return a string representation
     */
    public static String printable(char b) {
        switch (b) {
            case '\\':
                return "\\\\";
            case '"':
                return "\\\"";
            case '\b':
                return "\\b";
            case '\n':
                return "\\n";
            case '\r':
                return "\\r";
            case '\t':
                return "\\t";
        }
        if (b < 0x20 || 0x7E < b || b == '\\' || b == '\"') {
            return "\\" + "u" // escaped
                + hex(b >> 12) + hex(b >> 8) + hex(b >> 4) + hex(b >> 0);
        }
        // Printable.
        return new String(new char[]{b});
    }

    /**
     * Converts string to printable, quoted string.
     *
     * @param data  the byte array, or null
     * @return a string representation
     */
    public static String printable(String data) {
        if (data == null) {
            return "null";
        }
        StringBuffer sb = new StringBuffer("\"");
        int length = data.length();
        for (int i = 0; i < length; i++) {
            sb.append(printable(data.charAt(i)));
        }
        sb.append('"');
        return sb.toString();
    }

    /**
     * Converts a printable string to a string with real char values (unescaped).
     * @param printableData the printable string
     * @return unescaped string
     */
    public static String nonPrintable(String printableData) {
        if (printableData == null) {
            return null;
        }
        StringBuffer sb = new StringBuffer();
        final int length = printableData.length();
        char c;
        for (int i = 0; i < length; i++) {
            if (printableData.charAt(i) == '\\' && i + 1 < length) {
                c = printableData.charAt(i + 1);
                switch (c) {
                    case '\\':
                        sb.append('\\');
                        i++;
                        continue;
                    case 'b':
                        sb.append('\b');
                        i++;
                        continue;
                    case 'f':
                        sb.append('\f');
                        i++;
                        continue;
                    case 'n':
                        sb.append('\n');
                        i++;
                        continue;
                    case 'r':
                        sb.append('\r');
                        i++;
                        continue;
                    case 't':
                        sb.append('\t');
                        i++;
                        continue;
                    case 'x':
                        if (i + 3 < length) {
                            try {
                                int b = Integer.parseInt(printableData.substring(i + 2, i + 4), 16);
                                sb.append((char) b);
                                i += 3;
                                continue;
                            } catch (NumberFormatException e) {
                                //Ignore.  Not treat it as escape sequence
                            }
                        }
                        break;
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                        if (i + 3 < length) {
                            try {
                                int b = Integer.parseInt(printableData.substring(i + 1, i + 4), 8);
                                sb.append((char) b);
                                i += 3;
                                continue;
                            } catch (NumberFormatException e) {
                                //Ignore.  Not treat it as escape sequence
                            }
                        }
                }
            }
            sb.append(printableData.charAt(i));
        }
        return sb.toString();
    }

    /**
     * Copies the indicated slice of a byte-array as a new array.
     *
     * @param data  the input array
     * @param offset  offset to start of slice
     * @param length  size of slice
     * @return a copy of the data slice
     */
    public static byte[] byteslice(byte[] data, int offset, int length) {
        if (data == null) {
            throw new NullPointerException("no data");
        }
        if (offset < 0 || length < 0 || data.length < offset + length) {
            throw new IllegalArgumentException("offset=" + offset + ", length="
                + length + " for " + data.length + " bytes data");
        }
        byte[] result = new byte[length];
        System.arraycopy(data, offset, result, 0, length);
        return result;
    }

    /**
     * Escapes a character.
     *
     * @param c the character value
     * @return a string representation
     */
    public static String escapeChar(char c) {
        switch (c) {
            case '\\':
                return "\\\\";
            case '\b':
                return "\\b";
            case '\n':
                return "\\n";
            case '\r':
                return "\\r";
            case '\t':
                return "\\t";
        }
        if (c < 0x20 || (c > 0x7E && c <= 0xFF)) {
            return "\\" + "x" // escaped
                + hex(c >> 4) + hex(c >> 0);
        }
        if (c > 0xff) {
            return "\\" + "u" // escaped
                + hex(c >> 12) + hex(c >> 8) + hex(c >> 4) + hex(c >> 0);
        }
        // Printable.
        return new String(new char[]{c});
    }

    public static String escapeString(CharSequence str) {
        if (str == null) {
            return null;
        }
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < str.length(); i++) {
            sb.append(escapeChar(str.charAt(i)));
        }
        return sb.toString();
    }

    /**
     * Show fragment of data as printable characters with a maximum characters
     * limit. The fragment string is left aligned at the beginning of data.
     *
     * @param data data bytes.
     * @return string fragment with a maximum characters limit.
     */
    public static String showFragment(final byte[] data) {
        boolean allowLeadingEclipsis = false;
        return showFragment(data, 0, data.length, allowLeadingEclipsis);
    }

    /**
     * Show fragment of data as printable characters from the given fromPos
     * to the given toPos with a maximum characters limit. The fragment string
     * is left aligned at the fromPos position of data.
     *
     * @param data data bytes.
     * @param fromPos from position.
     * @param toPos to position.
     * @param allowLeadingEclipsis if leading eclipsis is allowed in output.
     * @return string fragment from given start to end positions with a maximum
     * characters limit..
     */
    public static String showFragment(final byte[] data, final int fromPos,
        final int toPos, final boolean allowLeadingEclipsis) {
        return showFragment(data, fromPos, toPos, allowLeadingEclipsis,
            SHOW_MAX_SIZE);
    }

    /**
     * Show fragment of data as printable characters from the given fromPos
     * to the given toPos with a maximum characters limit. The fragment string
     * is left aligned at the fromPos position of data.
     *
     * @param data data bytes.
     * @param fromPos from position.
     * @param toPos to position.
     * @param allowLeadingEclipsis if leading eclipsis is allowed in output.
     * @param maxShowSize maximum show size.
     * @return string fragment from given start to end positions with a maximum
     * characters limit..
     */
    public static String showFragment(final byte[] data, final int fromPos,
        final int toPos, final boolean allowLeadingEclipsis,
        final int maxShowSize) {
        boolean alignAtFromPos = true;
        return showFragment(data, fromPos, toPos, allowLeadingEclipsis,
            maxShowSize, alignAtFromPos);
    }

    /**
     * Show fragment of data as printable characters from the given fromPos
     * to the given toPos with a maximum characters limit. The fragment string
     * is left aligned at the fromPos position of data if alignedAtFromPos
     * is true, or right aligned at the toPos position of the data.
     *
     * @param data data bytes.
     * @param fromPos from position.
     * @param toPos to position.
     * @param allowLeadingEclipsis if leading eclipsis is allowed in output.
     * @param maxShowSize maximum show size.
     * @param alignedAtFromPos true if showing data left aligned with fromPos,
     * or false if showing data right aligned with toPos.
     * @return string fragment from given start to end positions with a maximum
     * characters limit..
     */
    public static String showFragment(final byte[] data, final int fromPos,
        final int toPos, final boolean allowLeadingEclipsis,
        int maxShowSize, final boolean alignedAtFromPos) {
        if (fromPos > toPos || fromPos > data.length || toPos > data.length) {
            // some parameters do not make sense. something is wrong.
            return null;
        }
        if (maxShowSize < 0) {
            maxShowSize = SHOW_MAX_SIZE;
        }
        StringBuffer buf = new StringBuffer();
        int delta = toPos - fromPos;
        int showSize = delta;
        if (delta > maxShowSize) {
            showSize = maxShowSize;
        }
        if (alignedAtFromPos) {
            if (fromPos > 0 && allowLeadingEclipsis) {
                buf.append(ECLIPSIS);
            }
            for (int i = fromPos; i < fromPos + showSize; i++) {
                buf.append(Misc.printable(data[i]));
            }
            if (delta > maxShowSize) {
                buf.append(ECLIPSIS);
            }
        } else {
            if ((fromPos > 0 && allowLeadingEclipsis)
                || (fromPos == 0 && delta > maxShowSize) ) {
                buf.append(ECLIPSIS);
            }
            for (int i = toPos - showSize; i < toPos; i++) {
                buf.append(Misc.printable(data[i]));
            }
        }
        return buf.toString();
    }
}
