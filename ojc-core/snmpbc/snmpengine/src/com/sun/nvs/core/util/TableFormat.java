/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.util;

import java.util.Vector;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class TableFormat {
    private static final String rowSepStr = "-------------------------------------------------" +
        "------------------------------------------------------------------";

    /**
     * DOCUMENT ME!
     */
    public static final boolean LEFT_ALIGN = true;

    /**
     * DOCUMENT ME!
     */
    public static final boolean RIGHT_ALIGN = false;

    /**
     * DOCUMENT ME!
     */
    public static final String SPACES0 = "                                                                   ";

    /**
     * DOCUMENT ME!
     */
    public static final String SPACES = SPACES0 + SPACES0 + SPACES0 + SPACES0 + SPACES0;
    private StringBuffer _buffer = new StringBuffer();
    private String _rowPrefix = "";
    private int[] _colWidths = null;
    private boolean[] _leftAlign = null;
    private String _colSpace = null;

    /**
     * Creates a new TableFormat object.
     */
    protected TableFormat() {
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     */
    public void specifyAttributes(int[] colWidths, String colSpace, boolean[] leftAlign) {
        specifyAttributes("", colWidths, colSpace, leftAlign);
    }

    /**
     * DOCUMENT ME!
     *
     * @param rowPrefix DOCUMENT ME!
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     */
    public void specifyAttributes(
        String rowPrefix, int[] colWidths, String colSpace, boolean[] leftAlign
    ) {
        _rowPrefix = rowPrefix;
        _colWidths = autoAdjustColumnWidth(colWidths);
        _leftAlign = leftAlign;
        _colSpace = colSpace;

        if (_leftAlign == null) {
            _leftAlign = new boolean[_colWidths.length];

            for (int i = 0; i < _leftAlign.length; i++) {
                _leftAlign[i] = true;
            }
        }

        if ((_leftAlign != null) && (colWidths.length != _leftAlign.length)) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and align");
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param heads DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     */
    public void specifyAttributes(
        int[] colWidths, String colSpace, String[] heads, boolean[] leftAlign
    ) {
        specifyAttributes("", colWidths, colSpace, leftAlign);

        if (colWidths.length != heads.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and headings");
        }

        addHeaderRow(heads);
    }

    /**
     * DOCUMENT ME!
     *
     * @param rowPrefix DOCUMENT ME!
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param heads DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     */
    public void specifyAttributes(
        String rowPrefix, int[] colWidths, String colSpace, String[] heads, boolean[] leftAlign
    ) {
        specifyAttributes(rowPrefix, colWidths, colSpace, leftAlign);

        if (colWidths.length != heads.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and headings");
        }

        addHeaderRow(heads);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     */
    public void specifyAttributes(int[] colWidths, String colSpace) {
        specifyAttributes("", colWidths, colSpace, (boolean[]) null);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param heads DOCUMENT ME!
     */
    public void specifyAttributes(int[] colWidths, String colSpace, String[] heads) {
        specifyAttributes(colWidths, colSpace);

        addHeaderRow(heads);
    }

    /**
     * DOCUMENT ME!
     *
     * @param rowPrefix DOCUMENT ME!
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param heads DOCUMENT ME!
     */
    public void specifyAttributes(
        String rowPrefix, int[] colWidths, String colSpace, String[] heads
    ) {
        specifyAttributes(colWidths, colSpace);
        this._rowPrefix = rowPrefix;

        if (colWidths.length != heads.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and headings");
        }

        addHeaderRow(heads);
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     */
    public void addHeaderRow(String[] values) {
        String[] beginAttrs = new String[values.length];
        String[] endAttrs = new String[values.length];

        for (int i = 0; i < beginAttrs.length; i++) {
            beginAttrs[i] = "<color:header>";
            endAttrs[i] = "<color:none>";
        }

        addRow(values, beginAttrs, endAttrs);
    }

    /**
     * DOCUMENT ME!
     *
     * @param heads DOCUMENT ME!
     */
    public void deduceEmptyHeaders(String[] heads) {
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     */
    public void addRow(String[] values) {
        if (_colWidths.length != values.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and values");
        }

        _buffer.append(_rowPrefix + formatRow(_colWidths, _colSpace, values, _leftAlign));
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     * @param beginAttr DOCUMENT ME!
     * @param endAttr DOCUMENT ME!
     */
    public void addRow(String[] values, String beginAttr, String endAttr) {
        String[] ba = new String[values.length];
        String[] ea = new String[values.length];

        for (int i = 0; i < ba.length; i++) {
            ba[i] = beginAttr;
            ea[i] = endAttr;
        }

        addRow(values, ba, ea);
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     * @param beginAttrs DOCUMENT ME!
     * @param endAttrs DOCUMENT ME!
     */
    public void addRow(String[] values, String[] beginAttrs, String[] endAttrs) {
        if (_colWidths.length != values.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and values");
        }

        _buffer.append(
            _rowPrefix +
            formatRow(_colWidths, _colSpace, values, _leftAlign, true, beginAttrs, endAttrs)
        );
    }

    /**
     * DOCUMENT ME!
     */
    public void addSeperatorRow() {
        String[] sep = new String[_colWidths.length];

        for (int i = 0; i < _colWidths.length; i++) {
            int w = _colWidths[i];

            if (w < 0) {
                sep[i] = "------------";
            } else if (w == 0) {
                sep[i] = "";
            } else {
                sep[i] = rowSepStr.substring(0, w);
            }
        }

        addRow(sep);
    }

    /**
     * DOCUMENT ME!
     *
     * @param cols DOCUMENT ME!
     */
    public void addSeperatorRow(String[] cols) {
        String[] sep = new String[_colWidths.length];

        for (int i = 0; i < _colWidths.length; i++) {
            if (cols[i].trim().length() == 0) {
                sep[i] = "";

                continue;
            }

            int w = _colWidths[i];

            if (w < 0) {
                sep[i] = "------------";
            } else if (w == 0) {
                sep[i] = "";
            } else {
                sep[i] = rowSepStr.substring(0, w);
            }
        }

        addRow(sep);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String formatSeperatorRow(int[] colWidths, String colSpace) {
        String[] sep = new String[colWidths.length];

        for (int i = 0; i < colWidths.length; i++) {
            int w = colWidths[i];

            if (w < 0) {
                sep[i] = "------------";
            } else if (w == 0) {
                sep[i] = "";
            } else {
                sep[i] = rowSepStr.substring(0, w);
            }
        }

        return formatRow(colWidths, colSpace, sep);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return _buffer.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param values DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String formatRow(int[] colWidths, String colSpace, String[] values) {
        return formatRow(colWidths, colSpace, values, true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param values DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String formatHeaderRow(int[] colWidths, String colSpace, String[] values) {
        String[] beginAttrs = new String[values.length];
        String[] endAttrs = new String[values.length];

        for (int i = 0; i < beginAttrs.length; i++) {
            beginAttrs[i] = "<color:header>";
            endAttrs[i] = "<color:none>";
        }

        return formatRow(colWidths, colSpace, values, null, true, beginAttrs, endAttrs);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param values DOCUMENT ME!
     * @param appendNewLine DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String formatRow(
        int[] colWidths, String colSpace, String[] values, boolean appendNewLine
    ) {
        if (colWidths.length != values.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and values");
        }

        boolean[] leftAlign = new boolean[colWidths.length];

        for (int i = 0; i < leftAlign.length; i++)
            leftAlign[i] = true;

        return formatRow(colWidths, colSpace, values, leftAlign, appendNewLine);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param values DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String formatRow(
        int[] colWidths, String colSpace, String[] values, boolean[] leftAlign
    ) {
        return formatRow(colWidths, colSpace, values, leftAlign, true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param values DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     * @param appendNewLine DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String formatRow(
        int[] colWidths, String colSpace, String[] values, boolean[] leftAlign,
        boolean appendNewLine
    ) {
        return formatRow(colWidths, colSpace, values, leftAlign, appendNewLine, null, null);
    }

    /**
     * DOCUMENT ME!
     *
     * @param cw DOCUMENT ME!
     * @param colSpace DOCUMENT ME!
     * @param values DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     * @param appendNewLine DOCUMENT ME!
     * @param beginAttr DOCUMENT ME!
     * @param endAttr DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String formatRow(
        int[] cw, String colSpace, String[] values, boolean[] leftAlign, boolean appendNewLine,
        String[] beginAttr, String[] endAttr
    ) {
        int[] colWidths = autoAdjustColumnWidth(cw);

        if (leftAlign == null) {
            leftAlign = new boolean[colWidths.length];

            for (int i = 0; i < leftAlign.length; i++)
                leftAlign[i] = true;
        }

        if (colWidths.length != values.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and values");
        }

        if (colWidths.length != leftAlign.length) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and aligns");
        }

        for (int i = 0; i < values.length; i++) {
            if (values[i] == null) {
                values[i] = "-";
            }
        }

        Vector[] data = new Vector[values.length];

        int maxLines = 0;

        for (int i = 0; i < values.length; i++) {
            data[i] = splitAndAlign(values[i], colWidths[i], leftAlign[i]);
            maxLines = Math.max(maxLines, data[i].size());
        }

        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < maxLines; i++) {
            for (int j = 0; j < data.length; j++) {
                if (j != 0) {
                    sb.append(colSpace);
                }

                Vector v = data[j];

                if (
                    (beginAttr == null) || ((endAttr == null) | (beginAttr[j] == null)) ||
                        (endAttr[j] == null)
                ) {
                    if (v.size() <= i) {
                        sb.append(fixLength("", colWidths[j], leftAlign[j]));
                    } else {
                        sb.append(v.elementAt(i).toString());
                    }
                } else {
                    if (v.size() <= i) {
                        sb.append(
                            beginAttr[j] + fixLength("", colWidths[j], leftAlign[j]) + endAttr[j]
                        );
                    } else {
                        sb.append(beginAttr[j] + v.elementAt(i).toString() + endAttr[j]);
                    }
                }
            }

            if (appendNewLine) {
                sb.append("\n");
            }
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param len DOCUMENT ME!
     * @param leftAlign DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static Vector splitAndAlign(String s, int len, boolean leftAlign) {
        s = s.trim();

        Vector result = new Vector();

        if ((s.length() > 0) && (len == 0)) {
            throw new IllegalArgumentException("Invalid array sizes for colWidths and aligns");
        }

        if (len == -1) {
            result.addElement(s);

            return result;
        }

        if (s.length() <= len) {
            result.addElement(fixLength(s, len, leftAlign));

            return result;
        }

        String last = "";
        String first = "";
        int idx = 0;

        while (s.length() > len) {
            first = s.substring(0, len);

            if ((idx = first.lastIndexOf(" ")) < 0) {
                result.addElement(fixLength(first.trim(), len, leftAlign));
                s = s.substring(len);
            } else {
                first = first.substring(0, idx);
                result.addElement(fixLength(first.trim(), len, leftAlign));
                s = s.substring(idx);
            }

            s = s.trim();
        }

        if (s.length() > 0) {
            result.addElement(fixLength(s, len, leftAlign));
        }

        return result;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param len DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String fixLength(String s, int len) {
        return fixLength(s, len, true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param len DOCUMENT ME!
     * @param isLeftAlign DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String fixLength(String s, int len, boolean isLeftAlign) {
        if (isLeftAlign) {
            return fixLengthLeftAlign(s, len);
        } else {
            return fixLengthRightAlign(s, len);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param len DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String fixLengthLeftAlign(String s, int len) {
        if (len < 0) {
            return s;
        }

        s = s + SPACES;

        return s.substring(0, len);
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param len DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String fixLengthRightAlign(String s, int len) {
        if (len < 0) {
            return s;
        }

        s = SPACES + s;

        return s.substring(s.length() - 1 - len);
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param prefix DOCUMENT ME!
     * @param len DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String fixLengthLeftAlign(String s, String prefix, int len) {
        if (len < 0) {
            return s;
        }

        int num = len - s.length();
        int itr = (int) Math.floor((num * 1.0) / prefix.length());
        StringBuffer strB = new StringBuffer();

        for (int i = 0; i < itr; i++) {
            strB.append(prefix);
        }

        s = strB.toString() + s;

        return s.substring(0, len);
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param prefix DOCUMENT ME!
     * @param len DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String fixLengthRightAlign(String s, String prefix, int len) {
        if (len < 0) {
            return s;
        }

        int num = len - s.length();
        int itr = (int) Math.floor((num * 1.0) / prefix.length());
        StringBuffer strB = new StringBuffer();

        for (int i = 0; i < itr; i++) {
            strB.append(prefix);
        }

        s = strB.toString() + s;

        return fixLength(s, len, false);
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     * @param colNumber DOCUMENT ME!
     * @param minWidth DOCUMENT ME!
     * @param maxWidth DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int[] autoAdjustColumnWidth(
        int[] colWidths, int colNumber, int minWidth, int maxWidth
    ) {
        int termWidth = 80; //com.sun.core.north.cli.ThreadIOContext.getTerminalWidth();

        termWidth--;

        if (termWidth <= 0) {
            return colWidths;
        }

        if ((colNumber < 0) || (colNumber >= colWidths.length)) {
            throw new IllegalArgumentException(
                "autoAdjustColumnWidth: colNumber=" + colNumber + " is not in the valid range"
            );
        }

        int sum = 0;
        int[] ret = new int[colWidths.length];

        for (int i = 0; i < colWidths.length; i++) {
            if (i == colNumber) {
                continue;
            }

            if (colWidths[i] < 0) {
                throw new IllegalArgumentException(
                    "cannot auto adjust column " + colNumber + " while column " + i + " is " +
                    colWidths[i]
                );
            }

            sum += (1 + colWidths[i]);
            ret[i] = colWidths[i];
        }

        int remaining = termWidth - sum;

        if (remaining < minWidth) {
            remaining = minWidth;
        } else if (remaining > maxWidth) {
            remaining = maxWidth;
        }

        ret[colNumber] = remaining;

        return ret;
    }

    /**
     * DOCUMENT ME!
     *
     * @param colWidths DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int[] autoAdjustColumnWidth(int[] colWidths) {
        for (int i = 0; i < colWidths.length; i++) {
            if (colWidths[i] < 0) {
                return autoAdjustColumnWidth(colWidths, i, Math.abs(colWidths[i]), 70);
            }
        }

        return colWidths;
    }

    /**
     * DOCUMENT ME!
     */
    public void clearBuffer() {
        _buffer = new StringBuffer();
    }

    /**
     * DOCUMENT ME!
     *
     * @param args DOCUMENT ME!
     */
    public static void main(String[] args) {
        final int[] widths = {10, 12, 13,};

        final String[] values1 = {"Hello World Raju", "Yes, that is Ok", "Simple",};

        final String[] values2 = {
                "Hello ", "Yes, that is Ok, but not real ok",
                "Simple-and-effective-means-of-communication",
            };

        System.err.println(formatRow(widths, " | ", values1));
        System.err.println(formatRow(widths, " | ", values2));
    }
}
