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
public class XMLTableFormat extends TableFormat {
    private StringBuffer _buffer = new StringBuffer();
    private String[] header = null;

    /**
     * Creates a new XMLTableFormat object.
     */
    protected XMLTableFormat() {
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     */
    public void addHeaderRow(String[] values) {
        header = values;
    }

    /**
     * DOCUMENT ME!
     *
     * @param heads DOCUMENT ME!
     */
    public void deduceEmptyHeaders(String[] heads) {
        header = heads;
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     * @param beginAttr DOCUMENT ME!
     * @param endAttr DOCUMENT ME!
     */
    public void addRow(String[] values, String[] beginAttr, String[] endAttr) {
        addRow(values);
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     * @param beginAttr DOCUMENT ME!
     * @param endAttr DOCUMENT ME!
     */
    public void addRow(String[] values, String beginAttr, String endAttr) {
        addRow(values);
    }

    /**
     * DOCUMENT ME!
     *
     * @param values DOCUMENT ME!
     */
    public void addRow(String[] values) {
        _buffer.append("<Row>");

        for (int i = 0; i < values.length; i++) {
            if (!isEmptyHeader(i)) {
                _buffer.append("<Col>");
                _buffer.append(Formats.escape(values[i]));
                _buffer.append("</Col>");
            }
        }

        _buffer.append("</Row>");
    }

    private boolean isEmptyHeader(int i) {
        try {
            String str = header[i].trim();

            if (str.length() == 0) {
                return true;
            }

            return false;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void addSeperatorRow() {
    }

    /**
     * DOCUMENT ME!
     *
     * @param cols DOCUMENT ME!
     */
    public void addSeperatorRow(String[] cols) {
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
     * @return DOCUMENT ME!
     */
    public String toString() {
        StringBuffer _sb = new StringBuffer();
        //_sb.append("<?xml version='1.0' ?>\n");
        //_sb.append("<TableData>");
        _sb.append(_buffer.toString());

        //_sb.append("</TableData>");
        return _sb.toString();
    }
}
