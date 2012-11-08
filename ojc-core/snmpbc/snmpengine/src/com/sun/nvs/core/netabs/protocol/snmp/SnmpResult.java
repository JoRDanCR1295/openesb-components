/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.protocol.snmp;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class SnmpResult implements SnmpConstants {
    private int errorStatus = 0;
    private int errorIndex = 0;
    private String errorString = "";
    private int i;
    private String[] varNames;
    private Object[] values;

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasErrors() {
        return (errorStatus != 0);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getErrorStatus() {
        return errorStatus;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getErrorIndex() {
        return errorIndex;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     */
    public void setErrorString(String s) {
        errorString = s;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getErrorString() {
        return errorString;
    }

    /**
     * DOCUMENT ME!
     *
     * @param status DOCUMENT ME!
     */
    public void setErrorStatus(int status) {
        errorStatus = status;
    }

    /**
     * DOCUMENT ME!
     *
     * @param idx DOCUMENT ME!
     */
    public void setErrorIndex(int idx) {
        errorIndex = idx;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws SnmpException DOCUMENT ME!
     */
    public String[] getVarNames() throws SnmpException {
        return varNames;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Object[] getValues() {
        return values;
    }

    /**
     * DOCUMENT ME!
     *
     * @param r DOCUMENT ME!
     */
    public void setVarNames(String[] r) {
        varNames = r;
    }

    /**
     * DOCUMENT ME!
     *
     * @param vals DOCUMENT ME!
     */
    public void setValues(Object[] vals) {
        values = vals;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();

        if (errorStatus != 0) {
            sb.append("ERROR OCCURED (Code = " + errorStatus + ")\n");
        }

        if (values != null) {
            for (int i = 0; i < values.length; i++) {
                sb.append("=> " + values[i] + "\n");
            }
        }

        return sb.toString();
    }
}
