/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.protocol.snmp;

import java.util.Vector;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class SnmpWalkResult implements SnmpConstants {
    private int errorStatus = 0;
    private int errorIndex = 0;
    private Vector oidsList = new Vector();
    private Vector valueList = new Vector();
    private Vector indexList = new Vector();

    /**
     * Creates a new SnmpWalkResult object.
     */
    public SnmpWalkResult() {
    }

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
     * @param est DOCUMENT ME!
     */
    public void setErrorStatus(int est) {
        errorStatus = est;
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
     * @param ei DOCUMENT ME!
     */
    public void setErrorIndex(int ei) {
        errorIndex = ei;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getNumRows() {
        return oidsList.size();
    }

    /**
     * DOCUMENT ME!
     *
     * @param row DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getVarNames(int row) {
        return (String[]) oidsList.elementAt(row);
    }

    /**
     * DOCUMENT ME!
     *
     * @param row DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Object[] getValues(int row) {
        return (Object[]) valueList.elementAt(row);
    }

    /**
     * DOCUMENT ME!
     *
     * @param row DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getStringValues(int row) {
        Object[] objs = getValues(row);
        String[] vals = new String[objs.length];

        for (int i = 0; i < objs.length; i++) {
            vals[i] = "" + objs[i];
        }

        return vals;
    }

    /**
     * DOCUMENT ME!
     *
     * @param r DOCUMENT ME!
     */
    public void addVarNames(String[] r) {
        oidsList.addElement(r);
    }

    /**
     * DOCUMENT ME!
     *
     * @param vals DOCUMENT ME!
     */
    public void addValues(Object[] vals) {
        valueList.addElement(vals);
    }

    /**
     * DOCUMENT ME!
     *
     * @param ind DOCUMENT ME!
     */
    public void addIndices(int[] ind) {
        indexList.addElement(ind);
    }

    /**
     * DOCUMENT ME!
     *
     * @param row DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int[] getIndices(int row) {
        return (int[]) indexList.elementAt(row);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return "SnmpWalkResult [NumRows = " + getNumRows() + "]";
    }
}
