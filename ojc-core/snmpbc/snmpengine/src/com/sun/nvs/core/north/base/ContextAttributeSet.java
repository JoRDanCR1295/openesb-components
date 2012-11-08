/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.north.base;

import java.util.*;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public final class ContextAttributeSet {
    /**
     * DOCUMENT ME!
     */
    Hashtable attributes = new Hashtable();

    /**
     * Creates a new ContextAttributeSet object.
     */
    public ContextAttributeSet() {
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getStringAttribute(String name) {
        return (String) attributes.get(name);
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param defValue DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getStringAttribute(String name, String defValue) {
        String s = (String) attributes.get(name);

        if (s == null) {
            return defValue;
        } else {
            return s;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Object getAttribute(String name) {
        return attributes.get(name);
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param defValue DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Object getAttribute(String name, Object defValue) {
        Object obj = attributes.get(name);

        if (obj == null) {
            return defValue;
        } else {
            return obj;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getUserAttributeNames() {
        ArrayList list = new ArrayList();
        Enumeration en = attributes.keys();

        while (en.hasMoreElements()) {
            String aName = (String) en.nextElement();

            if (isSettableUserAttribute(aName)) {
                list.add(aName);
            }
        }

        String[] ret = new String[list.size()];
        list.toArray(ret);

        return ret;
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static boolean isSettableUserAttribute(String name) {
        if (name.indexOf("$") >= 0) {
            return false;
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static boolean isReadbleUserAttribute(String name) {
        if (name.indexOf("$") >= 0) {
            return false;
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param value DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public void setAttribute(String name, Object value)
        throws Exception {
        attributes.put(name, value);
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public void unsetAttribute(String name) throws Exception {
        attributes.remove(name);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return attributes.toString();
    }
}
