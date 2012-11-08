/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.util;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public final class Enumerator {
    private Hashtable enumMappings = new Hashtable();
    private Hashtable descrToValue = new Hashtable();
    private String[] names = null;
    private Class enumClass;

    /**
     * Creates a new Enumerator object.
     *
     * @param enumClass DOCUMENT ME!
     */
    public Enumerator(Class enumClass) {
        this.enumClass = enumClass;

        try {
            initMappings();
        } catch (Exception e) {
        }
    }

    private void initMappings() {
        try {
            Vector v = new Vector();

            Class c = enumClass;
            Field[] f = c.getFields();

            for (int i = 0; i < f.length; i++) {
                if (!f[i].getType().equals(int.class)) {
                    continue;
                }

                int mod = f[i].getModifiers();

                if (Modifier.isStatic(mod) && Modifier.isFinal(mod)) {
                    Integer val = (Integer) f[i].get(null);
                    String name = f[i].getName();
                    String label = name;

                    try {
                        Field nf = c.getField(name + "_label");

                        if (nf != null) {
                            label = (String) nf.get(null);
                        }
                    } catch (Exception e) {
                    }

                    v.addElement(label);
                    descrToValue.put(label, val);
                    enumMappings.put(val, label);
                }
            }

            names = new String[v.size()];

            v.toArray(names);

            v = null;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public final Class getEnumClass() {
        return enumClass;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public final String[] getAllLabels() {
        return names;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public final int[] getAllLValues() {
        Integer[] vals = new Integer[enumMappings.size()];

        enumMappings.keySet().toArray(vals);

        int[] valsI = new int[vals.length];

        for (int i = 0; i < vals.length; i++) {
            valsI[i] = vals[i].intValue();
        }

        return valsI;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public final int toValue(String s) {
        Integer I = (Integer) descrToValue.get(s);

        if (I == null) {
            return -1;
        } else {
            return I.intValue();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param flag DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public final String toLabel(int flag) {
        String s = (String) enumMappings.get(new Integer(flag));

        if (s == null) {
            return "" + flag;
        } else {
            return s;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param hint DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public final int toFlag(String hint) {
        Enumeration en = descrToValue.keys();

        Vector match = new Vector();

        while (en.hasMoreElements()) {
            String descr = (String) en.nextElement();

            if (descr.endsWith(hint)) {
                Integer capFlag = (Integer) descrToValue.get(descr);
                match.add(capFlag);
            }
        }

        if (match.size() == 1) {
            return ((Integer) match.get(0)).intValue();
        } else {
            return -1;
        }
    }
}
