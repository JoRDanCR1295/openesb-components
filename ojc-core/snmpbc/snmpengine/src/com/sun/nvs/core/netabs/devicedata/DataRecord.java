/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.devicedata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class DataRecord {
    /**
     * DOCUMENT ME!
     */
    public static final boolean doValidation = true;
    private RecordMetaData metaData;

    /**
     * DOCUMENT ME!
     */
    protected HashMap map = new HashMap();

    /**
     * DOCUMENT ME!
     */
    protected ArrayList<DataRecord> children = new ArrayList<DataRecord>();

    /**
     * Creates a new DataRecord object.
     *
     * @param metaData DOCUMENT ME!
     */
    public DataRecord(RecordMetaData metaData) {
        this.metaData = metaData;

        if (doValidation) {
            if (metaData == null) {
                throw new NullPointerException("data record has null meta data object");
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public RecordMetaData getMetaData() {
        return metaData;
    }

    /**
     * DOCUMENT ME!
     *
     * @param rec DOCUMENT ME!
     */
    public void addChild(DataRecord rec) {
        children.add(rec);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public List<DataRecord> getChildren() {
        return children;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws MetaDataComplianceException DOCUMENT ME!
     */
    public void validate() throws MetaDataComplianceException {
        String[] attrNames = metaData.getAttributeNames();

        for (int i = 0; i < attrNames.length; i++) {
            AttributeMetaData am = metaData.getAttributeMetaData(attrNames[i]);
            Object val = map.get(attrNames[i]);

            if ((val == null) && am.isRequired()) {
                throw new MetaDataComplianceException(
                    "Attribute " + attrNames[i] + " is required but missing."
                );
            }

            if (val == null) {
                continue;
            }

            if (!am.getAttributeType().isAssignableFrom(val.getClass())) {
                throw new MetaDataComplianceException(
                    "Attribute " + attrNames[i] + " expected type " + am.getAttributeType() +
                    ", but found " + val.getClass()
                );
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param value DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     * @throws MetaDataComplianceException DOCUMENT ME!
     */
    public void setAttribute(String name, Object value)
        throws InvalidAttributeException, MetaDataComplianceException {
        if (doValidation) {
            AttributeMetaData am = metaData.getAttributeMetaData(name);

            if (am == null) {
                throw new InvalidAttributeException("no such attribute defined " + name);
            }

            if (am.isRequired() && (value == null)) {
                throw new MetaDataComplianceException(
                    "Attribute " + name + " is a required field. but value is null"
                );
            }

            if (value != null) {
                Class c = value.getClass();

                if (!am.getAttributeType().isAssignableFrom(c)) {
                    throw new MetaDataComplianceException(
                        "Attribute " + name + " expected type is " + am.getAttributeType() +
                        ", but found " + c
                    );
                }
            }
        }

        map.put(name, value);
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public Object getAttribute(String name) throws InvalidAttributeException {
        if (doValidation) {
            if (metaData.getAttributeMetaData(name) == null) {
                throw new InvalidAttributeException("no such attribute defined " + name);
            }
        }

        return map.get(name);
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasAttribute(String name) {
        return (map.get(name) != null);
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public int getAttributeAsInt(String name) throws InvalidAttributeException {
        Integer I = (Integer) getAttribute(name);

        return I.intValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param defValue DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public int getAttributeAsInt(String name, int defValue)
        throws InvalidAttributeException {
        Integer I = (Integer) getAttribute(name);

        if (I == null) {
            return defValue;
        }

        return I.intValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public long getAttributeAsLong(String name) throws InvalidAttributeException {
        Long L = (Long) getAttribute(name);

        return L.longValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param defValue DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public long getAttributeAsLong(String name, long defValue)
        throws InvalidAttributeException {
        Long L = (Long) getAttribute(name);

        if (L == null) {
            return defValue;
        }

        return L.longValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param defValue DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public byte getAttributeAsByte(String name, byte defValue)
        throws InvalidAttributeException {
        Byte B = (Byte) getAttribute(name);

        if (B == null) {
            return defValue;
        }

        return B.byteValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public short getAttributeAsShort(String name) throws InvalidAttributeException {
        Short L = (Short) getAttribute(name);

        return L.shortValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param defValue DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public short getAttributeAsShort(String name, short defValue)
        throws InvalidAttributeException {
        Short L = (Short) getAttribute(name);

        if (L == null) {
            return defValue;
        }

        return L.shortValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public boolean getAttributeAsBoolean(String name) throws InvalidAttributeException {
        Boolean L = (Boolean) getAttribute(name);

        return L.booleanValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     * @param defValue DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public boolean getAttributeAsBoolean(String name, boolean defValue)
        throws InvalidAttributeException {
        Boolean L = (Boolean) getAttribute(name);

        if (L == null) {
            return defValue;
        }

        return L.booleanValue();
    }

    /**
     * DOCUMENT ME!
     *
     * @param name DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws InvalidAttributeException DOCUMENT ME!
     */
    public String getAttributeAsString(String name) throws InvalidAttributeException {
        return (String) getAttribute(name);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toXmlString() {
        return toXmlString(false);
    }

    /**
     * DOCUMENT ME!
     *
     * @param recursive DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toXmlString(boolean recursive) {
        StringBuffer sb = new StringBuffer();
        String[] attrs = metaData.getAttributeNames();

        sb.append("<" + metaData.getRecordTypeName() + ">");

        for (int i = 0; i < attrs.length; i++) {
            String attrName = attrs[i];

            try {
                Object value = getAttribute(attrName);
                System.err.println("  > " + attrName + " = " + value);

                if (value != null) {
                    sb.append(
                        "    <" + attrName + ">" + value.toString() + "</" + attrName + ">\n"
                    );
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        sb.append("</" + metaData.getRecordTypeName() + ">\n");

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toNormalizedXmlString() {
        return toNormalizedXmlString(false);
    }

    /**
     * DOCUMENT ME!
     *
     * @param recursive DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toNormalizedXmlString(boolean recursive) {
        StringBuffer sb = new StringBuffer();
        String[] attrs = metaData.getAttributeNames();

        sb.append("<Row Name='" + metaData.getRecordTypeName() + "'>");

        for (int i = 0; i < attrs.length; i++) {
            String attrName = attrs[i];
            sb.append("<Col Name='" + attrName + "' Type='String' Visible='true'>");

            try {
                Object value = getAttribute(attrName);

                if (value != null) {
                    sb.append(value.toString());
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

            sb.append("</Col>\n");
        }

        sb.append("</Row>\n");

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getStringValues() {
        return getStringValues(metaData.getAttributeNames());
    }

    /**
     * DOCUMENT ME!
     *
     * @param attrs DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getStringValues(String[] attrs) {
        String[] values = new String[attrs.length];

        for (int i = 0; i < attrs.length; i++) {
            String attrName = attrs[i];

            try {
                Object value = getAttribute(attrName);

                if (value != null) {
                    values[i] = value.toString();
                } else {
                    values[i] = "-";
                }
            } catch (Exception e) {
                values[i] = "-";
            }
        }

        return values;
    }
}
