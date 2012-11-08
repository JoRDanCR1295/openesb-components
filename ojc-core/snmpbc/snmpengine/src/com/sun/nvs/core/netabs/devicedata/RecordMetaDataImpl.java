/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.devicedata;

import java.util.ArrayList;
import java.util.HashMap;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class RecordMetaDataImpl implements RecordMetaData {
    private String typeName;
    private int typeId;
    private boolean frozen = false;
    private HashMap map = new HashMap();
    private String[] names = new String[0];
    private transient ArrayList list = new ArrayList();

    /**
     * Creates a new RecordMetaDataImpl object.
     *
     * @param typeName DOCUMENT ME!
     * @param typeId DOCUMENT ME!
     */
    public RecordMetaDataImpl(String typeName, int typeId) {
        this.typeName = typeName;
        this.typeId = typeId;
    }

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AttributeMetaData getAttributeMetaData(String attrName) {
        // TODO Auto-generated method stub
        return (AttributeMetaData) map.get(attrName);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getAttributeNames() {
        // TODO Auto-generated method stub
        return names;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getRecordTypeId() {
        // TODO Auto-generated method stub
        return typeId;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getRecordTypeName() {
        // TODO Auto-generated method stub
        return typeName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isFrozen() {
        return frozen;
    }

    /**
     * DOCUMENT ME!
     */
    public void freeze() {
        if (frozen) {
            return;
        }

        frozen = true;

        names = new String[list.size()];
        list.toArray(names);
        list = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     * @param attrType DOCUMENT ME!
     * @param isRequired DOCUMENT ME!
     * @param isIndex DOCUMENT ME!
     */
    public void addAttribute(String attrName, Class attrType, boolean isRequired, boolean isIndex) {
        addAttribute(attrName, attrType, isRequired, isIndex, -1);
    }

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     * @param attrType DOCUMENT ME!
     * @param isRequired DOCUMENT ME!
     * @param isIndex DOCUMENT ME!
     * @param maxDbLen DOCUMENT ME!
     */
    public void addAttribute(
        String attrName, Class attrType, boolean isRequired, boolean isIndex, int maxDbLen
    ) {
        addAttribute(attrName, attrType, isRequired, isIndex, maxDbLen, false);
    }

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     * @param attrType DOCUMENT ME!
     * @param isRequired DOCUMENT ME!
     * @param isIndex DOCUMENT ME!
     * @param maxDbLen DOCUMENT ME!
     * @param isAutoIncrement DOCUMENT ME!
     */
    public void addAttribute(
        String attrName, Class attrType, boolean isRequired, boolean isIndex, int maxDbLen,
        boolean isAutoIncrement
    ) {
        if (frozen) {
            throw new RuntimeException(
                "Invalid addAttribute addition, RecordMetaData is frozen: attribute " + attrName
            );
        }

        if (map.get(attrName) != null) {
            throw new RuntimeException(
                "Invalid addAttribute addition, attribute already exists: " + attrName
            );
        }

        if (!attrType.equals(Integer.class) && isAutoIncrement) {
            throw new RuntimeException(
                "Invalid addAttribute addition, AutoIncrement can be specified only for Integer class Attribute: " +
                attrName
            );
        }

        list.add(attrName);
        map.put(
            attrName,
            new Attribute(attrName, attrType, isRequired, isIndex, maxDbLen, isAutoIncrement)
        );
    }

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     * @param width DOCUMENT ME!
     */
    public void setDisplayWidth(String attrName, int width) {
        if (frozen) {
            throw new RuntimeException(
                "Invalid addAttribute modification, RecordMetaData is frozen: attribute " +
                attrName
            );
        }

        if (map.get(attrName) == null) {
            throw new RuntimeException(
                "Invalid addAttribute modification, attribute does not exist: " + attrName
            );
        }

        Attribute a = (Attribute) map.get(attrName);
        a.setDisplayWidth(width);
    }

    /**
     * DOCUMENT ME!
     *
     * @param attrName DOCUMENT ME!
     * @param dn DOCUMENT ME!
     */
    public void setDisplayName(String attrName, String dn) {
        if (frozen) {
            throw new RuntimeException(
                "Invalid addAttribute modification, RecordMetaData is frozen: attribute " +
                attrName
            );
        }

        if (map.get(attrName) == null) {
            throw new RuntimeException(
                "Invalid addAttribute modification, attribute does not exist: " + attrName
            );
        }

        Attribute a = (Attribute) map.get(attrName);
        a.setDisplayName(dn);
    }

    /**
     * DOCUMENT ME!
     *
     * @author $author$
     * @version $Revision: 1.2 $
      */
    static class Attribute implements AttributeMetaData {
        //~ Instance fields ------------------------------------------------------------------------

        /**
         * DOCUMENT ME!
         */
        String aName;

        /**
         * DOCUMENT ME!
         */
        Class aType;

        /**
         * DOCUMENT ME!
         */
        boolean isRequired;

        /**
         * DOCUMENT ME!
         */
        boolean isIndex;

        /**
         * DOCUMENT ME!
         */
        int displayWidth = 0;

        /**
         * DOCUMENT ME!
         */
        String displayName = null;

        /**
         * DOCUMENT ME!
         */
        int maxDbLen;

        /**
         * DOCUMENT ME!
         */
        boolean isAutoIncrement;

        //~ Constructors ---------------------------------------------------------------------------

        /**
         * Creates a new Attribute object.
         *
         * @param attrName DOCUMENT ME!
         * @param attrType DOCUMENT ME!
         * @param isReq DOCUMENT ME!
         * @param isInd DOCUMENT ME!
         * @param dblen DOCUMENT ME!
         * @param isAutoIncr DOCUMENT ME!
         */
        Attribute(
            String attrName, Class attrType, boolean isReq, boolean isInd, int dblen,
            boolean isAutoIncr
        ) {
            aName = attrName;
            aType = attrType;
            isRequired = isReq;
            isIndex = isInd;
            maxDbLen = dblen;
            isAutoIncrement = isAutoIncr;
        }

        //~ Methods --------------------------------------------------------------------------------

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public String getAttributeName() {
            return aName;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public Class getAttributeType() {
            return aType;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public boolean isRequired() {
            return isRequired;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public boolean isIndex() {
            return isIndex;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public boolean isAutoIncrement() {
            return isAutoIncrement;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public int getDisplayWidth() {
            return displayWidth;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public String getDisplayName() {
            if (displayName != null) {
                return displayName;
            } else {
                return aName;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param w DOCUMENT ME!
         */
        public void setDisplayWidth(int w) {
            displayWidth = w;
        }

        /**
         * DOCUMENT ME!
         *
         * @param dn DOCUMENT ME!
         */
        public void setDisplayName(String dn) {
            displayName = dn;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public int getMaxDbSize() {
            return maxDbLen;
        }
    }
}
