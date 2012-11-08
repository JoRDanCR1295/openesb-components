/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import java.util.StringTokenizer;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public final class EntityId {
    /**
     * DOCUMENT ME!
     */
    public static final String ENTITY_SEP_CHAR = "/";
    private String entityIdStr;
    private String[] entitySubIds;

    /**
     * Creates a new EntityId object.
     *
     * @param s DOCUMENT ME!
     */
    public EntityId(String s) {
        entitySubIds = toSubIds(s);
        buildIdStr();
    }

    /**
     * Creates a new EntityId object.
     *
     * @param subIds DOCUMENT ME!
     * @param length DOCUMENT ME!
     */
    public EntityId(String[] subIds, int length) {
        entitySubIds = new String[length];

        for (int i = 0; i < length; i++) {
            entitySubIds[i] = subIds[i];
        }

        buildIdStr();
    }

    /**
     * Creates a new EntityId object.
     *
     * @param parent DOCUMENT ME!
     * @param childSubId DOCUMENT ME!
     */
    public EntityId(EntityId parent, String childSubId) {
        this(parent.getId() + ENTITY_SEP_CHAR + childSubId);
    }

    private void buildIdStr() {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < entitySubIds.length; i++) {
            sb.append(ENTITY_SEP_CHAR + entitySubIds[i]);
        }

        entityIdStr = sb.toString();

        if (!entityIdStr.startsWith(ENTITY_SEP_CHAR)) {
            entityIdStr = ENTITY_SEP_CHAR + entityIdStr;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getId() {
        return entityIdStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getSubIds() {
        return entitySubIds;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public EntityId getParent() {
        if ((entityIdStr.length() == 0) || (entitySubIds.length == 0)) {
            return null;
        }

        return new EntityId(entitySubIds, entitySubIds.length - 1);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return entityIdStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param o DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(Object o) {
        if (o instanceof EntityId) {
            return equals((EntityId) o);
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int hashCode() {
        return entityIdStr.hashCode();
    }

    /**
     * DOCUMENT ME!
     *
     * @param id DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(EntityId id) {
        if (entitySubIds.length != id.entitySubIds.length) {
            return false;
        }

        for (int i = 0; i < entitySubIds.length; i++)
            if (!entitySubIds[i].equals(id.entitySubIds[i])) {
                return false;
            }

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param filter DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean matchesFilter(EntityId filter) {
        if (filter.entitySubIds.length > entitySubIds.length) {
            return false;
        }

        for (int i = 0; i < filter.entitySubIds.length; i++) {
            if (filter.entitySubIds[i].equals("*")) {
                continue;
            }

            if (!filter.entitySubIds[i].equals(entitySubIds[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String[] toSubIds(String s) {
        StringTokenizer st = new StringTokenizer(s, ENTITY_SEP_CHAR);
        String[] tokens = new String[st.countTokens()];
        int idx = 0;

        while (st.hasMoreTokens()) {
            tokens[idx++] = st.nextToken();
        }

        return tokens;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isNetworkEntityId() {
        if (entitySubIds[0].equals("network")) {
            return true;
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isServerEntityId() {
        if (entitySubIds[0].equals("server")) {
            return true;
        }

        return false;
    }
}
