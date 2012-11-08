/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.event;

import java.util.StringTokenizer;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class EventId {
    /**
     * DOCUMENT ME!
     */
    public static final EventId UNKNOWN = new EventId("unknown");
    private String eventIdStr;
    private String[] eventSubIds;

    /**
     * Creates a new EventId object.
     *
     * @param s DOCUMENT ME!
     */
    public EventId(String s) {
        eventIdStr = s;
        eventSubIds = toSubIds(s);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getId() {
        return eventIdStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getSubIds() {
        return eventSubIds;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return eventIdStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param o DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(Object o) {
        if (o instanceof EventId) {
            return equals((EventId) o);
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
        return eventIdStr.hashCode();
    }

    /**
     * DOCUMENT ME!
     *
     * @param id DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(EventId id) {
        if (eventSubIds.length != id.eventSubIds.length) {
            return false;
        }

        for (int i = 0; i < eventSubIds.length; i++)
            if (!eventSubIds[i].equals(id.eventSubIds[i])) {
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
    public boolean matchesFilter(EventId filter) {
        if (filter.eventSubIds.length > eventSubIds.length) {
            return false;
        }

        for (int i = 0; i < filter.eventSubIds.length; i++) {
            if (filter.eventSubIds[i].equals("*")) {
                continue;
            }

            if (!filter.eventSubIds[i].equals(eventSubIds[i])) {
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
        StringTokenizer st = new StringTokenizer(s, ".");
        String[] tokens = new String[st.countTokens()];
        int idx = 0;

        while (st.hasMoreTokens()) {
            tokens[idx++] = st.nextToken();
        }

        return tokens;
    }
}
