/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.event;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class GeneralLocalEvent extends NetEvent {
    /**
     * DOCUMENT ME!
     */
    Object data;

    /**
     * DOCUMENT ME!
     */
    long timestamp = 0;

    /**
     * Creates a new GeneralLocalEvent object.
     *
     * @param eType DOCUMENT ME!
     * @param eid DOCUMENT ME!
     * @param data DOCUMENT ME!
     */
    public GeneralLocalEvent(int eType, String eid, Object data) {
        super(eType, Events.EVENT_SOURCE_TYPE_LOCAL);
        this.data = data;
        setId(new EventId(eid));
    }

    /**
     * Creates a new GeneralLocalEvent object.
     *
     * @param eType DOCUMENT ME!
     * @param id DOCUMENT ME!
     * @param data DOCUMENT ME!
     */
    public GeneralLocalEvent(int eType, EventId id, Object data) {
        super(eType, Events.EVENT_SOURCE_TYPE_LOCAL);
        this.data = data;
        setId(id);
    }

    /**
     * DOCUMENT ME!
     *
     * @param time DOCUMENT ME!
     */
    public void setTimestamp(long time) {
        timestamp = time;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Object getData() {
        return data;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getFullMessage() {
        return "GeneralLocalEvent";
    }
}
