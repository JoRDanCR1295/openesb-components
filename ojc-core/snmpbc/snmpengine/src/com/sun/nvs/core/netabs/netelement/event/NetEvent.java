/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.event;

import java.net.InetAddress;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public abstract class NetEvent implements java.io.Serializable, Events {
    /**
     * DOCUMENT ME!
     */
    protected int eventType;

    /**
     * DOCUMENT ME!
     */
    protected int eventSourceType;

    /**
     * DOCUMENT ME!
     */
    protected EventId id = EventId.UNKNOWN;

    /**
     * Creates a new NetEvent object.
     *
     * @param eType DOCUMENT ME!
     * @param srcType DOCUMENT ME!
     */
    protected NetEvent(int eType, int srcType) {
        eventType = eType;
        eventSourceType = srcType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public abstract String getFullMessage();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getEventType() {
        return eventType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getEventSourceType() {
        return eventSourceType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public EventId getId() {
        return id;
    }

    /**
     * DOCUMENT ME!
     *
     * @param eid DOCUMENT ME!
     */
    public void setId(EventId eid) {
        id = eid;
    }
}
