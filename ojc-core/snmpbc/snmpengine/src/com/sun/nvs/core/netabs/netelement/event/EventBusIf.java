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
public interface EventBusIf {
    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void publish(NetEvent event);

    /**
     * DOCUMENT ME!
     *
     * @param idFilter DOCUMENT ME!
     * @param queue DOCUMENT ME!
     */
    public void subscribe(EventId idFilter, EventQueue queue);

    /**
     * DOCUMENT ME!
     *
     * @param eventTypeFilter DOCUMENT ME!
     * @param idFilter DOCUMENT ME!
     * @param queue DOCUMENT ME!
     */
    public void subscribe(int eventTypeFilter, EventId idFilter, EventQueue queue);

    /**
     * DOCUMENT ME!
     *
     * @param eventTypeFilter DOCUMENT ME!
     * @param sourceTypeFilter DOCUMENT ME!
     * @param idFilter DOCUMENT ME!
     * @param queue DOCUMENT ME!
     */
    public void subscribe(
        int eventTypeFilter, int sourceTypeFilter, EventId idFilter, EventQueue queue
    );
}
