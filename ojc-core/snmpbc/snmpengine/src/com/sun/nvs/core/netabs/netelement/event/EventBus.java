/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.event;

import java.util.ArrayList;
import java.util.HashMap;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class EventBus implements EventBusIf {
    private static EventBus _instance = new EventBus();
    private ArrayList subscribers = new ArrayList();
    private HashMap deliveredQueues = new HashMap();

    private EventBus() {
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static EventBusIf instance() {
        return _instance;
    }

    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public synchronized void publish(NetEvent event) {
        deliveredQueues.clear();

        boolean requiresCleanup = false;

        int sz = subscribers.size();

        for (int i = 0; i < sz; i++) {
            Subscriber s = (Subscriber) subscribers.get(i);

            if (!s.isValid()) {
                requiresCleanup = true;

                continue;
            }

            if (deliveredQueues.containsKey(s.queue.getQueueId())) {
                continue;
            }

            if (s.deliver(event)) {
                deliveredQueues.put(s.queue.getQueueId(), s);
            }
        }

        if (requiresCleanup) {
            ArrayList newList = new ArrayList();

            for (int i = 0; i < sz; i++) {
                Subscriber s = (Subscriber) subscribers.get(i);

                if (s.isValid()) {
                    newList.add(s);
                }
            }

            subscribers = newList;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param idFilter DOCUMENT ME!
     * @param queue DOCUMENT ME!
     */
    public void subscribe(EventId idFilter, EventQueue queue) {
        subscribe(-1, -1, idFilter, queue);
    }

    /**
     * DOCUMENT ME!
     *
     * @param eventTypeFilter DOCUMENT ME!
     * @param idFilter DOCUMENT ME!
     * @param queue DOCUMENT ME!
     */
    public void subscribe(int eventTypeFilter, EventId idFilter, EventQueue queue) {
        subscribe(-1, eventTypeFilter, idFilter, queue);
    }

    /**
     * DOCUMENT ME!
     *
     * @param eventTypeFilter DOCUMENT ME!
     * @param sourceTypeFilter DOCUMENT ME!
     * @param idFilter DOCUMENT ME!
     * @param queue DOCUMENT ME!
     */
    public synchronized void subscribe(
        int eventTypeFilter, int sourceTypeFilter, EventId idFilter, EventQueue queue
    ) {
        subscribers.add(new Subscriber(eventTypeFilter, sourceTypeFilter, idFilter, queue));
    }

    /**
     * DOCUMENT ME!
     *
     * @author $author$
     * @version $Revision: 1.1 $
      */
    static class Subscriber {
        //~ Instance fields ------------------------------------------------------------------------

        /**
         * DOCUMENT ME!
         */
        int eventTypeFilter;

        /**
         * DOCUMENT ME!
         */
        int sourceTypeFilter;

        /**
         * DOCUMENT ME!
         */
        EventId idFilter;

        /**
         * DOCUMENT ME!
         */
        EventQueue queue;

        //~ Constructors ---------------------------------------------------------------------------

        /**
         * Creates a new Subscriber object.
         *
         * @param etf DOCUMENT ME!
         * @param stf DOCUMENT ME!
         * @param id DOCUMENT ME!
         * @param q DOCUMENT ME!
         */
        Subscriber(int etf, int stf, EventId id, EventQueue q) {
            eventTypeFilter = etf;
            sourceTypeFilter = stf;
            idFilter = id;
            queue = q;
        }

        //~ Methods --------------------------------------------------------------------------------

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public boolean isValid() {
            return queue.isValid();
        }

        /**
         * DOCUMENT ME!
         *
         * @param event DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public boolean deliver(NetEvent event) {
            if (eventTypeFilter != -1) {
                if (event.getEventType() != eventTypeFilter) {
                    return false;
                }
            }

            if (sourceTypeFilter != -1) {
                if (event.getEventSourceType() != sourceTypeFilter) {
                    return false;
                }
            }

            if (idFilter != null) {
                if (!event.getId().matchesFilter(idFilter)) {
                    return false;
                }
            }

            queue.add(event);

            return true;
        }
    }
}
