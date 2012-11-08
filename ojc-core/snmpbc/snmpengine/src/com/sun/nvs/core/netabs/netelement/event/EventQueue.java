/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.event;

import java.util.ArrayList;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class EventQueue {
    private static int _queueIdSeq = 0;

    /**
     * DOCUMENT ME!
     */
    public static final int DROP_POLICY_NONE = 0;

    /**
     * DOCUMENT ME!
     */
    public static final int DROP_POLICY_FIRST = 1;

    /**
     * DOCUMENT ME!
     */
    public static final int DROP_POLICY_LAST = 2;
    private int dropPolicy = DROP_POLICY_NONE;
    private ArrayList queue = new ArrayList(32);
    private int queueSizeLimit = -1;
    private int numDropped = 0;
    private boolean isValid = true;
    private Integer queueNum = null;

    /**
     * Creates a new EventQueue object.
     */
    public EventQueue() {
        queueNum = new Integer(nextId());
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    static synchronized int nextId() {
        return _queueIdSeq++;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Object getQueueId() {
        return queueNum;
    }

    /**
     * DOCUMENT ME!
     *
     * @param obj DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(Object obj) {
        if (obj instanceof EventQueue) {
            EventQueue other = (EventQueue) obj;

            return other.queueNum.equals(queueNum);
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void add(NetEvent event) {
        synchronized (this) {
            if (dropPolicy != DROP_POLICY_NONE) {
                int size = queue.size();

                if ((queueSizeLimit > 0) && (size > queueSizeLimit)) {
                    if (dropPolicy == DROP_POLICY_LAST) {
                        numDropped++;

                        return;
                    } else {
                        numDropped++;
                        queue.remove(0);
                    }
                }
            }

            queue.add(event);
            notify();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public synchronized boolean isValid() {
        return isValid;
    }

    /**
     * DOCUMENT ME!
     */
    public synchronized void invalidate() {
        isValid = false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public synchronized int getNumDropped() {
        return numDropped;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getDropPolicy() {
        return dropPolicy;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getSizeLimit() {
        return queueSizeLimit;
    }

    /**
     * DOCUMENT ME!
     */
    public synchronized void setNoDropPolicy() {
        queueSizeLimit = -1;
        dropPolicy = DROP_POLICY_NONE;
    }

    /**
     * DOCUMENT ME!
     *
     * @param limit DOCUMENT ME!
     */
    public synchronized void setFirstDropPolicy(int limit) {
        queueSizeLimit = limit;
        dropPolicy = DROP_POLICY_FIRST;
    }

    /**
     * DOCUMENT ME!
     *
     * @param limit DOCUMENT ME!
     */
    public synchronized void setLastDropPolicy(int limit) {
        queueSizeLimit = limit;
        dropPolicy = DROP_POLICY_LAST;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public NetEvent getNext() {
        synchronized (this) {
            try {
                while (queue.isEmpty()) {
                    wait();
                }
            } catch (InterruptedException e) {
                return null;
            }

            return (NetEvent) queue.remove(0);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isEmpty() {
        synchronized (this) {
            return (queue.isEmpty());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int size() {
        synchronized (this) {
            return queue.size();
        }
    }
}
