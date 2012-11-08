/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ThrottlingQueue.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.runtime.InvocationUnit;

/**
 * Utility to throttle service invocations.
 * @author Kevan Simpson
 */
public class ThrottlingQueue {
    private Map<EndpointInfo, Integer> mThrottlingCount;
    private Map<EndpointInfo, Queue<InvocationUnit>> mQueue;
    
    public ThrottlingQueue() {
        mThrottlingCount = new HashMap<EndpointInfo, Integer>();
        mQueue = new HashMap<EndpointInfo, Queue<InvocationUnit>>();
    }
    
    /**
     * Pushes the specified {@link InvocationUnit} in a queue to be
     * executed later, when throttling constraints ease.
     * @param unit The queued activity.
     */
    public void queue(InvocationUnit unit) {
        if (unit != null) {
            synchronized (mQueue) {
                EndpointInfo info = ((Invocation) unit.getActivity()).getInfo();
                Queue<InvocationUnit> fifo = mQueue.get(info);
                if (fifo == null) {
                    fifo = new LinkedList<InvocationUnit>();
                    mQueue.put(info, fifo);
                }
                
                fifo.add(unit);
            }
        }
    }
    
    /**
     * Removes and returns the next queued {@link InvocationUnit} to be
     * executed.
     * 
     * @param info The consuming endpoint related to the queued activity.
     * @return the next queued activity or <code>null</code>.
     */
    public InvocationUnit dequeue(EndpointInfo info) {
        if (info != null) {
            synchronized (mQueue) {
                Queue<InvocationUnit> fifo = mQueue.get(info);
                return (fifo != null && !fifo.isEmpty()) 
                        ? fifo.remove() : null;
            }
        }
    
        return null;
    }
    
    /**
     * Fetches the number of outstanding invocations for the specified endpoint.
     * @param info The specified consuming endpoint.
     * @return the number of outstanding invocations for the specified endpoint.
     */
    public int getInvokeCount(EndpointInfo info) {
        synchronized (mThrottlingCount) {
            Integer count = mThrottlingCount.get(info);
            return (count == null) ? 0 : count.intValue();
        }
    }
    
    /**
     * Increments the throttling count for the specified consuming endpoint.
     * @param info The specified consuming endpoint.
     */
    public void increment(EndpointInfo info) {
        update(info, 1);
    }
    
    /**
     * Decrements the throttling count for the specified consuming endpoint.
     * @param info The specified consuming endpoint.
     */
    public void decrement(EndpointInfo info) {
        update(info, -1);
    }
    
    /** Implementation of {@link #increment} and {@link #decrement}. */ 
    private void update(EndpointInfo info, int delta) {
        if (info != null) {
            synchronized (mThrottlingCount) {
                Integer count = mThrottlingCount.get(info);
                if (count == null) {
                    if (delta > 0) {
                        mThrottlingCount.put(info, Integer.valueOf(1));
                    }
                }
                else {
                    mThrottlingCount.put(info, Integer.valueOf(delta + count.intValue()));
                }
            }
        }
    }
}
