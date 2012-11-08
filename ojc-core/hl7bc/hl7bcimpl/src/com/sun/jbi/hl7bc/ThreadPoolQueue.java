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
 * @(#)ThreadPoolQueue.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc;

import java.util.Collection;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Raghunadh
 * 
 */
public class ThreadPoolQueue extends LinkedBlockingQueue<Runnable> {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private ThreadPoolExecutor threadPoolExecutor;

    /**
     * 
     */
    public ThreadPoolQueue() {
	// TODO Auto-generated constructor stub
    }

    /**
     * @param capacity
     */
    public ThreadPoolQueue(int capacity) {
	super(capacity);
	// TODO Auto-generated constructor stub
    }

    /**
     * @param c
     */
    @SuppressWarnings("unchecked")
    public ThreadPoolQueue(Collection c) {
	super(c);
	// TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.LinkedBlockingQueue#offer(java.lang.Object)
     */

    private Lock lock = new ReentrantLock();

    @Override
    public boolean offer(Runnable o) {

	boolean b1 = false;
	boolean b2 = false;
	try {
	    lock.lock();

	    b1 = threadPoolExecutor.getPoolSize() < threadPoolExecutor
		    .getMaximumPoolSize();

	    b2 = threadPoolExecutor.getPoolSize()
		    - threadPoolExecutor.getActiveCount() < 3;

	} finally {
	    lock.unlock();
	}

	if (b1 && b2) {
	    return false;
	}
	return super.offer(o);
    }

    public ThreadPoolExecutor getThreadPoolExecutor() {
	return threadPoolExecutor;
    }

    public void setThreadPoolExecutor(ThreadPoolExecutor threadPoolExecutor) {
	this.threadPoolExecutor = threadPoolExecutor;
    }
}
