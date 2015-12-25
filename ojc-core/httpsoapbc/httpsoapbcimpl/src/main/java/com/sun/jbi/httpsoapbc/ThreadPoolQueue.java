/**
 * 
 */
package com.sun.jbi.httpsoapbc;

import java.util.Collection;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Sujit Biswas
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
