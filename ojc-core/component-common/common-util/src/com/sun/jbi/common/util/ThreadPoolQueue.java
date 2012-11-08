/**
 * 
 */
package com.sun.jbi.common.util;

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

    private ThreadPoolExecutor mThreadPool;
    private Lock lock = new ReentrantLock();

    /**
     * 
     */
    public ThreadPoolQueue() {
    }

    /**
     * @param capacity
     */
    public ThreadPoolQueue(int capacity) {
        super(capacity);
    }

    /**
     * @param c
     */
    @SuppressWarnings("unchecked")
    public ThreadPoolQueue(Collection c) {
        super(c);
    }

    /**
     * Locks and returns <code>false</code> if thread pool is not maxed out
     * AND non-active threads is less than 3, otherwise defers to base class.
     * @see java.util.concurrent.LinkedBlockingQueue#offer(java.lang.Object)
     */
    @Override
    public boolean offer(Runnable o) {
    	boolean b1 = false;
    	boolean b2 = false;
    	try {
    	    lock.lock();
    
    	    b1 = mThreadPool.getPoolSize() < mThreadPool
    		    .getMaximumPoolSize();
    
    	    b2 = mThreadPool.getPoolSize()
    		    - mThreadPool.getActiveCount() < 3;
    
    	} 
    	finally {
    	    lock.unlock();
    	}
    
    	return (b1 && b2) ? false : super.offer(o);
    }

    public ThreadPoolExecutor getThreadPoolExecutor() {
        return mThreadPool;
    }

    public void setThreadPoolExecutor(ThreadPoolExecutor threadPoolExecutor) {
        mThreadPool = threadPoolExecutor;
    }
}
